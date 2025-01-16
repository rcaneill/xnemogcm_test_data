#include "field.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"

#include "node_type.hpp"
#include "calendar_util.hpp"
#include "message.hpp"
#include "xios_spl.hpp"
#include "type.hpp"
#include "timer.hpp"
#include "context_client.hpp"
#include "context_server.hpp"
#include <set>
#include "garbage_collector.hpp"
#include "source_filter.hpp"
#include "store_filter.hpp"
#include "file_writer_filter.hpp"
#include "pass_through_filter.hpp"
#include "filter_expr_node.hpp"
#include "lex_parser.hpp"
#include "temporal_filter.hpp"
#include "spatial_transform_filter.hpp"
#include "file_server_writer_filter.hpp"

namespace xios{

   /// ////////////////////// Définitions ////////////////////// ///

   CField::CField(void)
      : CObjectTemplate<CField>(), CFieldAttributes()
      , grid(), file()
      , written(false)
      , nstep(0), nstepMax(0)
      , hasOutputFile(false)
      , domAxisScalarIds_(vector<StdString>(3,""))
      , areAllReferenceSolved(false), isReferenceSolved(false), isReferenceSolvedAndTransformed(false)
      , isGridChecked(false)
      , useCompressedOutput(false)
      , hasTimeInstant(false)
      , hasTimeCentered(false)
      , wasDataRequestedFromServer(false)
      , wasDataAlreadyReceivedFromServer(false)
      , mustAutoTrigger(false)
      , isEOF(false), nstepMaxRead(false)
   { setVirtualVariableGroup(CVariableGroup::create(getId() + "_virtual_variable_group")); }

   CField::CField(const StdString& id)
      : CObjectTemplate<CField>(id), CFieldAttributes()
      , grid(), file()
      , written(false)
      , nstep(0), nstepMax(0)
      , hasOutputFile(false)
      , domAxisScalarIds_(vector<StdString>(3,""))
      , areAllReferenceSolved(false), isReferenceSolved(false), isReferenceSolvedAndTransformed(false)
      , isGridChecked(false)
      , useCompressedOutput(false)
      , hasTimeInstant(false)
      , hasTimeCentered(false)
      , wasDataRequestedFromServer(false)
      , wasDataAlreadyReceivedFromServer(false)
      , mustAutoTrigger(false)
      , isEOF(false), nstepMaxRead(false)
   { setVirtualVariableGroup(CVariableGroup::create(getId() + "_virtual_variable_group")); }

   CField::~CField(void)
   {}

  //----------------------------------------------------------------

   void CField::setVirtualVariableGroup(CVariableGroup* newVVariableGroup)
   {
      this->vVariableGroup = newVVariableGroup;
   }

   CVariableGroup* CField::getVirtualVariableGroup(void) const
   {
      return this->vVariableGroup;
   }

   std::vector<CVariable*> CField::getAllVariables(void) const
   {
      return this->vVariableGroup->getAllChildren();
   }

   void CField::solveDescInheritance(bool apply, const CAttributeMap* const parent)
   {
      SuperClassAttribute::setAttributes(parent, apply);
      this->getVirtualVariableGroup()->solveDescInheritance(apply, NULL);
   }

  //----------------------------------------------------------------

  bool CField::dispatchEvent(CEventServer& event)
  {
    if (SuperClass::dispatchEvent(event)) return true;
    else
    {
      switch(event.type)
      {
        case EVENT_ID_UPDATE_DATA :
          recvUpdateData(event);
          return true;
          break;

        case EVENT_ID_READ_DATA :
          recvReadDataRequest(event);
          return true;
          break;

        case EVENT_ID_READ_DATA_READY :
          recvReadDataReady(event);
          return true;
          break;

        case EVENT_ID_ADD_VARIABLE :
          recvAddVariable(event);
          return true;
          break;

        case EVENT_ID_ADD_VARIABLE_GROUP :
          recvAddVariableGroup(event);
          return true;
          break;

        default :
          ERROR("bool CField::dispatchEvent(CEventServer& event)", << "Unknown Event");
          return false;
      }
    }
  }

  void CField::sendUpdateData(const CArray<double,1>& data)
  {
    CTimer::get("Field : send data").resume();

    CContext* context = CContext::getCurrent();
    CContextClient* client = (!context->hasServer) ? context->client : this->file->getContextClient();
    int receiverSize = client->serverSize;

    CEventClient event(getType(), EVENT_ID_UPDATE_DATA);

    map<int, CArray<int,1> >::iterator it;
    list<CMessage> list_msg;
    list<CArray<double,1> > list_data;

    if (!grid->doGridHaveDataDistributed(client))
    {
       if (client->isServerLeader())
       {
          for (it = grid->storeIndex_toSrv[client].begin(); it != grid->storeIndex_toSrv[client].end(); it++)
          {
            int rank = it->first;
            CArray<int,1>& index = it->second;

            list_msg.push_back(CMessage());
            list_data.push_back(CArray<double,1>(index.numElements()));

            CArray<double,1>& data_tmp = list_data.back();
            for (int n = 0; n < data_tmp.numElements(); n++) data_tmp(n) = data(index(n));

            list_msg.back() << getId() << data_tmp;
            event.push(rank, 1, list_msg.back());
          }
          client->sendEvent(event);
        }
      else client->sendEvent(event);
    }
    else
    {
      for (it = grid->storeIndex_toSrv[client].begin(); it != grid->storeIndex_toSrv[client].end(); it++)
      {
        int rank = it->first;
        CArray<int,1>& index = it->second;

        list_msg.push_back(CMessage());
        list_data.push_back(CArray<double,1>(index.numElements()));

        CArray<double,1>& data_tmp = list_data.back();
        for (int n = 0; n < data_tmp.numElements(); n++) data_tmp(n) = data(index(n));

        list_msg.back() << getId() << data_tmp;
        event.push(rank, grid->nbSenders[receiverSize][rank], list_msg.back());
      }
      client->sendEvent(event);
    }

    CTimer::get("Field : send data").suspend();
  }

  void CField::recvUpdateData(CEventServer& event)
  {
    std::map<int,CBufferIn*> rankBuffers;

    list<CEventServer::SSubEvent>::iterator it;
    string fieldId;
    CTimer::get("Field : recv data").resume();
    for (it = event.subEvents.begin(); it != event.subEvents.end(); ++it)
    {
      int rank = it->rank;
      CBufferIn* buffer = it->buffer;
      *buffer >> fieldId;
      rankBuffers[rank] = buffer;
    }
    get(fieldId)->recvUpdateData(rankBuffers);
    CTimer::get("Field : recv data").suspend();
  }

  void  CField::recvUpdateData(std::map<int,CBufferIn*>& rankBuffers)
  {
    CContext* context = CContext::getCurrent();

    size_t sizeData = 0;
    if (0 == recvDataSrv.numElements())
    {            
      CArray<int,1>& storeClient = grid->storeIndex_client;

      // Gather all data from different clients      
      recvDataSrv.resize(storeClient.numElements());
      recvFoperationSrv = boost::shared_ptr<func::CFunctor>(new func::CInstant(recvDataSrv));
    }

    CArray<double,1> recv_data_tmp(recvDataSrv.numElements());    
    const CDate& currDate = context->getCalendar()->getCurrentDate();
    CDuration offsetAllButMonth (freq_offset.getValue().year, 0 , freq_offset.getValue().day,
                                   freq_offset.getValue().hour, freq_offset.getValue().minute,
                                   freq_offset.getValue().second, freq_offset.getValue().timestep);
    const CDate opeDate   = (last_operation_srv - offsetAllButMonth + context->getCalendar()->getTimeStep())
                              + freq_op + freq_operation_srv - freq_op - context->getCalendar()->getTimeStep() + offsetAllButMonth;

    if (opeDate <= currDate)
    {
      for (map<int, CArray<size_t, 1> >::iterator it = grid->outLocalIndexStoreOnClient.begin(); it != grid->outLocalIndexStoreOnClient.end(); ++it)
      {
        CArray<double,1> tmp;
        CArray<size_t,1>& indexTmp = it->second;
        *(rankBuffers[it->first]) >> tmp;
        for (int idx = 0; idx < indexTmp.numElements(); ++idx)
        {
          recv_data_tmp(indexTmp(idx)) = tmp(idx);
        }      
      }
    }

    this->setData(recv_data_tmp);
    // delete incomming flux for server only
    recvFoperationSrv.reset() ;
    recvDataSrv.reset() ;
  }

  void CField::writeUpdateData(const CArray<double,1>& data)
  {
    CContext* context = CContext::getCurrent();

    const CDate& currDate = context->getCalendar()->getCurrentDate();
    CDuration offsetAllButMonth (freq_offset.getValue().year, 0 , freq_offset.getValue().day,
                                   freq_offset.getValue().hour, freq_offset.getValue().minute,
                                   freq_offset.getValue().second, freq_offset.getValue().timestep);
    const CDate opeDate   = (last_operation_srv - offsetAllButMonth + context->getCalendar()->getTimeStep())
                              + freq_op + freq_operation_srv - freq_op - context->getCalendar()->getTimeStep() + offsetAllButMonth;
    const CDate writeDate = last_Write_srv + freq_write_srv;

    if (opeDate <= currDate)
    {
      (*recvFoperationSrv)(data);
      last_operation_srv = currDate;
    }

    if (writeDate < (currDate + freq_operation_srv))
    {
      recvFoperationSrv->final();
      last_Write_srv = writeDate;
      grid->computeWrittenIndex();
      writeField();
      lastlast_Write_srv = last_Write_srv;
    }
  }

  void CField::writeField(void)
  {
    if (!getRelFile()->isEmptyZone())
    {
      if (grid->doGridHaveDataToWrite() || getRelFile()->type == CFile::type_attr::one_file)
      {
        getRelFile()->checkWriteFile();
        this->incrementNStep();
        getRelFile()->getDataOutput()->writeFieldData(CField::get(this));
      }
    }
  }

  /*
    Send a request for reading data.
    Client sends a request to server for demanding server to read data and send back to it.
    For now, this function is called only by client
    In the future, it can be called by level-1 servers
    \param [in] tsDataRequested timestamp when the call is made
  */
  bool CField::sendReadDataRequest(const CDate& tsDataRequested)
  {
    CContext* context = CContext::getCurrent();
    // CContextClient* client = context->client;

    // This code is for future: If we want to read file with level-2 servers
    CContextClient* client = (!context->hasServer) ? context->client : this->file->getContextClient();

    lastDataRequestedFromServer = tsDataRequested;

    // No need to send the request if we are sure that we are already at EOF
    if (!isEOF || context->getCalendar()->getCurrentDate() <= dateEOF)
    {
      CEventClient event(getType(), EVENT_ID_READ_DATA);
      if (client->isServerLeader())
      {
        CMessage msg;
        msg << getId();
        const std::list<int>& ranks = client->getRanksServerLeader();
        for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
          event.push(*itRank, 1, msg);
        client->sendEvent(event);
      }
      else client->sendEvent(event);
    }
    else
      serverSourceFilter->signalEndOfStream(tsDataRequested);

    wasDataRequestedFromServer = true;

    return !isEOF;
  }

  /*!
  Send request new data read from file if need be, that is the current data is out-of-date.
  \return true if and only if some data was requested
  */
  bool CField::sendReadDataRequestIfNeeded(void)
  {
    const CDate& currentDate = CContext::getCurrent()->getCalendar()->getCurrentDate();

    bool dataRequested = false;

    while (currentDate >= lastDataRequestedFromServer)
    {
      info(20) << "currentDate : " << currentDate << endl ;
      info(20) << "lastDataRequestedFromServer : " << lastDataRequestedFromServer << endl ;
      info(20) << "file->output_freq.getValue() : " << file->output_freq.getValue() << endl ;
      info(20) << "lastDataRequestedFromServer + file->output_freq.getValue() : " << lastDataRequestedFromServer + file->output_freq << endl ;

      dataRequested |= sendReadDataRequest(lastDataRequestedFromServer + file->output_freq);
    }

    return dataRequested;
  }

  void CField::recvReadDataRequest(CEventServer& event)
  {
    CBufferIn* buffer = event.subEvents.begin()->buffer;
    StdString fieldId;
    *buffer >> fieldId;
    get(fieldId)->recvReadDataRequest();
  }

  /*!
    Receive data request sent from client and process it
    Every time server receives this request, it will try to read data and sent read data back to client
    At the moment, this function is called by server level 1
    In the future, this should (only) be done by the last level servers.
  */
  void CField::recvReadDataRequest(void)
  {
    CContext* context = CContext::getCurrent();
    CContextClient* client = context->client;

    CEventClient event(getType(), EVENT_ID_READ_DATA_READY);
    std::list<CMessage> msgs;

    EReadField hasData = readField();

    map<int, CArray<double,1> >::iterator it;
    if (!grid->doGridHaveDataDistributed(client))
    {
       if (client->isServerLeader())
       {
          if (0 != recvDataSrv.numElements())
          {            
            const std::list<int>& ranks = client->getRanksServerLeader();
            for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
            {
              msgs.push_back(CMessage());
              CMessage& msg = msgs.back();
              msg << getId();
              switch (hasData)
              {
                case RF_DATA:
                  msg << getNStep() - 1 << recvDataSrv;
                  break;
                case RF_NODATA:
                  msg << int(-2) << recvDataSrv;
                  break;
                case RF_EOF:                  
                default:
                  msg << int(-1);
                  break;
              }

              event.push(*itRank, 1, msg);
            }
          }
          client->sendEvent(event);
       }
       else
       {
          client->sendEvent(event);
       }
    }
    else
    {
      for (map<int, CArray<size_t, 1> >::iterator it = grid->outLocalIndexStoreOnClient.begin(); 
                                                  it != grid->outLocalIndexStoreOnClient.end(); ++it)
      {
        CArray<size_t,1>& indexTmp = it->second;
        CArray<double,1> tmp(indexTmp.numElements());
        for (int idx = 0; idx < indexTmp.numElements(); ++idx)
        {
          tmp(idx) = recvDataSrv(indexTmp(idx));
        } 

        msgs.push_back(CMessage());
        CMessage& msg = msgs.back();
        msg << getId();
        switch (hasData)
        {
          case RF_DATA:
            msg << getNStep() - 1 << tmp;
            break;
          case RF_NODATA:
            msg << int(-2) << tmp;
            break;
          case RF_EOF:                  
          default:
            msg << int(-1);
            break;
        }

        event.push(it->first, grid->nbReadSenders[client][it->first], msg);
      }
      client->sendEvent(event);
    }
  }

  /*!
    Read field from a file.
    A field is read with the distribution of data on the server side
    \return State of field can be read from a file
  */
  CField::EReadField CField::readField(void)
  {
    CContext* context = CContext::getCurrent();
    grid->computeWrittenIndex();
    getRelFile()->initRead();
    EReadField readState = RF_DATA;

    if (!getRelFile()->isEmptyZone())
    {      
      if (grid->doGridHaveDataToWrite() || getRelFile()->type == CFile::type_attr::one_file)      
      {
        if (0 == recvDataSrv.numElements())
        {            
          CArray<int,1>& storeClient = grid->storeIndex_client;          
          recvDataSrv.resize(storeClient.numElements());          
        }
        
        getRelFile()->checkReadFile();

        if (!nstepMax)
        {
          nstepMax = getRelFile()->getDataInput()->getFieldNbRecords(CField::get(this));
        }

        this->incrementNStep();

        if (getNStep() > nstepMax && (getRelFile()->cyclic.isEmpty() || !getRelFile()->cyclic) )
          readState = RF_EOF;

        if (RF_EOF != readState)
          getRelFile()->getDataInput()->readFieldData(CField::get(this));
      }
    }
    else
    {
      this->incrementNStep();
      if (getNStep() > nstepMax && (getRelFile()->cyclic.isEmpty() || !getRelFile()->cyclic) )
        readState = RF_EOF;
      else
        readState = RF_NODATA;

      if (!nstepMaxRead) // This can be a bug if we try to read field from zero time record
        readState = RF_NODATA;
    }

    if (!nstepMaxRead)
    {
       MPI_Allreduce(MPI_IN_PLACE, &nstepMax, 1, MPI_INT, MPI_MAX, context->server->intraComm);
       nstepMaxRead = true;
    }

    return readState;
  }

  /*
    Receive read data from server.
    At the moment, this function is called in the client side.
    In the future, this function can be called hiearachically (server n-1, server n -2, ..., client)
    \param event event containing read data
  */
  void CField::recvReadDataReady(CEventServer& event)
  {
    string fieldId;
    vector<int> ranks;
    vector<CBufferIn*> buffers;

    list<CEventServer::SSubEvent>::iterator it;
    for (it = event.subEvents.begin(); it != event.subEvents.end(); ++it)
    {
      ranks.push_back(it->rank);
      CBufferIn* buffer = it->buffer;
      *buffer >> fieldId;
      buffers.push_back(buffer);
    }
    get(fieldId)->recvReadDataReady(ranks, buffers);
  }

  /*!
    Receive read data from server
    \param [in] ranks Ranks of sending processes
    \param [in] buffers buffers containing read data
  */
  void CField::recvReadDataReady(vector<int> ranks, vector<CBufferIn*> buffers)
  {
    CContext* context = CContext::getCurrent();
    std::map<int, CArray<double,1> > data;
    const bool wasEOF = isEOF;

    for (int i = 0; i < ranks.size(); i++)
    {
      int rank = ranks[i];
      int record;
      *buffers[i] >> record;
      isEOF = (record == int(-1));

      if (!isEOF)
        *buffers[i] >> data[rank];
      else
        break;
    }

    if (wasDataAlreadyReceivedFromServer)
      lastDataReceivedFromServer = lastDataReceivedFromServer + file->output_freq;
    else
    {
      lastDataReceivedFromServer = context->getCalendar()->getInitDate();
      wasDataAlreadyReceivedFromServer = true;
    }

    if (isEOF)
    {
      if (!wasEOF)
        dateEOF = lastDataReceivedFromServer;

      serverSourceFilter->signalEndOfStream(lastDataReceivedFromServer);
    }
    else
      serverSourceFilter->streamDataFromServer(lastDataReceivedFromServer, data);
  }

  void CField::checkForLateDataFromServer(void)
  {
    CContext* context = CContext::getCurrent();
    const CDate& currentDate = context->getCalendar()->getCurrentDate();

    // Check if data previously requested has been received as expected
    if (wasDataRequestedFromServer && !isEOF)
    {
      CTimer timer("CField::checkForLateDataFromServer");

      bool isDataLate;
      do
      {
        const CDate nextDataDue = wasDataAlreadyReceivedFromServer ? (lastDataReceivedFromServer + file->output_freq) : context->getCalendar()->getInitDate();
        isDataLate = (nextDataDue <= currentDate);

        if (isDataLate)
        {
          timer.resume();

          context->checkBuffersAndListen();

          timer.suspend();
        }
      }
      while (isDataLate && timer.getCumulatedTime() < CXios::recvFieldTimeout);

      if (isDataLate)
        ERROR("void CField::checkForLateDataFromServer(void)",
              << "Late data at timestep = " << currentDate);
    }
  }

  void CField::checkIfMustAutoTrigger(void)
  {
    mustAutoTrigger = serverSourceFilter ? serverSourceFilter->mustAutoTrigger() : false;
  }

  void CField::autoTriggerIfNeeded(void)
  {
    if (mustAutoTrigger)
      serverSourceFilter->trigger(CContext::getCurrent()->getCalendar()->getCurrentDate());
  }

   //----------------------------------------------------------------

   void CField::setRelFile(CFile* _file)
   {
      this->file = _file;
      hasOutputFile = true;
   }

   //----------------------------------------------------------------

   StdString CField::GetName(void)    { return StdString("field"); }
   StdString CField::GetDefName(void) { return CField::GetName(); }
   ENodeType CField::GetType(void)    { return eField; }

   //----------------------------------------------------------------

   CGrid* CField::getRelGrid(void) const
   {
      return this->grid;
   }

   //----------------------------------------------------------------

   CFile* CField::getRelFile(void) const
   {
      return this->file;
   }

   int CField::getNStep(void) const
   {
      return this->nstep;
   }

   func::CFunctor::ETimeType CField::getOperationTimeType() const
   {
     return operationTimeType;
   }

   //----------------------------------------------------------------

   void CField::incrementNStep(void)
   {
      this->nstep++;
   }

   void CField::resetNStep(int nstep /*= 0*/)
   {
      this->nstep = nstep;
   }

   void CField::resetNStepMax(void)
   {
      this->nstepMax = 0;
      nstepMaxRead = false;
   }

   //----------------------------------------------------------------

   bool CField::isActive(bool atCurrentTimestep /*= false*/) const
   {
      if (clientSourceFilter)
        return atCurrentTimestep ? clientSourceFilter->isDataExpected(CContext::getCurrent()->getCalendar()->getCurrentDate()) : true;
      else if (storeFilter)
        return true;
      else if (instantDataFilter)
        ERROR("bool CField::isActive(bool atCurrentTimestep)",
              << "Impossible to check if field [ id = " << getId() << " ] is active as it cannot be used to receive nor send data.");

      return false;
   }

   //----------------------------------------------------------------

   bool CField::wasWritten() const
   {
     return written;
   }

   void CField::setWritten()
   {
     written = true;
   }

   //----------------------------------------------------------------

   bool CField::getUseCompressedOutput() const
   {
     return useCompressedOutput;
   }

   void CField::setUseCompressedOutput()
   {
     useCompressedOutput = true;
   }

   //----------------------------------------------------------------

   boost::shared_ptr<COutputPin> CField::getInstantDataFilter()
   {
     return instantDataFilter;
   }

   //----------------------------------------------------------------

   /*!
     Build up graph of grids which plays role of destination and source in grid transformation
     This function should be called before \func solveGridReference()
   */
   void CField::buildGridTransformationGraph()
   {
     CContext* context = CContext::getCurrent();
     if (context->hasClient && !context->hasServer)
     {
       if (grid && !grid->isTransformed() && hasDirectFieldReference() && grid != getDirectFieldReference()->grid)
       {
         grid->addTransGridSource(getDirectFieldReference()->grid);
       }
     }
   }

   /*!
     Generate a new grid destination if there are more than one grid source pointing to a same grid destination
   */
   void CField::generateNewTransformationGridDest()
   {
     CContext* context = CContext::getCurrent();
     if (context->hasClient && !context->hasServer)
     {
       std::map<CGrid*,std::pair<bool,StdString> >& gridSrcMap = grid->getTransGridSource();
       if (1 < gridSrcMap.size())
       {
         // Search for grid source
         CGrid* gridSrc = grid;
         CField* currField = this;
         std::vector<CField*> hieraField;

         while (currField->hasDirectFieldReference() && (gridSrc == grid))
         {
           hieraField.push_back(currField);
           CField* tmp = currField->getDirectFieldReference();
           currField = tmp;
           gridSrc = currField->grid;
         }

         if (gridSrcMap.end() != gridSrcMap.find(gridSrc))
         {
           CGrid* gridTmp;
           std::pair<bool,StdString> newGridDest = gridSrcMap[gridSrc];
           if (newGridDest.first)
           {
             StdString newIdGridDest = newGridDest.second;
             if (!CGrid::has(newIdGridDest))
             {
                ERROR("CGrid* CGrid::generateNewTransformationGridDest()",
                  << " Something wrong happened! Grid whose id " << newIdGridDest
                  << "should exist ");
             }
             gridTmp = CGrid::get(newIdGridDest);
           }
           else
           {
             StdString newIdGridDest = CGrid::generateId(gridSrc, grid);
             gridTmp = CGrid::cloneGrid(newIdGridDest, grid);

             (gridSrcMap[gridSrc]).first = true;
             (gridSrcMap[gridSrc]).second = newIdGridDest;
           }

           // Update all descendants
           for (std::vector<CField*>::iterator it = hieraField.begin(); it != hieraField.end(); ++it)
           {
             (*it)->grid = gridTmp;
             (*it)->updateRef((*it)->grid);
           }
         }
       }
     }
   }

   void CField::updateRef(CGrid* grid)
   {
     if (!grid_ref.isEmpty()) grid_ref.setValue(grid->getId());
     else
     {
       std::vector<CAxis*> axisTmp = grid->getAxis();
       std::vector<CDomain*> domainTmp = grid->getDomains();
       if ((1<axisTmp.size()) || (1<domainTmp.size()))
         ERROR("void CField::updateRef(CGrid* grid)",
           << "More than one domain or axis is available for domain_ref/axis_ref of field " << this->getId());

       if ((!domain_ref.isEmpty()) && (domainTmp.empty()))
         ERROR("void CField::updateRef(CGrid* grid)",
           << "Incoherent between available domain and domain_ref of field " << this->getId());
       if ((!axis_ref.isEmpty()) && (axisTmp.empty()))
         ERROR("void CField::updateRef(CGrid* grid)",
           << "Incoherent between available axis and axis_ref of field " << this->getId());

       if (!domain_ref.isEmpty()) domain_ref.setValue(domainTmp[0]->getId());
       if (!axis_ref.isEmpty()) axis_ref.setValue(axisTmp[0]->getId());
     }
   }
   
   /*!
     Solve reference of all enabled fields even the source fields .
     In this step, we do transformations.
   */
   void CField::solveAllEnabledFieldsAndTransform()
   {
     CContext* context = CContext::getCurrent();
     bool hasClient = context->hasClient;
     bool hasServer = context->hasServer;

     if (!isReferenceSolvedAndTransformed)
     {
        isReferenceSolvedAndTransformed = true;

        if (hasClient && !hasServer)
        {
          solveRefInheritance(true);
          if (hasDirectFieldReference()) getDirectFieldReference()->solveAllEnabledFieldsAndTransform();
        }

        if (hasServer)
          solveServerOperation();

        solveGridReference();

        if (hasClient && !hasServer)
       {
         solveGenerateGrid();
         buildGridTransformationGraph();
       }

       solveGridDomainAxisRef(false);

       if (hasClient && !hasServer)
       {
         solveTransformedGrid();
       }

       solveGridDomainAxisRef(false);
     }
   }

   void CField::checkGridOfEnabledFields()
   {
     if (!isGridChecked)
     {
       isGridChecked = true;
       solveCheckMaskIndex(false);
     }
   }

   void CField::sendGridComponentOfEnabledFields()
   {
      solveGridDomainAxisRef(true);
      // solveCheckMaskIndex(true);
   }

   void CField::sendGridOfEnabledFields()
   {
      // solveGridDomainAxisRef(true);
      solveCheckMaskIndex(true);
   }   

   void CField::solveOnlyReferenceEnabledField(bool doSending2Server)
   {
     CContext* context = CContext::getCurrent();
     if (!isReferenceSolved)
     {
        isReferenceSolved = true;

        if (context->hasClient && !context->hasServer)
        {
          solveRefInheritance(true);
          if (hasDirectFieldReference()) getDirectFieldReference()->solveOnlyReferenceEnabledField(false);
        }

        if (context->hasServer)
          solveServerOperation();

        solveGridReference();
        grid->solveDomainAxisRefInheritance(true); // make it again to solve grid reading from file

        if (context->hasClient && !context->hasServer)
       {
         solveGenerateGrid();
         buildGridTransformationGraph();
       }
     }
   }
     
   void CField::solveAllReferenceEnabledField(bool doSending2Server)
   {
     CContext* context = CContext::getCurrent();
     solveOnlyReferenceEnabledField(doSending2Server);

     if (!areAllReferenceSolved)
     {
        areAllReferenceSolved = true;
       
        if (context->hasClient && !context->hasServer)
        {
          solveRefInheritance(true);
          if (hasDirectFieldReference()) getDirectFieldReference()->solveAllReferenceEnabledField(false);
        }
        else if (context->hasServer)
          solveServerOperation();

        solveGridReference();
     }

     solveGridDomainAxisRef(doSending2Server);

     if (context->hasClient && !context->hasServer)
     {
       solveTransformedGrid();
     }

     solveCheckMaskIndex(doSending2Server);
   }

   std::map<int, StdSize> CField::getGridAttributesBufferSize(CContextClient* client, bool bufferForWriting /*= "false"*/)
   {
     return grid->getAttributesBufferSize(client, bufferForWriting);
   }

   std::map<int, StdSize> CField::getGridDataBufferSize(CContextClient* client, bool bufferForWriting /*= "false"*/)
   {
     return grid->getDataBufferSize(client, getId(), bufferForWriting);
   }

   size_t CField::getGlobalWrittenSize()
   {
     return grid->getGlobalWrittenSize();
   }

   //----------------------------------------------------------------

   void CField::solveServerOperation(void)
   {
      CContext* context = CContext::getCurrent();

      if (!context->hasServer || !hasOutputFile) return;

      if (freq_op.isEmpty())
        freq_op.setValue(TimeStep);

      if (freq_offset.isEmpty())
        freq_offset.setValue(NoneDu);

      freq_operation_srv = file->output_freq.getValue();
      freq_write_srv     = file->output_freq.getValue();

      lastlast_Write_srv = context->getCalendar()->getInitDate();
      last_Write_srv     = context->getCalendar()->getInitDate();
      last_operation_srv = context->getCalendar()->getInitDate();

      const CDuration toffset = freq_operation_srv - freq_offset.getValue() - context->getCalendar()->getTimeStep();
      last_operation_srv     = last_operation_srv - toffset;

      if (operation.isEmpty())
        ERROR("void CField::solveServerOperation(void)",
              << "An operation must be defined for field \"" << getId() << "\".");

      boost::shared_ptr<func::CFunctor> functor;
      CArray<double, 1> dummyData;

#define DECLARE_FUNCTOR(MType, mtype) \
      if (operation.getValue().compare(#mtype) == 0) \
      { \
        functor.reset(new func::C##MType(dummyData)); \
      }

#include "functor_type.conf"

      if (!functor)
        ERROR("void CField::solveServerOperation(void)",
              << "\"" << operation << "\" is not a valid operation.");

      operationTimeType = functor->timeType();
   }

   //----------------------------------------------------------------

   /*!
    * Constructs the graph filter for the field, enabling or not the data output.
    * This method should not be called more than once with enableOutput equal to true.
    *
    * \param gc the garbage collector to use when building the filter graph
    * \param enableOutput must be true when the field data is to be
    *                     read by the client or/and written to a file
    */
   void CField::buildFilterGraph(CGarbageCollector& gc, bool enableOutput)
   {     
    if (!isReferenceSolvedAndTransformed) solveAllEnabledFieldsAndTransform();
    if (!isGridChecked) checkGridOfEnabledFields();

     const bool detectMissingValues = (!detect_missing_value.isEmpty() && !default_value.isEmpty() && detect_missing_value == true);
     const double defaultValue  = detectMissingValues ? default_value : (!default_value.isEmpty() ? default_value : 0.0);

     CContext* context = CContext::getCurrent();
     bool hasWriterServer = context->hasServer && !context->hasClient;
     bool hasIntermediateServer = context->hasServer && context->hasClient;

     if (hasWriterServer)
     {
        if (!instantDataFilter)
          instantDataFilter = clientSourceFilter = boost::shared_ptr<CSourceFilter>(new CSourceFilter(gc, grid,true));


       // If the field data is to be read by the client or/and written to a file
       if (enableOutput && !storeFilter && !fileWriterFilter)
       {
         if (file && (file->mode.isEmpty() || file->mode == CFile::mode_attr::write))
         {
           fileServerWriterFilter = boost::shared_ptr<CFileServerWriterFilter>(new CFileServerWriterFilter(gc, this));
           instantDataFilter->connectOutput(fileServerWriterFilter, 0);
         }
       }
     }
     else if (hasIntermediateServer)
     {
       if (!instantDataFilter)
         instantDataFilter = clientSourceFilter = boost::shared_ptr<CSourceFilter>(new CSourceFilter(gc, grid, true));

             // If the field data is to be read by the client or/and written to a file
       if (enableOutput && !storeFilter && !fileWriterFilter)
       {
         if (file && (file->mode.isEmpty() || file->mode == CFile::mode_attr::write))
         {
           fileWriterFilter = boost::shared_ptr<CFileWriterFilter>(new CFileWriterFilter(gc, this));
           instantDataFilter->connectOutput(fileWriterFilter, 0);
         }
       }
     }
     else
     {
       // Start by building a filter which can provide the field's instant data
       if (!instantDataFilter)
       {
         // Check if we have an expression to parse
         if (hasExpression())
         {
           boost::scoped_ptr<IFilterExprNode> expr(parseExpr(getExpression() + '\0'));
           boost::shared_ptr<COutputPin> filter = expr->reduce(gc, *this);

           // Check if a spatial transformation is needed
           if (!field_ref.isEmpty())
           {
             CGrid* gridRef = CField::get(field_ref)->grid;

             if (grid && grid != gridRef && grid->hasTransform())
             {
                 std::pair<boost::shared_ptr<CFilter>, boost::shared_ptr<CFilter> > filters = CSpatialTransformFilter::buildFilterGraph(gc, gridRef, grid, detectMissingValues, defaultValue); 

               filter->connectOutput(filters.first, 0);
               filter = filters.second;
             }
           }

           instantDataFilter = filter;
         }
         // Check if we have a reference on another field
         else if (!field_ref.isEmpty())
           instantDataFilter = getFieldReference(gc);
         // Check if the data is to be read from a file
         else if (file && !file->mode.isEmpty() && file->mode == CFile::mode_attr::read)
         {
           checkTimeAttributes();
           instantDataFilter = serverSourceFilter = boost::shared_ptr<CSourceFilter>(new CSourceFilter(gc, grid, true, freq_offset, true,
                                                                                                       detectMissingValues, defaultValue));
         }
         else // The data might be passed from the model
         {
            if (check_if_active.isEmpty()) check_if_active = false; 
            instantDataFilter = clientSourceFilter = boost::shared_ptr<CSourceFilter>(new CSourceFilter(gc, grid, false, NoneDu, false,
                                                                                                        detectMissingValues, defaultValue));
         }
       }

       // If the field data is to be read by the client or/and written to a file
       if (enableOutput && !storeFilter && !fileWriterFilter)
       {
         if (!read_access.isEmpty() && read_access)
         {
           storeFilter = boost::shared_ptr<CStoreFilter>(new CStoreFilter(gc, CContext::getCurrent(), grid,
                                                                          detectMissingValues, defaultValue));
           instantDataFilter->connectOutput(storeFilter, 0);
         }

         if (file && (file->mode.isEmpty() || file->mode == CFile::mode_attr::write))
         {
           fileWriterFilter = boost::shared_ptr<CFileWriterFilter>(new CFileWriterFilter(gc, this));
           getTemporalDataFilter(gc, file->output_freq)->connectOutput(fileWriterFilter, 0);
         }
       }
     }
   }

   /*!
    * Returns the filter needed to handle the field reference.
    * This method should only be called when building the filter graph corresponding to the field.
    *
    * \param gc the garbage collector to use
    * \return the output pin corresponding to the field reference
    */
   boost::shared_ptr<COutputPin> CField::getFieldReference(CGarbageCollector& gc)
   {
     if (instantDataFilter || field_ref.isEmpty())
       ERROR("COutputPin* CField::getFieldReference(CGarbageCollector& gc)",
             "Impossible to get the field reference for a field which has already been parsed or which does not have a field_ref.");

     CField* fieldRef = CField::get(field_ref);
     fieldRef->buildFilterGraph(gc, false);

     std::pair<boost::shared_ptr<CFilter>, boost::shared_ptr<CFilter> > filters;
     // Check if a spatial transformation is needed
     if (grid && grid != fieldRef->grid && grid->hasTransform())
     {       
       bool hasMissingValue = (!detect_missing_value.isEmpty() && !default_value.isEmpty() && detect_missing_value == true);
       double defaultValue  = hasMissingValue ? default_value : (!default_value.isEmpty() ? default_value : 0.0);                                
       filters = CSpatialTransformFilter::buildFilterGraph(gc, fieldRef->grid, grid, hasMissingValue, defaultValue);
     }
     else
       filters.first = filters.second = boost::shared_ptr<CFilter>(new CPassThroughFilter(gc));

     fieldRef->getInstantDataFilter()->connectOutput(filters.first, 0);

     return filters.second;
   }

   /*!
    * Returns the filter needed to handle a self reference in the field's expression.
    * If the needed filter does not exist, it is created, otherwise it is reused.
    * This method should only be called when building the filter graph corresponding
    * to the field's expression.
    *
    * \param gc the garbage collector to use
    * \return the output pin corresponding to a self reference
    */
   boost::shared_ptr<COutputPin> CField::getSelfReference(CGarbageCollector& gc)
   {
     if (instantDataFilter || !hasExpression())
       ERROR("COutputPin* CField::getSelfReference(CGarbageCollector& gc)",
             "Impossible to add a self reference to a field which has already been parsed or which does not have an expression.");

     if (!selfReferenceFilter)
     {
       const bool detectMissingValues = (!detect_missing_value.isEmpty() && !default_value.isEmpty() && detect_missing_value == true);
       const double defaultValue  = detectMissingValues ? default_value : (!default_value.isEmpty() ? default_value : 0.0);

       if (file && !file->mode.isEmpty() && file->mode == CFile::mode_attr::read)
       {
         if (!serverSourceFilter)
         {
           checkTimeAttributes();
           serverSourceFilter = boost::shared_ptr<CSourceFilter>(new CSourceFilter(gc, grid, true, freq_offset, true,
                                                                                   detectMissingValues, defaultValue));
         }

         selfReferenceFilter = serverSourceFilter;
       }
       else if (!field_ref.isEmpty())
       {
         CField* fieldRef = CField::get(field_ref);
         fieldRef->buildFilterGraph(gc, false);
         selfReferenceFilter = fieldRef->getInstantDataFilter();
       }
       else
       {
         if (!clientSourceFilter)
         {
           if (check_if_active.isEmpty()) check_if_active = false;
           clientSourceFilter = boost::shared_ptr<CSourceFilter>(new CSourceFilter(gc, grid, true, NoneDu, false,
                                                                                   detectMissingValues, defaultValue));
         }

         selfReferenceFilter = clientSourceFilter;
       }
     }

     return selfReferenceFilter;
   }

   /*!
    * Returns the temporal filter corresponding to the field's temporal operation
    * for the specified operation frequency. The filter is created if it does not
    * exist, otherwise it is reused.
    *
    * \param gc the garbage collector to use
    * \param outFreq the operation frequency, i.e. the frequency at which the output data will be computed
    * \return the output pin corresponding to the requested temporal filter
    */
   boost::shared_ptr<COutputPin> CField::getTemporalDataFilter(CGarbageCollector& gc, CDuration outFreq)
   {
     std::map<CDuration, boost::shared_ptr<COutputPin> >::iterator it = temporalDataFilters.find(outFreq);

     if (it == temporalDataFilters.end())
     {
       if (operation.isEmpty())
         ERROR("void CField::getTemporalDataFilter(CGarbageCollector& gc, CDuration outFreq)",
               << "An operation must be defined for field \"" << getId() << "\".");

       checkTimeAttributes(&outFreq);

       const bool detectMissingValues = (!detect_missing_value.isEmpty()  && detect_missing_value == true);
       boost::shared_ptr<CTemporalFilter> temporalFilter(new CTemporalFilter(gc, operation,
                                                                             CContext::getCurrent()->getCalendar()->getInitDate(),
                                                                             freq_op, freq_offset, outFreq, detectMissingValues));

       instantDataFilter->connectOutput(temporalFilter, 0);

       it = temporalDataFilters.insert(std::make_pair(outFreq, temporalFilter)).first;
     }

     return it->second;
   }

  /*!
    * Returns the temporal filter corresponding to the field's temporal operation
    * for the specified operation frequency.
    *
    * \param gc the garbage collector to use
    * \param outFreq the operation frequency, i.e. the frequency at which the output data will be computed
    * \return the output pin corresponding to the requested temporal filter
    */
   
   boost::shared_ptr<COutputPin> CField::getSelfTemporalDataFilter(CGarbageCollector& gc, CDuration outFreq)
   {
     if (instantDataFilter || !hasExpression())
       ERROR("COutputPin* CField::getSelfTemporalDataFilter(CGarbageCollector& gc)",
             "Impossible to add a self reference to a field which has already been parsed or which does not have an expression.");

     if (!selfReferenceFilter) getSelfReference(gc) ;

     if (serverSourceFilter || clientSourceFilter)
     {
       if (operation.isEmpty())
         ERROR("void CField::getSelfTemporalDataFilter(CGarbageCollector& gc, CDuration outFreq)",
               << "An operation must be defined for field \"" << getId() << "\".");

       checkTimeAttributes(&outFreq);

       const bool detectMissingValues = (!detect_missing_value.isEmpty() && detect_missing_value == true);
       boost::shared_ptr<CTemporalFilter> temporalFilter(new CTemporalFilter(gc, operation,
                                                                             CContext::getCurrent()->getCalendar()->getInitDate(),
                                                                             freq_op, freq_offset, outFreq, detectMissingValues));

       selfReferenceFilter->connectOutput(temporalFilter, 0);
       return temporalFilter ;
     }
     else if (!field_ref.isEmpty())
     {
       CField* fieldRef = CField::get(field_ref);
       fieldRef->buildFilterGraph(gc, false); 
       return fieldRef->getTemporalDataFilter(gc, outFreq) ;
     }
  }

   //----------------------------------------------------------------
/*
   void CField::fromBinary(StdIStream& is)
   {
      SuperClass::fromBinary(is);
#define CLEAR_ATT(name_)\
      SuperClassAttribute::operator[](#name_)->reset()

         CLEAR_ATT(domain_ref);
         CLEAR_ATT(axis_ref);
#undef CLEAR_ATT

   }
*/
   //----------------------------------------------------------------

   void CField::solveGridReference(void)
   {
      if (grid_ref.isEmpty() && domain_ref.isEmpty() && axis_ref.isEmpty() && scalar_ref.isEmpty())
      {
        ERROR("CField::solveGridReference(void)",
              << "A grid must be defined for field '" << getFieldOutputName() << "' .");
      }
      else if (!grid_ref.isEmpty() && (!domain_ref.isEmpty() || !axis_ref.isEmpty() || !scalar_ref.isEmpty()))
      {
        ERROR("CField::solveGridReference(void)",
              << "Field '" << getFieldOutputName() << "' has both a grid and a domain/axis/scalar." << std::endl
              << "Please define either 'grid_ref' or 'domain_ref'/'axis_ref'/'scalar_ref'.");
      }

      if (grid_ref.isEmpty())
      {
        std::vector<CDomain*> vecDom;
        std::vector<CAxis*> vecAxis;
        std::vector<CScalar*> vecScalar;
        std::vector<int> axisDomainOrderTmp;

        std::vector<CDomain*> vecDomRef;
        std::vector<CAxis*> vecAxisRef;
        std::vector<CScalar*> vecScalarRef;

        
        if (!domain_ref.isEmpty())
        {
          StdString tmp = domain_ref.getValue();
          if (CDomain::has(domain_ref))
          {
            vecDom.push_back(CDomain::get(domain_ref));
            vecDomRef.push_back(CDomain::createDomain());
            vecDomRef.back()->domain_ref=domain_ref;
            axisDomainOrderTmp.push_back(2);
          }
          else  ERROR("CField::solveGridReference(void)",
                      << "Invalid reference to domain '" << domain_ref.getValue() << "'.");
        }

        if (!axis_ref.isEmpty())
        {
          if (CAxis::has(axis_ref))
          {
            vecAxis.push_back(CAxis::get(axis_ref));
            vecAxisRef.push_back(CAxis::createAxis());
            vecAxisRef.back()->axis_ref=axis_ref;
            axisDomainOrderTmp.push_back(1);
          }
          else  ERROR("CField::solveGridReference(void)",
                      << "Invalid reference to axis '" << axis_ref.getValue() << "'.");
        }

        if (!scalar_ref.isEmpty())
        {
          if (CScalar::has(scalar_ref))
          {
            vecScalar.push_back(CScalar::get(scalar_ref));
            vecScalarRef.push_back(CScalar::createScalar());
            vecScalarRef.back()->scalar_ref=scalar_ref;
            axisDomainOrderTmp.push_back(0);
          }
          else ERROR("CField::solveGridReference(void)",
                     << "Invalid reference to scalar '" << scalar_ref.getValue() << "'.");
        }
        
        CArray<int,1> axisDomainOrder(axisDomainOrderTmp.size());
        for (int idx = 0; idx < axisDomainOrderTmp.size(); ++idx)
        {
          axisDomainOrder(idx) = axisDomainOrderTmp[idx];
        }

        // Warning: the gridId shouldn't be set as the grid_ref since it could be inherited
        StdString gridId = CGrid::generateId(vecDom, vecAxis, vecScalar,axisDomainOrder);
        if (CGrid::has(gridId)) this->grid = CGrid::get(gridId);
        else  this->grid = CGrid::createGrid(gridId, vecDomRef, vecAxisRef, vecScalarRef,axisDomainOrder);
      }
      else
      {
        if (CGrid::has(grid_ref)) this->grid = CGrid::get(grid_ref);
        else  ERROR("CField::solveGridReference(void)",
                     << "Invalid reference to grid '" << grid_ref.getValue() << "'.");
      }
   }

   void CField::solveGridDomainAxisRef(bool checkAtt)
   {
     grid->solveDomainAxisRef(checkAtt);
   }

   void CField::solveCheckMaskIndex(bool doSendingIndex)
   {
     grid->checkMaskIndex(doSendingIndex);
   }

   void CField::solveTransformedGrid()
   {
     if (grid && !grid->isTransformed() && hasDirectFieldReference() && grid != getDirectFieldReference()->grid)
     {
       std::vector<CGrid*> grids;
       // Source grid
       grids.push_back(getDirectFieldReference()->grid);
       // Intermediate grids
       if (!grid_path.isEmpty())
       {
         std::string gridId;
         size_t start = 0, end;

         do
         {
           end = grid_path.getValue().find(',', start);
           if (end != std::string::npos)
           {
             gridId = grid_path.getValue().substr(start, end - start);
             start = end + 1;
           }
           else
             gridId = grid_path.getValue().substr(start);

           if (!CGrid::has(gridId))
             ERROR("void CField::solveTransformedGrid()",
                   << "Invalid grid_path, the grid '" << gridId << "' does not exist.");

           grids.push_back(CGrid::get(gridId));
         }
         while (end != std::string::npos);
       }
       // Destination grid
       grids.push_back(grid);

       for (size_t i = 0, count = grids.size() - 1; i < count; ++i)
       {
         CGrid *gridSrc  = grids[i];
         CGrid *gridDest = grids[i + 1];
         if (!gridDest->isTransformed())
           gridDest->transformGrid(gridSrc);
       }
     }
     else if (grid && grid->hasTransform() && !grid->isTransformed())
     {
       // Temporarily deactivate the self-transformation of grid
       // grid->transformGrid(grid);
     }
   }

   void CField::solveGenerateGrid()
   {
     if (grid && !grid->isTransformed() && hasDirectFieldReference() && grid != getDirectFieldReference()->grid)
       grid->completeGrid(getDirectFieldReference()->grid);
     else
       grid->completeGrid();
   }

   void CField::solveGridDomainAxisBaseRef()
   {
     grid->solveDomainAxisRef(false);
     grid->solveDomainAxisBaseRef();
   }

   ///-------------------------------------------------------------------

   template <>
   void CGroupTemplate<CField, CFieldGroup, CFieldAttributes>::solveRefInheritance(void)
   {
      if (this->group_ref.isEmpty()) return;
      StdString gref = this->group_ref.getValue();

      if (!CFieldGroup::has(gref))
         ERROR("CGroupTemplate<CField, CFieldGroup, CFieldAttributes>::solveRefInheritance(void)",
               << "[ gref = " << gref << "]"
               << " invalid group name !");

      CFieldGroup* group = CFieldGroup::get(gref);
      CFieldGroup* owner = CFieldGroup::get(boost::polymorphic_downcast<CFieldGroup*>(this));

      std::vector<CField*> allChildren  = group->getAllChildren();
      std::vector<CField*>::iterator it = allChildren.begin(), end = allChildren.end();

      for (; it != end; it++)
      {
         CField* child = *it;
         if (child->hasId()) owner->createChild()->field_ref.setValue(child->getId());

      }
   }

   void CField::scaleFactorAddOffset(double scaleFactor, double addOffset)
   {
     recvDataSrv = (recvDataSrv - addOffset) / scaleFactor;
   }

   void CField::invertScaleFactorAddOffset(double scaleFactor, double addOffset)
   {
     recvDataSrv = recvDataSrv * scaleFactor + addOffset;
   }

   void CField::outputField(CArray<double,1>& fieldOut)
   { 
      CArray<size_t,1>& outIndexClient = grid->localIndexToWriteOnClient;
      CArray<size_t,1>& outIndexServer = grid->localIndexToWriteOnServer;
      for (size_t idx = 0; idx < outIndexServer.numElements(); ++idx)
      {
        fieldOut(outIndexServer(idx)) = recvDataSrv(outIndexClient(idx));
      }
   }

   void CField::inputField(CArray<double,1>& fieldIn)
   {
      CArray<size_t,1>& outIndexClient = grid->localIndexToWriteOnClient;
      CArray<size_t,1>& outIndexServer = grid->localIndexToWriteOnServer;
      for (size_t idx = 0; idx < outIndexServer.numElements(); ++idx)
      {
        recvDataSrv(outIndexClient(idx)) = fieldIn(outIndexServer(idx));
      }

   }

   void CField::outputCompressedField(CArray<double,1>& fieldOut)
   {
      CArray<size_t,1>& outIndexClient = grid->localIndexToWriteOnClient;
      CArray<size_t,1>& outIndexServer = grid->localIndexToWriteOnServer;
      for (size_t idx = 0; idx < outIndexServer.numElements(); ++idx)
      {
        fieldOut((idx)) = recvDataSrv(outIndexClient(idx));
      }
   }

   ///-------------------------------------------------------------------

   void CField::parse(xml::CXMLNode& node)
   {
      string newContent ;
      SuperClass::parse(node);
      if (node.goToChildElement())
      {
        do
        {
          if (node.getElementName() == "variable" || node.getElementName() == "variable_group") this->getVirtualVariableGroup()->parseChild(node);
          else if (node.getElementName() == "expr") if (node.getContent(newContent)) content+=newContent ;
        } while (node.goToNextElement());
        node.goToParentElement();
      }
      if (node.getContent(newContent)) content=newContent ;
    }

   /*!
     This function retrieves Id of corresponding domain_ref and axis_ref (if any)
   of a field. In some cases, only domain exists but axis doesn't
   \return pair of Domain and Axis id
   */
   const std::vector<StdString>& CField::getRefDomainAxisIds()
   {
     CGrid* cgPtr = getRelGrid();
     if (NULL != cgPtr)
     {
       std::vector<StdString>::iterator it;
       if (!domain_ref.isEmpty())
       {
         std::vector<StdString> domainList = cgPtr->getDomainList();
         it = std::find(domainList.begin(), domainList.end(), domain_ref.getValue());
         if (domainList.end() != it) domAxisScalarIds_[0] = *it;
       }

       if (!axis_ref.isEmpty())
       {
         std::vector<StdString> axisList = cgPtr->getAxisList();
         it = std::find(axisList.begin(), axisList.end(), axis_ref.getValue());
         if (axisList.end() != it) domAxisScalarIds_[1] = *it;
       }

       if (!scalar_ref.isEmpty())
       {
         std::vector<StdString> scalarList = cgPtr->getScalarList();
         it = std::find(scalarList.begin(), scalarList.end(), scalar_ref.getValue());
         if (scalarList.end() != it) domAxisScalarIds_[2] = *it;
       }
     }
     return (domAxisScalarIds_);
   }

   CVariable* CField::addVariable(const string& id)
   {
     return vVariableGroup->createChild(id);
   }

   CVariableGroup* CField::addVariableGroup(const string& id)
   {
     return vVariableGroup->createChildGroup(id);
   }

   void CField::setContextClient(CContextClient* contextClient)
   {
     CContext* context = CContext::getCurrent();
     client = contextClient;
     if (context->hasClient)
     {
       // A grid is sent by a client (both for read or write) or by primary server (write only)
       if (context->hasServer)
       {
         if (file->mode.isEmpty() || (!file->mode.isEmpty() && file->mode == CFile::mode_attr::write))
           grid->setContextClient(contextClient);
       }
       else
           grid->setContextClient(contextClient);
     }
   }

   CContextClient* CField::getContextClient()
   {
     return client;
   }

   void CField::sendAddAllVariables(CContextClient* client)
   {
     std::vector<CVariable*> allVar = getAllVariables();
     std::vector<CVariable*>::const_iterator it = allVar.begin();
     std::vector<CVariable*>::const_iterator itE = allVar.end();

     for (; it != itE; ++it)
     {
       this->sendAddVariable((*it)->getId(), client);
       (*it)->sendAllAttributesToServer(client);
       (*it)->sendValue(client);
     }
   }


   /*!
    * Send all Attributes to server. This method is overloaded, since only grid_ref attribute
    * must be sent to server and not domain_ref/axis_ref/scalar_ref. 
    */
    
   void CField::sendAllAttributesToServer(CContextClient* client)
   {
     if (grid_ref.isEmpty())
     {
       grid_ref=grid->getId() ;
       SuperClass::sendAllAttributesToServer(client) ;
       grid_ref.reset();
     }
     else SuperClass::sendAllAttributesToServer(client) ;
   }
    
   void CField::sendAddVariable(const string& id, CContextClient* client)
   {
      sendAddItem(id, (int)EVENT_ID_ADD_VARIABLE, client);
   }

   void CField::sendAddVariableGroup(const string& id, CContextClient* client)
   {
      sendAddItem(id, (int)EVENT_ID_ADD_VARIABLE_GROUP, client);
   }

   void CField::recvAddVariable(CEventServer& event)
   {

      CBufferIn* buffer = event.subEvents.begin()->buffer;
      string id;
      *buffer >> id;
      get(id)->recvAddVariable(*buffer);
   }

   void CField::recvAddVariable(CBufferIn& buffer)
   {
      string id;
      buffer >> id;
      addVariable(id);
   }

   void CField::recvAddVariableGroup(CEventServer& event)
   {

      CBufferIn* buffer = event.subEvents.begin()->buffer;
      string id;
      *buffer >> id;
      get(id)->recvAddVariableGroup(*buffer);
   }

   void CField::recvAddVariableGroup(CBufferIn& buffer)
   {
      string id;
      buffer >> id;
      addVariableGroup(id);
   }

   /*!
    * Check on freq_off and freq_op attributes.
    */
   void CField::checkTimeAttributes(CDuration* freqOp)
   {
     bool isFieldRead  = file && !file->mode.isEmpty() && file->mode == CFile::mode_attr::read;
     bool isFieldWrite = file && ( file->mode.isEmpty() ||  file->mode == CFile::mode_attr::write);
     if (isFieldRead && !(operation.getValue() == "instant" || operation.getValue() == "once") )     
       ERROR("void CField::checkTimeAttributes(void)",
             << "Unsupported operation for field '" << getFieldOutputName() << "'." << std::endl
             << "Currently only \"instant\" is supported for fields read from file.")

     if (freq_op.isEmpty())
     {
       if (operation.getValue() == "instant")
       {
         if (isFieldRead || isFieldWrite) freq_op.setValue(file->output_freq.getValue());
         else freq_op=*freqOp ;
       }
       else
         freq_op.setValue(TimeStep);
     }
     if (freq_offset.isEmpty())
       freq_offset.setValue(isFieldRead ? NoneDu : (freq_op.getValue() - TimeStep));
   }

   /*!
    * Returns string arithmetic expression associated to the field.
    * \return if content is defined return content string, otherwise, if "expr" attribute is defined, return expr string.
    */
   const string& CField::getExpression(void)
   {
     if (!expr.isEmpty() && content.empty())
     {
       content = expr;
       expr.reset();
     }

     return content;
   }

   bool CField::hasExpression(void) const
   {
     return (!expr.isEmpty() || !content.empty());
   }


   DEFINE_REF_FUNC(Field,field)
} // namespace xios
