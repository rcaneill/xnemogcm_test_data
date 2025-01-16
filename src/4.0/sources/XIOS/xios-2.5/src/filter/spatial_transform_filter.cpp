#include "spatial_transform_filter.hpp"
#include "grid_transformation.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "timer.hpp"

namespace xios
{
  CSpatialTransformFilter::CSpatialTransformFilter(CGarbageCollector& gc, CSpatialTransformFilterEngine* engine, double outputValue, size_t inputSlotsCount)
    : CFilter(gc, inputSlotsCount, engine), outputDefaultValue(outputValue)
  { /* Nothing to do */ }

  std::pair<boost::shared_ptr<CSpatialTransformFilter>, boost::shared_ptr<CSpatialTransformFilter> >
  CSpatialTransformFilter::buildFilterGraph(CGarbageCollector& gc, CGrid* srcGrid, CGrid* destGrid, bool hasMissingValue, double missingValue)
  {
    if (!srcGrid || !destGrid)
      ERROR("std::pair<boost::shared_ptr<CSpatialTransformFilter>, boost::shared_ptr<CSpatialTransformFilter> >"
            "buildFilterGraph(CGarbageCollector& gc, CGrid* srcGrid, CGrid* destGrid)",
            "Impossible to build the filter graph if either the source or the destination grid are null.");

    boost::shared_ptr<CSpatialTransformFilter> firstFilter, lastFilter;
    // Note that this loop goes from the last transformation to the first transformation
    do
    {
      CGridTransformation* gridTransformation = destGrid->getTransformations();
      CSpatialTransformFilterEngine* engine = CSpatialTransformFilterEngine::get(destGrid->getTransformations());
      const std::vector<StdString>& auxInputs = gridTransformation->getAuxInputs();
      size_t inputCount = 1 + (auxInputs.empty() ? 0 : auxInputs.size());
      double defaultValue  = (hasMissingValue) ? std::numeric_limits<double>::quiet_NaN() : 0.0;


      const CGridTransformationSelector::ListAlgoType& algoList = gridTransformation->getAlgoList() ;
      CGridTransformationSelector::ListAlgoType::const_iterator it  ;

      bool isSpatialTemporal=false ;
      for (it=algoList.begin();it!=algoList.end();++it)  if (it->second.first == TRANS_TEMPORAL_SPLITTING) isSpatialTemporal=true ;

      boost::shared_ptr<CSpatialTransformFilter> filter ;
      if( isSpatialTemporal) filter = boost::shared_ptr<CSpatialTransformFilter>(new CSpatialTemporalFilter(gc, engine, gridTransformation, defaultValue, inputCount));
      else filter = boost::shared_ptr<CSpatialTransformFilter>(new CSpatialTransformFilter(gc, engine, defaultValue, inputCount));

      
      if (!lastFilter)
        lastFilter = filter;
      else
        filter->connectOutput(firstFilter, 0);

      firstFilter = filter;
      for (size_t idx = 0; idx < auxInputs.size(); ++idx)
      {
        CField* fieldAuxInput = CField::get(auxInputs[idx]);
        fieldAuxInput->buildFilterGraph(gc, false);
        fieldAuxInput->getInstantDataFilter()->connectOutput(firstFilter,idx+1);
      }

      destGrid = gridTransformation->getGridSource();
    }
    while (destGrid != srcGrid);

    return std::make_pair(firstFilter, lastFilter);
  }

  void CSpatialTransformFilter::onInputReady(std::vector<CDataPacketPtr> data)
  {
    CSpatialTransformFilterEngine* spaceFilter = static_cast<CSpatialTransformFilterEngine*>(engine);
    CDataPacketPtr outputPacket = spaceFilter->applyFilter(data, outputDefaultValue);
    if (outputPacket)
      onOutputReady(outputPacket);
  }





  CSpatialTemporalFilter::CSpatialTemporalFilter(CGarbageCollector& gc, CSpatialTransformFilterEngine* engine, CGridTransformation* gridTransformation, double outputValue, size_t inputSlotsCount)
    : CSpatialTransformFilter(gc, engine, outputValue, inputSlotsCount), record(0)
  {
      const CGridTransformationSelector::ListAlgoType& algoList = gridTransformation->getAlgoList() ;
      CGridTransformationSelector::ListAlgoType::const_iterator it  ;

      int pos ;
      for (it=algoList.begin();it!=algoList.end();++it) 
        if (it->second.first == TRANS_TEMPORAL_SPLITTING)
        {
          pos=it->first ;
          if (pos < algoList.size()-1)
            ERROR("SpatialTemporalFilter::CSpatialTemporalFilter(CGarbageCollector& gc, CSpatialTransformFilterEngine* engine, CGridTransformation* gridTransformation, double outputValue, size_t inputSlotsCount))",
                  "temporal splitting operation must be the last of whole transformation on same grid") ;
        }
          
      CGrid* grid=gridTransformation->getGridDestination() ;

      CAxis* axis = grid->getAxis(gridTransformation->getElementPositionInGridDst2AxisPosition().find(pos)->second) ;

      nrecords = axis->index.numElements() ;
  }


  void CSpatialTemporalFilter::onInputReady(std::vector<CDataPacketPtr> data)
  {
    CSpatialTransformFilterEngine* spaceFilter = static_cast<CSpatialTransformFilterEngine*>(engine);
    CDataPacketPtr outputPacket = spaceFilter->applyFilter(data, outputDefaultValue);

    if (outputPacket)
    {
      size_t nelements=outputPacket->data.numElements() ;
      if (!tmpData.numElements())
      {
        tmpData.resize(nelements);
        tmpData=outputDefaultValue ;
      }

      nelements/=nrecords ;
      size_t offset=nelements*record ;
      for(size_t i=0;i<nelements;++i)  tmpData(i+offset) = outputPacket->data(i) ;
    
      record ++ ;
      if (record==nrecords)
      {
        record=0 ;
        CDataPacketPtr packet = CDataPacketPtr(new CDataPacket);
        packet->date = data[0]->date;
        packet->timestamp = data[0]->timestamp;
        packet->status = data[0]->status;
        packet->data.resize(tmpData.numElements());
        packet->data = tmpData;
        onOutputReady(packet);
        tmpData.resize(0) ;
      }
    }
  }


  CSpatialTransformFilterEngine::CSpatialTransformFilterEngine(CGridTransformation* gridTransformation)
    : gridTransformation(gridTransformation)
  {
    if (!gridTransformation)
      ERROR("CSpatialTransformFilterEngine::CSpatialTransformFilterEngine(CGridTransformation* gridTransformation)",
            "Impossible to construct a spatial transform filter engine without a valid grid transformation.");
  }

  std::map<CGridTransformation*, boost::shared_ptr<CSpatialTransformFilterEngine> > CSpatialTransformFilterEngine::engines;

  CSpatialTransformFilterEngine* CSpatialTransformFilterEngine::get(CGridTransformation* gridTransformation)
  {
    if (!gridTransformation)
      ERROR("CSpatialTransformFilterEngine& CSpatialTransformFilterEngine::get(CGridTransformation* gridTransformation)",
            "Impossible to get the requested engine, the grid transformation is invalid.");

    std::map<CGridTransformation*, boost::shared_ptr<CSpatialTransformFilterEngine> >::iterator it = engines.find(gridTransformation);
    if (it == engines.end())
    {
      boost::shared_ptr<CSpatialTransformFilterEngine> engine(new CSpatialTransformFilterEngine(gridTransformation));
      it = engines.insert(std::make_pair(gridTransformation, engine)).first;
    }

    return it->second.get();
  }

  CDataPacketPtr CSpatialTransformFilterEngine::apply(std::vector<CDataPacketPtr> data)
  {
    /* Nothing to do */
  }

  CDataPacketPtr CSpatialTransformFilterEngine::applyFilter(std::vector<CDataPacketPtr> data, double defaultValue)
  {
    CDataPacketPtr packet(new CDataPacket);
    packet->date = data[0]->date;
    packet->timestamp = data[0]->timestamp;
    packet->status = data[0]->status;

    if (packet->status == CDataPacket::NO_ERROR)
    {
      if (1 < data.size())  // Dynamical transformations
      {
        std::vector<CArray<double,1>* > dataAuxInputs(data.size()-1);
        for (size_t idx = 0; idx < dataAuxInputs.size(); ++idx) dataAuxInputs[idx] = &(data[idx+1]->data);
        gridTransformation->computeAll(dataAuxInputs, packet->timestamp);
      }
      packet->data.resize(gridTransformation->getGridDestination()->storeIndex_client.numElements());
      if (0 != packet->data.numElements())
        (packet->data)(0) = defaultValue;
      apply(data[0]->data, packet->data);
    }

    return packet;
  }

  void CSpatialTransformFilterEngine::apply(const CArray<double, 1>& dataSrc, CArray<double,1>& dataDest)
  {
    CTimer::get("CSpatialTransformFilterEngine::apply").resume(); 
    
    CContextClient* client = CContext::getCurrent()->client;

    // Get default value for output data
    bool ignoreMissingValue = false; 
    double defaultValue = std::numeric_limits<double>::quiet_NaN();
    if (0 != dataDest.numElements()) ignoreMissingValue = NumTraits<double>::isNan(dataDest(0));

    const std::list<CGridTransformation::SendingIndexGridSourceMap>& listLocalIndexSend = gridTransformation->getLocalIndexToSendFromGridSource();
    const std::list<CGridTransformation::RecvIndexGridDestinationMap>& listLocalIndexToReceive = gridTransformation->getLocalIndexToReceiveOnGridDest();
    const std::list<size_t>& listNbLocalIndexToReceive = gridTransformation->getNbLocalIndexToReceiveOnGridDest();
    const std::list<std::vector<bool> >& listLocalIndexMaskOnDest = gridTransformation->getLocalMaskIndexOnGridDest();
    const std::vector<CGenericAlgorithmTransformation*>& listAlgos = gridTransformation->getAlgos();

    CArray<double,1> dataCurrentDest(dataSrc.copy());

    std::list<CGridTransformation::SendingIndexGridSourceMap>::const_iterator itListSend  = listLocalIndexSend.begin(),
                                                                              iteListSend = listLocalIndexSend.end();
    std::list<CGridTransformation::RecvIndexGridDestinationMap>::const_iterator itListRecv = listLocalIndexToReceive.begin();
    std::list<size_t>::const_iterator itNbListRecv = listNbLocalIndexToReceive.begin();
    std::list<std::vector<bool> >::const_iterator itLocalMaskIndexOnDest = listLocalIndexMaskOnDest.begin();
    std::vector<CGenericAlgorithmTransformation*>::const_iterator itAlgo = listAlgos.begin();

    for (; itListSend != iteListSend; ++itListSend, ++itListRecv, ++itNbListRecv, ++itLocalMaskIndexOnDest, ++itAlgo)
    {
      CArray<double,1> dataCurrentSrc(dataCurrentDest);
      const CGridTransformation::SendingIndexGridSourceMap& localIndexToSend = *itListSend;

      // Sending data from field sources to do transformations
      std::map<int, CArray<int,1> >::const_iterator itbSend = localIndexToSend.begin(), itSend,
                                                    iteSend = localIndexToSend.end();
      int idxSendBuff = 0;
      std::vector<double*> sendBuff(localIndexToSend.size());
      for (itSend = itbSend; itSend != iteSend; ++itSend, ++idxSendBuff)
      {
        if (0 != itSend->second.numElements())
          sendBuff[idxSendBuff] = new double[itSend->second.numElements()];
      }

      idxSendBuff = 0;
      std::vector<MPI_Request> sendRecvRequest;
      for (itSend = itbSend; itSend != iteSend; ++itSend, ++idxSendBuff)
      {
        int destRank = itSend->first;
        const CArray<int,1>& localIndex_p = itSend->second;
        int countSize = localIndex_p.numElements();
        for (int idx = 0; idx < countSize; ++idx)
        {
          sendBuff[idxSendBuff][idx] = dataCurrentSrc(localIndex_p(idx));
        }
        sendRecvRequest.push_back(MPI_Request());
        MPI_Isend(sendBuff[idxSendBuff], countSize, MPI_DOUBLE, destRank, 12, client->intraComm, &sendRecvRequest.back());
      }

      // Receiving data on destination fields
      const CGridTransformation::RecvIndexGridDestinationMap& localIndexToReceive = *itListRecv;
      CGridTransformation::RecvIndexGridDestinationMap::const_iterator itbRecv = localIndexToReceive.begin(), itRecv,
                                                                       iteRecv = localIndexToReceive.end();
      int recvBuffSize = 0;
      for (itRecv = itbRecv; itRecv != iteRecv; ++itRecv) recvBuffSize += itRecv->second.size(); //(recvBuffSize < itRecv->second.size())
                                                                       //? itRecv->second.size() : recvBuffSize;
      double* recvBuff;
      if (0 != recvBuffSize) recvBuff = new double[recvBuffSize];
      int currentBuff = 0;
      for (itRecv = itbRecv; itRecv != iteRecv; ++itRecv)
      {
        int srcRank = itRecv->first;
        int countSize = itRecv->second.size();
        sendRecvRequest.push_back(MPI_Request());
        MPI_Irecv(recvBuff + currentBuff, countSize, MPI_DOUBLE, srcRank, 12, client->intraComm, &sendRecvRequest.back());
        currentBuff += countSize;
      }
      std::vector<MPI_Status> status(sendRecvRequest.size());
      MPI_Waitall(sendRecvRequest.size(), &sendRecvRequest[0], &status[0]);

      dataCurrentDest.resize(*itNbListRecv);
      const std::vector<bool>& localMaskDest = *itLocalMaskIndexOnDest;
      for (int i = 0; i < localMaskDest.size(); ++i)
        if (localMaskDest[i]) dataCurrentDest(i) = 0.0;
        else dataCurrentDest(i) = defaultValue;

      std::vector<bool> localInitFlag(dataCurrentDest.numElements(), true);
      currentBuff = 0;
      bool firstPass=true; 
      for (itRecv = itbRecv; itRecv != iteRecv; ++itRecv)
      {
        int countSize = itRecv->second.size();
        const std::vector<std::pair<int,double> >& localIndex_p = itRecv->second;
        (*itAlgo)->apply(localIndex_p,
                         recvBuff+currentBuff,
                         dataCurrentDest,
                         localInitFlag,
                         ignoreMissingValue,firstPass);

        currentBuff += countSize;
        firstPass=false ;
      }

      (*itAlgo)->updateData(dataCurrentDest);

      idxSendBuff = 0;
      for (itSend = itbSend; itSend != iteSend; ++itSend, ++idxSendBuff)
      {
        if (0 != itSend->second.numElements())
          delete [] sendBuff[idxSendBuff];
      }
      if (0 != recvBuffSize) delete [] recvBuff;
    }
    if (dataCurrentDest.numElements() != dataDest.numElements())
    ERROR("CSpatialTransformFilterEngine::apply(const CArray<double, 1>& dataSrc, CArray<double,1>& dataDest)",
          "Incoherent between the received size and expected size. " << std::endl 
          << "Expected size: " << dataDest.numElements() << std::endl 
          << "Received size: " << dataCurrentDest.numElements());

    dataDest = dataCurrentDest;

    CTimer::get("CSpatialTransformFilterEngine::apply").suspend() ;
  }
} // namespace xios
