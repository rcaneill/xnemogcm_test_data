#include "context.hpp"
#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"

#include "calendar_type.hpp"
#include "duration.hpp"

#include "context_client.hpp"
#include "context_server.hpp"
#include "nc4_data_output.hpp"
#include "node_type.hpp"
#include "message.hpp"
#include "type.hpp"
#include "xios_spl.hpp"
#include "timer.hpp"
#include "memtrack.hpp"


namespace xios {

  shared_ptr<CContextGroup> CContext::root;

   /// ////////////////////// Définitions ////////////////////// ///

   CContext::CContext(void)
      : CObjectTemplate<CContext>(), CContextAttributes()
      , calendar(), hasClient(false), hasServer(false), isPostProcessed(false), finalized(false)
      , idServer_(), client(0), server(0)
   { /* Ne rien faire de plus */ }

   CContext::CContext(const StdString & id)
      : CObjectTemplate<CContext>(id), CContextAttributes()
      , calendar(), hasClient(false), hasServer(false), isPostProcessed(false), finalized(false)
      , idServer_(), client(0), server(0)
   { /* Ne rien faire de plus */ }

   CContext::~CContext(void)
   {
     delete client;
     delete server;
   }

   //----------------------------------------------------------------
   //! Get name of context
   StdString CContext::GetName(void)   { return (StdString("context")); }
   StdString CContext::GetDefName(void){ return (CContext::GetName()); }
   ENodeType CContext::GetType(void)   { return (eContext); }

   //----------------------------------------------------------------

   /*!
   \brief Get context group (context root)
   \return Context root
   */
   CContextGroup* CContext::getRoot(void)
   {
      if (root.get()==NULL) root=shared_ptr<CContextGroup>(new CContextGroup(xml::CXMLNode::GetRootName()));
      return root.get();
   }

   //----------------------------------------------------------------

   /*!
   \brief Get calendar of a context
   \return Calendar
   */
   boost::shared_ptr<CCalendar> CContext::getCalendar(void) const
   {
      return (this->calendar);
   }

   //----------------------------------------------------------------

   /*!
   \brief Set a context with a calendar
   \param[in] newCalendar new calendar
   */
   void CContext::setCalendar(boost::shared_ptr<CCalendar> newCalendar)
   {
      this->calendar = newCalendar;
   }

   //----------------------------------------------------------------
   /*!
   \brief Parse xml file and write information into context object
   \param [in] node xmld node corresponding in xml file
   */
   void CContext::parse(xml::CXMLNode & node)
   {
      CContext::SuperClass::parse(node);

      // PARSING POUR GESTION DES ENFANTS
      xml::THashAttributes attributes = node.getAttributes();

      if (attributes.end() != attributes.find("src"))
      {
         StdIFStream ifs ( attributes["src"].c_str() , StdIFStream::in );
         if ( (ifs.rdstate() & std::ifstream::failbit ) != 0 )
            ERROR("void CContext::parse(xml::CXMLNode & node)",
                  <<endl<< "Can not open <"<<attributes["src"].c_str()<<"> file" );
         if (!ifs.good())
            ERROR("CContext::parse(xml::CXMLNode & node)",
                  << "[ filename = " << attributes["src"] << " ] Bad xml stream !");
         xml::CXMLParser::ParseInclude(ifs, attributes["src"], *this);
      }

      if (node.getElementName().compare(CContext::GetName()))
         DEBUG("Le noeud is wrong defined but will be considered as a context !");

      if (!(node.goToChildElement()))
      {
         DEBUG("Le context ne contient pas d'enfant !");
      }
      else
      {
         do { // Parcours des contextes pour traitement.

            StdString name = node.getElementName();
            attributes.clear();
            attributes = node.getAttributes();

            if (attributes.end() != attributes.find("id"))
            {
              DEBUG(<< "Definition node has an id,"
                    << "it will not be taking account !");
            }

#define DECLARE_NODE(Name_, name_)    \
   if (name.compare(C##Name_##Definition::GetDefName()) == 0) \
   { C##Name_##Definition::create(C##Name_##Definition::GetDefName()) -> parse(node); continue; }
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"

            DEBUG(<< "The element \'"     << name
                  << "\' in the context \'" << CContext::getCurrent()->getId()
                  << "\' is not a definition !");

         } while (node.goToNextElement());

         node.goToParentElement(); // Retour au parent
      }
   }

   //----------------------------------------------------------------
   //! Show tree structure of context
   void CContext::ShowTree(StdOStream & out)
   {
      StdString currentContextId = CContext::getCurrent() -> getId();
      std::vector<CContext*> def_vector =
         CContext::getRoot()->getChildList();
      std::vector<CContext*>::iterator
         it = def_vector.begin(), end = def_vector.end();

      out << "<? xml version=\"1.0\" ?>" << std::endl;
      out << "<"  << xml::CXMLNode::GetRootName() << " >" << std::endl;

      for (; it != end; it++)
      {
         CContext* context = *it;
         CContext::setCurrent(context->getId());
         out << *context << std::endl;
      }

      out << "</" << xml::CXMLNode::GetRootName() << " >" << std::endl;
      CContext::setCurrent(currentContextId);
   }


   //----------------------------------------------------------------

   //! Convert context object into string (to print)
   StdString CContext::toString(void) const
   {
      StdOStringStream oss;
      oss << "<" << CContext::GetName()
          << " id=\"" << this->getId() << "\" "
          << SuperClassAttribute::toString() << ">" << std::endl;
      if (!this->hasChild())
      {
         //oss << "<!-- No definition -->" << std::endl; // fait planter l'incrémentation
      }
      else
      {

#define DECLARE_NODE(Name_, name_)    \
   if (C##Name_##Definition::has(C##Name_##Definition::GetDefName())) \
   oss << * C##Name_##Definition::get(C##Name_##Definition::GetDefName()) << std::endl;
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"

      }

      oss << "</" << CContext::GetName() << " >";

      return (oss.str());
   }

   //----------------------------------------------------------------

   /*!
   \brief Find all inheritace among objects in a context.
   \param [in] apply (true) write attributes of parent into ones of child if they are empty
                     (false) write attributes of parent into a new container of child
   \param [in] parent unused
   */
   void CContext::solveDescInheritance(bool apply, const CAttributeMap * const UNUSED(parent))
   {
#define DECLARE_NODE(Name_, name_)    \
   if (C##Name_##Definition::has(C##Name_##Definition::GetDefName())) \
     C##Name_##Definition::get(C##Name_##Definition::GetDefName())->solveDescInheritance(apply);
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"
   }

   //----------------------------------------------------------------

   //! Verify if all root definition in the context have child.
   bool CContext::hasChild(void) const
   {
      return (
#define DECLARE_NODE(Name_, name_)    \
   C##Name_##Definition::has(C##Name_##Definition::GetDefName())   ||
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"
      false);
}

   //----------------------------------------------------------------

   void CContext::CleanTree(void)
   {
#define DECLARE_NODE(Name_, name_) C##Name_##Definition::ClearAllAttributes();
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"
   }
   ///---------------------------------------------------------------

   //! Initialize client side
   void CContext::initClient(MPI_Comm intraComm, MPI_Comm interComm, CContext* cxtServer /*= 0*/)
   {
     hasClient=true;
     client = new CContextClient(this,intraComm, interComm, cxtServer);
     registryIn=new CRegistry(intraComm);
     registryIn->setPath(getId()) ;
     if (client->clientRank==0) registryIn->fromFile("xios_registry.bin") ;
     registryIn->bcastRegistry() ;

     registryOut=new CRegistry(intraComm) ;
     registryOut->setPath(getId()) ;

     MPI_Comm intraCommServer, interCommServer;
     if (cxtServer) // Attached mode
     {
       intraCommServer = intraComm;
       interCommServer = interComm;
     }
     else
     {
       MPI_Comm_dup(intraComm, &intraCommServer);
       comms.push_back(intraCommServer);
       MPI_Comm_dup(interComm, &interCommServer);
       comms.push_back(interCommServer);
     }
     server = new CContextServer(this,intraCommServer,interCommServer);
   }

   void CContext::setClientServerBuffer()
   {
     // Estimated minimum event size for small events (10 is an arbitrary constant just for safety)
     const size_t minEventSize = CEventClient::headerSize + getIdServer().size() + 10 * sizeof(int);
     // Ensure there is at least some room for 20 of such events in the buffers
     size_t minBufferSize = std::max(CXios::minBufferSize, 20 * minEventSize);
#define DECLARE_NODE(Name_, name_)    \
     if (minBufferSize < sizeof(C##Name_##Definition)) minBufferSize = sizeof(C##Name_##Definition);
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"
#undef DECLARE_NODE
#undef DECLARE_NODE_PAR

     // Compute the buffer sizes needed to send the attributes and data corresponding to fields
     std::map<int, StdSize> maxEventSize;
     std::map<int, StdSize> bufferSize = getAttributesBufferSize(maxEventSize);
     std::map<int, StdSize> dataBufferSize = getDataBufferSize(maxEventSize);

     std::map<int, StdSize>::iterator it, ite = dataBufferSize.end();
     for (it = dataBufferSize.begin(); it != ite; ++it)
       if (it->second > bufferSize[it->first]) bufferSize[it->first] = it->second;

     // Apply the buffer size factor and check that we are above the minimum buffer size
     ite = bufferSize.end();
     for (it = bufferSize.begin(); it != ite; ++it)
     {
       it->second *= CXios::bufferSizeFactor;
       if (it->second < minBufferSize) it->second = minBufferSize;
     }

     // Leaders will have to send some control events so ensure there is some room for those in the buffers
     if (client->isServerLeader())
     {
       const std::list<int>& ranks = client->getRanksServerLeader();
       for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
       {
         if (!bufferSize.count(*itRank))
         {
           bufferSize[*itRank] = minBufferSize;
           maxEventSize[*itRank] = minEventSize;
         }
       }
     }

     client->setBufferSize(bufferSize, maxEventSize);
   }

   //! Verify whether a context is initialized
   bool CContext::isInitialized(void)
   {
     return hasClient;
   }

   //! Initialize server
   void CContext::initServer(MPI_Comm intraComm,MPI_Comm interComm, CContext* cxtClient /*= 0*/)
   {
     hasServer=true;
     server = new CContextServer(this,intraComm,interComm);

     registryIn=new CRegistry(intraComm);
     registryIn->setPath(getId()) ;
     if (server->intraCommRank==0) registryIn->fromFile("xios_registry.bin") ;
     registryIn->bcastRegistry() ;
     registryOut=new CRegistry(intraComm) ;
     registryOut->setPath(getId()) ;

     MPI_Comm intraCommClient, interCommClient;
     if (cxtClient) // Attached mode
     {
       intraCommClient = intraComm;
       interCommClient = interComm;
     }
     else
     {
       MPI_Comm_dup(intraComm, &intraCommClient);
       comms.push_back(intraCommClient);
       MPI_Comm_dup(interComm, &interCommClient);
       comms.push_back(interCommClient);
     }
     client = new CContextClient(this,intraCommClient,interCommClient, cxtClient);
   }

   //! Try to send the buffers and receive possible answers
   bool CContext::checkBuffersAndListen(void)
   {
     client->checkBuffers();

     bool hasTmpBufferedEvent = client->hasTemporarilyBufferedEvent();
     if (hasTmpBufferedEvent)
       hasTmpBufferedEvent = !client->sendTemporarilyBufferedEvent();

     // Don't process events if there is a temporarily buffered event
     return server->eventLoop(!hasTmpBufferedEvent);
   }

   //! Terminate a context
   void CContext::finalize(void)
   {
      if (!finalized)
      {
        finalized = true;
        if (hasClient) sendRegistry() ;
        client->finalize();
        while (!server->hasFinished())
        {
          server->eventLoop();
        }

        if (hasServer)
        {
          closeAllFile();
          registryOut->hierarchicalGatherRegistry() ;
          if (server->intraCommRank==0) CXios::globalRegistry->mergeRegistry(*registryOut) ;
        }

        for (std::list<MPI_Comm>::iterator it = comms.begin(); it != comms.end(); ++it)
          MPI_Comm_free(&(*it));
        comms.clear();
      }
   }

   /*!
   \brief Close all the context defintion and do processing data
      After everything is well defined on client side, they will be processed and sent to server
   From the version 2.0, sever and client work no more on the same database. Moreover, client(s) will send
   all necessary information to server, from which each server can build its own database.
   Because the role of server is to write out field data on a specific netcdf file,
   the only information that it needs is the enabled files
   and the active fields (fields will be written onto active files)
   */
   void CContext::closeDefinition(void)
   {
     CTimer::get("Context : close definition").resume() ;
     // There is nothing client need to send to server
     if (hasClient)
     {
       // After xml is parsed, there are some more works with post processing
       postProcessing();
     }
     setClientServerBuffer();

     if (hasClient && !hasServer)
     {
      // Send all attributes of current context to server
      this->sendAllAttributesToServer();

      // Send all attributes of current calendar
      CCalendarWrapper::get(CCalendarWrapper::GetDefName())->sendAllAttributesToServer();

      // We have enough information to send to server
      // First of all, send all enabled files
       sendEnabledFiles();

      // Then, send all enabled fields
       sendEnabledFields();

      // At last, we have all info of domain and axis, then send them
       sendRefDomainsAxis();

      // After that, send all grid (if any)
       sendRefGrid();
    }

    // We have a xml tree on the server side and now, it should be also processed
    if (hasClient && !hasServer) sendPostProcessing();

    // There are some processings that should be done after all of above. For example: check mask or index
    if (hasClient)
    {
      this->buildFilterGraphOfEnabledFields();
      buildFilterGraphOfFieldsWithReadAccess();
      this->solveAllRefOfEnabledFields(true);
      postProcessFilterGraph();
    }

    // Now tell server that it can process all messages from client
    if (hasClient && !hasServer) this->sendCloseDefinition();

    // Nettoyage de l'arborescence
    if (hasClient && !hasServer) CleanTree(); // Only on client side??

    if (hasClient)
    {
      sendCreateFileHeader();

      startPrefetchingOfEnabledReadModeFiles();
    }
    CTimer::get("Context : close definition").suspend() ;
   }

   void CContext::findAllEnabledFields(void)
   {
     for (unsigned int i = 0; i < this->enabledFiles.size(); i++)
     (void)this->enabledFiles[i]->getEnabledFields();
   }

   void CContext::findAllEnabledFieldsInReadModeFiles(void)
   {
     for (unsigned int i = 0; i < this->enabledReadModeFiles.size(); ++i)
     (void)this->enabledReadModeFiles[i]->getEnabledFields();
   }

   void CContext::readAttributesOfEnabledFieldsInReadModeFiles()
   {
      for (unsigned int i = 0; i < this->enabledReadModeFiles.size(); ++i)
        (void)this->enabledReadModeFiles[i]->readAttributesOfEnabledFieldsInReadMode();
   }

   void CContext::solveOnlyRefOfEnabledFields(bool sendToServer)
   {
     int size = this->enabledFiles.size();
     for (int i = 0; i < size; ++i)
     {
       this->enabledFiles[i]->solveOnlyRefOfEnabledFields(sendToServer);
     }

     for (int i = 0; i < size; ++i)
     {
       this->enabledFiles[i]->generateNewTransformationGridDest();
     }
   }

   void CContext::solveAllRefOfEnabledFields(bool sendToServer)
   {
     int size = this->enabledFiles.size();
     for (int i = 0; i < size; ++i)
     {
       this->enabledFiles[i]->solveAllRefOfEnabledFields(sendToServer);
     }
   }

   void CContext::buildFilterGraphOfEnabledFields()
   {
     int size = this->enabledFiles.size();
     for (int i = 0; i < size; ++i)
     {
       this->enabledFiles[i]->buildFilterGraphOfEnabledFields(garbageCollector);
     }
   }

   void CContext::postProcessFilterGraph()
   {
     int size = enabledFiles.size();
     for (int i = 0; i < size; ++i)
     {
        enabledFiles[i]->postProcessFilterGraph();
     }
   }

   void CContext::startPrefetchingOfEnabledReadModeFiles()
   {
     int size = enabledReadModeFiles.size();
     for (int i = 0; i < size; ++i)
     {
        enabledReadModeFiles[i]->prefetchEnabledReadModeFields();
     }
   }

   void CContext::doPreTimestepOperationsForEnabledReadModeFiles()
   {
     int size = enabledReadModeFiles.size();
     for (int i = 0; i < size; ++i)
     {
        enabledReadModeFiles[i]->doPreTimestepOperationsForEnabledReadModeFields();
     }
   }

   void CContext::doPostTimestepOperationsForEnabledReadModeFiles()
   {
     int size = enabledReadModeFiles.size();
     for (int i = 0; i < size; ++i)
     {
        enabledReadModeFiles[i]->doPostTimestepOperationsForEnabledReadModeFields();
     }
   }

  void CContext::findFieldsWithReadAccess(void)
  {
    fieldsWithReadAccess.clear();
    const vector<CField*> allFields = CField::getAll();
    for (size_t i = 0; i < allFields.size(); ++i)
    {
      CField* field = allFields[i];

      if (field->file && !field->file->mode.isEmpty() && field->file->mode == CFile::mode_attr::read)
        field->read_access = true;
      else if (!field->read_access.isEmpty() && field->read_access && (field->enabled.isEmpty() || field->enabled))
        fieldsWithReadAccess.push_back(field);
    }
  }

  void CContext::solveAllRefOfFieldsWithReadAccess()
  {
    for (size_t i = 0; i < fieldsWithReadAccess.size(); ++i)
      fieldsWithReadAccess[i]->solveAllReferenceEnabledField(false);
  }

  void CContext::buildFilterGraphOfFieldsWithReadAccess()
  {
    for (size_t i = 0; i < fieldsWithReadAccess.size(); ++i)
      fieldsWithReadAccess[i]->buildFilterGraph(garbageCollector, true);
  }

   void CContext::solveAllInheritance(bool apply)
   {
     // Résolution des héritages descendants (càd des héritages de groupes)
     // pour chacun des contextes.
      solveDescInheritance(apply);

     // Résolution des héritages par référence au niveau des fichiers.
      const vector<CFile*> allFiles=CFile::getAll();
      const vector<CGrid*> allGrids= CGrid::getAll();

     //if (hasClient && !hasServer)
      if (hasClient)
      {
        for (unsigned int i = 0; i < allFiles.size(); i++)
          allFiles[i]->solveFieldRefInheritance(apply);
      }

      unsigned int vecSize = allGrids.size();
      unsigned int i = 0;
      for (i = 0; i < vecSize; ++i)
        allGrids[i]->solveDomainAxisRefInheritance(apply);

   }

   void CContext::findEnabledFiles(void)
   {
      const std::vector<CFile*> allFiles = CFile::getAll();
      const CDate& initDate = calendar->getInitDate();

      for (unsigned int i = 0; i < allFiles.size(); i++)
         if (!allFiles[i]->enabled.isEmpty()) // Si l'attribut 'enabled' est défini.
         {
            if (allFiles[i]->enabled.getValue()) // Si l'attribut 'enabled' est fixé à vrai.
            {
              if (allFiles[i]->output_freq.isEmpty())
              {
                 ERROR("CContext::findEnabledFiles()",
                     << "Mandatory attribute output_freq must be defined for file \""<<allFiles[i]->getFileOutputName()
                     <<" \".")
              }
              if ((initDate + allFiles[i]->output_freq.getValue()) < (initDate + this->getCalendar()->getTimeStep()))
              {
                error(0)<<"WARNING: void CContext::findEnabledFiles()"<<endl
                    << "Output frequency in file \""<<allFiles[i]->getFileOutputName()
                    <<"\" is less than the time step. File will not be written."<<endl;
              }
              else
               enabledFiles.push_back(allFiles[i]);
            }
         }
         else
         {
           if (allFiles[i]->output_freq.isEmpty())
           {
              ERROR("CContext::findEnabledFiles()",
                  << "Mandatory attribute output_freq must be defined for file \""<<allFiles[i]->getFileOutputName()
                  <<" \".")
           }  
           if ( (initDate + allFiles[i]->output_freq.getValue()) < (initDate + this->getCalendar()->getTimeStep()))
           {
             error(0)<<"WARNING: void CContext::findEnabledFiles()"<<endl
                 << "Output frequency in file \""<<allFiles[i]->getFileOutputName()
                 <<"\" is less than the time step. File will not be written."<<endl;
           }
           else
             enabledFiles.push_back(allFiles[i]); // otherwise true by default
         }

      if (enabledFiles.size() == 0)
         DEBUG(<<"Aucun fichier ne va être sorti dans le contexte nommé \""
               << getId() << "\" !");
   }

   void CContext::findEnabledReadModeFiles(void)
   {
     int size = this->enabledFiles.size();
     for (int i = 0; i < size; ++i)
     {
       if (!enabledFiles[i]->mode.isEmpty() && enabledFiles[i]->mode.getValue() == CFile::mode_attr::read)
        enabledReadModeFiles.push_back(enabledFiles[i]);
     }
   }

   void CContext::closeAllFile(void)
   {
     std::vector<CFile*>::const_iterator
            it = this->enabledFiles.begin(), end = this->enabledFiles.end();

     for (; it != end; it++)
     {
       info(30)<<"Closing File : "<<(*it)->getId()<<endl;
       (*it)->close();
     }
   }

   /*!
   \brief Dispatch event received from client
      Whenever a message is received in buffer of server, it will be processed depending on
   its event type. A new event type should be added in the switch list to make sure
   it processed on server side.
   \param [in] event: Received message
   */
   bool CContext::dispatchEvent(CEventServer& event)
   {

      if (SuperClass::dispatchEvent(event)) return true;
      else
      {
        switch(event.type)
        {
           case EVENT_ID_CLOSE_DEFINITION :
             recvCloseDefinition(event);
             return true;
             break;
           case EVENT_ID_UPDATE_CALENDAR:
             recvUpdateCalendar(event);
             return true;
             break;
           case EVENT_ID_CREATE_FILE_HEADER :
             recvCreateFileHeader(event);
             return true;
             break;
           case EVENT_ID_POST_PROCESS:
             recvPostProcessing(event);
             return true;
            case EVENT_ID_SEND_REGISTRY:
             recvRegistry(event);
             return true;
            break;

           default :
             ERROR("bool CContext::dispatchEvent(CEventServer& event)",
                    <<"Unknown Event");
           return false;
         }
      }
   }

   //! Client side: Send a message to server to make it close
   void CContext::sendCloseDefinition(void)
   {
     CEventClient event(getType(),EVENT_ID_CLOSE_DEFINITION);
     if (client->isServerLeader())
     {
       CMessage msg;
       msg<<this->getIdServer();
       const std::list<int>& ranks = client->getRanksServerLeader();
       for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
         event.push(*itRank,1,msg);
       client->sendEvent(event);
     }
     else client->sendEvent(event);
   }

   //! Server side: Receive a message of client announcing a context close
   void CContext::recvCloseDefinition(CEventServer& event)
   {

      CBufferIn* buffer=event.subEvents.begin()->buffer;
      string id;
      *buffer>>id;
      get(id)->closeDefinition();
   }

   //! Client side: Send a message to update calendar in each time step
   void CContext::sendUpdateCalendar(int step)
   {
     if (!hasServer)
     {
       CEventClient event(getType(),EVENT_ID_UPDATE_CALENDAR);
       if (client->isServerLeader())
       {
         CMessage msg;
         msg<<this->getIdServer()<<step;
         const std::list<int>& ranks = client->getRanksServerLeader();
         for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
           event.push(*itRank,1,msg);
         client->sendEvent(event);
       }
       else client->sendEvent(event);
     }
   }

   //! Server side: Receive a message of client annoucing calendar update
   void CContext::recvUpdateCalendar(CEventServer& event)
   {
      CBufferIn* buffer=event.subEvents.begin()->buffer;
      string id;
      *buffer>>id;
      get(id)->recvUpdateCalendar(*buffer);
   }

   //! Server side: Receive a message of client annoucing calendar update
   void CContext::recvUpdateCalendar(CBufferIn& buffer)
   {
      int step;
      buffer>>step;
      updateCalendar(step);
   }

   //! Client side: Send a message to create header part of netcdf file
   void CContext::sendCreateFileHeader(void)
   {
     CEventClient event(getType(),EVENT_ID_CREATE_FILE_HEADER);
     if (client->isServerLeader())
     {
       CMessage msg;
       msg<<this->getIdServer();
       const std::list<int>& ranks = client->getRanksServerLeader();
       for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
         event.push(*itRank,1,msg) ;
       client->sendEvent(event);
     }
     else client->sendEvent(event);
   }

   //! Server side: Receive a message of client annoucing the creation of header part of netcdf file
   void CContext::recvCreateFileHeader(CEventServer& event)
   {
      CBufferIn* buffer=event.subEvents.begin()->buffer;
      string id;
      *buffer>>id;
      get(id)->recvCreateFileHeader(*buffer);
   }

   //! Server side: Receive a message of client annoucing the creation of header part of netcdf file
   void CContext::recvCreateFileHeader(CBufferIn& buffer)
   {
      createFileHeader();
   }

   //! Client side: Send a message to do some post processing on server
   void CContext::sendPostProcessing()
   {
     if (!hasServer)
     {
       CEventClient event(getType(),EVENT_ID_POST_PROCESS);
       if (client->isServerLeader())
       {
         CMessage msg;
         msg<<this->getIdServer();
         const std::list<int>& ranks = client->getRanksServerLeader();
         for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
           event.push(*itRank,1,msg);
         client->sendEvent(event);
       }
       else client->sendEvent(event);
     }
   }

   //! Server side: Receive a message to do some post processing
   void CContext::recvPostProcessing(CEventServer& event)
   {
      CBufferIn* buffer=event.subEvents.begin()->buffer;
      string id;
      *buffer>>id;
      get(id)->recvPostProcessing(*buffer);
   }

   //! Server side: Receive a message to do some post processing
   void CContext::recvPostProcessing(CBufferIn& buffer)
   {
      CCalendarWrapper::get(CCalendarWrapper::GetDefName())->createCalendar();
      postProcessing();
   }

   const StdString& CContext::getIdServer()
   {
      if (hasClient)
      {
        idServer_ = this->getId();
        idServer_ += "_server";
        return idServer_;
      }
      if (hasServer) return (this->getId());
   }

   /*!
   \brief Do some simple post processings after parsing xml file
      After the xml file (iodef.xml) is parsed, it is necessary to build all relations among
   created object, e.g: inhertance among fields, domain, axis. After that, all fiels as well as their parents (reference fields),
   which will be written out into netcdf files, are processed
   */
   void CContext::postProcessing()
   {
     if (isPostProcessed) return;

      // Make sure the calendar was correctly created
      if (!calendar)
        ERROR("CContext::postProcessing()", << "A calendar must be defined for the context \"" << getId() << "!\"")
      else if (calendar->getTimeStep() == NoneDu)
        ERROR("CContext::postProcessing()", << "A timestep must be defined for the context \"" << getId() << "!\"")
      // Calendar first update to set the current date equals to the start date
      calendar->update(0);

      // Find all inheritance in xml structure
      this->solveAllInheritance();

      // Check if some axis, domains or grids are eligible to for compressed indexed output.
      // Warning: This must be done after solving the inheritance and before the rest of post-processing
      checkAxisDomainsGridsEligibilityForCompressedOutput();

      // Check if some automatic time series should be generated
      // Warning: This must be done after solving the inheritance and before the rest of post-processing
      prepareTimeseries();

      //Initialisation du vecteur 'enabledFiles' contenant la liste des fichiers à sortir.
      this->findEnabledFiles();
      this->findEnabledReadModeFiles();

      // Find all enabled fields of each file
      this->findAllEnabledFields();
      this->findAllEnabledFieldsInReadModeFiles();

     if (hasClient && !hasServer)
     {
      // Try to read attributes of fields in file then fill in corresponding grid (or domain, axis)
      this->readAttributesOfEnabledFieldsInReadModeFiles();
     }

      // Only search and rebuild all reference objects of enable fields, don't transform
      this->solveOnlyRefOfEnabledFields(false);

      // Search and rebuild all reference object of enabled fields
      this->solveAllRefOfEnabledFields(false);

      // Find all fields with read access from the public API
      findFieldsWithReadAccess();
      // and solve the all reference for them
      solveAllRefOfFieldsWithReadAccess();

      isPostProcessed = true;
   }

   /*!
    * Compute the required buffer size to send the attributes (mostly those grid related).
    *
    * \param maxEventSize [in/out] the size of the bigger event for each connected server
    */
   std::map<int, StdSize> CContext::getAttributesBufferSize(std::map<int, StdSize>& maxEventSize)
   {
     std::map<int, StdSize> attributesSize;

     if (hasClient)
     {
       size_t numEnabledFiles = this->enabledFiles.size();
       for (size_t i = 0; i < numEnabledFiles; ++i)
       {
         CFile* file = this->enabledFiles[i];

         std::vector<CField*> enabledFields = file->getEnabledFields();
         size_t numEnabledFields = enabledFields.size();
         for (size_t j = 0; j < numEnabledFields; ++j)
         {
           const std::map<int, StdSize> mapSize = enabledFields[j]->getGridAttributesBufferSize();
           std::map<int, StdSize>::const_iterator it = mapSize.begin(), itE = mapSize.end();
           for (; it != itE; ++it)
           {
             // If attributesSize[it->first] does not exist, it will be zero-initialized
             // so we can use it safely without checking for its existance
             if (attributesSize[it->first] < it->second)
               attributesSize[it->first] = it->second;

             if (maxEventSize[it->first] < it->second)
               maxEventSize[it->first] = it->second;
           }
         }
       }
     }

     return attributesSize;
   }

   /*!
    * Compute the required buffer size to send the fields data.
    *
    * \param maxEventSize [in/out] the size of the bigger event for each connected server
    */
   std::map<int, StdSize> CContext::getDataBufferSize(std::map<int, StdSize>& maxEventSize)
   {
     CFile::mode_attr::t_enum mode = hasClient ? CFile::mode_attr::write : CFile::mode_attr::read;

     std::map<int, StdSize> dataSize;

     // Find all reference domain and axis of all active fields
     size_t numEnabledFiles = this->enabledFiles.size();
     for (size_t i = 0; i < numEnabledFiles; ++i)
     {
       CFile* file = this->enabledFiles[i];
       CFile::mode_attr::t_enum fileMode = file->mode.isEmpty() ? CFile::mode_attr::write : file->mode.getValue();

       if (fileMode == mode)
       {
         std::vector<CField*> enabledFields = file->getEnabledFields();
         size_t numEnabledFields = enabledFields.size();
         for (size_t j = 0; j < numEnabledFields; ++j)
         {
           const std::map<int, StdSize> mapSize = enabledFields[j]->getGridDataBufferSize();
           std::map<int, StdSize>::const_iterator it = mapSize.begin(), itE = mapSize.end();
           for (; it != itE; ++it)
           {
             // If dataSize[it->first] does not exist, it will be zero-initialized
             // so we can use it safely without checking for its existance
             if (CXios::isOptPerformance)
               dataSize[it->first] += it->second;
             else if (dataSize[it->first] < it->second)
               dataSize[it->first] = it->second;

             if (maxEventSize[it->first] < it->second)
               maxEventSize[it->first] = it->second;
           }
         }
       }
     }

     return dataSize;
   }

   //! Client side: Send infomation of active files (files are enabled to write out)
   void CContext::sendEnabledFiles()
   {
     int size = this->enabledFiles.size();

     // In a context, each type has a root definition, e.g: axis, domain, field.
     // Every object must be a child of one of these root definition. In this case
     // all new file objects created on server must be children of the root "file_definition"
     StdString fileDefRoot("file_definition");
     CFileGroup* cfgrpPtr = CFileGroup::get(fileDefRoot);

     for (int i = 0; i < size; ++i)
     {
       cfgrpPtr->sendCreateChild(this->enabledFiles[i]->getId());
       this->enabledFiles[i]->sendAllAttributesToServer();
       this->enabledFiles[i]->sendAddAllVariables();
     }
   }

   //! Client side: Send information of active fields (ones are written onto files)
   void CContext::sendEnabledFields()
   {
     int size = this->enabledFiles.size();
     for (int i = 0; i < size; ++i)
     {
       this->enabledFiles[i]->sendEnabledFields();
     }
   }

   //! Client side: Check if the defined axis, domains and grids are eligible for compressed indexed output
   void CContext::checkAxisDomainsGridsEligibilityForCompressedOutput()
   {
     if (!hasClient) return;

     const vector<CAxis*> allAxis = CAxis::getAll();
     for (vector<CAxis*>::const_iterator it = allAxis.begin(); it != allAxis.end(); it++)
       (*it)->checkEligibilityForCompressedOutput();

     const vector<CDomain*> allDomains = CDomain::getAll();
     for (vector<CDomain*>::const_iterator it = allDomains.begin(); it != allDomains.end(); it++)
       (*it)->checkEligibilityForCompressedOutput();

     const vector<CGrid*> allGrids = CGrid::getAll();
     for (vector<CGrid*>::const_iterator it = allGrids.begin(); it != allGrids.end(); it++)
       (*it)->checkEligibilityForCompressedOutput();
   }

   //! Client side: Prepare the timeseries by adding the necessary files
   void CContext::prepareTimeseries()
   {
     if (!hasClient) return;

     const std::vector<CFile*> allFiles = CFile::getAll();
     for (size_t i = 0; i < allFiles.size(); i++)
     {
       CFile* file = allFiles[i];

       std::vector<CVariable*> fileVars, fieldVars, vars = file->getAllVariables();
       for (size_t k = 0; k < vars.size(); k++)
       {
         CVariable* var = vars[k];

         if (var->ts_target.isEmpty()
              || var->ts_target == CVariable::ts_target_attr::file || var->ts_target == CVariable::ts_target_attr::both)
           fileVars.push_back(var);

         if (!var->ts_target.isEmpty()
              && (var->ts_target == CVariable::ts_target_attr::field || var->ts_target == CVariable::ts_target_attr::both))
           fieldVars.push_back(var);
       }

       if (!file->timeseries.isEmpty() && file->timeseries != CFile::timeseries_attr::none)
       {
         StdString fileNameStr("%file_name%") ;
         StdString tsPrefix = !file->ts_prefix.isEmpty() ? file->ts_prefix : fileNameStr ;
         
         StdString fileName=file->getFileOutputName();
         size_t pos=tsPrefix.find(fileNameStr) ;
         while (pos!=std::string::npos)
         {
           tsPrefix=tsPrefix.replace(pos,fileNameStr.size(),fileName) ;
           pos=tsPrefix.find(fileNameStr) ;
         }
        
         const std::vector<CField*> allFields = file->getAllFields();
         for (size_t j = 0; j < allFields.size(); j++)
         {
           CField* field = allFields[j];

           if (!field->ts_enabled.isEmpty() && field->ts_enabled)
           {
             CFile* tsFile = CFile::create();
             tsFile->duplicateAttributes(file);

             // Add variables originating from file and targeted to timeserie file
             for (size_t k = 0; k < fileVars.size(); k++)
               tsFile->getVirtualVariableGroup()->addChild(fileVars[k]);

            
             tsFile->name = tsPrefix + "_";
             if (!field->name.isEmpty())
               tsFile->name.get() += field->name;
             else if (field->hasDirectFieldReference()) // We cannot use getBaseFieldReference() just yet
               tsFile->name.get() += field->field_ref;
             else
               tsFile->name.get() += field->getId();

             if (!field->ts_split_freq.isEmpty())
               tsFile->split_freq = field->ts_split_freq;

             CField* tsField = tsFile->addField();
             tsField->field_ref = field->getId();

             // Add variables originating from file and targeted to timeserie field
             for (size_t k = 0; k < fieldVars.size(); k++)
               tsField->getVirtualVariableGroup()->addChild(fieldVars[k]);

             vars = field->getAllVariables();
             for (size_t k = 0; k < vars.size(); k++)
             {
               CVariable* var = vars[k];

               // Add variables originating from field and targeted to timeserie field
               if (var->ts_target.isEmpty()
                    || var->ts_target == CVariable::ts_target_attr::field || var->ts_target == CVariable::ts_target_attr::both)
                 tsField->getVirtualVariableGroup()->addChild(var);

               // Add variables originating from field and targeted to timeserie file
               if (!var->ts_target.isEmpty()
                    && (var->ts_target == CVariable::ts_target_attr::file || var->ts_target == CVariable::ts_target_attr::both))
                 tsFile->getVirtualVariableGroup()->addChild(var);
             }

             tsFile->solveFieldRefInheritance(true);

             if (file->timeseries == CFile::timeseries_attr::exclusive)
               field->enabled = false;
           }
         }

         // Finally disable the original file is need be
         if (file->timeseries == CFile::timeseries_attr::only)
          file->enabled = false;
       }
     }
   }

   //! Client side: Send information of reference grid of active fields
   void CContext::sendRefGrid()
   {
     std::set<StdString> gridIds;
     int sizeFile = this->enabledFiles.size();
     CFile* filePtr(NULL);

     // Firstly, find all reference grids of all active fields
     for (int i = 0; i < sizeFile; ++i)
     {
       filePtr = this->enabledFiles[i];
       std::vector<CField*> enabledFields = filePtr->getEnabledFields();
       int sizeField = enabledFields.size();
       for (int numField = 0; numField < sizeField; ++numField)
       {
         if (0 != enabledFields[numField]->getRelGrid())
           gridIds.insert(CGrid::get(enabledFields[numField]->getRelGrid())->getId());
       }
     }

     // Create all reference grids on server side
     StdString gridDefRoot("grid_definition");
     CGridGroup* gridPtr = CGridGroup::get(gridDefRoot);
     std::set<StdString>::const_iterator it, itE = gridIds.end();
     for (it = gridIds.begin(); it != itE; ++it)
     {
       gridPtr->sendCreateChild(*it);
       CGrid::get(*it)->sendAllAttributesToServer();
       CGrid::get(*it)->sendAllDomains();
       CGrid::get(*it)->sendAllAxis();
       CGrid::get(*it)->sendAllScalars();
     }
   }


   //! Client side: Send information of reference domain and axis of active fields
   void CContext::sendRefDomainsAxis()
   {
     std::set<StdString> domainIds, axisIds, scalarIds;

     // Find all reference domain and axis of all active fields
     int numEnabledFiles = this->enabledFiles.size();
     for (int i = 0; i < numEnabledFiles; ++i)
     {
       std::vector<CField*> enabledFields = this->enabledFiles[i]->getEnabledFields();
       int numEnabledFields = enabledFields.size();
       for (int j = 0; j < numEnabledFields; ++j)
       {
         const std::vector<StdString>& prDomAxisScalarId = enabledFields[j]->getRefDomainAxisIds();
         if ("" != prDomAxisScalarId[0]) domainIds.insert(prDomAxisScalarId[0]);
         if ("" != prDomAxisScalarId[1]) axisIds.insert(prDomAxisScalarId[1]);
         if ("" != prDomAxisScalarId[2]) scalarIds.insert(prDomAxisScalarId[2]);
       }
     }

     // Create all reference axis on server side
     std::set<StdString>::iterator itDom, itAxis, itScalar;
     std::set<StdString>::const_iterator itE;

     StdString scalarDefRoot("scalar_definition");
     CScalarGroup* scalarPtr = CScalarGroup::get(scalarDefRoot);
     itE = scalarIds.end();
     for (itScalar = scalarIds.begin(); itScalar != itE; ++itScalar)
     {
       if (!itScalar->empty())
       {
         scalarPtr->sendCreateChild(*itScalar);
         CScalar::get(*itScalar)->sendAllAttributesToServer();
       }
     }

     StdString axiDefRoot("axis_definition");
     CAxisGroup* axisPtr = CAxisGroup::get(axiDefRoot);
     itE = axisIds.end();
     for (itAxis = axisIds.begin(); itAxis != itE; ++itAxis)
     {
       if (!itAxis->empty())
       {
         axisPtr->sendCreateChild(*itAxis);
         CAxis::get(*itAxis)->sendAllAttributesToServer();
       }
     }

     // Create all reference domains on server side
     StdString domDefRoot("domain_definition");
     CDomainGroup* domPtr = CDomainGroup::get(domDefRoot);
     itE = domainIds.end();
     for (itDom = domainIds.begin(); itDom != itE; ++itDom)
     {
       if (!itDom->empty()) {
          domPtr->sendCreateChild(*itDom);
          CDomain::get(*itDom)->sendAllAttributesToServer();
       }
     }
   }

   //! Update calendar in each time step
   void CContext::updateCalendar(int step)
   {
      int prevStep = calendar->getStep();

      if (prevStep < step)
      {
        if (hasClient)
        {
          doPreTimestepOperationsForEnabledReadModeFiles();
        }

        info(50) << "updateCalendar : before : " << calendar->getCurrentDate() << endl;
        calendar->update(step);
        info(50) << "updateCalendar : after : " << calendar->getCurrentDate() << endl;
  #ifdef XIOS_MEMTRACK_LIGHT
        info(50) << " Current memory used by XIOS : "<<  MemTrack::getCurrentMemorySize()*1.0/(1024*1024)<<" Mbyte, at timestep "<<step<<" of context "<<this->getId()<<endl ;
  #endif

        if (hasClient)
        {
          doPostTimestepOperationsForEnabledReadModeFiles();
          garbageCollector.invalidate(calendar->getCurrentDate());
        }
      }
      else if (prevStep == step)
        info(50) << "updateCalendar: already at step " << step << ", no operation done." << endl;
      else // if (prevStep > step)
        ERROR("void CContext::updateCalendar(int step)",
              << "Illegal calendar update: previous step was " << prevStep << ", new step " << step << "is in the past!")
   }

   //! Server side: Create header of netcdf file
   void CContext::createFileHeader(void )
   {
      vector<CFile*>::const_iterator it;

      for (it=enabledFiles.begin(); it != enabledFiles.end(); it++)
      {
         (*it)->initFile();
      }
   }

   //! Get current context
   CContext* CContext::getCurrent(void)
   {
     return CObjectFactory::GetObject<CContext>(CObjectFactory::GetCurrentContextId()).get();
   }

   /*!
   \brief Set context with an id be the current context
   \param [in] id identity of context to be set to current
   */
   void CContext::setCurrent(const string& id)
   {
     CObjectFactory::SetCurrentContextId(id);
     CGroupFactory::SetCurrentContextId(id);
   }

  /*!
  \brief Create a context with specific id
  \param [in] id identity of new context
  \return pointer to the new context or already-existed one with identity id
  */
  CContext* CContext::create(const StdString& id)
  {
    CContext::setCurrent(id);

    bool hasctxt = CContext::has(id);
    CContext* context = CObjectFactory::CreateObject<CContext>(id).get();
    getRoot();
    if (!hasctxt) CGroupFactory::AddChild(root, context->getShared());

#define DECLARE_NODE(Name_, name_) \
    C##Name_##Definition::create(C##Name_##Definition::GetDefName());
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"

    return (context);
  }



     //! Server side: Receive a message to do some post processing
  void CContext::recvRegistry(CEventServer& event)
  {
    CBufferIn* buffer=event.subEvents.begin()->buffer;
    string id;
    *buffer>>id;
    get(id)->recvRegistry(*buffer);
  }

  void CContext::recvRegistry(CBufferIn& buffer)
  {
    if (server->intraCommRank==0)
    {
      CRegistry registry(server->intraComm) ;
      registry.fromBuffer(buffer) ;
      registryOut->mergeRegistry(registry) ;
    }
  }

  void CContext::sendRegistry(void)
  {
    registryOut->hierarchicalGatherRegistry() ;

    CEventClient event(CContext::GetType(), CContext::EVENT_ID_SEND_REGISTRY);
    if (client->isServerLeader())
    {
       CMessage msg ;
       msg<<this->getIdServer();
       if (client->clientRank==0) msg<<*registryOut ;
       const std::list<int>& ranks = client->getRanksServerLeader();
       for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
         event.push(*itRank,1,msg);
       client->sendEvent(event);
     }
     else client->sendEvent(event);
  }

} // namespace xios
