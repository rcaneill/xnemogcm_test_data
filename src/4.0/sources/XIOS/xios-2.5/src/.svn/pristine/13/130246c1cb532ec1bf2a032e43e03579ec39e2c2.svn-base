#include "file.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "object_factory.hpp"
#include "context.hpp"
#include "context_server.hpp"
#include "nc4_data_output.hpp"
#include "nc4_data_input.hpp"
#include "calendar_util.hpp"
#include "date.hpp"
#include "message.hpp"
#include "type.hpp"
#include "xios_spl.hpp"
#include "context_client.hpp"
#include "mpi.hpp"
#include "timer.hpp"
#include "server.hpp"

namespace xios {

   /// ////////////////////// Dfinitions ////////////////////// ///

   CFile::CFile(void)
      : CObjectTemplate<CFile>(), CFileAttributes()
      , vFieldGroup(), data_out(), enabledFields(), fileComm(MPI_COMM_NULL)
      , isOpen(false), read_client(0), checkRead(false), allZoneEmpty(false)
   {
     setVirtualFieldGroup(CFieldGroup::create(getId() + "_virtual_field_group"));
     setVirtualVariableGroup(CVariableGroup::create(getId() + "_virtual_variable_group"));
   }

   CFile::CFile(const StdString & id)
      : CObjectTemplate<CFile>(id), CFileAttributes()
      , vFieldGroup(), data_out(), enabledFields(), fileComm(MPI_COMM_NULL)
      , isOpen(false), read_client(0), checkRead(false), allZoneEmpty(false)
    {
      setVirtualFieldGroup(CFieldGroup::create(getId() + "_virtual_field_group"));
      setVirtualVariableGroup(CVariableGroup::create(getId() + "_virtual_variable_group"));
    }

   CFile::~CFile(void)
   { /* Ne rien faire de plus */ }

   ///---------------------------------------------------------------
  //! Get name of file
   StdString CFile::GetName(void)   { return (StdString("file")); }
   StdString CFile::GetDefName(void){ return (CFile::GetName()); }
   ENodeType CFile::GetType(void)   { return (eFile); }

   //----------------------------------------------------------------

   const StdString CFile::getFileOutputName(void) const
   {
     return (name.isEmpty() ? getId() : name) + (name_suffix.isEmpty() ? StdString("") :  name_suffix.getValue());
   }

   //----------------------------------------------------------------
   /*!
   \brief Get data writer object.
   Each enabled file in xml represents a physical netcdf file.
   This function allows to access the data writer object.
   \return data writer object.
   */
   boost::shared_ptr<CDataOutput> CFile::getDataOutput(void) const
   {
      return data_out;
   }

   /*!
   \brief Get data reader object.
   Each enabled file in xml represents a physical netcdf file.
   This function allows to access the data reader object.
   \return data reader object.
   */
   boost::shared_ptr<CDataInput> CFile::getDataInput(void) const
   {
      return data_in;
   }

   /*!
   \brief Get virtual field group
      In each file, there always exists a field group which is the ancestor of all
   fields in the file. This is considered be virtual because it is created automatically during
   file initialization and it normally doesn't appear on xml file
   \return Pointer to field group
   */
   CFieldGroup* CFile::getVirtualFieldGroup(void) const
   {
      return (this->vFieldGroup);
   }

   /*!
   \brief Get virtual variable group
      In each file, there always exists a variable group which is the ancestor of all
   variable in the file. This is considered be virtual because it is created automatically during
   file initialization and it normally doesn't appear on xml file
   \return Pointer to variable group
   */
   CVariableGroup* CFile::getVirtualVariableGroup(void) const
   {
      return (this->vVariableGroup);
   }

   //! Get all fields of a file
   std::vector<CField*> CFile::getAllFields(void) const
   {
      return (this->vFieldGroup->getAllChildren());
   }

   //! Get all variables of a file
   std::vector<CVariable*> CFile::getAllVariables(void) const
   {
      return (this->vVariableGroup->getAllChildren());
   }

   //----------------------------------------------------------------
   /*!
   \brief Get all enabled fields of file
      A field is considered to be enabled if it fullfil these conditions: it is enabled, inside a enabled file
   and its own level is not larger than file output level.
   \param [in] default_outputlevel default value output level of file
   \param [in] default_level default value level of field
   \param [in] default_enabled flag determine by default if field is enabled
   \return Vector of pointers of enabled fields
   */
   std::vector<CField*> CFile::getEnabledFields(int default_outputlevel,
                                                int default_level,
                                                bool default_enabled)
   {
      if (!this->enabledFields.empty())
         return (this->enabledFields);

      const int _outputlevel =
         (!output_level.isEmpty()) ? output_level.getValue() : default_outputlevel;
      std::vector<CField*>::iterator it;
      this->enabledFields = this->getAllFields();

      std::vector<CField*> newEnabledFields;

      for ( it = this->enabledFields.begin(); it != this->enabledFields.end(); it++ )
      {
         if (!(*it)->enabled.isEmpty()) // Si l'attribut 'enabled' est dfini ...
         {
            if (! (*it)->enabled.getValue()) continue;
//            { it--; this->enabledFields.erase(it+1); continue; }
         }
         else // Si l'attribut 'enabled' n'est pas dfini ...
         {
            if (!default_enabled) continue;
//            { it--; this->enabledFields.erase(it+1); continue; }
         }

         if (!(*it)->level.isEmpty()) // Si l'attribut 'level' est dfini ...
         {
            if ((*it)->level.getValue() > _outputlevel) continue;
//            { it--; this->enabledFields.erase(it+1); continue; }
         }
         else // Si l'attribut 'level' n'est pas dfini ...
         {
            if (default_level > _outputlevel) continue;
//            { it--; this->enabledFields.erase(it+1); continue; }
         }

//         CField* field_tmp=(*it).get();
//         shared_ptr<CField> sptfield=*it;
//         field_tmp->refObject.push_back(sptfield);
         newEnabledFields.push_back(*it);
         // Le champ est finalement actif, on y ajoute sa propre reference.
//         (*it)->refObject.push_back(*it);
         // Le champ est finalement actif, on y ajoute la rfrence au champ de base.
         (*it)->setRelFile(CFile::get(this));
      }
      enabledFields = newEnabledFields;

      return (this->enabledFields);
   }

   //----------------------------------------------------------------
   //! Change virtual field group to a new one
   void CFile::setVirtualFieldGroup(CFieldGroup* newVFieldGroup)
   {
      this->vFieldGroup = newVFieldGroup;
   }

   //! Change virtual variable group to new one
   void CFile::setVirtualVariableGroup(CVariableGroup* newVVariableGroup)
   {
      this->vVariableGroup = newVVariableGroup;
   }

   //----------------------------------------------------------------
   bool CFile::isSyncTime(void)
   {
     CContext* context = CContext::getCurrent();
     const CDate& currentDate = context->calendar->getCurrentDate();
     if (!sync_freq.isEmpty())
     {
       if (lastSync + sync_freq.getValue() < currentDate)
       {
         lastSync = currentDate;
         return true;
        }
      }
      return false;
    }

   //! Initialize a file in order to write into it
   void CFile::initWrite(void)
   {
      CContext* context = CContext::getCurrent();
      const CDate& currentDate = context->calendar->getCurrentDate();
      CContextServer* server = context->server;

      lastSync  = currentDate;
      lastSplit = currentDate;
      if (!split_freq.isEmpty())
      {
        StdString keySuffix("CFile::"+getFileOutputName()+"::") ; 
        if (context->registryIn->foundKey(keySuffix+"splitStart") && context->registryIn->foundKey(keySuffix+"splitEnd"))
        {
          CDate savedSplitStart(*context->getCalendar()), savedSplitEnd(*context->getCalendar());
          context->registryIn->getKey(keySuffix+"splitStart", savedSplitStart);
          context->registryIn->getKey(keySuffix+"splitEnd",   savedSplitEnd);

          if (savedSplitStart <= lastSplit && lastSplit <= savedSplitEnd)
            lastSplit = savedSplitStart;
        }
      }
      isOpen = false;      

//      if (!record_offset.isEmpty() && record_offset < 0)
//        ERROR("void CFile::initFile(void)",
//              "Invalid 'record_offset', this attribute cannot be negative.");
      const int recordOffset = record_offset.isEmpty() ? 0 : record_offset;

      set<StdString> setAxis;
      set<StdString> setDomains;

      std::vector<CField*>::iterator it, end = this->enabledFields.end();
      for (it = this->enabledFields.begin(); it != end; it++)
      {
         CField* field = *it;         
         std::vector<CAxis*> vecAxis = field->grid->getAxis();
         for (size_t i = 0; i < vecAxis.size(); ++i)
           setAxis.insert(vecAxis[i]->getAxisOutputName());
         std::vector<CDomain*> vecDomains = field->grid->getDomains();
         for (size_t i = 0; i < vecDomains.size(); ++i)
           setDomains.insert(vecDomains[i]->getDomainOutputName());

         field->resetNStep(recordOffset);
      }
      nbAxis = setAxis.size();
      nbDomains = setDomains.size();

      // create sub communicator for file
      createSubComFile();

      // if (time_counter.isEmpty()) time_counter.setValue(time_counter_attr::centered);
      if (time_counter_name.isEmpty()) time_counter_name = "time_counter";
    }

    //! Initialize a file in order to write into it
    void CFile::initRead(void)
    {
      if (checkRead) return;
      createSubComFile();
      checkRead = true;
    }

    /*!
      Create a sub communicator in which processes participate in reading/opening file
    */
    void CFile::createSubComFile()
    {
      CContext* context = CContext::getCurrent();
      CContextServer* server = context->server;

      // create sub communicator for file
      allZoneEmpty = true;      
      std::vector<CField*>::iterator it, end = this->enabledFields.end();
      for (it = this->enabledFields.begin(); it != end; it++)
      {
         CField* field = *it;
         bool nullGrid = (0 == field->grid);
         allZoneEmpty &= nullGrid ? false : !field->grid->doGridHaveDataToWrite();
      }

      int color = allZoneEmpty ? 0 : 1;
      MPI_Comm_split(server->intraComm, color, server->intraCommRank, &fileComm);
      if (allZoneEmpty) MPI_Comm_free(&fileComm);
    }

    /*
       Check condition to write into a file
       For now, we only use the level-2 server to write files (if this mode is activated)
       or classical server to do this job.
    */
    void CFile::checkWriteFile(void)
    {
      CContext* context = CContext::getCurrent();
      // Done by classical server or secondary server
      // This condition should be changed soon
      if (CServer::serverLevel == 0 || CServer::serverLevel == 2)
      {
        if (mode.isEmpty() || mode.getValue() == mode_attr::write)
        {
          CTimer::get("Files : create headers").resume();
          if (!isOpen) createHeader();
          CTimer::get("Files : create headers").suspend();
          checkSync();
        }        
        checkSplit(); // REally need this?
      }
    }

    /*
       Check condition to read from a file
       For now, we only use the level-1 server to write files (if this mode is activated)
       or classical server to do this job.
       This function can be used by client for reading metadata
    */
    void CFile::checkReadFile(void)
    {
      CContext* context = CContext::getCurrent();
      // Done by classical server or secondary server
      // TODO: This condition should be changed soon. It only works with maximum number of level as 2
      if (CServer::serverLevel == 0 || CServer::serverLevel == 1)
      {
        if (!mode.isEmpty() && mode.getValue() == mode_attr::read)
        {
          CTimer::get("Files : open headers").resume();
          
          if (!isOpen) openInReadMode();

          CTimer::get("Files : open headers").suspend();
        }
        //checkSplit(); // Really need for reading?
      }
    }

    /*!
      Verify if a process participates in an opening-file communicator 
      \return true if the process doesn't participate in opening file
    */
    bool CFile::isEmptyZone()
    {
      return allZoneEmpty;
    }

    /*!
    \brief Verify if synchronisation should be done
        If syn option is enabled, syn frequence and current time will be used to
    calculate the moment to syn file(s)
    \return True if it is the moment to synchronize file, otherwise false
    */
   bool CFile::checkSync(void)
   {
     CContext* context = CContext::getCurrent();
     const CDate& currentDate = context->calendar->getCurrentDate();
     if (!sync_freq.isEmpty())
     {
       if (lastSync + sync_freq.getValue() <= currentDate)
       {
         lastSync = currentDate;
         data_out->syncFile();
         return true;
        }
      }
      return false;
    }

    /*!
    \brief Verify if splitting should be done
        If split option is enabled, split frequence and current time will be used to
    calculate the moment to split file
    \return True if it is the moment to split file, otherwise false
    */
    bool CFile::checkSplit(void)
    {
      CContext* context = CContext::getCurrent();
      const CDate& currentDate = context->calendar->getCurrentDate();
      if (!split_freq.isEmpty())
      {
        if (currentDate > lastSplit + split_freq.getValue())
        {
          lastSplit = lastSplit + split_freq.getValue();
          std::vector<CField*>::iterator it, end = this->enabledFields.end();
          for (it = this->enabledFields.begin(); it != end; it++)
          {
            (*it)->resetNStep();
            (*it)->resetNStepMax();
          }
          if (mode.isEmpty() || mode.getValue() == mode_attr::write)
            createHeader();
          else
            openInReadMode();
          return true;
        }
      }
      return false;
    }

   /*!
   \brief Create header of netcdf file
   There are some information to fill in header of each netcdf.
   */
   void CFile::createHeader(void)
   {
      CContext* context = CContext::getCurrent();
      CContextServer* server = context->server;

      if (!allZoneEmpty)
      {
         StdString filename = getFileOutputName();

// determine splitting format in the file name  : firstPart%start_date%middlePart%end_date%lastPart

         std::string strStartDate="%start_date%" ;
         std::string strEndDate="%end_date%" ;

         std::string firstPart ;
         std::string middlePart ;
         std::string lastPart ;
         size_t pos1, pos2 ;
         bool hasStartDate=false ;
         bool hasEndDate=false ;
         bool hasSplit = (!split_freq.isEmpty());
                  
         pos1=filename.find(strStartDate) ;
         if (pos1!=std::string::npos)
         {
           firstPart=filename.substr(0,pos1) ;
           pos1+=strStartDate.size() ;
           hasStartDate=true ;
         }
         else pos1=0 ;

         pos2=filename.find(strEndDate,pos1) ;
         if (pos2!=std::string::npos)
         {
           middlePart=filename.substr(pos1,pos2-pos1) ;
           pos2+=strEndDate.size() ;
           lastPart=filename.substr(pos2,filename.size()-pos2) ;
           hasEndDate=true ;
         }
         else middlePart=filename.substr(pos1,filename.size()) ;

         if (!hasStartDate && !hasEndDate)
         {
           hasStartDate=true ;
           hasEndDate=true;
           firstPart=middlePart ;
           if (hasSplit) firstPart +="_";
           middlePart="-" ;
         }
   
         StdOStringStream oss;

         if (!split_freq.isEmpty())
         {
           CDate split_start ;
           CDate splitEnd ;
           if (!split_start_offset.isEmpty()) split_start=lastSplit + split_start_offset ;
           else split_start=lastSplit ;

           splitEnd = lastSplit + split_freq ;
           if (!split_last_date.isEmpty())
           {
             CDate splitLastDate=CDate::FromString(split_last_date,*CContext::getCurrent()->getCalendar()) ;
             if( splitLastDate < splitEnd)  splitEnd=splitLastDate ;
           }
            
           if (!split_end_offset.isEmpty()) splitEnd = splitEnd + split_end_offset;
           else splitEnd = splitEnd - 1 * Second;

           string splitFormat;
           if (split_freq_format.isEmpty())
           {
             CDuration splitFreq = split_freq.getValue();
             splitFreq.solveTimeStep(*CContext::getCurrent()->getCalendar());
             if (splitFreq.second != 0) splitFormat = "%y%mo%d%h%mi%s";
             else if (splitFreq.minute != 0) splitFormat = "%y%mo%d%h%mi";
             else if (splitFreq.hour != 0) splitFormat = "%y%mo%d%h";
             else if (splitFreq.day != 0) splitFormat = "%y%mo%d";
             else if (splitFreq.month != 0) splitFormat = "%y%mo";
             else splitFormat = "%y";
           }
           else splitFormat = split_freq_format;

           oss << firstPart ;
           if (hasStartDate) oss << split_start.getStr(splitFormat) ;
           oss << middlePart ;
           if (hasEndDate) oss << splitEnd.getStr(splitFormat);
           oss << lastPart ;

           StdString keySuffix("CFile::"+getFileOutputName()+"::") ; 
           context->registryOut->setKey(keySuffix+"splitStart", lastSplit);
           context->registryOut->setKey(keySuffix+"splitEnd",   splitEnd);
         }
         else oss<<firstPart<<lastPart ;

        bool append = !this->append.isEmpty() && this->append.getValue();

         bool useClassicFormat = !format.isEmpty() && format == format_attr::netcdf4_classic;
         bool useCFConvention = convention.isEmpty() || convention == convention_attr::CF;

         bool multifile = true;
         if (!type.isEmpty())
         {
           if (type == type_attr::one_file) multifile = false;
           else if (type == type_attr::multiple_file) multifile = true;

         }
#ifndef USING_NETCDF_PAR
         if (!multifile)
         {
            info(0) << "!!! Warning -> Using non parallel version of netcdf, switching in multiple_file mode for file : " << filename << " ..." << endl;
            multifile = true;
          }
#endif
         if (multifile)
         {
            int commSize, commRank;
            MPI_Comm_size(fileComm, &commSize);
            MPI_Comm_rank(fileComm, &commRank);

            if (server->intraCommSize > 1)
            {
              oss << "_" ;
              int width=0; int n = commSize-1;
              while (n != 0) { n = n / 10; width++;}
              if (!min_digits.isEmpty())
                if (width < min_digits) width = min_digits;
              oss.width(width);
              oss.fill('0');
              oss << right << commRank;
            }
         }
         oss << ".nc";

         bool isCollective = par_access.isEmpty() ||  par_access == par_access_attr::collective;

         if (isOpen) data_out->closeFile();

        data_out = shared_ptr<CDataOutput>(new CNc4DataOutput(this, oss.str(), append, useClassicFormat, useCFConvention,
                                                              fileComm, multifile, isCollective, time_counter_name));
        isOpen = true;

        data_out->writeFile(CFile::get(this));

        if (!useCFConvention) sortEnabledFieldsForUgrid();

        // Do not recreate the file structure if opening an existing file
        if (!data_out->IsInAppendMode())
        {
          std::vector<CField*>::iterator it, end = this->enabledFields.end();
          for (it = this->enabledFields.begin(); it != end; it++)
          {
            CField* field = *it;
            this->data_out->writeFieldGrid(field);
          }
          this->data_out->writeTimeDimension();

          for (it = this->enabledFields.begin(); it != end; it++)
          {
            CField* field = *it;
            this->data_out->writeFieldTimeAxis(field);
          }
          
          for (it = this->enabledFields.begin(); it != end; it++)
          {
            CField* field = *it;
            this->data_out->writeField(field);
          }

          vector<CVariable*> listVars = getAllVariables();
          for (vector<CVariable*>::iterator it = listVars.begin(); it != listVars.end(); it++)
            this->data_out->writeAttribute(*it);

          this->data_out->definition_end();
        }
        else
        {
          // check time axis even in append mode
          std::vector<CField*>::iterator it, end = this->enabledFields.end();
          for (it = this->enabledFields.begin(); it != end; it++)
          {
            CField* field = *it;
            this->data_out->writeFieldTimeAxis(field);
          }
        }
      }
   }

  /*!
  \brief Open an existing NetCDF file in read-only mode
  */
  void CFile::openInReadMode()
  {
    CContext* context = CContext::getCurrent();
    CContextServer* server = context->server;
    MPI_Comm readComm = this->fileComm;

    if (!allZoneEmpty)
    {
      StdString filename = getFileOutputName();
      StdOStringStream oss;
      oss << filename;

      if (!split_freq.isEmpty())
      {
        string splitFormat;
        if (split_freq_format.isEmpty())
        {
          CDuration splitFreq = split_freq.getValue();
          splitFreq.solveTimeStep(*CContext::getCurrent()->getCalendar());
          if (splitFreq.second != 0) splitFormat = "%y%mo%d%h%mi%s";
          else if (splitFreq.minute != 0) splitFormat = "%y%mo%d%h%mi";
          else if (splitFreq.hour != 0) splitFormat = "%y%mo%d%h";
          else if (splitFreq.day != 0) splitFormat = "%y%mo%d";
          else if (splitFreq.month != 0) splitFormat = "%y%mo";
          else splitFormat = "%y";
        }
        else splitFormat = split_freq_format;
        oss << "_" << lastSplit.getStr(splitFormat)
        << "-" << (lastSplit + split_freq.getValue() - 1 * Second).getStr(splitFormat);
      }

      bool multifile = true;
      if (!type.isEmpty())
      {
        if (type == type_attr::one_file) multifile = false;
        else if (type == type_attr::multiple_file) multifile = true;
      }
  #ifndef USING_NETCDF_PAR
      if (!multifile)
      {
        info(0) << "!!! Warning -> Using non parallel version of netcdf, switching in multiple_file mode for file : " << filename << " ..." << endl;
        multifile = true;
      }
  #endif
      if (multifile)
      {
        int commSize, commRank;
        MPI_Comm_size(readComm, &commSize);
        MPI_Comm_rank(readComm, &commRank);

        if (server->intraCommSize > 1)
        {
          oss << "_";
          int width = 0, n = commSize - 1;
          while (n != 0) { n = n / 10; width++; }
          if (!min_digits.isEmpty() && width < min_digits)
            width = min_digits;
          oss.width(width);
          oss.fill('0');
          oss << right << commRank;
        }
      }
      oss << ".nc";

      bool isCollective = par_access.isEmpty() || par_access == par_access_attr::collective;
      bool readMetaDataPar = true;
      if (!context->hasServer) readMetaDataPar = (read_metadata_par.isEmpty()) ? false : read_metadata_par;

      if (isOpen) data_out->closeFile();
      bool ugridConvention = !convention.isEmpty() ? (convention == convention_attr::UGRID) : false;
      if (time_counter_name.isEmpty())
        data_in = shared_ptr<CDataInput>(new CNc4DataInput(oss.str(), readComm, multifile, isCollective, readMetaDataPar, ugridConvention));
      else
        data_in = shared_ptr<CDataInput>(new CNc4DataInput(oss.str(), readComm, multifile, isCollective, readMetaDataPar, ugridConvention, time_counter_name));
      isOpen = true;
    }
  }

   //! Close file
   void CFile::close(void)
   {
     if (!allZoneEmpty)
       if (isOpen)
       {
         if (mode.isEmpty() || mode.getValue() == mode_attr::write)
          this->data_out->closeFile();
         else
          this->data_in->closeFile();
        isOpen = false;
       }
      if (fileComm != MPI_COMM_NULL) MPI_Comm_free(&fileComm);
   }
   //----------------------------------------------------------------

   void CFile::readAttributesOfEnabledFieldsInReadMode()
   {
     if (enabledFields.empty()) return;

     // Just check file and try to open it
     if (time_counter_name.isEmpty()) time_counter_name = "time_counter";

     checkReadFile();

     for (int idx = 0; idx < enabledFields.size(); ++idx)
     {
        // First of all, find out which domain and axis associated with this field
        enabledFields[idx]->solveGridReference();

        // Read attributes of domain and axis from this file
        this->data_in->readFieldAttributesMetaData(enabledFields[idx]);

        // Now complete domain and axis associated with this field
        enabledFields[idx]->solveGenerateGrid();

        // Read necessary value from file
        this->data_in->readFieldAttributesValues(enabledFields[idx]);

        // Fill attributes for base reference
        enabledFields[idx]->solveGridDomainAxisBaseRef();
     }

     // Now everything is ok, close it
     close();
   }


   /*!
   \brief Parse xml file and write information into file object
   \param [in] node xmld node corresponding in xml file
   */
   void CFile::parse(xml::CXMLNode & node)
   {
      SuperClass::parse(node);

      if (node.goToChildElement())
      {
        do
        {
           if (node.getElementName()=="field" || node.getElementName()=="field_group") this->getVirtualFieldGroup()->parseChild(node);
           else if (node.getElementName()=="variable" || node.getElementName()=="variable_group") this->getVirtualVariableGroup()->parseChild(node);
        } while (node.goToNextElement());
        node.goToParentElement();
      }

   }
   //----------------------------------------------------------------

   /*!
   \brief Represent a file in form of string with all its info
   \return String
   */
   StdString CFile::toString(void) const
   {
      StdOStringStream oss;

      oss << "<" << CFile::GetName() << " ";
      if (this->hasId())
         oss << " id=\"" << this->getId() << "\" ";
      oss << SuperClassAttribute::toString() << ">" << std::endl;
      if (this->getVirtualFieldGroup() != NULL)
         oss << *this->getVirtualFieldGroup() << std::endl;
      oss << "</" << CFile::GetName() << " >";
      return (oss.str());
   }

   //----------------------------------------------------------------

   /*!
   \brief Find all inheritace among objects in a file.
   \param [in] apply (true) write attributes of parent into ones of child if they are empty
                     (false) write attributes of parent into a new container of child
   \param [in] parent
   */
   void CFile::solveDescInheritance(bool apply, const CAttributeMap * const parent)
   {
      SuperClassAttribute::setAttributes(parent,apply);
      this->getVirtualFieldGroup()->solveDescInheritance(apply, NULL);
      this->getVirtualVariableGroup()->solveDescInheritance(apply, NULL);
   }

   //----------------------------------------------------------------

   /*!
   \brief Resolve all reference of active fields.
      In order to know exactly which data each active field has, a search for all its
   reference to find its parents or/and its base reference object must be done. Moreover
   during this search, there are some information that can only be sent to server AFTER
   all information of active fields are created on server side, e.g: checking mask or index
   \param [in] sendToServer: Send all info to server (true) or only a part of it (false)
   */
   void CFile::solveOnlyRefOfEnabledFields(bool sendToServer)
   {
     int size = this->enabledFields.size();
     for (int i = 0; i < size; ++i)
     {
       this->enabledFields[i]->solveOnlyReferenceEnabledField(sendToServer);
     }
   }

   void CFile::checkGridOfEnabledFields()
   { 
     int size = this->enabledFields.size();
     for (int i = 0; i < size; ++i)
     {
       this->enabledFields[i]->checkGridOfEnabledFields();
     }
   }

   void CFile::sendGridComponentOfEnabledFields()
   { 
     int size = this->enabledFields.size();
     for (int i = 0; i < size; ++i)
     {
       this->enabledFields[i]->sendGridComponentOfEnabledFields();
     }
   }

   /*!
   \brief Sorting domains with the same name (= describing the same mesh) in the decreasing order of nvertex for UGRID files.
   This insures that the domain with the highest nvertex is written first and thus all known mesh connectivity is generated at once by this domain.
   */
   void CFile::sortEnabledFieldsForUgrid()
   {
     int size = this->enabledFields.size();
     std::vector<int> domainNvertices;
     std::vector<StdString> domainNames;

     for (int i = 0; i < size; ++i)
     {
       std::vector<CDomain*> domain = this->enabledFields[i]->getRelGrid()->getDomains();
       if (domain.size() != 1)
       {
         ERROR("void CFile::sortEnabledFieldsForUgrid()",
               "A domain, and only one, should be defined for grid "<< this->enabledFields[i]->getRelGrid()->getId() << ".");
       }
       StdString domainName = domain[0]->getDomainOutputName();
       int nvertex;
       if (domain[0]->nvertex.isEmpty())
       {
         ERROR("void CFile::sortEnabledFieldsForUgrid()",
               "Attributes nvertex must be defined for domain "<< domain[0]->getDomainOutputName() << ".");
       }
       else
         nvertex = domain[0]->nvertex;

       for (int j = 0; j < i; ++j)
       {
         if (domainName == domainNames[j] && nvertex > domainNvertices[j])
         {
           CField* tmpSwap = this->enabledFields[j];
           this->enabledFields[j] = this->enabledFields[i];
           this->enabledFields[i] = tmpSwap;
           domainNames.push_back(domainNames[j]);
           domainNames[j] = domainName;
           domainNvertices.push_back(domainNvertices[j]);
           domainNvertices[j] = nvertex;
         }
         else
         {
           domainNames.push_back(domainName);
           domainNvertices.push_back(nvertex);
         }
       }
       if (i==0)
       {
         domainNames.push_back(domainName);
         domainNvertices.push_back(nvertex);
       }
     }
   }

   void CFile::sendGridOfEnabledFields()
   { 
     int size = this->enabledFields.size();
     for (int i = 0; i < size; ++i)
     {
       this->enabledFields[i]->sendGridOfEnabledFields();
     }
   }

   void CFile::generateNewTransformationGridDest()
   {
     int size = this->enabledFields.size();
     for (int i = 0; i < size; ++i)
     {
       this->enabledFields[i]->generateNewTransformationGridDest();
     }
   }

   /*!
   \brief Resolve all reference of active fields.
      In order to know exactly which data each active field has, a search for all its
   reference to find its parents or/and its base reference object must be done. Moreover
   during this search, there are some information that can only be sent to server AFTER
   all information of active fields are created on server side, e.g: checking mask or index
   \param [in] sendToServer: Send all info to server (true) or only a part of it (false)
   */
   void CFile::solveAllRefOfEnabledFieldsAndTransform(bool sendToServer)
   {
     int size = this->enabledFields.size();
     for (int i = 0; i < size; ++i)
     {       
      this->enabledFields[i]->solveAllEnabledFieldsAndTransform();
     }
   }

   /*!
    * Constructs the filter graph for each active field.
    *
    * \param gc the garbage collector to use when building the filter graph
    */
   void CFile::buildFilterGraphOfEnabledFields(CGarbageCollector& gc)
   {
     int size = this->enabledFields.size();
     for (int i = 0; i < size; ++i)
     {
       this->enabledFields[i]->buildFilterGraph(gc, true);
     }
   }

   /*!
    * Post-process the filter graph for each active field.
    */
   void CFile::postProcessFilterGraph()
   {
     int size = this->enabledFields.size();
     for (int i = 0; i < size; ++i)
     {
       this->enabledFields[i]->checkIfMustAutoTrigger();
     }
   }

   /*!
     Prefetching the data for enabled fields read from file.
   */
   void CFile::prefetchEnabledReadModeFields(void)
   {
     if (mode.isEmpty() || mode.getValue() != mode_attr::read)
       return;

     int size = this->enabledFields.size();
     for (int i = 0; i < size; ++i)
       this->enabledFields[i]->sendReadDataRequest(CContext::getCurrent()->getCalendar()->getCurrentDate());
   }

   /*!
     Do all pre timestep operations for enabled fields in read mode:
      - Check that the data excepted from server has been received
      - Check if some filters must auto-trigger
   */
   void CFile::doPreTimestepOperationsForEnabledReadModeFields(void)
   {
     if (mode.isEmpty() || mode.getValue() != mode_attr::read)
       return;

     int size = this->enabledFields.size();
     for (int i = 0; i < size; ++i)
     {
       this->enabledFields[i]->checkForLateDataFromServer();
       this->enabledFields[i]->autoTriggerIfNeeded();
     }
   }

   /*!
     Do all post timestep operations for enabled fields in read mode:
      - Prefetch the data read from file when needed
   */
   void CFile::doPostTimestepOperationsForEnabledReadModeFields(void)
   {
     if (mode.isEmpty() || mode.getValue() != mode_attr::read)
       return;

     int size = this->enabledFields.size();
     for (int i = 0; i < size; ++i)
     {
       this->enabledFields[i]->sendReadDataRequestIfNeeded();
     }
   }

   void CFile::solveFieldRefInheritance(bool apply)
   {
      // Rsolution des hritages par rfrence de chacun des champs contenus dans le fichier.
      std::vector<CField*> allF = this->getAllFields();
      for (unsigned int i = 0; i < allF.size(); i++)
         allF[i]->solveRefInheritance(apply);
   }

   //----------------------------------------------------------------

   /*!
   \brief Add a field into file.
      A field is added into file and it will be written out if the file is enabled and
   level of this field is smaller than level_output. A new field won't be created if one
   with id has already existed
   \param [in] id String identity of new field
   \return Pointer to added (or already existed) field
   */
   CField* CFile::addField(const string& id)
   {
     return vFieldGroup->createChild(id);
   }

   /*!
   \brief Add a field group into file.
      A field group is added into file and it will play a role as parents for fields.
   A new field group won't be created if one with id has already existed
   \param [in] id String identity of new field group
   \return Pointer to added (or already existed) field group
   */
   CFieldGroup* CFile::addFieldGroup(const string& id)
   {
     return vFieldGroup->createChildGroup(id);
   }

   /*!
   \brief Add a variable into file.
      A variable is added into file and if one with id has already existed, pointer to
   it will be returned.
      Variable as long as attributes are information container of file.
   However, whereas attributes are "fixed" information, variables provides a more flexible way to user
   to fill in (extra) information for a file.
   \param [in] id String identity of new variable
   \return Pointer to added (or already existed) variable
   */
   CVariable* CFile::addVariable(const string& id)
   {
     return vVariableGroup->createChild(id);
   }

   /*!
   \brief Add a variable group into file.
      A variable group is added into file and it will play a role as parents for variables.
   A new variable group won't be created if one with id has already existed
   \param [in] id String identity of new variable group
   \return Pointer to added (or already existed) variable group
   */
   CVariableGroup* CFile::addVariableGroup(const string& id)
   {
     return vVariableGroup->createChildGroup(id);
   }

   void CFile::setContextClient(CContextClient* newContextClient)
   {
     client = newContextClient;
     size_t size = this->enabledFields.size();
     for (size_t i = 0; i < size; ++i)
     {
       this->enabledFields[i]->setContextClient(newContextClient);
     }
   }

   CContextClient* CFile::getContextClient()
   {
     return client;
   }

   void CFile::setReadContextClient(CContextClient* readContextclient)
   {
     read_client = readContextclient;
   }

   CContextClient* CFile::getReadContextClient()
   {
     return read_client;
   }

   /*!
   \brief Send a message to create a field on server side
   \param[in] id String identity of field that will be created on server
   */
   void CFile::sendAddField(const string& id, CContextClient* client)
   {
      sendAddItem(id, EVENT_ID_ADD_FIELD, client);
   }

   /*!
   \brief Send a message to create a field group on server side
   \param[in] id String identity of field group that will be created on server
   */
   void CFile::sendAddFieldGroup(const string& id, CContextClient* client)
   {
      sendAddItem(id, (int)EVENT_ID_ADD_FIELD_GROUP, client);
   }

   /*!
   \brief Receive a message annoucing the creation of a field on server side
   \param[in] event Received event
   */
   void CFile::recvAddField(CEventServer& event)
   {

      CBufferIn* buffer = event.subEvents.begin()->buffer;
      string id;
      *buffer>>id;
      get(id)->recvAddField(*buffer);
   }

   /*!
   \brief Receive a message annoucing the creation of a field on server side
   \param[in] buffer Buffer containing message
   */
   void CFile::recvAddField(CBufferIn& buffer)
   {
      string id;
      buffer>>id;
      addField(id);
   }

   /*!
   \brief Receive a message annoucing the creation of a field group on server side
   \param[in] event Received event
   */
   void CFile::recvAddFieldGroup(CEventServer& event)
   {

      CBufferIn* buffer = event.subEvents.begin()->buffer;
      string id;
      *buffer>>id;
      get(id)->recvAddFieldGroup(*buffer);
   }

   /*!
   \brief Receive a message annoucing the creation of a field group on server side
   \param[in] buffer Buffer containing message
   */
   void CFile::recvAddFieldGroup(CBufferIn& buffer)
   {
      string id;
      buffer>>id;
      addFieldGroup(id);
   }

   /*!
   \brief Send messages to duplicate all variables on server side
      Because each variable has also its attributes. So first thing to do is replicate
   all these attributes on server side. Because variable can have a value, the second thing
   is to duplicate this value on server, too.
   */
   void CFile::sendAddAllVariables(CContextClient* client)
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
   \brief Send a message to create a variable group on server side
   \param[in] id String identity of variable group that will be created on server
   \param [in] client client to which we will send this adding action
   */
   void CFile::sendAddVariableGroup(const string& id, CContextClient* client)
   {
      sendAddItem(id, (int)EVENT_ID_ADD_VARIABLE_GROUP, client);
   }

   /*
     Send message to add a variable into a file within a certain client
     \param [in] id String identity of a variable
     \param [in] client client to which we will send this adding action
   */
   void CFile::sendAddVariable(const string& id, CContextClient* client)
   {
      sendAddItem(id, (int)EVENT_ID_ADD_VARIABLE, client);
   }

   /*!
   \brief Receive a message annoucing the creation of a variable on server side
   \param[in] event Received event
   */
   void CFile::recvAddVariable(CEventServer& event)
   {

      CBufferIn* buffer = event.subEvents.begin()->buffer;
      string id;
      *buffer>>id;
      get(id)->recvAddVariable(*buffer);
   }

   /*!
   \brief Receive a message annoucing the creation of a variable on server side
   \param[in] buffer Buffer containing message
   */
   void CFile::recvAddVariable(CBufferIn& buffer)
   {
      string id;
      buffer>>id;
      addVariable(id);
   }

   /*!
   \brief Receive a message annoucing the creation of a variable group on server side
   \param[in] event Received event
   */
   void CFile::recvAddVariableGroup(CEventServer& event)
   {

      CBufferIn* buffer = event.subEvents.begin()->buffer;
      string id;
      *buffer>>id;
      get(id)->recvAddVariableGroup(*buffer);
   }

   /*!
   \brief Receive a message annoucing the creation of a variable group on server side
   \param[in] buffer Buffer containing message
   */
   void CFile::recvAddVariableGroup(CBufferIn& buffer)
   {
      string id;
      buffer>>id;
      addVariableGroup(id);
   }

   /*!
     \brief Sending all active (enabled) fields from client to server.
   Each field is identified uniquely by its string identity. Not only should we
   send the id to server but also we need to send ids of reference domain and reference axis.
   With these two id, it's easier to make reference to grid where all data should be written.
   Remark: This function must be called AFTER all active (enabled) files have been created on the server side
   */
   void CFile::sendEnabledFields(CContextClient* client)
   {
     size_t size = this->enabledFields.size();
     for (size_t i = 0; i < size; ++i)
     {
       CField* field = this->enabledFields[i];
       this->sendAddField(field->getId(), client);
       field->checkTimeAttributes();
       field->sendAllAttributesToServer(client);
       field->sendAddAllVariables(client);
     }
   }


   /*!
   \brief Dispatch event received from client
      Whenever a message is received in buffer of server, it will be processed depending on
   its event type. A new event type should be added in the switch list to make sure
   it processed on server side.
   \param [in] event: Received message
   */
   bool CFile::dispatchEvent(CEventServer& event)
   {
      if (SuperClass::dispatchEvent(event)) return true;
      else
      {
        switch(event.type)
        {
           case EVENT_ID_ADD_FIELD :
             recvAddField(event);
             return true;
             break;

           case EVENT_ID_ADD_FIELD_GROUP :
             recvAddFieldGroup(event);
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
              ERROR("bool CFile::dispatchEvent(CEventServer& event)", << "Unknown Event");
           return false;
        }
      }
   }




   ///---------------------------------------------------------------

} // namespace xios
