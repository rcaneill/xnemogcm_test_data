#include "domain.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"

#include "xios_spl.hpp"
#include "event_client.hpp"
#include "event_server.hpp"
#include "buffer_in.hpp"
#include "message.hpp"
#include "type.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "context_server.hpp"
#include "array_new.hpp"
#include "distribution_client.hpp"
#include "server_distribution_description.hpp"
#include "client_server_mapping_distributed.hpp"

#include <algorithm>

namespace xios {

   /// ////////////////////// Définitions ////////////////////// ///

   CDomain::CDomain(void)
      : CObjectTemplate<CDomain>(), CDomainAttributes()
      , isChecked(false), relFiles(), isClientChecked(false), nbSenders(), indSrv_(), connectedServerRank_()
      , hasBounds(false), hasArea(false), isCompressible_(false), isUnstructed_(false)
      , isClientAfterTransformationChecked(false), hasLonLat(false)
      , isRedistributed_(false), hasPole(false), doZoomByIndex_(false)
      , lonvalue(), latvalue(), bounds_lonvalue(), bounds_latvalue()
      , globalLocalIndexMap_(), computedWrittenIndex_(false)
      , clients(),hasLatInReadFile_(false), hasBoundsLatInReadFile_(false)
      , hasLonInReadFile_(false), hasBoundsLonInReadFile_(false)
   {
   }

   CDomain::CDomain(const StdString & id)
      : CObjectTemplate<CDomain>(id), CDomainAttributes()
      , isChecked(false), relFiles(), isClientChecked(false), nbSenders(), indSrv_(), connectedServerRank_() 
      , hasBounds(false), hasArea(false), isCompressible_(false), isUnstructed_(false)
      , isClientAfterTransformationChecked(false), hasLonLat(false)
      , isRedistributed_(false), hasPole(false), doZoomByIndex_(false)
      , lonvalue(), latvalue(), bounds_lonvalue(), bounds_latvalue()
      , globalLocalIndexMap_(), computedWrittenIndex_(false)
      , clients(), hasBoundsLatInReadFile_(false)
      , hasLonInReadFile_(false), hasBoundsLonInReadFile_(false)
   {
    }

   CDomain::~CDomain(void)
   {
   }

   ///---------------------------------------------------------------

   void CDomain::assignMesh(const StdString meshName, const int nvertex)
   {
     mesh = CMesh::getMesh(meshName, nvertex);
   }

   CDomain* CDomain::createDomain()
   {
     CDomain* domain = CDomainGroup::get("domain_definition")->createChild();
     return domain;
   }

   std::map<StdString, ETranformationType> CDomain::transformationMapList_ = std::map<StdString, ETranformationType>();
   bool CDomain::_dummyTransformationMapList = CDomain::initializeTransformationMap(CDomain::transformationMapList_);

   bool CDomain::initializeTransformationMap(std::map<StdString, ETranformationType>& m)
   {
     m["zoom_domain"] = TRANS_ZOOM_DOMAIN;
     m["interpolate_domain"] = TRANS_INTERPOLATE_DOMAIN;
     m["generate_rectilinear_domain"] = TRANS_GENERATE_RECTILINEAR_DOMAIN;
     m["compute_connectivity_domain"] = TRANS_COMPUTE_CONNECTIVITY_DOMAIN;
     m["expand_domain"] = TRANS_EXPAND_DOMAIN;
     m["reorder_domain"] = TRANS_REORDER_DOMAIN;
     return true ;
   }

   const std::set<StdString> & CDomain::getRelFiles(void) const
   {
      return (this->relFiles);
   }


   /*!
     Returns the number of indexes written by each server.
     \return the number of indexes written by each server
   */
   int CDomain::getNumberWrittenIndexes(MPI_Comm writtenCom)
   {
     int writtenSize;
     MPI_Comm_size(writtenCom, &writtenSize);
     return numberWrittenIndexes_[writtenSize];
   }

   /*!
     Returns the total number of indexes written by the servers.
     \return the total number of indexes written by the servers
   */
   int CDomain::getTotalNumberWrittenIndexes(MPI_Comm writtenCom)
   {
     int writtenSize;
     MPI_Comm_size(writtenCom, &writtenSize);
     return totalNumberWrittenIndexes_[writtenSize];
   }

   /*!
     Returns the offset of indexes written by each server.
     \return the offset of indexes written by each server
   */
   int CDomain::getOffsetWrittenIndexes(MPI_Comm writtenCom)
   {
     int writtenSize;
     MPI_Comm_size(writtenCom, &writtenSize);
     return offsetWrittenIndexes_[writtenSize];
   }

   CArray<int, 1>& CDomain::getCompressedIndexToWriteOnServer(MPI_Comm writtenCom)
   {
     int writtenSize;
     MPI_Comm_size(writtenCom, &writtenSize);
     return compressedIndexToWriteOnServer[writtenSize];
   }

   //----------------------------------------------------------------

   /*!
    * Compute the minimum buffer size required to send the attributes to the server(s).
    *
    * \return A map associating the server rank with its minimum buffer size.
    */
   std::map<int, StdSize> CDomain::getAttributesBufferSize(CContextClient* client, bool bufferForWriting /*= false*/)
   {

     std::map<int, StdSize> attributesSizes = getMinimumBufferSizeForAttributes(client);

     if (client->isServerLeader())
     {
       // size estimation for sendDistributionAttribut
       size_t size = 11 * sizeof(size_t);

       const std::list<int>& ranks = client->getRanksServerLeader();
       for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
       {
         if (size > attributesSizes[*itRank])
           attributesSizes[*itRank] = size;
       }
     }

     boost::unordered_map<int, vector<size_t> >::const_iterator itIndexEnd = indSrv_[client->serverSize].end();
     // std::map<int, std::vector<int> >::const_iterator itWrittenIndexEnd = indWrittenSrv_.end();
     for (size_t k = 0; k < connectedServerRank_[client->serverSize].size(); ++k)
     {
       int rank = connectedServerRank_[client->serverSize][k];
       boost::unordered_map<int, std::vector<size_t> >::const_iterator it = indSrv_[client->serverSize].find(rank);
       size_t idxCount = (it != itIndexEnd) ? it->second.size() : 0;

       // size estimation for sendIndex (and sendArea which is always smaller or equal)
       size_t sizeIndexEvent = 2 * sizeof(size_t) + 2 * CArray<int,1>::size(idxCount);

       // size estimation for sendLonLat
       size_t sizeLonLatEvent = CArray<double,1>::size(idxCount);
       if (hasBounds)
         sizeLonLatEvent += CArray<double,2>::size(nvertex * idxCount);

       size_t size = CEventClient::headerSize + getId().size() + sizeof(size_t) + std::max(sizeIndexEvent, sizeLonLatEvent);
       if (size > attributesSizes[rank])
         attributesSizes[rank] = size;
     }

     return attributesSizes;
   }

   //----------------------------------------------------------------

   bool CDomain::isEmpty(void) const
   {
     return ((this->i_index.isEmpty()) || (0 == this->i_index.numElements()));
   }

   //----------------------------------------------------------------

   bool CDomain::IsWritten(const StdString & filename) const
   {
      return (this->relFiles.find(filename) != this->relFiles.end());
   }

   bool CDomain::isWrittenCompressed(const StdString& filename) const
   {
      return (this->relFilesCompressed.find(filename) != this->relFilesCompressed.end());
   }

   //----------------------------------------------------------------

   bool CDomain::isDistributed(void) const
   {
      bool distributed =  !((!ni.isEmpty() && (ni == ni_glo) && !nj.isEmpty() && (nj == nj_glo)) ||
              (!i_index.isEmpty() && i_index.numElements() == ni_glo*nj_glo));
      distributed |= (1 == CContext::getCurrent()->client->clientSize);

      return distributed;
   }

   //----------------------------------------------------------------

   /*!
    * Test whether the data defined on the domain can be outputted in a compressed way.
    *
    * \return true if and only if a mask was defined for this domain
    */
   bool CDomain::isCompressible(void) const
   {
      return isCompressible_;
   }

   void CDomain::addRelFile(const StdString & filename)
   {
      this->relFiles.insert(filename);
   }

   void CDomain::addRelFileCompressed(const StdString& filename)
   {
      this->relFilesCompressed.insert(filename);
   }

   StdString CDomain::GetName(void)   { return (StdString("domain")); }
   StdString CDomain::GetDefName(void){ return (CDomain::GetName()); }
   ENodeType CDomain::GetType(void)   { return (eDomain); }

   //----------------------------------------------------------------

   /*!
      Verify if all distribution information of a domain are available
      This checking verifies the definition of distribution attributes (ni, nj, ibegin, jbegin)
   */
   bool CDomain::distributionAttributesHaveValue() const
   {
      bool hasValues = true;

      if (ni.isEmpty() && ibegin.isEmpty() && i_index.isEmpty())
      {
        hasValues = false;
        return hasValues;
      }

      return hasValues;
   }

   /*!
     Redistribute RECTILINEAR or CURVILINEAR domain with a number of local domains.
   All attributes ni,nj,ibegin,jbegin (if defined) will be rewritten
   The optional attributes lonvalue, latvalue will be added. Because this function only serves (for now)
   for interpolation from unstructured domain to rectilinear one, range of latvalue is 0-360 and lonvalue is -90 - +90
    \param [in] nbLocalDomain number of local domain on the domain destination
   */
   void CDomain::redistribute(int nbLocalDomain)
   {
     if (this->isRedistributed_) return;

     this->isRedistributed_ = true;
     CContext* context = CContext::getCurrent();
     // For now the assumption is that secondary server pools consist of the same number of procs.
     // CHANGE the line below if the assumption changes.
     CContextClient* client = (0 != context->clientPrimServer.size()) ? context->clientPrimServer[0] : context->client;
     int rankClient = client->clientRank;
     int rankOnDomain = rankClient%nbLocalDomain;

     if (ni_glo.isEmpty() || ni_glo <= 0 )
     {
        ERROR("CDomain::redistribute(int nbLocalDomain)",
           << "[ Id = " << this->getId() << " ] "
           << "The global domain is badly defined,"
           << " check the \'ni_glo\'  value !")
     }

     if (nj_glo.isEmpty() || nj_glo <= 0 )
     {
        ERROR("CDomain::redistribute(int nbLocalDomain)",
           << "[ Id = " << this->getId() << " ] "
           << "The global domain is badly defined,"
           << " check the \'nj_glo\'  value !")
     }

     if ((type_attr::rectilinear == type)  || (type_attr::curvilinear == type))
     {
        int globalDomainSize = ni_glo * nj_glo;
        if (globalDomainSize <= nbLocalDomain)
        {
          for (int idx = 0; idx < nbLocalDomain; ++idx)
          {
            if (rankOnDomain < globalDomainSize)
            {
              int iIdx = rankOnDomain % ni_glo;
              int jIdx = rankOnDomain / ni_glo;
              ibegin.setValue(iIdx); jbegin.setValue(jIdx);
              ni.setValue(1); nj.setValue(1);
            }
            else
            {
              ibegin.setValue(0); jbegin.setValue(0);
              ni.setValue(0); nj.setValue(0);
            }
          }
        }
        else
        {
          float njGlo = nj_glo.getValue();
          float niGlo = ni_glo.getValue();
          int nbProcOnX, nbProcOnY, range;

          // Compute (approximately) number of segment on x and y axis
          float yOverXRatio = njGlo/niGlo;

          nbProcOnX = std::ceil(std::sqrt(nbLocalDomain/yOverXRatio));
          nbProcOnY = std::ceil(((float)nbLocalDomain)/nbProcOnX);

          // Simple distribution: Sweep from top to bottom, left to right
          // Calculate local begin on x
          std::vector<int> ibeginVec(nbProcOnX,0), jbeginVec(nbProcOnY,0);
          std::vector<int> niVec(nbProcOnX), njVec(nbProcOnY);
          for (int i = 1; i < nbProcOnX; ++i)
          {
            range = ni_glo / nbProcOnX;
            if (i < (ni_glo%nbProcOnX)) ++range;
            niVec[i-1] = range;
            ibeginVec[i] = ibeginVec[i-1] + niVec[i-1];
          }
          niVec[nbProcOnX-1] = ni_glo - ibeginVec[nbProcOnX-1];

          // Calculate local begin on y
          for (int j = 1; j < nbProcOnY; ++j)
          {
            range = nj_glo / nbProcOnY;
            if (j < (nj_glo%nbProcOnY)) ++range;
            njVec[j-1] = range;
            jbeginVec[j] = jbeginVec[j-1] + njVec[j-1];
          }
          njVec[nbProcOnY-1] = nj_glo - jbeginVec[nbProcOnY-1];

          // Now assign value to ni, ibegin, nj, jbegin
          int iIdx = rankOnDomain % nbProcOnX;
          int jIdx = rankOnDomain / nbProcOnX;

          if (rankOnDomain != (nbLocalDomain-1))
          {
            ibegin.setValue(ibeginVec[iIdx]);
            jbegin.setValue(jbeginVec[jIdx]);
            nj.setValue(njVec[jIdx]);
            ni.setValue(niVec[iIdx]);
          }
          else // just merge all the remaining rectangle into the last one
          {
            ibegin.setValue(ibeginVec[iIdx]);
            jbegin.setValue(jbeginVec[jIdx]);
            nj.setValue(njVec[jIdx]);
            ni.setValue(ni_glo - ibeginVec[iIdx]);
          }
        } 
     }
     else  // unstructured domain
     {
       if (this->i_index.isEmpty())
       {
          int globalDomainSize = ni_glo * nj_glo;
          if (globalDomainSize <= nbLocalDomain)
          {
            for (int idx = 0; idx < nbLocalDomain; ++idx)
            {
              if (rankOnDomain < globalDomainSize)
              {
                int iIdx = rankOnDomain % ni_glo;
                int jIdx = rankOnDomain / ni_glo;
                ibegin.setValue(iIdx); jbegin.setValue(jIdx);
                ni.setValue(1); nj.setValue(1);
              }
              else
              {
                ibegin.setValue(0); jbegin.setValue(0);
                ni.setValue(0); nj.setValue(0);
              }
            }
          }
          else
          {
            float njGlo = nj_glo.getValue();
            float niGlo = ni_glo.getValue();
            std::vector<int> ibeginVec(nbLocalDomain,0);
            std::vector<int> niVec(nbLocalDomain);
            for (int i = 1; i < nbLocalDomain; ++i)
            {
              int range = ni_glo / nbLocalDomain;
              if (i < (ni_glo%nbLocalDomain)) ++range;
              niVec[i-1] = range;
              ibeginVec[i] = ibeginVec[i-1] + niVec[i-1];
            }
            niVec[nbLocalDomain-1] = ni_glo - ibeginVec[nbLocalDomain-1];

            int iIdx = rankOnDomain % nbLocalDomain;
            ibegin.setValue(ibeginVec[iIdx]);
            jbegin.setValue(0);
            ni.setValue(niVec[iIdx]);
            nj.setValue(1);
          }

          i_index.resize(ni);          
          for(int idx = 0; idx < ni; ++idx) i_index(idx)=ibegin+idx;
        }
        else
        {
          ibegin.setValue(this->i_index(0));
          jbegin.setValue(0);
          ni.setValue(this->i_index.numElements());
          nj.setValue(1);
        }
     }

     checkDomain();
   }

   /*!
     Fill in longitude and latitude whose values are read from file
   */
   void CDomain::fillInLonLat()
   {
     switch (type)
     {
      case type_attr::rectilinear:
        fillInRectilinearLonLat();
        break;
      case type_attr::curvilinear:
        fillInCurvilinearLonLat();
        break;
      case type_attr::unstructured:
        fillInUnstructuredLonLat();
        break;

      default:
      break;
     }
     completeLonLatClient() ;

   }

   /*!
     Fill in the values for lonvalue_1d and latvalue_1d of rectilinear domain
     Range of longitude value from 0 - 360
     Range of latitude value from -90 - +90
   */
   void CDomain::fillInRectilinearLonLat()
   {
     if (!lonvalue_rectilinear_read_from_file.isEmpty()  && lonvalue_2d.isEmpty() && lonvalue_1d.isEmpty())
     {
       lonvalue_1d.resize(ni);
       for (int idx = 0; idx < ni; ++idx)
         lonvalue_1d(idx) = lonvalue_rectilinear_read_from_file(idx+ibegin);
       lon_start.setValue(lonvalue_rectilinear_read_from_file(0));
       lon_end.setValue(lonvalue_rectilinear_read_from_file(ni_glo-1));
     }
     else if (!hasLonInReadFile_)
     {
       if (!lonvalue_2d.isEmpty()) lonvalue_2d.free();
       lonvalue_1d.resize(ni);
       double lonRange = lon_end - lon_start;
       double lonStep = (1 == ni_glo.getValue()) ? lonRange : lonRange/double(ni_glo.getValue()-1);

        // Assign lon value
       for (int i = 0; i < ni; ++i)
       {
         if (0 == (ibegin + i))
         {
           lonvalue_1d(i) = lon_start;
         }
         else if (ni_glo == (ibegin + i + 1))
         {
           lonvalue_1d(i) = lon_end;
         }
         else
         {
           lonvalue_1d(i) = (ibegin + i) * lonStep  + lon_start;
         }
       }
     }


     if (!latvalue_rectilinear_read_from_file.isEmpty() && latvalue_2d.isEmpty() && latvalue_1d.isEmpty())
     {
       latvalue_1d.resize(nj);
       for (int idx = 0; idx < nj; ++idx)
         latvalue_1d(idx) = latvalue_rectilinear_read_from_file(idx+jbegin);
       lat_start.setValue(latvalue_rectilinear_read_from_file(0));
       lat_end.setValue(latvalue_rectilinear_read_from_file(nj_glo-1));
     }
     else if (!hasLatInReadFile_)
     {
       if (!latvalue_2d.isEmpty()) latvalue_1d.free();
       latvalue_1d.resize(nj);

       double latRange = lat_end - lat_start;
       double latStep = (1 == nj_glo.getValue()) ? latRange : latRange/double(nj_glo.getValue()-1);

       for (int j = 0; j < nj; ++j)
       {
         if (0 == (jbegin + j))
         {
            latvalue_1d(j) = lat_start;
         }
         else if (nj_glo == (jbegin + j + 1))
         {
            latvalue_1d(j) = lat_end;
         }
         else
         {
           latvalue_1d(j) =  (jbegin + j) * latStep + lat_start;
         }
       }
     }
   }

    /*
      Fill in longitude and latitude of curvilinear domain read from a file
      If there are already longitude and latitude defined by model. We just igonore reading value.
    */
   void CDomain::fillInCurvilinearLonLat()
   {
     if (!lonvalue_curvilinear_read_from_file.isEmpty() && lonvalue_2d.isEmpty() && lonvalue_1d.isEmpty())
     {
       lonvalue_2d.resize(ni,nj);
       for (int jdx = 0; jdx < nj; ++jdx)
        for (int idx = 0; idx < ni; ++idx)
         lonvalue_2d(idx,jdx) = lonvalue_curvilinear_read_from_file(idx, jdx);

       lonvalue_curvilinear_read_from_file.free();
     }

     if (!latvalue_curvilinear_read_from_file.isEmpty() && latvalue_2d.isEmpty() && latvalue_1d.isEmpty())
     {
       latvalue_2d.resize(ni,nj);
       for (int jdx = 0; jdx < nj; ++jdx)
        for (int idx = 0; idx < ni; ++idx)
           latvalue_2d(idx,jdx) = latvalue_curvilinear_read_from_file(idx, jdx);

       latvalue_curvilinear_read_from_file.free();
     }

     if (!bounds_lonvalue_curvilinear_read_from_file.isEmpty() && bounds_lon_2d.isEmpty() && bounds_lon_1d.isEmpty())
     {
       bounds_lon_2d.resize(nvertex,ni,nj);
       for (int jdx = 0; jdx < nj; ++jdx)
        for (int idx = 0; idx < ni; ++idx)
          for (int ndx = 0; ndx < nvertex; ++ndx)
            bounds_lon_2d(ndx,idx,jdx) = bounds_lonvalue_curvilinear_read_from_file(ndx,idx, jdx);

       bounds_lonvalue_curvilinear_read_from_file.free();
     }

     if (!bounds_latvalue_curvilinear_read_from_file.isEmpty() && bounds_lat_2d.isEmpty() && bounds_lat_1d.isEmpty())
     {
       bounds_lat_2d.resize(nvertex,ni,nj);
       for (int jdx = 0; jdx < nj; ++jdx)
        for (int idx = 0; idx < ni; ++idx)
          for (int ndx = 0; ndx < nvertex; ++ndx)
            bounds_lat_2d(ndx,idx,jdx) = bounds_latvalue_curvilinear_read_from_file(ndx,idx, jdx);

       bounds_latvalue_curvilinear_read_from_file.free();
     }

   }

    /*
      Fill in longitude and latitude of unstructured domain read from a file
      If there are already longitude and latitude defined by model. We just igonore reading value.
    */
   void CDomain::fillInUnstructuredLonLat()
   {
     if (i_index.isEmpty())
     {
       i_index.resize(ni);
       for(int idx = 0; idx < ni; ++idx) i_index(idx)=ibegin+idx;
     }

     if (!lonvalue_unstructured_read_from_file.isEmpty() && lonvalue_1d.isEmpty())
     {
        lonvalue_1d.resize(ni);
        for (int idx = 0; idx < ni; ++idx)
          lonvalue_1d(idx) = lonvalue_unstructured_read_from_file(idx);

        // We dont need these values anymore, so just delete them
        lonvalue_unstructured_read_from_file.free();
     } 

     if (!latvalue_unstructured_read_from_file.isEmpty() && latvalue_1d.isEmpty())
     {
        latvalue_1d.resize(ni);
        for (int idx = 0; idx < ni; ++idx)
          latvalue_1d(idx) =  latvalue_unstructured_read_from_file(idx);

        // We dont need these values anymore, so just delete them
        latvalue_unstructured_read_from_file.free();
     }

     if (!bounds_lonvalue_unstructured_read_from_file.isEmpty() && bounds_lon_1d.isEmpty())
     {
        int nbVertex = nvertex;
        bounds_lon_1d.resize(nbVertex,ni);
        for (int idx = 0; idx < ni; ++idx)
          for (int jdx = 0; jdx < nbVertex; ++jdx)
            bounds_lon_1d(jdx,idx) = bounds_lonvalue_unstructured_read_from_file(jdx, idx);

        // We dont need these values anymore, so just delete them
        bounds_lonvalue_unstructured_read_from_file.free();
     }

     if (!bounds_latvalue_unstructured_read_from_file.isEmpty() && bounds_lat_1d.isEmpty())
     {
        int nbVertex = nvertex;
        bounds_lat_1d.resize(nbVertex,ni);
        for (int idx = 0; idx < ni; ++idx)
          for (int jdx = 0; jdx < nbVertex; ++jdx)
            bounds_lat_1d(jdx,idx) = bounds_latvalue_unstructured_read_from_file(jdx, idx);

        // We dont need these values anymore, so just delete them
        bounds_latvalue_unstructured_read_from_file.free();
     }
   }

  /*
    Get global longitude and latitude of rectilinear domain.
  */
   void CDomain::AllgatherRectilinearLonLat(CArray<double,1>& lon, CArray<double,1>& lat, CArray<double,1>& lon_g, CArray<double,1>& lat_g)
   {
     CContext* context = CContext::getCurrent();
    // For now the assumption is that secondary server pools consist of the same number of procs.
    // CHANGE the line below if the assumption changes.
    CContextClient* client = (0 != context->clientPrimServer.size()) ? context->clientPrimServer[0] : context->client;
     lon_g.resize(ni_glo) ;
     lat_g.resize(nj_glo) ;


     int* ibegin_g = new int[client->clientSize] ;
     int* jbegin_g = new int[client->clientSize] ;
     int* ni_g = new int[client->clientSize] ;
     int* nj_g = new int[client->clientSize] ;
     int v ;
     v=ibegin ;
     MPI_Allgather(&v,1,MPI_INT,ibegin_g,1,MPI_INT,client->intraComm) ;
     v=jbegin ;
     MPI_Allgather(&v,1,MPI_INT,jbegin_g,1,MPI_INT,client->intraComm) ;
     v=ni ;
     MPI_Allgather(&v,1,MPI_INT,ni_g,1,MPI_INT,client->intraComm) ;
     v=nj ;
     MPI_Allgather(&v,1,MPI_INT,nj_g,1,MPI_INT,client->intraComm) ;

     MPI_Allgatherv(lon.dataFirst(),ni,MPI_DOUBLE,lon_g.dataFirst(),ni_g, ibegin_g,MPI_DOUBLE,client->intraComm) ;
     MPI_Allgatherv(lat.dataFirst(),nj,MPI_DOUBLE,lat_g.dataFirst(),nj_g, jbegin_g,MPI_DOUBLE,client->intraComm) ;

      delete[] ibegin_g ;
      delete[] jbegin_g ;
      delete[] ni_g ;
      delete[] nj_g ;
   }

   void CDomain::fillInRectilinearBoundLonLat(CArray<double,1>& lon, CArray<double,1>& lat,
                                              CArray<double,2>& boundsLon, CArray<double,2>& boundsLat)
   {
     int i,j,k;

     const int nvertexValue = 4;
     boundsLon.resize(nvertexValue,ni*nj);

     if (ni_glo>1)
     {
       double lonStepStart = lon(1)-lon(0);
       bounds_lon_start=lon(0) - lonStepStart/2;
       double lonStepEnd = lon(ni_glo-1)-lon(ni_glo-2);
       bounds_lon_end=lon(ni_glo-1) + lonStepEnd/2;
       double errorBoundsLon = std::abs(360-std::abs(bounds_lon_end-bounds_lon_start));

       // if errorBoundsLon is reasonably small (0.1 x cell size) consider it as closed in longitude
       if (errorBoundsLon < std::abs(lonStepStart)*1e-1 || errorBoundsLon < std::abs(lonStepEnd)*1e-1 )
       {
         bounds_lon_start= (lon(0) + lon(ni_glo-1)-360)/2 ;
         bounds_lon_end= (lon(0) +360 + lon(ni_glo-1))/2 ;
       }
     }
     else
     {
       if (bounds_lon_start.isEmpty()) bounds_lon_start=-180. ;
       if (bounds_lon_end.isEmpty()) bounds_lon_end=180.-1e-8 ;
     }

     for(j=0;j<nj;++j)
       for(i=0;i<ni;++i)
       {
         k=j*ni+i;
         boundsLon(0,k) = boundsLon(1,k) = (0 == (ibegin + i)) ? bounds_lon_start
                                                               : (lon(ibegin + i)+lon(ibegin + i-1))/2;
         boundsLon(2,k) = boundsLon(3,k) = ((ibegin + i + 1) == ni_glo) ? bounds_lon_end
                                                                        : (lon(ibegin + i + 1)+lon(ibegin + i))/2;
       }


    boundsLat.resize(nvertexValue,nj*ni);
    bool isNorthPole=false ;
    bool isSouthPole=false ;
    if (std::abs(90 - std::abs(lat(0))) < NumTraits<double>::epsilon()) isNorthPole = true;
    if (std::abs(90 - std::abs(lat(nj_glo-1))) < NumTraits<double>::epsilon()) isSouthPole = true;

    // lat boundaries beyond pole the assimilate it to pole
    // lat boundarie is relativelly close to pole (0.1 x cell size) assimilate it to pole
    if (nj_glo>1)
    {
      double latStepStart = lat(1)-lat(0);
      if (isNorthPole) bounds_lat_start=lat(0);
      else
      {
        bounds_lat_start=lat(0)-latStepStart/2;
        if (bounds_lat_start >= 90 ) bounds_lat_start=90 ;
        else if (bounds_lat_start <= -90 ) bounds_lat_start=-90 ;
        else if (bounds_lat_start <= 90 && bounds_lat_start >= lat(0))
        {
          if ( std::abs(90-bounds_lat_start) <= 0.1*std::abs(latStepStart)) bounds_lat_start=90 ;
        }
        else if (bounds_lat_start >= -90 && bounds_lat_start <= lat(0))
        {
          if ( std::abs(-90 - bounds_lat_start) <= 0.1*std::abs(latStepStart)) bounds_lat_start=-90 ;
        }
      }

      double latStepEnd = lat(nj_glo-1)-lat(nj_glo-2);
      if (isSouthPole) bounds_lat_end=lat(nj_glo-1);
      else
      {
        bounds_lat_end=lat(nj_glo-1)+latStepEnd/2;

        if (bounds_lat_end >= 90 ) bounds_lat_end=90 ;
        else if (bounds_lat_end <= -90 ) bounds_lat_end=-90 ;
        else if (bounds_lat_end <= 90 && bounds_lat_end >= lat(nj_glo-1))
        {
          if ( std::abs(90-bounds_lat_end) <= 0.1*std::abs(latStepEnd)) bounds_lat_end=90 ;
        }
        else if (bounds_lat_end >= -90 && bounds_lat_end <= lat(nj_glo-1))
        {
          if ( std::abs(-90 - bounds_lat_end) <= 0.1*std::abs(latStepEnd)) bounds_lat_end=-90 ;
        }
      }
    }
    else
    {
      if (bounds_lat_start.isEmpty()) bounds_lat_start=-90. ;
      if (bounds_lat_end.isEmpty()) bounds_lat_end=90 ;
    }

    for(j=0;j<nj;++j)
      for(i=0;i<ni;++i)
      {
        k=j*ni+i;
        boundsLat(1,k) = boundsLat(2,k) = (0 == (jbegin + j)) ? bounds_lat_start
                                                              : (lat(jbegin + j)+lat(jbegin + j-1))/2;
        boundsLat(0,k) = boundsLat(3,k) = ((jbegin + j +1) == nj_glo) ? bounds_lat_end
                                                                      : (lat(jbegin + j + 1)+lat(jbegin + j))/2;
      }
   }

   /*
     General check of the domain to verify its mandatory attributes
   */
   void CDomain::checkDomain(void)
   {
     if (type.isEmpty())
     {
       ERROR("CDomain::checkDomain(void)",
             << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
             << "The domain type is mandatory, "
             << "please define the 'type' attribute.")
     }

     if (type == type_attr::gaussian) 
     {
        hasPole=true ;
        type.setValue(type_attr::unstructured) ;
      }
      else if (type == type_attr::rectilinear) hasPole=true ;

     if (type == type_attr::unstructured)
     {
        if (ni_glo.isEmpty())
        {
          ERROR("CDomain::checkDomain(void)",
                << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                << "The global domain is badly defined, "
                << "the mandatory 'ni_glo' attribute is missing.")
        }
        else if (ni_glo <= 0)
        {
          ERROR("CDomain::checkDomain(void)",
                << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                << "The global domain is badly defined, "
                << "'ni_glo' attribute should be strictly positive so 'ni_glo = " << ni_glo.getValue() << "' is invalid.")
        }
        isUnstructed_ = true;
        nj_glo = 1;
        nj = 1;
        jbegin = 0;
        if (!i_index.isEmpty()) ni = i_index.numElements();
        j_index.resize(ni);
        for(int i=0;i<ni;++i) j_index(i)=0;

        if (!area.isEmpty())
          area.transposeSelf(1, 0);
     }

     if (ni_glo.isEmpty())
     {
       ERROR("CDomain::checkDomain(void)",
             << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
             << "The global domain is badly defined, "
             << "the mandatory 'ni_glo' attribute is missing.")
     }
     else if (ni_glo <= 0)
     {
       ERROR("CDomain::checkDomain(void)",
             << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
             << "The global domain is badly defined, "
             << "'ni_glo' attribute should be strictly positive so 'ni_glo = " << ni_glo.getValue() << "' is invalid.")
     }

     if (nj_glo.isEmpty())
     {
       ERROR("CDomain::checkDomain(void)",
             << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
             << "The global domain is badly defined, "
             << "the mandatory 'nj_glo' attribute is missing.")
     }
     else if (nj_glo <= 0)
     {
       ERROR("CDomain::checkDomain(void)",
             << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
             << "The global domain is badly defined, "
             << "'nj_glo' attribute should be strictly positive so 'nj_glo = " << nj_glo.getValue() << "' is invalid.")
     }

     checkLocalIDomain();
     checkLocalJDomain();

     if (i_index.isEmpty())
     {
       i_index.resize(ni*nj);
       for (int j = 0; j < nj; ++j)
         for (int i = 0; i < ni; ++i) i_index(i+j*ni) = i+ibegin;
     }

     if (j_index.isEmpty())
     {
       j_index.resize(ni*nj);
       for (int j = 0; j < nj; ++j)
         for (int i = 0; i < ni; ++i) j_index(i+j*ni) = j+jbegin;
     }
     
     checkZoom();
   }

   // Check global zoom of a domain
   // If there is no zoom defined for the domain, zoom will have value of global doamin
   void CDomain::checkZoom(void)
   {
     if (global_zoom_ibegin.isEmpty())
      global_zoom_ibegin.setValue(0);
     if (global_zoom_ni.isEmpty())
      global_zoom_ni.setValue(ni_glo);
     if (global_zoom_jbegin.isEmpty())
      global_zoom_jbegin.setValue(0);
     if (global_zoom_nj.isEmpty())
      global_zoom_nj.setValue(nj_glo);
    if (zoom_i_index.isEmpty()) zoom_i_index.setValue(i_index.getValue());
    if (zoom_j_index.isEmpty()) zoom_j_index.setValue(j_index.getValue());
    if (zoom_ibegin.isEmpty()) zoom_ibegin.setValue(ibegin);
    if (zoom_ni.isEmpty()) zoom_ni.setValue(ni);
    if (zoom_jbegin.isEmpty()) zoom_jbegin.setValue(jbegin);
    if (zoom_nj.isEmpty()) zoom_nj.setValue(nj);
   }

   size_t CDomain::getGlobalWrittenSize(void)
   {
      return global_zoom_ni*global_zoom_nj ;
   }
   //----------------------------------------------------------------

   // Check validity of local domain on using the combination of 3 parameters: ibegin, ni and i_index
   void CDomain::checkLocalIDomain(void)
   {
      // If ibegin and ni are provided then we use them to check the validity of local domain
      if (i_index.isEmpty() && !ibegin.isEmpty() && !ni.isEmpty())
      {
        if ((ni.getValue() < 0 || ibegin.getValue() < 0) || ((ibegin.getValue() + ni.getValue()) > ni_glo.getValue()))
        {
          ERROR("CDomain::checkLocalIDomain(void)",
                << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                << "The local domain is wrongly defined,"
                << " check the attributes 'ni_glo' (" << ni_glo.getValue() << "), 'ni' (" << ni.getValue() << ") and 'ibegin' (" << ibegin.getValue() << ")");
        }
      }

      // i_index has higher priority than ibegin and ni
      if (!i_index.isEmpty())
      {
        int minIIndex = (0 < i_index.numElements()) ? i_index(0) : 0;
        if (ni.isEmpty()) 
        {          
         // No information about ni
          int minIndex = ni_glo - 1;
          int maxIndex = 0;
          for (int idx = 0; idx < i_index.numElements(); ++idx)
          {
            if (i_index(idx) < minIndex) minIndex = i_index(idx);
            if (i_index(idx) > maxIndex) maxIndex = i_index(idx);
          }
          ni = maxIndex - minIndex + 1; 
          minIIndex = minIIndex;          
        }

        // It's not so correct but if ibegin is not the first value of i_index 
        // then data on local domain has user-defined distribution. In this case, ibegin, ni have no meaning.
        if (ibegin.isEmpty()) ibegin = minIIndex;
      }
      else if (ibegin.isEmpty() && ni.isEmpty())
      {
        ibegin = 0;
        ni = ni_glo;
      }
      else if ((!ibegin.isEmpty() && ni.isEmpty()) || (ibegin.isEmpty() && !ni.isEmpty()))
      {
        ERROR("CDomain::checkLocalIDomain(void)",
              << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
              << "The local domain is wrongly defined," << endl
              << "i_index is empty and either 'ni' or 'ibegin' is not defined. " 
              << "If 'ni' and 'ibegin' are used to define a domain, both of them must not be empty.");
      }
       

      if ((ni.getValue() < 0 || ibegin.getValue() < 0))
      {
        ERROR("CDomain::checkLocalIDomain(void)",
              << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
              << "The local domain is wrongly defined,"
              << " check the attributes 'ni_glo' (" << ni_glo.getValue() << "), 'ni' (" << ni.getValue() << ") and 'ibegin' (" << ibegin.getValue() << ")");
      }
   }

   // Check validity of local domain on using the combination of 3 parameters: jbegin, nj and j_index
   void CDomain::checkLocalJDomain(void)
   {
    // If jbegin and nj are provided then we use them to check the validity of local domain
     if (j_index.isEmpty() && !jbegin.isEmpty() && !nj.isEmpty())
     {
       if ((nj.getValue() < 0 || jbegin.getValue() < 0) || (jbegin.getValue() + nj.getValue()) > nj_glo.getValue())
       {
         ERROR("CDomain::checkLocalJDomain(void)",
                << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                << "The local domain is wrongly defined,"
                << " check the attributes 'nj_glo' (" << nj_glo.getValue() << "), 'nj' (" << nj.getValue() << ") and 'jbegin' (" << jbegin.getValue() << ")");
       }
     }

     if (!j_index.isEmpty())
     {
        int minJIndex = (0 < j_index.numElements()) ? j_index(0) : 0;
        if (nj.isEmpty()) 
        {
          // No information about nj
          int minIndex = nj_glo - 1;
          int maxIndex = 0;
          for (int idx = 0; idx < j_index.numElements(); ++idx)
          {
            if (j_index(idx) < minIndex) minIndex = j_index(idx);
            if (j_index(idx) > maxIndex) maxIndex = j_index(idx);
          }
          nj = maxIndex - minIndex + 1;
          minJIndex = minIndex; 
        }  
        // It's the same as checkLocalIDomain. It's not so correct but if jbegin is not the first value of j_index 
        // then data on local domain has user-defined distribution. In this case, jbegin has no meaning.
       if (jbegin.isEmpty()) jbegin = minJIndex;       
     }
     else if (jbegin.isEmpty() && nj.isEmpty())
     {
       jbegin = 0;
       nj = nj_glo;
     }      


     if ((nj.getValue() < 0 || jbegin.getValue() < 0))
     {
       ERROR("CDomain::checkLocalJDomain(void)",
              << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
              << "The local domain is wrongly defined,"
              << " check the attributes 'nj_glo' (" << nj_glo.getValue() << "), 'nj' (" << nj.getValue() << ") and 'jbegin' (" << jbegin.getValue() << ")");
     }
   }

   //----------------------------------------------------------------

   void CDomain::checkMask(void)
   {
      if (!mask_1d.isEmpty() && !mask_2d.isEmpty())
        ERROR("CDomain::checkMask(void)",
              << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
              << "Both mask_1d and mask_2d are defined but only one can be used at the same time." << std::endl
              << "Please define only one mask: 'mask_1d' or 'mask_2d'.");

      if (!mask_1d.isEmpty() && mask_2d.isEmpty())
      {
        if (mask_1d.numElements() != i_index.numElements())
          ERROR("CDomain::checkMask(void)",
                << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                << "'mask_1d' does not have the same size as the local domain." << std::endl
                << "Local size is " << i_index.numElements() << "." << std::endl
                << "Mask size is " << mask_1d.numElements() << ".");
      }

      if (mask_1d.isEmpty() && !mask_2d.isEmpty())
      {
        if (mask_2d.extent(0) != ni || mask_2d.extent(1) != nj)
          ERROR("CDomain::checkMask(void)",
                << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                << "The mask does not have the same size as the local domain." << std::endl
                << "Local size is " << ni.getValue() << " x " << nj.getValue() << "." << std::endl
                << "Mask size is " << mask_2d.extent(0) << " x " << mask_2d.extent(1) << ".");
      }

      if (!mask_2d.isEmpty())
      {
        domainMask.resize(mask_2d.extent(0) * mask_2d.extent(1));
        for (int j = 0; j < nj; ++j)
          for (int i = 0; i < ni; ++i) domainMask(i+j*ni) = mask_2d(i,j);
//        mask_2d.reset();
      }
      else if (mask_1d.isEmpty())
      {
        domainMask.resize(i_index.numElements());
        for (int i = 0; i < i_index.numElements(); ++i) domainMask(i) = true;
      }
      else
      {
      domainMask.resize(mask_1d.numElements());
      domainMask=mask_1d ;
     }
   }

   //----------------------------------------------------------------

   void CDomain::checkDomainData(void)
   {
      if (data_dim.isEmpty())
      {
        data_dim.setValue(1);
      }
      else if (!(data_dim.getValue() == 1 || data_dim.getValue() == 2))
      {
        ERROR("CDomain::checkDomainData(void)",
              << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
              << "The data dimension is invalid, 'data_dim' must be 1 or 2 not << " << data_dim.getValue() << ".");
      }

      if (data_ibegin.isEmpty())
         data_ibegin.setValue(0);
      if (data_jbegin.isEmpty())
         data_jbegin.setValue(0);

      if (data_ni.isEmpty())
      {
        data_ni.setValue((data_dim == 1) ? (ni.getValue() * nj.getValue()) : ni.getValue());
      }
      else if (data_ni.getValue() < 0)
      {
        ERROR("CDomain::checkDomainData(void)",
              << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
              << "The data size cannot be negative ('data_ni' = " << data_ni.getValue() << ").");
      }

      if (data_nj.isEmpty())
      {
        data_nj.setValue((data_dim.getValue() == 1) ? (ni.getValue() * nj.getValue()) : nj.getValue());
      }
      else if (data_nj.getValue() < 0)
      {
        ERROR("CDomain::checkDomainData(void)",
              << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
              << "The data size cannot be negative ('data_nj' = " << data_nj.getValue() << ").");
      }
   }

   //----------------------------------------------------------------

   void CDomain::checkCompression(void)
   {
      if (!data_i_index.isEmpty())
      {
        if (!data_j_index.isEmpty() &&
            data_j_index.numElements() != data_i_index.numElements())
        {
           ERROR("CDomain::checkCompression(void)",
                 << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                 << "'data_i_index' and 'data_j_index' arrays must have the same size." << std::endl
                 << "'data_i_index' size = " << data_i_index.numElements() << std::endl
                 << "'data_j_index' size = " << data_j_index.numElements());
        }

        if (2 == data_dim)
        {
          if (data_j_index.isEmpty())
          {
             ERROR("CDomain::checkCompression(void)",
                   << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                   << "'data_j_index' must be defined when 'data_i_index' is set and 'data_dim' is 2.");
          }
        }
        else // (1 == data_dim)
        {
          if (data_j_index.isEmpty())
          {
            data_j_index.resize(data_ni);
            for (int j = 0; j < data_ni; ++j) data_j_index(j) = 0;
          }
        }
      }
      else
      {
        if (data_dim == 2 && !data_j_index.isEmpty())
          ERROR("CDomain::checkCompression(void)",
                << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                << "'data_i_index' must be defined when 'data_j_index' is set and 'data_dim' is 2.");

        if (1 == data_dim)
        {
          data_i_index.resize(data_ni);
          data_j_index.resize(data_ni);

          for (int i = 0; i < data_ni; ++i)
          {
            data_i_index(i) = i;
            data_j_index(i) = 0;
          }
        }
        else // (data_dim == 2)
        {
          const int dsize = data_ni * data_nj;
          data_i_index.resize(dsize);
          data_j_index.resize(dsize);

          for(int count = 0, j = 0; j < data_nj; ++j)
          {
            for(int i = 0; i < data_ni; ++i, ++count)
            {
              data_i_index(count) = i;
              data_j_index(count) = j;
            }
          }
        }
      }
   }

   //----------------------------------------------------------------
   void CDomain::computeLocalMask(void)
   {
     localMask.resize(i_index.numElements()) ;
     localMask=false ;

     size_t dn=data_i_index.numElements() ;
     int i,j ;
     size_t k,ind ;

     for(k=0;k<dn;k++)
     {
       if (data_dim==2)
       {
          i=data_i_index(k)+data_ibegin ;
          j=data_j_index(k)+data_jbegin ;
          if (i>=0 && i<ni && j>=0 && j<nj)
          {
            ind=j*ni+i ;
            localMask(ind)=domainMask(ind) ;
          }
       }
       else
       {
          i=data_i_index(k)+data_ibegin ;
          if (i>=0 && i<i_index.numElements())
          {
            ind=i ;
            localMask(ind)=domainMask(ind) ;
          }
       }
     }
   }

   void CDomain::checkEligibilityForCompressedOutput(void)
   {
     // We don't check if the mask or the indexes are valid here, just if they have been defined at this point.
     isCompressible_ = !mask_1d.isEmpty() || !mask_2d.isEmpty() || !data_i_index.isEmpty();
   }

   //----------------------------------------------------------------

   /*
     Fill in longitude and latitude value from clients (or models) into internal values lonvalue, latvalue which
     will be used by XIOS.
   */
   void CDomain::completeLonLatClient(void)
   {
     bool lonlatValueExisted = (0 != lonvalue.numElements()) || (0 != latvalue.numElements());
     checkBounds() ;
     if (!lonvalue_2d.isEmpty() && !lonlatValueExisted)
     {
       lonvalue.resize(ni * nj);
       latvalue.resize(ni * nj);
       if (hasBounds)
       {
         bounds_lonvalue.resize(nvertex, ni * nj);
         bounds_latvalue.resize(nvertex, ni * nj);
       }

       for (int j = 0; j < nj; ++j)
       {
         for (int i = 0; i < ni; ++i)
         {
           int k = j * ni + i;

           lonvalue(k) = lonvalue_2d(i,j);
           latvalue(k) = latvalue_2d(i,j);

           if (hasBounds)
           {
             for (int n = 0; n < nvertex; ++n)
             {
               bounds_lonvalue(n,k) = bounds_lon_2d(n,i,j);
               bounds_latvalue(n,k) = bounds_lat_2d(n,i,j);
             }
           }
         }
       }
     }
     else if (!lonvalue_1d.isEmpty()  && !lonlatValueExisted)
     {
       if (type_attr::rectilinear == type)
       {
         if (ni == lonvalue_1d.numElements() && nj == latvalue_1d.numElements())
         {
           lonvalue.resize(ni * nj);
           latvalue.resize(ni * nj);
           if (hasBounds)
           {
             bounds_lonvalue.resize(nvertex, ni * nj);
             bounds_latvalue.resize(nvertex, ni * nj);
           }

           for (int j = 0; j < nj; ++j)
           {
             for (int i = 0; i < ni; ++i)
             {
               int k = j * ni + i;

               lonvalue(k) = lonvalue_1d(i);
               latvalue(k) = latvalue_1d(j);

               if (hasBounds)
               {
                 for (int n = 0; n < nvertex; ++n)
                 {
                   bounds_lonvalue(n,k) = bounds_lon_1d(n,i);
                   bounds_latvalue(n,k) = bounds_lat_1d(n,j);
                 }
               }
             }
           }
         }
         else if (i_index.numElements() == lonvalue_1d.numElements() && j_index.numElements() == latvalue_1d.numElements()  && !lonlatValueExisted)
         {
           lonvalue.reference(lonvalue_1d);
           latvalue.reference(latvalue_1d);
            if (hasBounds)
           {
             bounds_lonvalue.reference(bounds_lon_1d);
             bounds_latvalue.reference(bounds_lat_1d);
           }
         }
         else
           ERROR("CDomain::completeLonClient(void)",
                 << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                 << "'lonvalue_1d' and 'latvalue_1d' does not have the same size as the local domain." << std::endl
                 << "'lonvalue_1d' size is " << lonvalue_1d.numElements() 
                 << " and 'latvalue_1d' size is " << latvalue_1d.numElements() << std::endl 
                 << " They should be correspondingly " << ni.getValue() << " and "  << nj.getValue() << " or " << std::endl
                 << i_index.numElements() << " and "  << j_index.numElements() << ".");
       }
       else if (type == type_attr::curvilinear || type == type_attr::unstructured  && !lonlatValueExisted)
       {
         lonvalue.reference(lonvalue_1d);
         latvalue.reference(latvalue_1d);
         if (hasBounds)
         {
           bounds_lonvalue.reference(bounds_lon_1d);
           bounds_latvalue.reference(bounds_lat_1d);
         }
       }
     }
   }

   /*
     Convert internal longitude latitude value used by XIOS to "lonvalue_*" which can be retrieved with Fortran interface
   */
   void CDomain::convertLonLatValue(void)
   {
     bool lonlatValueExisted = (0 != lonvalue.numElements()) || (0 != latvalue.numElements());
     if (!lonvalue_2d.isEmpty() && lonlatValueExisted)
     {
       lonvalue_2d.resize(ni,nj);
       latvalue_2d.resize(ni,nj);
       if (hasBounds)
       {
         bounds_lon_2d.resize(nvertex, ni, nj);
         bounds_lat_2d.resize(nvertex, ni, nj);
       }

       for (int j = 0; j < nj; ++j)
       {
         for (int i = 0; i < ni; ++i)
         {
           int k = j * ni + i;

           lonvalue_2d(i,j) = lonvalue(k);
           latvalue_2d(i,j) = latvalue(k);

           if (hasBounds)
           {
             for (int n = 0; n < nvertex; ++n)
             {
               bounds_lon_2d(n,i,j) = bounds_lonvalue(n,k);
               bounds_lat_2d(n,i,j) = bounds_latvalue(n,k);
             }
           }
         }
       }
     }
     else if (!lonvalue_1d.isEmpty()  && lonlatValueExisted)
     {
       if (type_attr::rectilinear == type)
       {
         if (ni == lonvalue_1d.numElements() && nj == latvalue_1d.numElements())
         {
           lonvalue.resize(ni * nj);
           latvalue.resize(ni * nj);
           if (hasBounds)
           {
             bounds_lonvalue.resize(nvertex, ni * nj);
             bounds_latvalue.resize(nvertex, ni * nj);
           }

           for (int j = 0; j < nj; ++j)
           {
             for (int i = 0; i < ni; ++i)
             {
               int k = j * ni + i;

               lonvalue(k) = lonvalue_1d(i);
               latvalue(k) = latvalue_1d(j);

               if (hasBounds)
               {
                 for (int n = 0; n < nvertex; ++n)
                 {
                   bounds_lonvalue(n,k) = bounds_lon_1d(n,i);
                   bounds_latvalue(n,k) = bounds_lat_1d(n,j);
                 }
               }
             }
           }
         }
         else if (i_index.numElements() == lonvalue_1d.numElements() && j_index.numElements() == latvalue_1d.numElements()  && !lonlatValueExisted)
         {
           lonvalue.reference(lonvalue_1d);
           latvalue.reference(latvalue_1d);
            if (hasBounds)
           {
             bounds_lonvalue.reference(bounds_lon_1d);
             bounds_latvalue.reference(bounds_lat_1d);
           }
         }
         else
           ERROR("CDomain::completeLonClient(void)",
                 << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                 << "'lonvalue_1d' and 'latvalue_1d' does not have the same size as the local domain." << std::endl
                 << "'lonvalue_1d' size is " << lonvalue_1d.numElements() 
                 << " and 'latvalue_1d' size is " << latvalue_1d.numElements() << std::endl 
                 << " They should be correspondingly " << ni.getValue() << " and "  << nj.getValue() << " or " << std::endl
                 << i_index.numElements() << " and "  << j_index.numElements() << ".");
       }
       else if (type == type_attr::curvilinear || type == type_attr::unstructured  && !lonlatValueExisted)
       {
         lonvalue.reference(lonvalue_1d);
         latvalue.reference(latvalue_1d);
         if (hasBounds)
         {
           bounds_lonvalue.reference(bounds_lon_1d);
           bounds_latvalue.reference(bounds_lat_1d);
         }
       }
     }
   }


   void CDomain::checkBounds(void)
   {
     bool hasBoundValues = (0 != bounds_lonvalue.numElements()) || (0 != bounds_latvalue.numElements());
     if (!nvertex.isEmpty() && nvertex > 0 && !hasBoundValues)
     {
       if (!bounds_lon_1d.isEmpty() && !bounds_lon_2d.isEmpty())
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Only one longitude boundary attribute can be used but both 'bounds_lon_1d' and 'bounds_lon_2d' are defined." << std::endl
               << "Define only one longitude boundary attribute: 'bounds_lon_1d' or 'bounds_lon_2d'.");

       if (!bounds_lat_1d.isEmpty() && !bounds_lat_2d.isEmpty())
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Only one latitude boundary attribute can be used but both 'bounds_lat_1d' and 'bounds_lat_2d' are defined." << std::endl
               << "Define only one latitude boundary attribute: 'bounds_lat_1d' or 'bounds_lat_2d'.");

       if ((!bounds_lon_1d.isEmpty() && bounds_lat_1d.isEmpty()) || (bounds_lon_1d.isEmpty() && !bounds_lat_1d.isEmpty()))
       {
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Only 'bounds_lon_1d' or 'bounds_lat_1d' is defined." << std::endl
               << "Please define either both attributes or none.");
       }

       if ((!bounds_lon_2d.isEmpty() && bounds_lat_2d.isEmpty()) || (bounds_lon_2d.isEmpty() && !bounds_lat_2d.isEmpty()))
       {
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Only 'bounds_lon_2d' or 'bounds_lat_2d' is defined." << std::endl
               << "Please define either both attributes or none.");
       }

       if (!bounds_lon_1d.isEmpty() && nvertex.getValue() != bounds_lon_1d.extent(0))
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "'bounds_lon_1d' dimension is not compatible with 'nvertex'." << std::endl
               << "'bounds_lon_1d' dimension is " << bounds_lon_1d.extent(0)
               << " but nvertex is " << nvertex.getValue() << ".");

       if (!bounds_lon_2d.isEmpty() && nvertex.getValue() != bounds_lon_2d.extent(0))
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "'bounds_lon_2d' dimension is not compatible with 'nvertex'." << std::endl
               << "'bounds_lon_2d' dimension is " << bounds_lon_2d.extent(0)
               << " but nvertex is " << nvertex.getValue() << ".");

       if (!bounds_lon_1d.isEmpty() && lonvalue_1d.isEmpty())
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Since 'bounds_lon_1d' is defined, 'lonvalue_1d' must be defined too." << std::endl);

       if (!bounds_lon_2d.isEmpty() && lonvalue_2d.isEmpty())
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Since 'bounds_lon_2d' is defined, 'lonvalue_2d' must be defined too." << std::endl);

       if (!bounds_lat_1d.isEmpty() && nvertex.getValue() != bounds_lat_1d.extent(0))
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "'bounds_lat_1d' dimension is not compatible with 'nvertex'." << std::endl
               << "'bounds_lat_1d' dimension is " << bounds_lat_1d.extent(0)
               << " but nvertex is " << nvertex.getValue() << ".");

       if (!bounds_lat_2d.isEmpty() && nvertex.getValue() != bounds_lat_2d.extent(0))
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "'bounds_lat_2d' dimension is not compatible with 'nvertex'." << std::endl
               << "'bounds_lat_2d' dimension is " << bounds_lat_2d.extent(0)
               << " but nvertex is " << nvertex.getValue() << ".");

       if (!bounds_lat_1d.isEmpty() && latvalue_1d.isEmpty())
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Since 'bounds_lat_1d' is defined, 'latvalue_1d' must be defined too." << std::endl);

       if (!bounds_lat_2d.isEmpty() && latvalue_2d.isEmpty())
         ERROR("CDomain::checkBounds(void)",
               << "Since 'bounds_lat_2d' is defined, 'latvalue_2d' must be defined too." << std::endl);

       // In case of reading UGRID bounds values are not required
       hasBounds = (!bounds_lat_1d.isEmpty() || !bounds_lat_2d.isEmpty() );
     }
     else if (hasBoundValues)
     {
       hasBounds = true;       
     }
     else
     {
       hasBounds = false;
     }
   }

   void CDomain::checkArea(void)
   {
     bool hasAreaValue = (!areavalue.isEmpty() && 0 != areavalue.numElements());
     hasArea = !area.isEmpty();
     if (hasArea && !hasAreaValue)
     {
       if (area.extent(0) != ni || area.extent(1) != nj)
       {
         ERROR("CDomain::checkArea(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "The area does not have the same size as the local domain." << std::endl
               << "Local size is " << ni.getValue() << " x " << nj.getValue() << "." << std::endl
               << "Area size is " << area.extent(0) << " x " << area.extent(1) << ".");
       }
       if (areavalue.isEmpty())
       {
          areavalue.resize(ni*nj);
         for (int j = 0; j < nj; ++j)
         {
           for (int i = 0; i < ni; ++i)
           {
             int k = j * ni + i;
             areavalue(k) = area(i,j);
           }
         }
       }
     }
   }

   void CDomain::checkLonLat()
   {
     if (!hasLonLat) hasLonLat = (!latvalue_1d.isEmpty() && !lonvalue_1d.isEmpty()) ||
                                 (!latvalue_2d.isEmpty() && !lonvalue_2d.isEmpty());
     bool hasLonLatValue = (0 != lonvalue.numElements()) || (0 != latvalue.numElements());
     if (hasLonLat && !hasLonLatValue)
     {
       if (!lonvalue_1d.isEmpty() && !lonvalue_2d.isEmpty())
         ERROR("CDomain::checkLonLat()",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Only one longitude attribute can be used but both 'lonvalue_1d' and 'lonvalue_2d' are defined." << std::endl
               << "Define only one longitude attribute: 'lonvalue_1d' or 'lonvalue_2d'.");

       if (!lonvalue_1d.isEmpty() && lonvalue_2d.isEmpty())
       {
         if ((type_attr::rectilinear != type) && (lonvalue_1d.numElements() != i_index.numElements()))
           ERROR("CDomain::checkLonLat()",
                 << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                 << "'lonvalue_1d' does not have the same size as the local domain." << std::endl
                 << "Local size is " << i_index.numElements() << "." << std::endl
                 << "'lonvalue_1d' size is " << lonvalue_1d.numElements() << ".");
       }

       if (lonvalue_1d.isEmpty() && !lonvalue_2d.isEmpty())
       {
         if (lonvalue_2d.extent(0) != ni || lonvalue_2d.extent(1) != nj)
           ERROR("CDomain::checkLonLat()",
                 << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                 << "'lonvalue_2d' does not have the same size as the local domain." << std::endl
                 << "Local size is " << ni.getValue() << " x " << nj.getValue() << "." << std::endl
                 << "'lonvalue_2d' size is " << lonvalue_2d.extent(0) << " x " << lonvalue_2d.extent(1) << ".");
       }

       if (!latvalue_1d.isEmpty() && !latvalue_2d.isEmpty())
         ERROR("CDomain::checkLonLat()",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Only one latitude attribute can be used but both 'latvalue_1d' and 'latvalue_2d' are defined." << std::endl
               << "Define only one latitude attribute: 'latvalue_1d' or 'latvalue_2d'.");

       if (!latvalue_1d.isEmpty() && latvalue_2d.isEmpty())
       {
         if ((type_attr::rectilinear != type) && (latvalue_1d.numElements() != i_index.numElements()))
           ERROR("CDomain::checkLonLat()",
                 << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                 << "'latvalue_1d' does not have the same size as the local domain." << std::endl
                 << "Local size is " << i_index.numElements() << "." << std::endl
                 << "'latvalue_1d' size is " << latvalue_1d.numElements() << ".");
       }

       if (latvalue_1d.isEmpty() && !latvalue_2d.isEmpty())
       {
         if (latvalue_2d.extent(0) != ni || latvalue_2d.extent(1) != nj)
           ERROR("CDomain::checkLonLat()",
                 << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                 << "'latvalue_2d' does not have the same size as the local domain." << std::endl
                 << "Local size is " << ni.getValue() << " x " << nj.getValue() << "." << std::endl
                 << "'latvalue_2d' size is " << latvalue_2d.extent(0) << " x " << latvalue_2d.extent(1) << ".");
       }
     }
   }

   void CDomain::checkAttributesOnClientAfterTransformation()
   {
     CContext* context=CContext::getCurrent() ;

     if (this->isClientAfterTransformationChecked) return;
     if (context->hasClient)
     {
      this->computeConnectedClients();
       if (hasLonLat)
         if (!context->hasServer)
           this->completeLonLatClient();
     }

     this->isClientAfterTransformationChecked = true;
   }

   //----------------------------------------------------------------
   // Divide function checkAttributes into 2 seperate ones
   // This function only checks all attributes of current domain
   void CDomain::checkAttributesOnClient()
   {
     if (this->isClientChecked) return;
     CContext* context=CContext::getCurrent();

      if (context->hasClient && !context->hasServer)
      {
        this->checkDomain();
        this->checkBounds();
        this->checkArea();
        this->checkLonLat();
      }

      if (context->hasClient && !context->hasServer)
      { // Ct client uniquement
         this->checkMask();
         this->checkDomainData();
         this->checkCompression();
         this->computeLocalMask() ;
      }
      else
      { // Ct serveur uniquement
      }

      this->isClientChecked = true;
   }

   // Send all checked attributes to server
   void CDomain::sendCheckedAttributes()
   {
     if (!this->isClientChecked) checkAttributesOnClient();
     if (!this->isClientAfterTransformationChecked) checkAttributesOnClientAfterTransformation();
     CContext* context=CContext::getCurrent() ;

     if (this->isChecked) return;
     if (context->hasClient)
     {
       sendAttributes();
     }
     this->isChecked = true;
   }

   void CDomain::checkAttributes(void)
   {
      if (this->isChecked) return;
      CContext* context=CContext::getCurrent() ;

      this->checkDomain();
      this->checkLonLat();
      this->checkBounds();
      this->checkArea();

      if (context->hasClient)
      { // Ct client uniquement
         this->checkMask();
         this->checkDomainData();
         this->checkCompression();
         this->computeLocalMask() ;

      }
      else
      { // Ct serveur uniquement
      }

      if (context->hasClient)
      {
        this->computeConnectedClients();
        this->completeLonLatClient();
      }

      this->isChecked = true;
   }

  /*!
     Compute the connection of a client to other clients to determine which clients to send attributes to.
     The sending clients are supposed to already know the distribution of receiving clients (In simple cases, it's band)
     The connection among clients is calculated by using global index. 
     A client connects to other clients which holds the same global index as it.     
  */
  void CDomain::computeConnectedClients()
  {
    CContext* context=CContext::getCurrent() ;
    
    // This line should be changed soon.
    int nbSrvPools = (context->hasServer) ? (context->hasClient ? context->clientPrimServer.size() : 0) : 1;

    nbSenders.clear();
    connectedServerRank_.clear();

    for (int p = 0; p < nbSrvPools; ++p)
    {
      CContextClient* client = (0 != context->clientPrimServer.size()) ? context->clientPrimServer[p] : context->client;
      int nbServer = client->serverSize;
      int nbClient = client->clientSize;
      int rank     = client->clientRank;
      bool doComputeGlobalIndexServer = true;

      if (connectedServerRank_.find(nbServer) == connectedServerRank_.end())
      {

        if (indSrv_.find(nbServer) == indSrv_.end())
        {
          int i,j,i_ind,j_ind, nbIndex, nbIndexZoom;
          int global_zoom_iend=global_zoom_ibegin+global_zoom_ni-1;
          int global_zoom_jend=global_zoom_jbegin+global_zoom_nj-1;

           // Precompute number of index
           int globalIndexCountZoom = 0;
           nbIndex = i_index.numElements();

           if (doZoomByIndex_)
           {
             globalIndexCountZoom = zoom_i_index.numElements();
           }
           else
           {
             for (i = 0; i < nbIndex; ++i)
             {
               i_ind=i_index(i);
               j_ind=j_index(i);

               if (i_ind >= global_zoom_ibegin && i_ind <= global_zoom_iend && j_ind >= global_zoom_jbegin && j_ind <= global_zoom_jend)
               {
                  ++globalIndexCountZoom;
               }
             }
           }

           // Fill in index
           CArray<size_t,1> globalIndexDomainZoom(globalIndexCountZoom);
           CArray<size_t,1> localIndexDomainZoom(globalIndexCountZoom);
           CArray<size_t,1> globalIndexDomain(nbIndex);
           size_t globalIndex;
           int globalIndexCount = 0;

           for (i = 0; i < nbIndex; ++i)
           {
             i_ind=i_index(i);
             j_ind=j_index(i);
             globalIndex = i_ind + j_ind * ni_glo;
             globalIndexDomain(i) = globalIndex;
           }

           if (globalLocalIndexMap_.empty())
           {
             for (i = 0; i < nbIndex; ++i)
               globalLocalIndexMap_[globalIndexDomain(i)] = i;
           }

           globalIndexCountZoom = 0;
           if (doZoomByIndex_)
           {
             int nbIndexZoom = zoom_i_index.numElements();

             for (i = 0; i < nbIndexZoom; ++i)
             {
               i_ind=zoom_i_index(i);
               j_ind=zoom_j_index(i);
               globalIndex = i_ind + j_ind * ni_glo;
               globalIndexDomainZoom(globalIndexCountZoom) = globalIndex;
               ++globalIndexCountZoom;
             }
           }
           else
           {
             int global_zoom_iend=global_zoom_ibegin+global_zoom_ni-1;
             int global_zoom_jend=global_zoom_jbegin+global_zoom_nj-1;
             for (i = 0; i < nbIndex; ++i)
             {
               i_ind=i_index(i);
               j_ind=j_index(i);
               globalIndex = i_ind + j_ind * ni_glo;
               if (i_ind >= global_zoom_ibegin && i_ind <= global_zoom_iend && j_ind >= global_zoom_jbegin && j_ind <= global_zoom_jend)
               {
                  globalIndexDomainZoom(globalIndexCountZoom) = globalIndex;
                  ++globalIndexCountZoom;
               }
             }

             int iend = ibegin + ni -1;
             int jend = jbegin + nj -1;
             zoom_ibegin = global_zoom_ibegin > ibegin ? global_zoom_ibegin.getValue() : ibegin;
             int zoom_iend  = global_zoom_iend < iend ? zoom_iend : iend ;
             zoom_ni     = zoom_iend-zoom_ibegin+1 ;

             zoom_jbegin = global_zoom_jbegin > jbegin ? global_zoom_jbegin.getValue() : jbegin ;
             int zoom_jend   = global_zoom_jend < jend ? zoom_jend : jend;
             zoom_nj     = zoom_jend-zoom_jbegin+1;
           }

           size_t globalSizeIndex = 1, indexBegin, indexEnd;
           int range, clientSize = client->clientSize;
           std::vector<int> nGlobDomain(2);
           nGlobDomain[0] = this->ni_glo;
           nGlobDomain[1] = this->nj_glo;
           for (int i = 0; i < nGlobDomain.size(); ++i) globalSizeIndex *= nGlobDomain[i];
           indexBegin = 0;
           if (globalSizeIndex <= clientSize)
           {
             indexBegin = rank%globalSizeIndex;
             indexEnd = indexBegin;
           }
           else
           {
             for (int i = 0; i < clientSize; ++i)
             {
               range = globalSizeIndex / clientSize;
               if (i < (globalSizeIndex%clientSize)) ++range;
               if (i == client->clientRank) break;
               indexBegin += range;
             }
             indexEnd = indexBegin + range - 1;
           }

           // Even if servers have no index, they must received something from client
           // We only use several client to send "empty" message to these servers
           CServerDistributionDescription serverDescription(nGlobDomain, nbServer);
           std::vector<int> serverZeroIndex;
           if (isUnstructed_) serverZeroIndex = serverDescription.computeServerGlobalIndexInRange(std::make_pair<size_t,size_t>(indexBegin, indexEnd), 0);
           else serverZeroIndex = serverDescription.computeServerGlobalIndexInRange(std::make_pair<size_t,size_t>(indexBegin, indexEnd), 1);

           std::list<int> serverZeroIndexLeader;
           std::list<int> serverZeroIndexNotLeader;
           CContextClient::computeLeader(client->clientRank, client->clientSize, serverZeroIndex.size(), serverZeroIndexLeader, serverZeroIndexNotLeader);
           for (std::list<int>::iterator it = serverZeroIndexLeader.begin(); it != serverZeroIndexLeader.end(); ++it)
              *it = serverZeroIndex[*it];

           CClientServerMapping* clientServerMap = new CClientServerMappingDistributed(serverDescription.getGlobalIndexRange(),
                 client->intraComm);
           clientServerMap->computeServerIndexMapping(globalIndexDomain, nbServer);
           CClientServerMapping::GlobalIndexMap& globalIndexDomainOnServer = clientServerMap->getGlobalIndexOnServer();

           CClientServerMapping::GlobalIndexMap::const_iterator it  = globalIndexDomainOnServer.begin(),
                 ite = globalIndexDomainOnServer.end();
           indSrv_[nbServer].swap(globalIndexDomainOnServer);
           connectedServerRank_[nbServer].clear();
           for (it = indSrv_[nbServer].begin(); it != ite; ++it)
             connectedServerRank_[nbServer].push_back(it->first);

           for (std::list<int>::const_iterator it = serverZeroIndexLeader.begin(); it != serverZeroIndexLeader.end(); ++it)
              connectedServerRank_[nbServer].push_back(*it);

           // Even if a client has no index, it must connect to at least one server and
           // send an "empty" data to this server
           if (connectedServerRank_[nbServer].empty())
              connectedServerRank_[nbServer].push_back(client->clientRank % client->serverSize);

           nbSenders[nbServer] = clientServerMap->computeConnectedClients(client->serverSize, client->clientSize, client->intraComm, connectedServerRank_[nbServer]);
           delete clientServerMap;
        }
      }
    }
  }

   /*!
     Compute index to write data. We only write data on the zoomed region, therefore, there should
     be a map between the complete grid and the reduced grid where we write data.
     By using global index we can easily create this kind of mapping.
   */
   void CDomain::computeWrittenIndex()
   {  
      if (computedWrittenIndex_) return;
      computedWrittenIndex_ = true;

      CContext* context=CContext::getCurrent();      
      CContextServer* server = context->server;  

      std::vector<int> nBegin(2), nSize(2), nBeginGlobal(2), nGlob(2);
      nBegin[0]       = zoom_ibegin;  nBegin[1] = zoom_jbegin;
      nSize[0]        = zoom_ni;      nSize[1]  = zoom_nj;
      nBeginGlobal[0] = 0; nBeginGlobal[1] = 0;
      nGlob[0]        = ni_glo;   nGlob[1] = nj_glo;
      CDistributionServer srvDist(server->intraCommSize, nBegin, nSize, nBeginGlobal, nGlob); 
      const CArray<size_t,1>& writtenGlobalIndex  = srvDist.getGlobalIndex();

      size_t nbWritten = 0, indGlo;      
      boost::unordered_map<size_t,size_t>::const_iterator itb = globalLocalIndexMap_.begin(),
                                                          ite = globalLocalIndexMap_.end(), it;          
      CArray<size_t,1>::const_iterator itSrvb = writtenGlobalIndex.begin(),
                                       itSrve = writtenGlobalIndex.end(), itSrv;

      localIndexToWriteOnServer.resize(writtenGlobalIndex.numElements());
      nbWritten = 0;
      for (itSrv = itSrvb; itSrv != itSrve; ++itSrv)
      {
        indGlo = *itSrv;
        if (ite != globalLocalIndexMap_.find(indGlo))
        {
          localIndexToWriteOnServer(nbWritten) = globalLocalIndexMap_[indGlo];
        }
        else
        {
          localIndexToWriteOnServer(nbWritten) = -1;
        }
        ++nbWritten;
      }
   }

  void CDomain::computeWrittenCompressedIndex(MPI_Comm writtenComm)
  {
    int writtenCommSize;
    MPI_Comm_size(writtenComm, &writtenCommSize);
    if (compressedIndexToWriteOnServer.find(writtenCommSize) != compressedIndexToWriteOnServer.end())
      return;

    if (isCompressible())
    {
      size_t nbWritten = 0, indGlo;
      CContext* context=CContext::getCurrent();      
      CContextServer* server = context->server;  

      std::vector<int> nBegin(2), nSize(2), nBeginGlobal(2), nGlob(2);
      nBegin[0]       = zoom_ibegin;  nBegin[1] = zoom_jbegin;
      nSize[0]        = zoom_ni;      nSize[1]  = zoom_nj;
      nBeginGlobal[0] = 0; nBeginGlobal[1] = 0;
      nGlob[0]        = ni_glo;   nGlob[1] = nj_glo;
      CDistributionServer srvDist(server->intraCommSize, nBegin, nSize, nBeginGlobal, nGlob); 
      const CArray<size_t,1>& writtenGlobalIndex  = srvDist.getGlobalIndex();

      boost::unordered_map<size_t,size_t>::const_iterator itb = globalLocalIndexMap_.begin(),
                                                          ite = globalLocalIndexMap_.end(), it;   
      CArray<size_t,1>::const_iterator itSrvb = writtenGlobalIndex.begin(),
                                       itSrve = writtenGlobalIndex.end(), itSrv;
      boost::unordered_map<size_t,size_t> localGlobalIndexMap;
      for (itSrv = itSrvb; itSrv != itSrve; ++itSrv)
      {
        indGlo = *itSrv;
        if (ite != globalLocalIndexMap_.find(indGlo))
        {
          localGlobalIndexMap[localIndexToWriteOnServer(nbWritten)] = indGlo;
          ++nbWritten;
        }                 
      }

      nbWritten = 0;
      for (int idx = 0; idx < data_i_index.numElements(); ++idx)
      {
        if (localGlobalIndexMap.end() != localGlobalIndexMap.find(data_i_index(idx)))
        {
          ++nbWritten;
        }
      }

      compressedIndexToWriteOnServer[writtenCommSize].resize(nbWritten);
      nbWritten = 0;
      for (int idx = 0; idx < data_i_index.numElements(); ++idx)
      {
        if (localGlobalIndexMap.end() != localGlobalIndexMap.find(data_i_index(idx)))
        {
          compressedIndexToWriteOnServer[writtenCommSize](nbWritten) = localGlobalIndexMap[data_i_index(idx)];
          ++nbWritten;
        }
      }

      numberWrittenIndexes_[writtenCommSize] = nbWritten;
      if (isDistributed())
      {
             
        MPI_Allreduce(&numberWrittenIndexes_[writtenCommSize], &totalNumberWrittenIndexes_[writtenCommSize], 1, MPI_INT, MPI_SUM, writtenComm);
        MPI_Scan(&numberWrittenIndexes_[writtenCommSize], &offsetWrittenIndexes_[writtenCommSize], 1, MPI_INT, MPI_SUM, writtenComm);
        offsetWrittenIndexes_[writtenCommSize] -= numberWrittenIndexes_[writtenCommSize];
      }
      else
        totalNumberWrittenIndexes_[writtenCommSize] = numberWrittenIndexes_[writtenCommSize];
      }
  }

  /*!
    Send all attributes from client to connected clients
    The attributes will be rebuilt on receiving side
  */
  void CDomain::sendAttributes()
  {
    sendDistributionAttributes();
    sendIndex();       
    sendMask();
    sendLonLat();
    sendArea();    
    sendDataIndex();
  }

  /*!
    Send global index and zoom index from client to connected client(s)
    zoom index can be smaller than global index
  */
  void CDomain::sendIndex()
  {
    int ns, n, i, j, ind, nv, idx;
    std::list<CContextClient*>::iterator it;
    for (it=clients.begin(); it!=clients.end(); ++it)
    {
      CContextClient* client = *it;

      int serverSize = client->serverSize;
      CEventClient eventIndex(getType(), EVENT_ID_INDEX);

      list<CMessage> list_msgsIndex;
      list<CArray<int,1> > list_indZoom, list_writtenInd, list_indGlob;

      boost::unordered_map<int, vector<size_t> >::const_iterator itIndex, iteIndex;
      iteIndex = indSrv_[serverSize].end();
      for (int k = 0; k < connectedServerRank_[serverSize].size(); ++k)
      {
        int nbIndGlob = 0;
        int rank = connectedServerRank_[serverSize][k];
        itIndex = indSrv_[serverSize].find(rank);
        if (iteIndex != itIndex)
          nbIndGlob = itIndex->second.size();

        list_indGlob.push_back(CArray<int,1>(nbIndGlob));        

        CArray<int,1>& indGlob = list_indGlob.back();
        for (n = 0; n < nbIndGlob; ++n)
        {
          indGlob(n) = static_cast<int>(itIndex->second[n]);
        }

        list_msgsIndex.push_back(CMessage());
        list_msgsIndex.back() << this->getId() << (int)type; // enum ne fonctionne pour les message => ToFix
        list_msgsIndex.back() << isCurvilinear;
        list_msgsIndex.back() << list_indGlob.back(); //list_indi.back() << list_indj.back();
       
        eventIndex.push(rank, nbSenders[serverSize][rank], list_msgsIndex.back());
      }

      client->sendEvent(eventIndex);
    }
  }

  /*!
    Send distribution from client to other clients
    Because a client in a level knows correctly the grid distribution of client on the next level
    it calculates this distribution then sends it to the corresponding clients on the next level
  */
  void CDomain::sendDistributionAttributes(void)
  {
    std::list<CContextClient*>::iterator it;
    for (it=clients.begin(); it!=clients.end(); ++it)
    {
      CContextClient* client = *it;
      int nbServer = client->serverSize;
      std::vector<int> nGlobDomain(2);
      nGlobDomain[0] = this->ni_glo;
      nGlobDomain[1] = this->nj_glo;

      CServerDistributionDescription serverDescription(nGlobDomain, nbServer);
      if (isUnstructed_) serverDescription.computeServerDistribution(false, 0);
      else serverDescription.computeServerDistribution(false, 1);

      std::vector<std::vector<int> > serverIndexBegin = serverDescription.getServerIndexBegin();
      std::vector<std::vector<int> > serverDimensionSizes = serverDescription.getServerDimensionSizes();

      CEventClient event(getType(),EVENT_ID_SERVER_ATTRIBUT);
      if (client->isServerLeader())
      {
        std::list<CMessage> msgs;

        const std::list<int>& ranks = client->getRanksServerLeader();
        for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
        {
          // Use const int to ensure CMessage holds a copy of the value instead of just a reference
          const int ibegin_srv = serverIndexBegin[*itRank][0];
          const int jbegin_srv = serverIndexBegin[*itRank][1];
          const int ni_srv = serverDimensionSizes[*itRank][0];
          const int nj_srv = serverDimensionSizes[*itRank][1];

          msgs.push_back(CMessage());
          CMessage& msg = msgs.back();
          msg << this->getId() ;
          msg << isUnstructed_;
          msg << ni_srv << ibegin_srv << nj_srv << jbegin_srv;
          msg << global_zoom_ni.getValue() << global_zoom_ibegin.getValue() << global_zoom_nj.getValue() << global_zoom_jbegin.getValue();        
          msg << isCompressible_;

          event.push(*itRank,1,msg);
        }
        client->sendEvent(event);
      }
      else client->sendEvent(event);
    }
  }

  /*!
    Send mask index from client to connected(s) clients    
  */
  void CDomain::sendMask()
  {
    int ns, n, i, j, ind, nv, idx;
    std::list<CContextClient*>::iterator it;
    for (it=clients.begin(); it!=clients.end(); ++it)
    {
      CContextClient* client = *it;
      int serverSize = client->serverSize;

      // send area for each connected server
      CEventClient eventMask(getType(), EVENT_ID_MASK);

      list<CMessage> list_msgsMask;
      list<CArray<bool,1> > list_mask;

      boost::unordered_map<int, vector<size_t> >::const_iterator it, iteMap;
      iteMap = indSrv_[serverSize].end();
      for (int k = 0; k < connectedServerRank_[serverSize].size(); ++k)
      {
        int nbData = 0;
        int rank = connectedServerRank_[serverSize][k];
        it = indSrv_[serverSize].find(rank);
        if (iteMap != it)
          nbData = it->second.size();
        list_mask.push_back(CArray<bool,1>(nbData));

        const std::vector<size_t>& temp = it->second;
        for (n = 0; n < nbData; ++n)
        {
          idx = static_cast<int>(it->second[n]);
          list_mask.back()(n) = domainMask(globalLocalIndexMap_[idx]);
        }

        list_msgsMask.push_back(CMessage());
        list_msgsMask.back() << this->getId() << list_mask.back();
        eventMask.push(rank, nbSenders[serverSize][rank], list_msgsMask.back());
      }
      client->sendEvent(eventMask);
    }
  }

  /*!
    Send area from client to connected client(s)
  */
  void CDomain::sendArea()
  {
    if (!hasArea) return;

    int ns, n, i, j, ind, nv, idx;
    std::list<CContextClient*>::iterator it;

    for (it=clients.begin(); it!=clients.end(); ++it)
    {
      CContextClient* client = *it;
      int serverSize = client->serverSize;

      // send area for each connected server
      CEventClient eventArea(getType(), EVENT_ID_AREA);

      list<CMessage> list_msgsArea;
      list<CArray<double,1> > list_area;

      boost::unordered_map<int, vector<size_t> >::const_iterator it, iteMap;
      iteMap = indSrv_[serverSize].end();
      for (int k = 0; k < connectedServerRank_[serverSize].size(); ++k)
      {
        int nbData = 0;
        int rank = connectedServerRank_[serverSize][k];
        it = indSrv_[serverSize].find(rank);
        if (iteMap != it)
          nbData = it->second.size();
        list_area.push_back(CArray<double,1>(nbData));

        const std::vector<size_t>& temp = it->second;
        for (n = 0; n < nbData; ++n)
        {
          idx = static_cast<int>(it->second[n]);
          list_area.back()(n) = areavalue(globalLocalIndexMap_[idx]);
        }

        list_msgsArea.push_back(CMessage());
        list_msgsArea.back() << this->getId() << hasArea;
        list_msgsArea.back() << list_area.back();
        eventArea.push(rank, nbSenders[serverSize][rank], list_msgsArea.back());
      }
      client->sendEvent(eventArea);
    }
  }

  /*!
    Send longitude and latitude from client to servers
    Each client send long and lat information to corresponding connected clients(s).
    Because longitude and latitude are optional, this function only called if latitude and longitude exist
  */
  void CDomain::sendLonLat()
  {
    if (!hasLonLat) return;

    int ns, n, i, j, ind, nv, idx;
    std::list<CContextClient*>::iterator it;
    for (it=clients.begin(); it!=clients.end(); ++it)
    {
      CContextClient* client = *it;
      int serverSize = client->serverSize;

      // send lon lat for each connected server
      CEventClient eventLon(getType(), EVENT_ID_LON);
      CEventClient eventLat(getType(), EVENT_ID_LAT);

      list<CMessage> list_msgsLon, list_msgsLat;
      list<CArray<double,1> > list_lon, list_lat;
      list<CArray<double,2> > list_boundslon, list_boundslat;

      boost::unordered_map<int, vector<size_t> >::const_iterator it, iteMap;
      iteMap = indSrv_[serverSize].end();
      for (int k = 0; k < connectedServerRank_[serverSize].size(); ++k)
      {
        int nbData = 0;
        int rank = connectedServerRank_[serverSize][k];
        it = indSrv_[serverSize].find(rank);
        if (iteMap != it)
          nbData = it->second.size();

        list_lon.push_back(CArray<double,1>(nbData));
        list_lat.push_back(CArray<double,1>(nbData));

        if (hasBounds)
        {
          list_boundslon.push_back(CArray<double,2>(nvertex, nbData));
          list_boundslat.push_back(CArray<double,2>(nvertex, nbData));
        }

        CArray<double,1>& lon = list_lon.back();
        CArray<double,1>& lat = list_lat.back();
        const std::vector<size_t>& temp = it->second;
        for (n = 0; n < nbData; ++n)
        {
          idx = static_cast<int>(it->second[n]);
          int localInd = globalLocalIndexMap_[idx];
          lon(n) = lonvalue(localInd);
          lat(n) = latvalue(localInd);

          if (hasBounds)
          {
            CArray<double,2>& boundslon = list_boundslon.back();
            CArray<double,2>& boundslat = list_boundslat.back();

            for (nv = 0; nv < nvertex; ++nv)
            {
              boundslon(nv, n) = bounds_lonvalue(nv, localInd);
              boundslat(nv, n) = bounds_latvalue(nv, localInd);
            }
          }
        }

        list_msgsLon.push_back(CMessage());
        list_msgsLat.push_back(CMessage());

        list_msgsLon.back() << this->getId() << hasLonLat;
        if (hasLonLat) 
          list_msgsLon.back() << list_lon.back();
        list_msgsLon.back()  << hasBounds;
        if (hasBounds)
        {
          list_msgsLon.back() << list_boundslon.back();
        }

        list_msgsLat.back() << this->getId() << hasLonLat;
        if (hasLonLat)
          list_msgsLat.back() << list_lat.back();
        list_msgsLat.back() << hasBounds;
        if (hasBounds)
        {          
          list_msgsLat.back() << list_boundslat.back();
        }

        eventLon.push(rank, nbSenders[serverSize][rank], list_msgsLon.back());
        eventLat.push(rank, nbSenders[serverSize][rank], list_msgsLat.back());
      }
      client->sendEvent(eventLon);
      client->sendEvent(eventLat);
    }
  }

  /*!
    Send data index to corresponding connected clients.
    Data index can be compressed however, we always send decompressed data index
    and they will be compressed on receiving.
    The compressed index are represented with 1 and others are represented with -1
  */
  void CDomain::sendDataIndex()
  {
    int ns, n, i, j, ind, nv, idx;
    std::list<CContextClient*>::iterator it;
    for (it=clients.begin(); it!=clients.end(); ++it)
    {
      CContextClient* client = *it;

      int serverSize = client->serverSize;

      // send area for each connected server
      CEventClient eventDataIndex(getType(), EVENT_ID_DATA_INDEX);

      list<CMessage> list_msgsDataIndex;
      list<CArray<int,1> > list_data_i_index, list_data_j_index;

      int nbIndex = i_index.numElements();
      int niByIndex = max(i_index) - min(i_index) + 1;
      int njByIndex = max(j_index) - min(j_index) + 1; 
      int dataIindexBound = (1 == data_dim) ? (niByIndex * njByIndex) : niByIndex;
      int dataJindexBound = (1 == data_dim) ? (niByIndex * njByIndex) : njByIndex;

      
      CArray<int,1> dataIIndex(nbIndex), dataJIndex(nbIndex);
      dataIIndex = -1; 
      dataJIndex = -1;
      ind = 0;

      for (idx = 0; idx < data_i_index.numElements(); ++idx)
      {
        int dataIidx = data_i_index(idx) + data_ibegin;
        int dataJidx = data_j_index(idx) + data_jbegin;
        if ((0 <= dataIidx) && (dataIidx < dataIindexBound) &&
            (0 <= dataJidx) && (dataJidx < dataJindexBound))
        {
          dataIIndex((1 == data_dim) ? dataIidx : dataJidx * ni + dataIidx) = 1; //i_index(dataIidx);//dataIidx;
          dataJIndex((1 == data_dim) ? dataIidx : dataJidx * ni + dataIidx) = 1; //j_index(dataJidx);//          
        }
      }

      boost::unordered_map<int, vector<size_t> >::const_iterator it, iteMap;
      iteMap = indSrv_[serverSize].end();
      for (int k = 0; k < connectedServerRank_[serverSize].size(); ++k)
      {
        int nbData = 0;
        int rank = connectedServerRank_[serverSize][k];
        it = indSrv_[serverSize].find(rank);
        if (iteMap != it)
          nbData = it->second.size();
        list_data_i_index.push_back(CArray<int,1>(nbData));
        list_data_j_index.push_back(CArray<int,1>(nbData));

        const std::vector<size_t>& temp = it->second;
        for (n = 0; n < nbData; ++n)
        {
          idx = static_cast<int>(it->second[n]);
          i = globalLocalIndexMap_[idx];
          list_data_i_index.back()(n) = dataIIndex(i);
          list_data_j_index.back()(n) = dataJIndex(i);
        }

        list_msgsDataIndex.push_back(CMessage());
        list_msgsDataIndex.back() << this->getId();
        list_msgsDataIndex.back() << list_data_i_index.back() << list_data_j_index.back();
        eventDataIndex.push(rank, nbSenders[serverSize][rank], list_msgsDataIndex.back());
      }
      client->sendEvent(eventDataIndex);
    }
  }
  
  bool CDomain::dispatchEvent(CEventServer& event)
  {
    if (SuperClass::dispatchEvent(event)) return true;
    else
    {
      switch(event.type)
      {
        case EVENT_ID_SERVER_ATTRIBUT:
          recvDistributionAttributes(event);
          return true;
          break;
        case EVENT_ID_INDEX:
          recvIndex(event);
          return true;
          break;
        case EVENT_ID_MASK:
          recvMask(event);
          return true;
          break;
        case EVENT_ID_LON:
          recvLon(event);
          return true;
          break;
        case EVENT_ID_LAT:
          recvLat(event);
          return true;
          break;
        case EVENT_ID_AREA:
          recvArea(event);
          return true;
          break;  
        case EVENT_ID_DATA_INDEX:
          recvDataIndex(event);
          return true;
          break;
        default:
          ERROR("bool CDomain::dispatchEvent(CEventServer& event)",
                << "Unknown Event");
          return false;
       }
    }
  }

  /*!
    Receive index event from clients(s)
    \param[in] event event contain info about rank and associated index
  */
  void CDomain::recvIndex(CEventServer& event)
  {
    string domainId;
    std::map<int, CBufferIn*> rankBuffers;

    list<CEventServer::SSubEvent>::iterator it;
    for (it = event.subEvents.begin(); it != event.subEvents.end(); ++it)
    {      
      CBufferIn* buffer = it->buffer;
      *buffer >> domainId;
      rankBuffers[it->rank] = buffer;        
    }
    get(domainId)->recvIndex(rankBuffers);
  }

  /*!
    Receive index information from client(s). We use the global index for mapping index between
    sending clients and receiving clients.
    \param[in] rankBuffers rank of sending client and the corresponding receive buffer  
  */
  void CDomain::recvIndex(std::map<int, CBufferIn*>& rankBuffers)
  {
    int nbReceived = rankBuffers.size(), i, ind, index, type_int, iIndex, jIndex;
    recvClientRanks_.resize(nbReceived);        

    std::map<int, CBufferIn*>::iterator it = rankBuffers.begin(), ite = rankBuffers.end();
    ind = 0;
    for (ind = 0; it != ite; ++it, ++ind)
    {       
       recvClientRanks_[ind] = it->first;
       CBufferIn& buffer = *(it->second);
       buffer >> type_int >> isCurvilinear >> indGlob_[it->first]; 
       type.setValue((type_attr::t_enum)type_int); // probleme des type enum avec les buffers : ToFix
    }
    int nbIndGlob = 0;
    for (i = 0; i < nbReceived; ++i)
    {
      nbIndGlob += indGlob_[recvClientRanks_[i]].numElements();
    }
    
    globalLocalIndexMap_.rehash(std::ceil(nbIndGlob/globalLocalIndexMap_.max_load_factor()));
//    i_index.resize(nbIndGlob);
//    j_index.resize(nbIndGlob);
    i_index.resize(ni*nj);
    j_index.resize(ni*nj);
    i_index = -1;
    j_index = -1;
    int nbIndexGlobMax = nbIndGlob, nbIndLoc;

    nbIndGlob = 0;
    for (i = 0; i < nbReceived; ++i)
    {
      CArray<int,1>& tmp = indGlob_[recvClientRanks_[i]];
      for (ind = 0; ind < tmp.numElements(); ++ind)
      {
         index = tmp(ind);
         if (0 == globalLocalIndexMap_.count(index))
         {
           iIndex = (index%ni_glo)-ibegin;
           iIndex = (iIndex < 0) ? 0 : iIndex;
           jIndex = (index/ni_glo)-jbegin;
           jIndex = (jIndex < 0) ? 0 : jIndex;
           nbIndLoc = iIndex + ni * jIndex;
           i_index(nbIndLoc) = index % ni_glo;
           j_index(nbIndLoc) = index / ni_glo;
           globalLocalIndexMap_[index] = nbIndLoc;
//           i_index(nbIndGlob) = index % ni_glo;
//           j_index(nbIndGlob) = index / ni_glo;
//           globalLocalIndexMap_[index] = nbIndGlob;
           ++nbIndGlob;
//           if (nbIndLoc < nbIndexGlobMax)
//          {
//             i_index(nbIndLoc) = index % ni_glo;
//             j_index(nbIndLoc) = index / ni_glo;
//             globalLocalIndexMap_[index] = nbIndLoc;  
//             ++nbIndGlob;
//           }
//           // i_index(nbIndGlob) = index % ni_glo;
//           // j_index(nbIndGlob) = index / ni_glo;
//           // globalLocalIndexMap_[index] = nbIndGlob;  
//           // ++nbIndGlob;
         } 
      } 
    } 

//    if (nbIndGlob==0)
//    {
//      i_index.resize(nbIndGlob);
//      j_index.resize(nbIndGlob);
//    }
//    else
//    {
//      i_index.resizeAndPreserve(nbIndGlob);
//      j_index.resizeAndPreserve(nbIndGlob);
//    }
  }

  /*!
    Receive attributes event from clients(s)
    \param[in] event event contain info about rank and associated attributes
  */
  void CDomain::recvDistributionAttributes(CEventServer& event)
  {
    CBufferIn* buffer=event.subEvents.begin()->buffer;
    string domainId ;
    *buffer>>domainId ;
    get(domainId)->recvDistributionAttributes(*buffer);
  }

  /*!
    Receive attributes from client(s): zoom info and begin and n of each server
    \param[in] rank rank of client source
    \param[in] buffer message containing attributes info
  */
  void CDomain::recvDistributionAttributes(CBufferIn& buffer)
  {
    int ni_tmp, ibegin_tmp, nj_tmp, jbegin_tmp;
    int global_zoom_ni_tmp, global_zoom_ibegin_tmp, global_zoom_nj_tmp, global_zoom_jbegin_tmp;
    buffer >> isUnstructed_ >> ni_tmp >> ibegin_tmp >> nj_tmp >> jbegin_tmp
           >> global_zoom_ni_tmp >> global_zoom_ibegin_tmp >> global_zoom_nj_tmp >> global_zoom_jbegin_tmp           
           >> isCompressible_;
    ni.setValue(ni_tmp);
    ibegin.setValue(ibegin_tmp);
    nj.setValue(nj_tmp);
    jbegin.setValue(jbegin_tmp);

    global_zoom_ni.setValue(global_zoom_ni_tmp);
    global_zoom_ibegin.setValue(global_zoom_ibegin_tmp);
    global_zoom_nj.setValue(global_zoom_nj_tmp);
    global_zoom_jbegin.setValue(global_zoom_jbegin_tmp);

    int iend = ibegin + ni  - 1;
    int jend = jbegin + nj  - 1;
    int zoom_iend_glob = global_zoom_ibegin + global_zoom_ni - 1;
    int zoom_jend_glob = global_zoom_jbegin + global_zoom_nj - 1;

    zoom_ibegin.setValue(global_zoom_ibegin > ibegin ? global_zoom_ibegin.getValue() : ibegin);
    int zoom_iend = zoom_iend_glob < iend ? zoom_iend_glob : iend ;
    zoom_ni.setValue(zoom_iend-zoom_ibegin+1);

    zoom_jbegin.setValue(global_zoom_jbegin > jbegin ? global_zoom_jbegin.getValue() : jbegin);
    int zoom_jend = zoom_jend_glob < jend ? zoom_jend_glob : jend ;
    zoom_nj.setValue(zoom_jend-zoom_jbegin+1);

    if (zoom_ni<=0 || zoom_nj<=0)
    {
      zoom_ni=0 ; zoom_ibegin=global_zoom_ibegin ; //=0; zoom_iend=0 ; 
      zoom_nj=0 ; zoom_jbegin=global_zoom_jbegin ; //=0; zoom_jend=0 ;
    }

  }

  /*!
    Receive area event from clients(s)
    \param[in] event event contain info about rank and associated area
  */
  void CDomain::recvMask(CEventServer& event)
  {
    string domainId;
    std::map<int, CBufferIn*> rankBuffers;

    list<CEventServer::SSubEvent>::iterator it;
    for (it = event.subEvents.begin(); it != event.subEvents.end(); ++it)
    {      
      CBufferIn* buffer = it->buffer;
      *buffer >> domainId;
      rankBuffers[it->rank] = buffer;     
    }
    get(domainId)->recvMask(rankBuffers);
  }


  /*!
    Receive mask information from client(s)
    \param[in] rankBuffers rank of sending client and the corresponding receive buffer  
  */
  void CDomain::recvMask(std::map<int, CBufferIn*>& rankBuffers)
  {
    int nbReceived = rankBuffers.size(), i, ind, index, lInd;
    if (nbReceived != recvClientRanks_.size())
      ERROR("void CDomain::recvMask(std::map<int, CBufferIn*>& rankBuffers)",
           << "The number of sending clients is not correct."
           << "Expected number: " << recvClientRanks_.size() << " but received " << nbReceived);

    vector<CArray<bool,1> > recvMaskValue(nbReceived);      
    for (i = 0; i < recvClientRanks_.size(); ++i)
    {
      int rank = recvClientRanks_[i];
      CBufferIn& buffer = *(rankBuffers[rank]);      
      buffer >> recvMaskValue[i];
    }

    int nbMaskInd = 0;
    for (i = 0; i < nbReceived; ++i)
    {
      nbMaskInd += recvMaskValue[i].numElements();
    }
  
    if (nbMaskInd != globalLocalIndexMap_.size())
      info (0) << "If domain " << this->getDomainOutputName() <<" does not have overlapped regions between processes "
               << "something must be wrong with mask index "<< std::endl;

    nbMaskInd = globalLocalIndexMap_.size();
//    mask_1d.resize(nbMaskInd);
//    domainMask.resize(nbMaskInd);
    mask_1d.resize(ni*nj);
    domainMask.resize(ni*nj);
    mask_1d = false;
    
    for (i = 0; i < nbReceived; ++i)
    {
      CArray<int,1>& tmpInd = indGlob_[recvClientRanks_[i]];
      CArray<bool,1>& tmp = recvMaskValue[i];
      for (ind = 0; ind < tmp.numElements(); ++ind)
      {
        lInd = globalLocalIndexMap_[size_t(tmpInd(ind))];
        if (!mask_1d(lInd)) // Only rewrite mask_1d if it's not true
          mask_1d(lInd) = tmp(ind);
      }
    }
    domainMask=mask_1d ;
  }

  /*!
    Receive longitude event from clients(s)
    \param[in] event event contain info about rank and associated longitude
  */
  void CDomain::recvLon(CEventServer& event)
  {
    string domainId;
    std::map<int, CBufferIn*> rankBuffers;

    list<CEventServer::SSubEvent>::iterator it;
    for (it = event.subEvents.begin(); it != event.subEvents.end(); ++it)
    {      
      CBufferIn* buffer = it->buffer;
      *buffer >> domainId;
      rankBuffers[it->rank] = buffer;        
    }
    get(domainId)->recvLon(rankBuffers);
  }

  /*!
    Receive longitude information from client(s)
    \param[in] rankBuffers rank of sending client and the corresponding receive buffer  
  */
  void CDomain::recvLon(std::map<int, CBufferIn*>& rankBuffers)
  {
    int nbReceived = rankBuffers.size(), i, ind, index, iindex, jindex, lInd;
    if (nbReceived != recvClientRanks_.size())
      ERROR("void CDomain::recvLon(std::map<int, CBufferIn*>& rankBuffers)",
           << "The number of sending clients is not correct."
           << "Expected number: " << recvClientRanks_.size() << " but received " << nbReceived);

    vector<CArray<double,1> > recvLonValue(nbReceived);
    vector<CArray<double,2> > recvBoundsLonValue(nbReceived);    
    for (i = 0; i < recvClientRanks_.size(); ++i)
    {
      int rank = recvClientRanks_[i];
      CBufferIn& buffer = *(rankBuffers[rank]);
      buffer >> hasLonLat;
      if (hasLonLat)
        buffer >> recvLonValue[i];
      buffer >> hasBounds;
      if (hasBounds)
        buffer >> recvBoundsLonValue[i];
    }

    if (hasLonLat)
    {
      int nbLonInd = 0;
      for (i = 0; i < nbReceived; ++i)
      {
        nbLonInd += recvLonValue[i].numElements();
      }
    
      if (nbLonInd != globalLocalIndexMap_.size())
        info (0) << "If domain " << this->getDomainOutputName() <<" does not have overlapped regions between processes "
                 << "something must be wrong with longitude index "<< std::endl;

      nbLonInd = globalLocalIndexMap_.size();
//      lonvalue.resize(nbLonInd);
      lonvalue.resize(ni*nj);
      lonvalue = -1.0;
      if (hasBounds)
      {
//        bounds_lonvalue.resize(nvertex,nbLonInd);
//        bounds_lonvalue = 0.;
        bounds_lonvalue.resize(nvertex,ni*nj);
        bounds_lonvalue = -1.0;
      }

      nbLonInd = 0;
      for (i = 0; i < nbReceived; ++i)
      {
        CArray<int,1>& tmpInd = indGlob_[recvClientRanks_[i]];
        CArray<double,1>& tmp = recvLonValue[i];
        for (ind = 0; ind < tmp.numElements(); ++ind)
        {
          lInd = globalLocalIndexMap_[size_t(tmpInd(ind))];
          lonvalue(lInd) = tmp(ind); 
           if (hasBounds)
           {          
            for (int nv = 0; nv < nvertex; ++nv)
              bounds_lonvalue(nv, lInd) = recvBoundsLonValue[i](nv, ind);
           }                  
        }
      }       
    }
  }

  /*!
    Receive latitude event from clients(s)
    \param[in] event event contain info about rank and associated latitude
  */
  void CDomain::recvLat(CEventServer& event)
  {
    string domainId;
    std::map<int, CBufferIn*> rankBuffers;

    list<CEventServer::SSubEvent>::iterator it;
    for (it = event.subEvents.begin(); it != event.subEvents.end(); ++it)
    {      
      CBufferIn* buffer = it->buffer;
      *buffer >> domainId;
      rankBuffers[it->rank] = buffer;    
    }
    get(domainId)->recvLat(rankBuffers);
  }

  /*!
    Receive latitude information from client(s)
    \param[in] rankBuffers rank of sending client and the corresponding receive buffer  
  */
  void CDomain::recvLat(std::map<int, CBufferIn*>& rankBuffers)
  {
    int nbReceived = rankBuffers.size(), i, ind, index, iindex, jindex, lInd;
    if (nbReceived != recvClientRanks_.size())
      ERROR("void CDomain::recvLat(std::map<int, CBufferIn*>& rankBuffers)",
           << "The number of sending clients is not correct."
           << "Expected number: " << recvClientRanks_.size() << " but received " << nbReceived);

    vector<CArray<double,1> > recvLatValue(nbReceived);
    vector<CArray<double,2> > recvBoundsLatValue(nbReceived);    
    for (i = 0; i < recvClientRanks_.size(); ++i)
    {
      int rank = recvClientRanks_[i];
      CBufferIn& buffer = *(rankBuffers[rank]);
      buffer >> hasLonLat;
      if (hasLonLat)
        buffer >> recvLatValue[i];
      buffer >> hasBounds;
      if (hasBounds)
        buffer >> recvBoundsLatValue[i];
    }

    if (hasLonLat)
    {
      int nbLatInd = 0;
      for (i = 0; i < nbReceived; ++i)
      {
        nbLatInd += recvLatValue[i].numElements();
      }
    
      if (nbLatInd != globalLocalIndexMap_.size())
        info (0) << "If domain " << this->getDomainOutputName() <<" does not have overlapped regions between processes "
                << "something must be wrong with latitude index "<< std::endl;

      nbLatInd = globalLocalIndexMap_.size();
//      latvalue.resize(nbLatInd);
      latvalue.resize(ni*nj);
      if (hasBounds)
      {
//        bounds_latvalue.resize(nvertex,nbLatInd);
//        bounds_latvalue = 0. ;
        bounds_latvalue.resize(nvertex,ni*nj);
        bounds_latvalue = -1. ;
      }

      nbLatInd = 0;
      for (i = 0; i < nbReceived; ++i)
      {
        CArray<int,1>& tmpInd = indGlob_[recvClientRanks_[i]];
        CArray<double,1>& tmp = recvLatValue[i];
        for (ind = 0; ind < tmp.numElements(); ++ind)
        {
          lInd = globalLocalIndexMap_[size_t(tmpInd(ind))];
          latvalue(lInd) = tmp(ind);    
           if (hasBounds)
           {
            CArray<double,2>& boundslat = recvBoundsLatValue[i];
            for (int nv = 0; nv < nvertex; ++nv)
              bounds_latvalue(nv, lInd) = boundslat(nv, ind);
           }   
          ++nbLatInd;
        }
      }       
    }
  }

  /*!
    Receive area event from clients(s)
    \param[in] event event contain info about rank and associated area
  */
  void CDomain::recvArea(CEventServer& event)
  {
    string domainId;
    std::map<int, CBufferIn*> rankBuffers;

    list<CEventServer::SSubEvent>::iterator it;
    for (it = event.subEvents.begin(); it != event.subEvents.end(); ++it)
    {      
      CBufferIn* buffer = it->buffer;
      *buffer >> domainId;
      rankBuffers[it->rank] = buffer;     
    }
    get(domainId)->recvArea(rankBuffers);
  }

  /*!
    Receive area information from client(s)
    \param[in] rankBuffers rank of sending client and the corresponding receive buffer     
  */
  void CDomain::recvArea(std::map<int, CBufferIn*>& rankBuffers)
  {
    int nbReceived = rankBuffers.size(), i, ind, index, lInd;
    if (nbReceived != recvClientRanks_.size())
      ERROR("void CDomain::recvArea(std::map<int, CBufferIn*>& rankBuffers)",
           << "The number of sending clients is not correct."
           << "Expected number: " << recvClientRanks_.size() << " but received " << nbReceived);

    vector<CArray<double,1> > recvAreaValue(nbReceived);      
    for (i = 0; i < recvClientRanks_.size(); ++i)
    {
      int rank = recvClientRanks_[i];
      CBufferIn& buffer = *(rankBuffers[rank]);      
      buffer >> hasArea;
      if (hasArea)
        buffer >> recvAreaValue[i];
    }

    if (hasArea)
    {
      int nbAreaInd = 0;
      for (i = 0; i < nbReceived; ++i)
      {      
        nbAreaInd += recvAreaValue[i].numElements();
      }

      if (nbAreaInd != globalLocalIndexMap_.size())
        info (0) << "If domain " << this->getDomainOutputName() <<" does not have overlapped regions between processes "
                 << "something must be wrong with area index "<< std::endl;

      nbAreaInd = globalLocalIndexMap_.size();
//      areavalue.resize(nbAreaInd);
//      nbAreaInd = 0;
      areavalue.resize(ni*nj);
      nbAreaInd = -1.0;
      for (i = 0; i < nbReceived; ++i)
      {
        CArray<int,1>& tmpInd = indGlob_[recvClientRanks_[i]];
        CArray<double,1>& tmp = recvAreaValue[i];
        for (ind = 0; ind < tmp.numElements(); ++ind)
        {
          lInd = globalLocalIndexMap_[size_t(tmpInd(ind))];
          areavalue(lInd) = tmp(ind);          
        }
      }
      
    }
  }

  /*!
    Compare two domain objects. 
    They are equal if only if they have identical attributes as well as their values.
    Moreover, they must have the same transformations.
  \param [in] domain Compared domain
  \return result of the comparison
  */
  bool CDomain::isEqual(CDomain* obj)
  {
    vector<StdString> excludedAttr;
    excludedAttr.push_back("domain_ref");
    bool objEqual = SuperClass::isEqual(obj, excludedAttr);
    if (!objEqual) return objEqual;

    TransMapTypes thisTrans = this->getAllTransformations();
    TransMapTypes objTrans  = obj->getAllTransformations();

    TransMapTypes::const_iterator it, itb, ite;
    std::vector<ETranformationType> thisTransType, objTransType;
    for (it = thisTrans.begin(); it != thisTrans.end(); ++it)
      thisTransType.push_back(it->first);
    for (it = objTrans.begin(); it != objTrans.end(); ++it)
      objTransType.push_back(it->first);

    if (thisTransType.size() != objTransType.size()) return false;
    for (int idx = 0; idx < thisTransType.size(); ++idx)
      objEqual &= (thisTransType[idx] == objTransType[idx]);

    return objEqual;
  }

  /*!
    Receive data index event from clients(s)
    \param[in] event event contain info about rank and associated index
  */
  void CDomain::recvDataIndex(CEventServer& event)
  {
    string domainId;
    std::map<int, CBufferIn*> rankBuffers;

    list<CEventServer::SSubEvent>::iterator it;
    for (it = event.subEvents.begin(); it != event.subEvents.end(); ++it)
    {      
      CBufferIn* buffer = it->buffer;
      *buffer >> domainId;
      rankBuffers[it->rank] = buffer;        
    }
    get(domainId)->recvDataIndex(rankBuffers);
  }

  /*!
    Receive data index information from client(s)
    A client receives data index from different clients to rebuild its own data index.
    Because we use global index + mask info to calculate the sending data to client(s), 
    this data index must be updated with mask info (maybe it will change in the future)
    Because the data index is local, to rebuild data index of received client, we should use global index along with. 

    \param[in] rankBuffers rank of sending client and the corresponding receive buffer     
  */
  void CDomain::recvDataIndex(std::map<int, CBufferIn*>& rankBuffers)
  {
    int nbReceived = rankBuffers.size(), i, ind, index, indexI, indexJ, type_int, lInd;    
    if (nbReceived != recvClientRanks_.size())
      ERROR("void CDomain::recvDataIndex(std::map<int, CBufferIn*>& rankBuffers)",
           << "The number of sending clients is not correct."
           << "Expected number: " << recvClientRanks_.size() << " but received " << nbReceived);

    vector<CArray<int,1> > recvDataIIndex(nbReceived),recvDataJIndex(nbReceived);     
    for (i = 0; i < recvClientRanks_.size(); ++i)
    {
      int rank = recvClientRanks_[i];
      CBufferIn& buffer = *(rankBuffers[rank]);
      buffer >> recvDataIIndex[i];
      buffer >> recvDataJIndex[i];
    }
   
    int nbIndex = i_index.numElements();
    CArray<int,1> dataIIndex(nbIndex), dataJIndex(nbIndex);
    dataIIndex = -1; dataJIndex = -1;
     
    nbIndex = 0;
    for (i = 0; i < nbReceived; ++i)
    {      
      CArray<int,1>& tmpInd = indGlob_[recvClientRanks_[i]];
      CArray<int,1>& tmpI = recvDataIIndex[i];    
      CArray<int,1>& tmpJ = recvDataJIndex[i];     
      if ((tmpI.numElements() != tmpInd.numElements()) || (tmpJ.numElements() != tmpInd.numElements()))
          ERROR("void CDomain::recvDataIndex(std::map<int, CBufferIn*>& rankBuffers)",
             << "The number of global received index is not coherent with the number of received data index."
             << "Expected number of global index: " << tmpI.numElements() << " but received " << tmpInd.numElements());

      for (ind = 0; ind < tmpI.numElements(); ++ind)
      {
         lInd = globalLocalIndexMap_[size_t(tmpInd(ind))];
         dataIIndex(lInd) = (-1 == dataIIndex(lInd)) ? tmpI(ind) : dataIIndex(lInd); // Only fill in dataIndex if there is no data
         dataJIndex(lInd) = (-1 == dataJIndex(lInd)) ? tmpJ(ind) : dataJIndex(lInd);  
      } 
    }

    int nbCompressedData = 0;  
    for (ind = 0; ind < dataIIndex.numElements(); ++ind)
    {
       indexI = dataIIndex(ind); indexJ = dataJIndex(ind);
       if ((0 <= indexI) && (0 <= indexJ))
         ++nbCompressedData;
    }        
  
    data_i_index.resize(nbCompressedData);
    data_j_index.resize(nbCompressedData);

    nbCompressedData = 0;  
    for (ind = 0; ind < dataIIndex.numElements(); ++ind)
    {
       indexI = dataIIndex(ind); indexJ = dataJIndex(ind);
       if ((0 <= indexI) && (0 <= indexJ))
       {
          data_i_index(nbCompressedData) = (1 == data_dim) ? ind : ind % ni;
          data_j_index(nbCompressedData) = (1 == data_dim) ? 0   : ind / ni;
         ++nbCompressedData;
       }
    }

    // Reset data_ibegin, data_jbegin
    data_ibegin.setValue(0);
    data_jbegin.setValue(0);
  }

  CTransformation<CDomain>* CDomain::addTransformation(ETranformationType transType, const StdString& id)
  {
    transformationMap_.push_back(std::make_pair(transType, CTransformation<CDomain>::createTransformation(transType,id)));
    return transformationMap_.back().second;
  }

  /*!
    Check whether a domain has transformation
    \return true if domain has transformation
  */
  bool CDomain::hasTransformation()
  {
    return (!transformationMap_.empty());
  }

  /*!
    Set transformation for current domain. It's the method to move transformation in hierarchy
    \param [in] domTrans transformation on domain
  */
  void CDomain::setTransformations(const TransMapTypes& domTrans)
  {
    transformationMap_ = domTrans;
  }

  /*!
    Get all transformation current domain has
    \return all transformation
  */
  CDomain::TransMapTypes CDomain::getAllTransformations(void)
  {
    return transformationMap_;
  }

  void CDomain::duplicateTransformation(CDomain* src)
  {
    if (src->hasTransformation())
    {
      this->setTransformations(src->getAllTransformations());
    }
  }

  /*!
   * Go through the hierarchy to find the domain from which the transformations must be inherited
   */
  void CDomain::solveInheritanceTransformation()
  {
    if (hasTransformation() || !hasDirectDomainReference())
      return;

    CDomain* domain = this;
    std::vector<CDomain*> refDomains;
    while (!domain->hasTransformation() && domain->hasDirectDomainReference())
    {
      refDomains.push_back(domain);
      domain = domain->getDirectDomainReference();
    }

    if (domain->hasTransformation())
      for (size_t i = 0; i < refDomains.size(); ++i)
        refDomains[i]->setTransformations(domain->getAllTransformations());
  }

  void CDomain::setContextClient(CContextClient* contextClient)
  {
    if (clientsSet.find(contextClient)==clientsSet.end())
    {
      clients.push_back(contextClient) ;
      clientsSet.insert(contextClient);
    }
  }

  /*!
    Parse children nodes of a domain in xml file.
    Whenver there is a new transformation, its type and name should be added into this function
    \param node child node to process
  */
  void CDomain::parse(xml::CXMLNode & node)
  {
    SuperClass::parse(node);

    if (node.goToChildElement())
    {
      StdString nodeElementName;
      do
      {
        StdString nodeId("");
        if (node.getAttributes().end() != node.getAttributes().find("id"))
        { nodeId = node.getAttributes()["id"]; }

        nodeElementName = node.getElementName();
        std::map<StdString, ETranformationType>::const_iterator ite = transformationMapList_.end(), it;
        it = transformationMapList_.find(nodeElementName);
        if (ite != it)
        {
          transformationMap_.push_back(std::make_pair(it->second, CTransformation<CDomain>::createTransformation(it->second,
                                                                                                                nodeId,
                                                                                                                &node)));
        }
        else
        {
          ERROR("void CDomain::parse(xml::CXMLNode & node)",
                << "The transformation " << nodeElementName << " has not been supported yet.");
        }
      } while (node.goToNextElement()) ;
      node.goToParentElement();
    }
  }
   //----------------------------------------------------------------

   DEFINE_REF_FUNC(Domain,domain)

   ///---------------------------------------------------------------

} // namespace xios
