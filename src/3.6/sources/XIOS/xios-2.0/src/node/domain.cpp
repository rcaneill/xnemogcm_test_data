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
#include "zoom_domain.hpp"
#include "interpolate_domain.hpp"
#include "generate_rectilinear_domain.hpp"

#include <algorithm>

namespace xios {

   /// ////////////////////// Définitions ////////////////////// ///

   CDomain::CDomain(void)
      : CObjectTemplate<CDomain>(), CDomainAttributes()
      , isChecked(false), relFiles(), isClientChecked(false), nbConnectedClients_(), indSrv_(), connectedServerRank_()
      , hasBounds(false), hasArea(false), isDistributed_(false), isCompressible_(false), isUnstructed_(false)
      , isClientAfterTransformationChecked(false), hasLonLat(false)
      , lonvalue_client(), latvalue_client(), bounds_lon_client(), bounds_lat_client()
      , isRedistributed_(false), hasPole(false)
   {
   }

   CDomain::CDomain(const StdString & id)
      : CObjectTemplate<CDomain>(id), CDomainAttributes()
      , isChecked(false), relFiles(), isClientChecked(false), nbConnectedClients_(), indSrv_(), connectedServerRank_()
      , hasBounds(false), hasArea(false), isDistributed_(false), isCompressible_(false), isUnstructed_(false)
      , isClientAfterTransformationChecked(false), hasLonLat(false)
      , lonvalue_client(), latvalue_client(), bounds_lon_client(), bounds_lat_client()
      , isRedistributed_(false), hasPole(false)
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
   }

   const std::set<StdString> & CDomain::getRelFiles(void) const
   {
      return (this->relFiles);
   }


   const std::vector<int>& CDomain::getIndexesToWrite(void) const
   {
     return indexesToWrite;
   }

   /*!
     Returns the number of indexes written by each server.
     \return the number of indexes written by each server
   */
   int CDomain::getNumberWrittenIndexes() const
   {
     return numberWrittenIndexes_;
   }

   /*!
     Returns the total number of indexes written by the servers.
     \return the total number of indexes written by the servers
   */
   int CDomain::getTotalNumberWrittenIndexes() const
   {
     return totalNumberWrittenIndexes_;
   }

   /*!
     Returns the offset of indexes written by each server.
     \return the offset of indexes written by each server
   */
   int CDomain::getOffsetWrittenIndexes() const
   {
     return offsetWrittenIndexes_;
   }

   //----------------------------------------------------------------

   /*!
    * Compute the minimum buffer size required to send the attributes to the server(s).
    *
    * \return A map associating the server rank with its minimum buffer size.
    */
   std::map<int, StdSize> CDomain::getAttributesBufferSize()
   {
     CContextClient* client = CContext::getCurrent()->client;

     std::map<int, StdSize> attributesSizes = getMinimumBufferSizeForAttributes();

     if (client->isServerLeader())
     {
       // size estimation for sendServerAttribut
       size_t size = 11 * sizeof(size_t);

       const std::list<int>& ranks = client->getRanksServerLeader();
       for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
       {
         if (size > attributesSizes[*itRank])
           attributesSizes[*itRank] = size;
       }
     }

     std::map<int, std::vector<size_t> >::const_iterator itIndexEnd = indSrv_.end();
     std::map<int, std::vector<int> >::const_iterator itWrittenIndexEnd = indWrittenSrv_.end();
     for (size_t k = 0; k < connectedServerRank_.size(); ++k)
     {
       int rank = connectedServerRank_[k];
       std::map<int, std::vector<size_t> >::const_iterator it = indSrv_.find(rank);
       size_t idxCount = (it != itIndexEnd) ? it->second.size() : 0;

       // size estimation for sendIndex (and sendArea which is always smaller or equal)
       size_t sizeIndexEvent = 2 * sizeof(size_t) + 2 * CArray<int,1>::size(idxCount);
       if (isCompressible_)
       {
         std::map<int, std::vector<int> >::const_iterator itWritten = indWrittenSrv_.find(rank);
         size_t writtenIdxCount = (itWritten != itWrittenIndexEnd) ? itWritten->second.size() : 0;
         sizeIndexEvent += CArray<int,1>::size(writtenIdxCount);
       }

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
      return ((this->zoom_ni_srv == 0) ||
              (this->zoom_nj_srv == 0));
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
      return isDistributed_;
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
     Redistribute RECTILINEAR domain with a number of local domains.
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
     CContextClient* client = context->client;
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
     if (!lonvalue_rectilinear_read_from_file.isEmpty())
     {
       lonvalue_1d.resize(ni);
       for (int idx = 0; idx < ni; ++idx)
         lonvalue_1d(idx) = lonvalue_rectilinear_read_from_file(idx+ibegin);
       lon_start.setValue(lonvalue_rectilinear_read_from_file(0));
       lon_end.setValue(lonvalue_rectilinear_read_from_file(ni_glo-1));
     }
     else
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


     if (!latvalue_rectilinear_read_from_file.isEmpty())
     {
       latvalue_1d.resize(nj);
       for (int idx = 0; idx < nj; ++idx)
         latvalue_1d(idx) = latvalue_rectilinear_read_from_file(idx+jbegin);
       lat_start.setValue(latvalue_rectilinear_read_from_file(0));
       lat_end.setValue(latvalue_rectilinear_read_from_file(nj_glo-1));
     }
     else
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
     if (!lonvalue_curvilinear_read_from_file.isEmpty() && lonvalue_2d.isEmpty())
     {
       lonvalue_2d.resize(ni,nj);
       for (int jdx = 0; jdx < nj; ++jdx)
        for (int idx = 0; idx < ni; ++idx)
         lonvalue_2d(idx,jdx) = lonvalue_curvilinear_read_from_file(idx+ibegin, jdx+jbegin);

       lonvalue_curvilinear_read_from_file.free();
     }

     if (!latvalue_curvilinear_read_from_file.isEmpty() && latvalue_2d.isEmpty())
     {
       latvalue_2d.resize(ni,nj);
       for (int jdx = 0; jdx < nj; ++jdx)
        for (int idx = 0; idx < ni; ++idx)
         latvalue_2d(idx,jdx) = latvalue_curvilinear_read_from_file(idx+ibegin, jdx+jbegin);

       latvalue_curvilinear_read_from_file.free();
     }

     if (!bounds_lonvalue_curvilinear_read_from_file.isEmpty() && bounds_lon_2d.isEmpty())
     {
       bounds_lon_2d.resize(nvertex,ni,nj);
       for (int jdx = 0; jdx < nj; ++jdx)
        for (int idx = 0; idx < ni; ++idx)
          for (int ndx = 0; ndx < nvertex; ++ndx)
         bounds_lon_2d(ndx,idx,jdx) = bounds_lonvalue_curvilinear_read_from_file(ndx,idx+ibegin, jdx+jbegin);

       bounds_lonvalue_curvilinear_read_from_file.free();
     }

     if (!bounds_latvalue_curvilinear_read_from_file.isEmpty() && bounds_lat_2d.isEmpty())
     {
       bounds_lat_2d.resize(nvertex,ni,nj);
       for (int jdx = 0; jdx < nj; ++jdx)
        for (int idx = 0; idx < ni; ++idx)
          for (int ndx = 0; ndx < nvertex; ++ndx)
            bounds_lat_2d(ndx,idx,jdx) = bounds_latvalue_curvilinear_read_from_file(ndx,idx+ibegin, jdx+jbegin);

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
          lonvalue_1d(idx) = lonvalue_unstructured_read_from_file(i_index(idx));

        // We dont need these values anymore, so just delete them
        lonvalue_unstructured_read_from_file.free();
     } 

     if (!latvalue_unstructured_read_from_file.isEmpty() && latvalue_1d.isEmpty())
     {
        latvalue_1d.resize(ni);
        for (int idx = 0; idx < ni; ++idx)
          latvalue_1d(idx) =  latvalue_unstructured_read_from_file(i_index(idx));

        // We dont need these values anymore, so just delete them
        latvalue_unstructured_read_from_file.free();
     }

     if (!bounds_lonvalue_unstructured_read_from_file.isEmpty() && bounds_lon_1d.isEmpty())
     {
        int nbVertex = nvertex;
        bounds_lon_1d.resize(nbVertex,ni);
        for (int idx = 0; idx < ni; ++idx)
          for (int jdx = 0; jdx < nbVertex; ++jdx)
            bounds_lon_1d(jdx,idx) = bounds_lonvalue_unstructured_read_from_file(jdx, i_index(idx));

        // We dont need these values anymore, so just delete them
        lonvalue_unstructured_read_from_file.free();
     }

     if (!bounds_latvalue_unstructured_read_from_file.isEmpty() && bounds_lat_1d.isEmpty())
     {
        int nbVertex = nvertex;
        bounds_lat_1d.resize(nbVertex,ni);
        for (int idx = 0; idx < ni; ++idx)
          for (int jdx = 0; jdx < nbVertex; ++jdx)
            bounds_lat_1d(jdx,idx) = bounds_latvalue_unstructured_read_from_file(jdx, i_index(idx));

        // We dont need these values anymore, so just delete them
        lonvalue_unstructured_read_from_file.free();
     }
   }

  /*
    Get global longitude and latitude of rectilinear domain.
  */
   void CDomain::AllgatherRectilinearLonLat(CArray<double,1>& lon, CArray<double,1>& lat, CArray<double,1>& lon_g, CArray<double,1>& lat_g)
   {
	  CContext* context = CContext::getCurrent();
      CContextClient* client = context->client;
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
     
     isDistributed_ = !((!ni.isEmpty() && (ni == ni_glo) && !nj.isEmpty() && (nj == nj_glo)) ||
                        (!i_index.isEmpty() && i_index.numElements() == ni_glo*nj_glo));

     // A stupid condition to make sure that if there is only one client, domain
     // should be considered to be distributed. This should be a temporary solution     
     isDistributed_ |= (1 == CContext::getCurrent()->client->clientSize);
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
        mask_1d.resize(mask_2d.extent(0) * mask_2d.extent(1));
        for (int j = 0; j < nj; ++j)
          for (int i = 0; i < ni; ++i) mask_1d(i+j*ni) = mask_2d(i,j);
        mask_2d.reset();
      }
      else if (mask_1d.isEmpty())
      {
        mask_1d.resize(i_index.numElements());
        for (int i = 0; i < i_index.numElements(); ++i) mask_1d(i) = true;
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
     localMask.resize(ni*nj) ;
     localMask=false ;
     size_t zoom_ibegin=global_zoom_ibegin ;
     size_t zoom_iend=global_zoom_ibegin+global_zoom_ni-1 ;
     size_t zoom_jbegin=global_zoom_jbegin ;
     size_t zoom_jend=global_zoom_jbegin+global_zoom_nj-1 ;


     size_t dn=data_i_index.numElements() ;
     int i,j ;
     size_t k,ind ;

     for(k=0;k<dn;k++)
     {
       if (data_dim==2)
       {
          i=data_i_index(k)+data_ibegin ;
          j=data_j_index(k)+data_jbegin ;
       }
       else
       {
          i=(data_i_index(k)+data_ibegin)%ni ;
          j=(data_i_index(k)+data_ibegin)/ni ;
       }

       if (i>=0 && i<ni && j>=0 && j<nj)
         if (i+ibegin>=zoom_ibegin && i+ibegin<=zoom_iend && j+jbegin>=zoom_jbegin && j+jbegin<=zoom_jend)
         {
           ind=i+ni*j ;
           localMask(ind)=mask_1d(ind) ;
         }
     }
   }







   void CDomain::checkEligibilityForCompressedOutput(void)
   {
     // We don't check if the mask or the indexes are valid here, just if they have been defined at this point.
     isCompressible_ = !mask_1d.isEmpty() || !mask_2d.isEmpty() || !data_i_index.isEmpty();
   }

   //----------------------------------------------------------------

   void CDomain::completeLonLatClient(void)
   {
     if (!lonvalue_2d.isEmpty())
     {
       lonvalue_client.resize(ni * nj);
       latvalue_client.resize(ni * nj);
       if (hasBounds)
       {
         bounds_lon_client.resize(nvertex, ni * nj);
         bounds_lat_client.resize(nvertex, ni * nj);
       }

       for (int j = 0; j < nj; ++j)
       {
         for (int i = 0; i < ni; ++i)
         {
           int k = j * ni + i;

           lonvalue_client(k) = lonvalue_2d(i,j);
           latvalue_client(k) = latvalue_2d(i,j);

           if (hasBounds)
           {
             for (int n = 0; n < nvertex; ++n)
             {
               bounds_lon_client(n,k) = bounds_lon_2d(n,i,j);
               bounds_lat_client(n,k) = bounds_lat_2d(n,i,j);
             }
           }
         }
       }
     }
     else if (!lonvalue_1d.isEmpty())
     {
       if (type_attr::rectilinear == type)
       {
         if (ni == lonvalue_1d.numElements() && nj == latvalue_1d.numElements())
         {
           lonvalue_client.resize(ni * nj);
           latvalue_client.resize(ni * nj);
           if (hasBounds)
           {
             bounds_lon_client.resize(nvertex, ni * nj);
             bounds_lat_client.resize(nvertex, ni * nj);
           }

           for (int j = 0; j < nj; ++j)
           {
             for (int i = 0; i < ni; ++i)
             {
               int k = j * ni + i;

               lonvalue_client(k) = lonvalue_1d(i);
               latvalue_client(k) = latvalue_1d(j);

               if (hasBounds)
               {
                 for (int n = 0; n < nvertex; ++n)
                 {
                   bounds_lon_client(n,k) = bounds_lon_1d(n,i);
                   bounds_lat_client(n,k) = bounds_lat_1d(n,j);
                 }
               }
             }
           }
         }
         else if (i_index.numElements() == lonvalue_1d.numElements() && j_index.numElements() == latvalue_1d.numElements())
         {
           lonvalue_client.reference(lonvalue_1d);
           latvalue_client.reference(latvalue_1d);
            if (hasBounds)
           {
             bounds_lon_client.reference(bounds_lon_1d);
             bounds_lat_client.reference(bounds_lat_1d);
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
       else if (type == type_attr::curvilinear || type == type_attr::unstructured)
       {
         lonvalue_client.reference(lonvalue_1d);
         latvalue_client.reference(latvalue_1d);
         if (hasBounds)
         {
           bounds_lon_client.reference(bounds_lon_1d);
           bounds_lat_client.reference(bounds_lat_1d);
         }
       }
     }
   }

   void CDomain::checkBounds(void)
   {
     if (!nvertex.isEmpty() && nvertex > 0)
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
               << "'bounds_lon_1d' dimension is " << bounds_lon_1d.extent(1)
               << " but nvertex is " << nvertex.getValue() << ".");

       if (!bounds_lon_2d.isEmpty() && nvertex.getValue() != bounds_lon_2d.extent(0))
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "'bounds_lon_2d' dimension is not compatible with 'nvertex'." << std::endl
               << "'bounds_lon_2d' dimension is " << bounds_lon_2d.extent(2)
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
               << "'bounds_lat_1d' dimension is " << bounds_lat_1d.extent(1)
               << " but nvertex is " << nvertex.getValue() << ".");

       if (!bounds_lat_2d.isEmpty() && nvertex.getValue() != bounds_lat_2d.extent(0))
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "'bounds_lat_2d' dimension is not compatible with 'nvertex'." << std::endl
               << "'bounds_lat_2d' dimension is " << bounds_lat_2d.extent(2)
               << " but nvertex is " << nvertex.getValue() << ".");

       if (!bounds_lat_1d.isEmpty() && latvalue_1d.isEmpty())
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Since 'bounds_lat_1d' is defined, 'latvalue_1d' must be defined too." << std::endl);

       if (!bounds_lat_2d.isEmpty() && latvalue_2d.isEmpty())
         ERROR("CDomain::checkBounds(void)",
               << "Since 'bounds_lat_2d' is defined, 'latvalue_2d' must be defined too." << std::endl);

       //hasBounds = true;
       // In case of reading UGRID bounds values are not required
       hasBounds = (!bounds_lat_1d.isEmpty() || !bounds_lat_2d.isEmpty() );
     }
     else
     {
       hasBounds = false;
       nvertex = 0;
     }
   }

   void CDomain::checkArea(void)
   {
     hasArea = !area.isEmpty();
     if (hasArea)
     {
       if (area.extent(0) != ni || area.extent(1) != nj)
       {
         ERROR("CDomain::checkArea(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "The area does not have the same size as the local domain." << std::endl
               << "Local size is " << ni.getValue() << " x " << nj.getValue() << "." << std::endl
               << "Area size is " << area.extent(0) << " x " << area.extent(1) << ".");
       }
     }
   }

   void CDomain::checkLonLat()
   {
     hasLonLat = (!latvalue_1d.isEmpty() && !lonvalue_1d.isEmpty()) ||
                 (!latvalue_2d.isEmpty() && !lonvalue_2d.isEmpty());
     if (hasLonLat)
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
       this->checkMask();
       if (hasLonLat || hasArea || isCompressible_) this->computeConnectedServer();
       if (hasLonLat) this->completeLonLatClient();
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

      this->checkDomain();
      this->checkBounds();
      this->checkArea();
      this->checkLonLat();

      if (context->hasClient)
      { // Côté client uniquement
         this->checkMask();
         this->checkDomainData();
         this->checkCompression();
         this->computeLocalMask() ;
      }
      else
      { // Côté serveur uniquement
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
       sendServerAttribut();
       if (hasLonLat || hasArea || isCompressible_) sendLonLatArea();
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
      { // Côté client uniquement
         this->checkMask();
         this->checkDomainData();
         this->checkCompression();
         this->computeLocalMask() ;

      }
      else
      { // Côté serveur uniquement
      }

      if (context->hasClient)
      {
        this->computeConnectedServer();
        this->completeLonLatClient();
        this->sendServerAttribut();
        this->sendLonLatArea();
      }

      this->isChecked = true;
   }

  void CDomain::sendServerAttribut(void)
  {
    CContext* context = CContext::getCurrent();
    CContextClient* client = context->client;
    int nbServer = client->serverSize;

    CServerDistributionDescription serverDescription(getNbGlob(), nbServer);
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
        const int iend_srv = ibegin_srv + ni_srv - 1;
        const int jend_srv = jbegin_srv + nj_srv - 1;

        msgs.push_back(CMessage());
        CMessage& msg = msgs.back();
        msg << this->getId() ;
        msg << ni_srv << ibegin_srv << iend_srv << nj_srv << jbegin_srv << jend_srv;
        msg << global_zoom_ni.getValue() << global_zoom_ibegin.getValue() << global_zoom_nj.getValue() << global_zoom_jbegin.getValue();
        msg << isCompressible_;

        event.push(*itRank,1,msg);
      }
      client->sendEvent(event);
    }
    else client->sendEvent(event);
  }

  std::vector<int> CDomain::getNbGlob()
  {
     std::vector<int> nbGlob(2);
     nbGlob[0] = ni_glo.getValue();
     nbGlob[1] = nj_glo.getValue();

     return nbGlob;
  }

  void CDomain::computeConnectedServer(void)
  {
    CContext* context=CContext::getCurrent() ;
    CContextClient* client=context->client ;
    int nbServer=client->serverSize;
    int rank = client->clientRank;
    bool doComputeGlobalIndexServer = true;

    int i,j,i_ind,j_ind, nbIndex;
    int global_zoom_iend=global_zoom_ibegin+global_zoom_ni-1 ;
    int global_zoom_jend=global_zoom_jbegin+global_zoom_nj-1 ;

    // Precompute number of index
    int globalIndexCountZoom = 0;
    nbIndex = i_index.numElements();
    for (i = 0; i < nbIndex; ++i)
    {
      i_ind=i_index(i);
      j_ind=j_index(i);

      if (i_ind >= global_zoom_ibegin && i_ind <= global_zoom_iend && j_ind >= global_zoom_jbegin && j_ind <= global_zoom_jend)
      {
        ++globalIndexCountZoom;
      }
    }

    int globalIndexWrittenCount = 0;
    if (isCompressible_)
    {
      for (i = 0; i < data_i_index.numElements(); ++i)
      {
        i_ind = CDistributionClient::getDomainIndex(data_i_index(i), data_j_index(i),
                                                    data_ibegin, data_jbegin, data_dim, ni,
                                                    j_ind);
        if (i_ind >= 0 && i_ind < ni && j_ind >= 0 && j_ind < nj && mask_1d(i_ind + j_ind * ni))
        {
          i_ind += ibegin;
          j_ind += jbegin;
          if (i_ind >= global_zoom_ibegin && i_ind <= global_zoom_iend && j_ind >= global_zoom_jbegin && j_ind <= global_zoom_jend)
            ++globalIndexWrittenCount;
        }
      }
    }

    // Fill in index
    CArray<size_t,1> globalIndexDomainZoom(globalIndexCountZoom);
    CArray<size_t,1> localIndexDomainZoom(globalIndexCountZoom);
    CArray<size_t,1> globalIndexDomain(nbIndex);
    size_t globalIndex;
    int globalIndexCount = 0;
    globalIndexCountZoom = 0;

    for (i = 0; i < nbIndex; ++i)
    {
      i_ind=i_index(i);
      j_ind=j_index(i);
      globalIndex = i_ind + j_ind * ni_glo;
      globalIndexDomain(globalIndexCount) = globalIndex;
      ++globalIndexCount;
      if (i_ind >= global_zoom_ibegin && i_ind <= global_zoom_iend && j_ind >= global_zoom_jbegin && j_ind <= global_zoom_jend)
      {
        globalIndexDomainZoom(globalIndexCountZoom) = globalIndex;
        localIndexDomainZoom(globalIndexCountZoom) = i;
        ++globalIndexCountZoom;
      }
    }

    CArray<int,1> globalIndexWrittenDomain(globalIndexWrittenCount);
    if (isCompressible_)
    {
      globalIndexWrittenCount = 0;
      for (i = 0; i < data_i_index.numElements(); ++i)
      {
        i_ind = CDistributionClient::getDomainIndex(data_i_index(i), data_j_index(i),
                                                    data_ibegin, data_jbegin, data_dim, ni,
                                                    j_ind);
        if (i_ind >= 0 && i_ind < ni && j_ind >= 0 && j_ind < nj && mask_1d(i_ind + j_ind * ni))
        {
          i_ind += ibegin;
          j_ind += jbegin;
          if (i_ind >= global_zoom_ibegin && i_ind <= global_zoom_iend && j_ind >= global_zoom_jbegin && j_ind <= global_zoom_jend)
          {
            globalIndexWrittenDomain(globalIndexWrittenCount) = i_ind + j_ind * ni_glo;
            ++globalIndexWrittenCount;
          }
        }
      }
    }

    size_t globalSizeIndex = 1, indexBegin, indexEnd;
    int range, clientSize = client->clientSize;
    for (int i = 0; i < getNbGlob().size(); ++i) globalSizeIndex *= getNbGlob()[i];
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

    CServerDistributionDescription serverDescription(getNbGlob(), nbServer);
    if (isUnstructed_) serverDescription.computeServerGlobalIndexInRange(std::make_pair<size_t,size_t>(indexBegin, indexEnd), 0);
    else serverDescription.computeServerGlobalIndexInRange(std::make_pair<size_t,size_t>(indexBegin, indexEnd), 1);

    CClientServerMapping* clientServerMap = new CClientServerMappingDistributed(serverDescription.getGlobalIndexRange(),
                                                                                client->intraComm);
    clientServerMap->computeServerIndexMapping(globalIndexDomain);
    const CClientServerMapping::GlobalIndexMap& globalIndexDomainOnServer = clientServerMap->getGlobalIndexOnServer();

    CClientServerMapping::GlobalIndexMap::const_iterator it  = globalIndexDomainOnServer.begin(),
                                                         ite = globalIndexDomainOnServer.end();
    typedef XIOSBinarySearchWithIndex<size_t> BinarySearch;
    std::vector<int>::iterator itVec;

    indSrv_.clear();
    indWrittenSrv_.clear();
    for (; it != ite; ++it)
    {
      int rank = it->first;
      int indexSize = it->second.size();
      std::vector<int> permutIndex(indexSize);
      XIOSAlgorithms::fillInIndex(indexSize, permutIndex);
      XIOSAlgorithms::sortWithIndex<size_t, CVectorStorage>(it->second, permutIndex);
      BinarySearch binSearch(it->second);
      int nb = globalIndexDomainZoom.numElements();
      for (int i = 0; i < nb; ++i)
      {
        if (binSearch.search(permutIndex.begin(), permutIndex.end(), globalIndexDomainZoom(i), itVec))
        {
          indSrv_[rank].push_back(localIndexDomainZoom(i));
        }
      }
      for (int i = 0; i < globalIndexWrittenDomain.numElements(); ++i)
      {
        if (binSearch.search(permutIndex.begin(), permutIndex.end(), globalIndexWrittenDomain(i), itVec))
        {
          indWrittenSrv_[rank].push_back(globalIndexWrittenDomain(i));
        }
      }
    }

    connectedServerRank_.clear();
    for (it = globalIndexDomainOnServer.begin(); it != ite; ++it) {
      connectedServerRank_.push_back(it->first);
    }

    nbConnectedClients_ = clientServerMap->computeConnectedClients(client->serverSize, client->clientSize, client->intraComm, connectedServerRank_);

    delete clientServerMap;
  }

  const std::map<int, vector<size_t> >& CDomain::getIndexServer() const
  {
    return indSrv_;
  }

  /*!
    Send index from client to server(s)
  */
  void CDomain::sendIndex()
  {
    int ns, n, i, j, ind, nv, idx;
    CContext* context = CContext::getCurrent();
    CContextClient* client=context->client;

    CEventClient eventIndex(getType(), EVENT_ID_INDEX);

    list<CMessage> list_msgsIndex;
    list<CArray<int,1> > list_indi, list_indj, list_writtenInd;

    std::map<int, std::vector<size_t> >::const_iterator it, iteMap;
    iteMap = indSrv_.end();
    for (int k = 0; k < connectedServerRank_.size(); ++k)
    {
      int nbData = 0;
      int rank = connectedServerRank_[k];
      it = indSrv_.find(rank);
      if (iteMap != it)
        nbData = it->second.size();

      list_indi.push_back(CArray<int,1>(nbData));
      list_indj.push_back(CArray<int,1>(nbData));

      CArray<int,1>& indi = list_indi.back();
      CArray<int,1>& indj = list_indj.back();
      const std::vector<size_t>& temp = it->second;
      for (n = 0; n < nbData; ++n)
      {
        idx = static_cast<int>(it->second[n]);
        indi(n) = i_index(idx);
        indj(n) = j_index(idx);
      }

      list_msgsIndex.push_back(CMessage());

      list_msgsIndex.back() << this->getId() << (int)type; // enum ne fonctionne pour les message => ToFix
      list_msgsIndex.back() << isCurvilinear;
      list_msgsIndex.back() << list_indi.back() << list_indj.back();

      if (isCompressible_)
      {
        std::vector<int>& writtenIndSrc = indWrittenSrv_[rank];
        list_writtenInd.push_back(CArray<int,1>(writtenIndSrc.size()));
        CArray<int,1>& writtenInd = list_writtenInd.back();

        for (n = 0; n < writtenInd.numElements(); ++n)
          writtenInd(n) = writtenIndSrc[n];

        list_msgsIndex.back() << writtenInd;
      }

      eventIndex.push(rank, nbConnectedClients_[rank], list_msgsIndex.back());
    }

    client->sendEvent(eventIndex);
  }

  /*!
    Send area from client to server(s)
  */
  void CDomain::sendArea()
  {
    if (!hasArea) return;

    int ns, n, i, j, ind, nv, idx;
    CContext* context = CContext::getCurrent();
    CContextClient* client=context->client;

    // send area for each connected server
    CEventClient eventArea(getType(), EVENT_ID_AREA);

    list<CMessage> list_msgsArea;
    list<CArray<double,1> > list_area;

    std::map<int, std::vector<size_t> >::const_iterator it, iteMap;
    iteMap = indSrv_.end();
    for (int k = 0; k < connectedServerRank_.size(); ++k)
    {
      int nbData = 0;
      int rank = connectedServerRank_[k];
      it = indSrv_.find(rank);
      if (iteMap != it)
        nbData = it->second.size();
      list_area.push_back(CArray<double,1>(nbData));

      const std::vector<size_t>& temp = it->second;
      for (n = 0; n < nbData; ++n)
      {
        idx = static_cast<int>(it->second[n]);
        i = i_index(idx);
        j = j_index(idx);
        if (hasArea)
          list_area.back()(n) = area(i - ibegin, j - jbegin);
      }

      list_msgsArea.push_back(CMessage());
      list_msgsArea.back() << this->getId() << list_area.back();
      eventArea.push(rank, nbConnectedClients_[rank], list_msgsArea.back());
    }
    client->sendEvent(eventArea);
  }

  /*!
    Send longitude and latitude from client to servers
    Each client send long and lat information to corresponding connected server(s).
    Because longitude and latitude are optional, this function only called if latitude and longitude exist
  */
  void CDomain::sendLonLat()
  {
    if (!hasLonLat) return;

    int ns, n, i, j, ind, nv, idx;
    CContext* context = CContext::getCurrent();
    CContextClient* client=context->client;

    // send lon lat for each connected server
    CEventClient eventLon(getType(), EVENT_ID_LON);
    CEventClient eventLat(getType(), EVENT_ID_LAT);

    list<CMessage> list_msgsLon, list_msgsLat;
    list<CArray<double,1> > list_lon, list_lat;
    list<CArray<double,2> > list_boundslon, list_boundslat;

    std::map<int, std::vector<size_t> >::const_iterator it, iteMap;
    iteMap = indSrv_.end();
    for (int k = 0; k < connectedServerRank_.size(); ++k)
    {
      int nbData = 0;
      int rank = connectedServerRank_[k];
      it = indSrv_.find(rank);
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
        lon(n) = lonvalue_client(idx);
        lat(n) = latvalue_client(idx);

        if (hasBounds)
        {
          CArray<double,2>& boundslon = list_boundslon.back();
          CArray<double,2>& boundslat = list_boundslat.back();

          for (nv = 0; nv < nvertex; ++nv)
          {
            boundslon(nv, n) = bounds_lon_client(nv, idx);
            boundslat(nv, n) = bounds_lat_client(nv, idx);
          }
        }
      }

      list_msgsLon.push_back(CMessage());
      list_msgsLat.push_back(CMessage());

      list_msgsLon.back() << this->getId() << list_lon.back();
      list_msgsLat.back() << this->getId() << list_lat.back();

      if (hasBounds)
      {
        list_msgsLon.back() << list_boundslon.back();
        list_msgsLat.back() << list_boundslat.back();
      }

      eventLon.push(rank, nbConnectedClients_[rank], list_msgsLon.back());
      eventLat.push(rank, nbConnectedClients_[rank], list_msgsLat.back());
    }

    client->sendEvent(eventLon);
    client->sendEvent(eventLat);
  }

  /*!
    Send some optional information to server(s)
    In the future, this function can be extended with more optional information to send
  */
  void CDomain::sendLonLatArea(void)
  {
    sendIndex();
    sendLonLat();
    sendArea();
  }

  bool CDomain::dispatchEvent(CEventServer& event)
  {
    if (SuperClass::dispatchEvent(event)) return true;
    else
    {
      switch(event.type)
      {
        case EVENT_ID_SERVER_ATTRIBUT:
          recvServerAttribut(event);
          return true;
          break;
        case EVENT_ID_INDEX:
          recvIndex(event);
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
        default:
          ERROR("bool CDomain::dispatchEvent(CEventServer& event)",
                << "Unknown Event");
          return false;
       }
    }
  }

  /*!
    Receive attributes event from clients(s)
    \param[in] event event contain info about rank and associated attributes
  */
  void CDomain::recvServerAttribut(CEventServer& event)
  {
    CBufferIn* buffer=event.subEvents.begin()->buffer;
    string domainId ;
    *buffer>>domainId ;
    get(domainId)->recvServerAttribut(*buffer) ;
  }

  /*!
    Receive attributes from client(s): zoom info and begin and n of each server
    \param[in] rank rank of client source
    \param[in] buffer message containing attributes info
  */
  void CDomain::recvServerAttribut(CBufferIn& buffer)
  {
    int global_zoom_ni_tmp, global_zoom_ibegin_tmp, global_zoom_nj_tmp, global_zoom_jbegin_tmp;
    buffer >> ni_srv >> ibegin_srv >> iend_srv >> nj_srv >> jbegin_srv >> jend_srv
           >> global_zoom_ni_tmp >> global_zoom_ibegin_tmp >> global_zoom_nj_tmp >> global_zoom_jbegin_tmp
           >> isCompressible_;

    global_zoom_ni.setValue(global_zoom_ni_tmp);
    global_zoom_ibegin.setValue(global_zoom_ibegin_tmp);
    global_zoom_nj.setValue(global_zoom_nj_tmp);
    global_zoom_jbegin.setValue(global_zoom_jbegin_tmp);

    int zoom_iend = global_zoom_ibegin + global_zoom_ni - 1;
    int zoom_jend = global_zoom_jbegin + global_zoom_nj - 1;

    zoom_ibegin_srv = global_zoom_ibegin > ibegin_srv ? global_zoom_ibegin : ibegin_srv ;
    zoom_iend_srv = zoom_iend < iend_srv ? zoom_iend : iend_srv ;
    zoom_ni_srv=zoom_iend_srv-zoom_ibegin_srv+1 ;

    zoom_jbegin_srv = global_zoom_jbegin > jbegin_srv ? global_zoom_jbegin : jbegin_srv ;
    zoom_jend_srv = zoom_jend < jend_srv ? zoom_jend : jend_srv ;
    zoom_nj_srv=zoom_jend_srv-zoom_jbegin_srv+1 ;

    if (zoom_ni_srv<=0 || zoom_nj_srv<=0)
    {
      zoom_ibegin_srv=0 ; zoom_iend_srv=0 ; zoom_ni_srv=0 ;
      zoom_jbegin_srv=0 ; zoom_jend_srv=0 ; zoom_nj_srv=0 ;
    }
    lonvalue_srv.resize(zoom_ni_srv*zoom_nj_srv) ;
    lonvalue_srv = 0. ;
    latvalue_srv.resize(zoom_ni_srv*zoom_nj_srv) ;
    latvalue_srv = 0. ;
    if (hasBounds)
    {
      bounds_lon_srv.resize(nvertex,zoom_ni_srv*zoom_nj_srv) ;
      bounds_lon_srv = 0. ;
      bounds_lat_srv.resize(nvertex,zoom_ni_srv*zoom_nj_srv) ;
      bounds_lat_srv = 0. ;
    }

    if (hasArea)
    {
      area_srv.resize(zoom_ni_srv * zoom_nj_srv);
      area_srv = 0.;
    }

  }

  /*!
    Receive index event from clients(s)
    \param[in] event event contain info about rank and associated index
  */
  void CDomain::recvIndex(CEventServer& event)
  {
    CDomain* domain;

    list<CEventServer::SSubEvent>::iterator it;
    for (it = event.subEvents.begin(); it != event.subEvents.end(); ++it)
    {
      CBufferIn* buffer = it->buffer;
      string domainId;
      *buffer >> domainId;
      domain = get(domainId);
      domain->recvIndex(it->rank, *buffer);
    }

    if (domain->isCompressible_)
    {
      std::sort(domain->indexesToWrite.begin(), domain->indexesToWrite.end());

      CContextServer* server = CContext::getCurrent()->server;
      domain->numberWrittenIndexes_ = domain->indexesToWrite.size();
      MPI_Allreduce(&domain->numberWrittenIndexes_, &domain->totalNumberWrittenIndexes_, 1, MPI_INT, MPI_SUM, server->intraComm);
      MPI_Scan(&domain->numberWrittenIndexes_, &domain->offsetWrittenIndexes_, 1, MPI_INT, MPI_SUM, server->intraComm);
      domain->offsetWrittenIndexes_ -= domain->numberWrittenIndexes_;
    }
  }

  /*!
    Receive index information from client(s)
    \param[in] rank rank of client source
    \param[in] buffer message containing index info
  */
  void CDomain::recvIndex(int rank, CBufferIn& buffer)
  {
    int type_int;
    buffer >> type_int >> isCurvilinear >> indiSrv[rank] >> indjSrv[rank];
    type.setValue((type_attr::t_enum)type_int); // probleme des type enum avec les buffers : ToFix

    if (isCompressible_)
    {
      CArray<int, 1> writtenIndexes;
      buffer >> writtenIndexes;
      indexesToWrite.reserve(indexesToWrite.size() + writtenIndexes.numElements());
      for (int i = 0; i < writtenIndexes.numElements(); ++i)
        indexesToWrite.push_back(writtenIndexes(i));
    }
  }

  /*!
    Receive longitude event from clients(s)
    \param[in] event event contain info about rank and associated longitude
  */
  void CDomain::recvLon(CEventServer& event)
  {
    list<CEventServer::SSubEvent>::iterator it;
    for (it = event.subEvents.begin(); it != event.subEvents.end(); ++it)
    {
      CBufferIn* buffer = it->buffer;
      string domainId;
      *buffer >> domainId;
      get(domainId)->recvLon(it->rank, *buffer);
    }
  }

  /*!
    Receive longitude information from client(s)
    \param[in] rank rank of client source
    \param[in] buffer message containing longitude info
  */
  void CDomain::recvLon(int rank, CBufferIn& buffer)
  {
    CArray<int,1> &indi = indiSrv[rank], &indj = indjSrv[rank];
    CArray<double,1> lon;
    CArray<double,2> boundslon;

    buffer >> lon;

    if (hasBounds) buffer >> boundslon;

    int i, j, ind_srv;
    for (int ind = 0; ind < indi.numElements(); ind++)
    {
      i = indi(ind); j = indj(ind);
      ind_srv = (i - zoom_ibegin_srv) + (j - zoom_jbegin_srv) * zoom_ni_srv;
      lonvalue_srv(ind_srv) = lon(ind);
      if (hasBounds)
      {
        for (int nv = 0; nv < nvertex; ++nv)
          bounds_lon_srv(nv, ind_srv) = boundslon(nv, ind);
      }
    }
  }

  /*!
    Receive latitude event from clients(s)
    \param[in] event event contain info about rank and associated latitude
  */
  void CDomain::recvLat(CEventServer& event)
  {
    list<CEventServer::SSubEvent>::iterator it;
    for (it = event.subEvents.begin(); it != event.subEvents.end(); ++it)
    {
      CBufferIn* buffer = it->buffer;
      string domainId;
      *buffer >> domainId;
      get(domainId)->recvLat(it->rank, *buffer);
    }
  }

  /*!
    Receive latitude information from client(s)
    \param[in] rank rank of client source
    \param[in] buffer message containing latitude info
  */
  void CDomain::recvLat(int rank, CBufferIn& buffer)
  {
    CArray<int,1> &indi = indiSrv[rank], &indj = indjSrv[rank];
    CArray<double,1> lat;
    CArray<double,2> boundslat;

    buffer >> lat;
    if (hasBounds) buffer >> boundslat;

    int i, j, ind_srv;
    for (int ind = 0; ind < indi.numElements(); ind++)
    {
      i = indi(ind); j = indj(ind);
      ind_srv = (i - zoom_ibegin_srv) + (j - zoom_jbegin_srv) * zoom_ni_srv;
      latvalue_srv(ind_srv) = lat(ind);
      if (hasBounds)
      {
        for (int nv = 0; nv < nvertex; nv++)
          bounds_lat_srv(nv, ind_srv) = boundslat(nv, ind);
      }
    }
  }

  /*!
    Receive area event from clients(s)
    \param[in] event event contain info about rank and associated area
  */
  void CDomain::recvArea(CEventServer& event)
  {
    list<CEventServer::SSubEvent>::iterator it;
    for (it = event.subEvents.begin(); it != event.subEvents.end(); ++it)
    {
      CBufferIn* buffer = it->buffer;
      string domainId;
      *buffer >> domainId;
      get(domainId)->recvArea(it->rank, *buffer);
    }
  }

  /*!
    Receive area information from client(s)
    \param[in] rank rank of client source
    \param[in] buffer message containing area info
  */
  void CDomain::recvArea(int rank, CBufferIn& buffer)
  {
    CArray<int,1> &indi = indiSrv[rank], &indj = indjSrv[rank];
    CArray<double,1> clientArea;

    buffer >> clientArea;

    int i, j, ind_srv;
    for (int ind = 0; ind < indi.numElements(); ind++)
    {
      i = indi(ind); j = indj(ind);
      ind_srv = (i - zoom_ibegin_srv) + (j - zoom_jbegin_srv) * zoom_ni_srv;
      area_srv(ind_srv) = clientArea(ind);
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
