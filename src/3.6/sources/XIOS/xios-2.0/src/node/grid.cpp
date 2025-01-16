
#include "grid.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "message.hpp"
#include <iostream>
#include "xios_spl.hpp"
#include "type.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "context_server.hpp"
#include "array_new.hpp"
#include "server_distribution_description.hpp"
#include "client_server_mapping_distributed.hpp"
#include "distribution_client.hpp"
#include "grid_transformation.hpp"
#include "grid_generate.hpp"

namespace xios {

   /// ////////////////////// Dfinitions ////////////////////// ///

   CGrid::CGrid(void)
      : CObjectTemplate<CGrid>(), CGridAttributes()
      , isChecked(false), isDomainAxisChecked(false)
      , vDomainGroup_(), domList_(), isDomListSet(false)
      , vAxisGroup_(), axisList_(), isAxisListSet(false)
      , vScalarGroup_(), scalarList_(), isScalarListSet(false)
      , clientDistribution_(0), isIndexSent(false) , serverDistribution_(0), clientServerMap_(0)
      , writtenDataSize_(0), numberWrittenIndexes_(0), totalNumberWrittenIndexes_(0), offsetWrittenIndexes_(0)
      , connectedDataSize_(), connectedServerRank_(), isDataDistributed_(true), isCompressible_(false)
      , transformations_(0), isTransformed_(false)
      , axisPositionInGrid_(), hasDomainAxisBaseRef_(false)
      , gridSrc_(), hasTransform_(false), isGenerated_(false), order_(), globalIndexOnServer_()
   {
     setVirtualDomainGroup(CDomainGroup::create(getId() + "_virtual_domain_group"));
     setVirtualAxisGroup(CAxisGroup::create(getId() + "_virtual_axis_group"));
     setVirtualScalarGroup(CScalarGroup::create(getId() + "_virtual_scalar_group"));
   }

   CGrid::CGrid(const StdString& id)
      : CObjectTemplate<CGrid>(id), CGridAttributes()
      , isChecked(false), isDomainAxisChecked(false)
      , vDomainGroup_(), domList_(), isDomListSet(false)
      , vAxisGroup_(), axisList_(), isAxisListSet(false)
      , vScalarGroup_(), scalarList_(), isScalarListSet(false)
      , clientDistribution_(0), isIndexSent(false) , serverDistribution_(0), clientServerMap_(0)
      , writtenDataSize_(0), numberWrittenIndexes_(0), totalNumberWrittenIndexes_(0), offsetWrittenIndexes_(0)
      , connectedDataSize_(), connectedServerRank_(), isDataDistributed_(true), isCompressible_(false)
      , transformations_(0), isTransformed_(false)
      , axisPositionInGrid_(), hasDomainAxisBaseRef_(false)
      , gridSrc_(), hasTransform_(false), isGenerated_(false), order_(), globalIndexOnServer_()
   {
     setVirtualDomainGroup(CDomainGroup::create(getId() + "_virtual_domain_group"));
     setVirtualAxisGroup(CAxisGroup::create(getId() + "_virtual_axis_group"));
     setVirtualScalarGroup(CScalarGroup::create(getId() + "_virtual_scalar_group"));
   }

   CGrid::~CGrid(void)
   {
    if (0 != clientDistribution_) delete clientDistribution_;
    if (0 != serverDistribution_) delete serverDistribution_;
    if (0 != clientServerMap_) delete clientServerMap_;
    if (0 != transformations_) delete transformations_;
   }

   ///---------------------------------------------------------------

   StdString CGrid::GetName(void)    { return StdString("grid"); }
   StdString CGrid::GetDefName(void) { return CGrid::GetName(); }
   ENodeType CGrid::GetType(void)    { return eGrid; }


   StdSize CGrid::getDimension(void)
   {
      return getGlobalDimension().size();
   }

   //---------------------------------------------------------------

   StdSize CGrid::getDataSize(void) const
   {
     StdSize retvalue = 1;
     if (!isScalarGrid())
     {
       std::vector<int> dataNindex = clientDistribution_->getDataNIndex();
       for (int i = 0; i < dataNindex.size(); ++i) retvalue *= dataNindex[i];
     }
     return retvalue;
   }

   /*!
    * Compute the minimum buffer size required to send the attributes to the server(s).
    *
    * \return A map associating the server rank with its minimum buffer size.
    */
   std::map<int, StdSize> CGrid::getAttributesBufferSize()
   {
     std::map<int, StdSize> attributesSizes = getMinimumBufferSizeForAttributes();

     // The grid indexes require a similar size as the actual data
     std::map<int, StdSize> dataSizes = getDataBufferSize();
     std::map<int, StdSize>::iterator it, itE = dataSizes.end();
     for (it = dataSizes.begin(); it != itE; ++it)
     {
       it->second += 2 * sizeof(bool);
       if (it->second > attributesSizes[it->first])
         attributesSizes[it->first] = it->second;
     }

     // Account for the axis attributes
     std::vector<CAxis*> axisList = getAxis();
     for (size_t i = 0; i < axisList.size(); ++i)
     {
       std::map<int, StdSize> axisAttBuffSize = axisList[i]->getAttributesBufferSize();
       for (it = axisAttBuffSize.begin(), itE = axisAttBuffSize.end(); it != itE; ++it)
       {
         if (it->second > attributesSizes[it->first])
           attributesSizes[it->first] = it->second;
       }
     }

     // Account for the domain attributes
     std::vector<CDomain*> domList = getDomains();
     for (size_t i = 0; i < domList.size(); ++i)
     {
       std::map<int, StdSize> domAttBuffSize = domList[i]->getAttributesBufferSize();
       for (it = domAttBuffSize.begin(), itE = domAttBuffSize.end(); it != itE; ++it)
       {
         if (it->second > attributesSizes[it->first])
           attributesSizes[it->first] = it->second;
       }
     }

     return attributesSizes;
   }

   /*!
    * Compute the minimum buffer size required to send the data to the server(s).
    *
    * \param id the id used to tag the data
    * \return A map associating the server rank with its minimum buffer size.
    */
   std::map<int, StdSize> CGrid::getDataBufferSize(const std::string& id /*= ""*/)
   {
     std::map<int, StdSize> dataSizes;
     // The record index is sometimes sent along with the data but we always
     // include it in the size calculation for the sake of simplicity
     const size_t extraSize = CEventClient::headerSize + (id.empty() ? getId() : id).size() + 2 * sizeof(size_t);

     std::map<int, size_t>::const_iterator itEnd = connectedDataSize_.end();
     for (size_t k = 0; k < connectedServerRank_.size(); ++k)
     {
       int rank = connectedServerRank_[k];
       std::map<int, size_t>::const_iterator it = connectedDataSize_.find(rank);
       size_t count = (it != itEnd) ? it->second : 0;

       dataSizes.insert(std::make_pair(rank, extraSize + CArray<double,1>::size(count)));
     }

     return dataSizes;
   }

   void CGrid::checkAttributesAfterTransformation()
   {
      setAxisList();
      std::vector<CAxis*> axisListP = this->getAxis();
      if (!axisListP.empty())
      {
        int idx = 0;
        axisPositionInGrid_.resize(0);
        for (int i = 0; i < axis_domain_order.numElements(); ++i)
        {
          int elementDimension = axis_domain_order(i);
          if (1 == elementDimension)
          {
            axisPositionInGrid_.push_back(idx);
            ++idx;
          }
          else if (2 == elementDimension) idx += 2;
        }

        for (int i = 0; i < axisListP.size(); ++i)
        {
          axisListP[i]->checkAttributesOnClientAfterTransformation(getGlobalDimension(),axisPositionInGrid_[i]);
        }
      }

      setDomainList();
      std::vector<CDomain*> domListP = this->getDomains();
      if (!domListP.empty())
      {
        for (int i = 0; i < domListP.size(); ++i)
        {
          domListP[i]->checkAttributesOnClientAfterTransformation();
        }
      }
   }

   //---------------------------------------------------------------

   /*!
    * Test whether the data defined on the grid can be outputted in a compressed way.
    *
    * \return true if and only if a mask was defined for this grid
    */
   bool CGrid::isCompressible(void) const
   {
      return isCompressible_;
   }

   //---------------------------------------------------------------

   void CGrid::addRelFileCompressed(const StdString& filename)
   {
      this->relFilesCompressed.insert(filename);
   }

   bool CGrid::isWrittenCompressed(const StdString& filename) const
   {
      return (this->relFilesCompressed.find(filename) != this->relFilesCompressed.end());
   }

   //---------------------------------------------------------------

   void CGrid::solveDomainAxisRef(bool areAttributesChecked)
   {
     if (this->isDomainAxisChecked) return;

     this->solveScalarRef(areAttributesChecked);
     this->solveAxisRef(areAttributesChecked);
     this->solveDomainRef(areAttributesChecked);     
     this->isDomainAxisChecked = areAttributesChecked;
   }

   void CGrid::solveDomainAxisBaseRef()
   {
     if (this->hasDomainAxisBaseRef_) return;
     // Account for the scalar attributes
     std::vector<CScalar*> scalarList = getScalars();
     for (size_t i = 0; i < scalarList.size(); ++i)
     {
       scalarList[i]->setAttributesReference();
     }

     // Account for the axis attributes
     std::vector<CAxis*> axisList = getAxis();
     for (size_t i = 0; i < axisList.size(); ++i)
     {
       axisList[i]->setAttributesReference();
     }

     // Account for the domain attributes
     std::vector<CDomain*> domList = getDomains();
     for (size_t i = 0; i < domList.size(); ++i)
     {
       domList[i]->setAttributesReference();
     }

     this->hasDomainAxisBaseRef_ = true;
   }

   void CGrid::checkEligibilityForCompressedOutput()
   {
     // We don't check if the mask is valid here, just if a mask has been defined at this point.
     isCompressible_ = !mask_1d.isEmpty() || !mask_2d.isEmpty() || !mask_3d.isEmpty();
   }

   void CGrid::checkMaskIndex(bool doSendingIndex)
   {
     CContext* context = CContext::getCurrent();
     CContextClient* client=context->client;

     if (isScalarGrid())
     {
       if (context->hasClient)
          if (this->isChecked && doSendingIndex && !isIndexSent) { sendIndexScalarGrid(); this->isIndexSent = true; }

       if (this->isChecked) return;

       if (context->hasClient)
       {
          this->computeIndexScalarGrid();
       }

       if (!(this->hasTransform() && !this->isTransformed()))
        this->isChecked = true;
       return;
     }

     if (context->hasClient)
      if (this->isChecked && doSendingIndex && !isIndexSent) { sendIndex(); this->isIndexSent = true; }

     if (this->isChecked) return;

     if (context->hasClient)
     {
        this->checkAttributesAfterTransformation();
        this->checkMask();
        this->computeIndex();
     }

     if (!(this->hasTransform() && !this->isTransformed())) 
      this->isChecked = true;

     if (!(this->hasTransform() && (!this->isGenerated())))
      this->isChecked = true; 
   }

   void CGrid::createMask(void)
   {
      using namespace std;
      std::vector<CDomain*> domainP = this->getDomains();
      std::vector<CAxis*> axisP = this->getAxis();
      int dim = domainP.size() * 2 + axisP.size();

      std::vector<CArray<bool,1>* > domainMasks(domainP.size());
      for (int i = 0; i < domainMasks.size(); ++i) domainMasks[i] = &(domainP[i]->mask_1d);
      std::vector<CArray<bool,1>* > axisMasks(axisP.size());
      for (int i = 0; i < axisMasks.size(); ++i) axisMasks[i] = &(axisP[i]->mask);

      switch (dim) {
        case 1:
          checkGridMask(mask_1d, domainMasks, axisMasks, axis_domain_order, true);
          break;
        case 2:
          checkGridMask(mask_2d, domainMasks, axisMasks, axis_domain_order, true);
          break;
        case 3:
          checkGridMask(mask_3d, domainMasks, axisMasks, axis_domain_order, true);
          break;
        case 4:
          checkGridMask(mask_4d, domainMasks, axisMasks, axis_domain_order, true);
          break;
        case 5:
          checkGridMask(mask_5d, domainMasks, axisMasks, axis_domain_order, true);
          break;
        case 6:
          checkGridMask(mask_6d, domainMasks, axisMasks, axis_domain_order, true);
          break;
        case 7:
          checkGridMask(mask_7d, domainMasks, axisMasks, axis_domain_order, true);
          break;
        default:
          break;
      }
   }

   void CGrid::checkMask(void)
   {
      using namespace std;
      std::vector<CDomain*> domainP = this->getDomains();
      std::vector<CAxis*> axisP = this->getAxis();
      int dim = domainP.size() * 2 + axisP.size();

      std::vector<CArray<bool,1>* > domainMasks(domainP.size());
      for (int i = 0; i < domainMasks.size(); ++i) domainMasks[i] = &(domainP[i]->mask_1d);
      std::vector<CArray<bool,1>* > axisMasks(axisP.size());
      for (int i = 0; i < axisMasks.size(); ++i) axisMasks[i] = &(axisP[i]->mask);

      switch (dim) {
        case 1:
          checkGridMask(mask_1d, domainMasks, axisMasks, axis_domain_order);
          break;
        case 2:
          checkGridMask(mask_2d, domainMasks, axisMasks, axis_domain_order);
          break;
        case 3:
          checkGridMask(mask_3d, domainMasks, axisMasks, axis_domain_order);
          break;
        case 4:
          checkGridMask(mask_4d, domainMasks, axisMasks, axis_domain_order);
          break;
        case 5:
          checkGridMask(mask_5d, domainMasks, axisMasks, axis_domain_order);
          break;
        case 6:
          checkGridMask(mask_6d, domainMasks, axisMasks, axis_domain_order);
          break;
        case 7:
          checkGridMask(mask_7d, domainMasks, axisMasks, axis_domain_order);
          break;
        default:
          break;
      }
   }

   void CGrid::modifyMask(const CArray<int,1>& indexToModify)
   {
      using namespace std;
      std::vector<CDomain*> domainP = this->getDomains();
      std::vector<CAxis*> axisP = this->getAxis();
      int dim = domainP.size() * 2 + axisP.size();

      switch (dim) {
        case 1:
          modifyGridMask(mask_1d, indexToModify);
          break;
        case 2:
          modifyGridMask(mask_2d, indexToModify);
          break;
        case 3:
          modifyGridMask(mask_3d, indexToModify);
          break;
        case 4:
          modifyGridMask(mask_1d, indexToModify);
          break;
        case 5:
          modifyGridMask(mask_2d, indexToModify);
          break;
        case 6:
          modifyGridMask(mask_3d, indexToModify);
          break;
        case 7:
          modifyGridMask(mask_3d, indexToModify);
          break;
        default:
          break;
      }
   }

   //---------------------------------------------------------------

   void CGrid::solveDomainRef(bool sendAtt)
   {
      setDomainList();
      std::vector<CDomain*> domListP = this->getDomains();
      if (!domListP.empty())
      {
        for (int i = 0; i < domListP.size(); ++i)
        {
          if (sendAtt) domListP[i]->sendCheckedAttributes();
          else domListP[i]->checkAttributesOnClient();
        }
      }
   }

   //---------------------------------------------------------------

   void CGrid::solveAxisRef(bool sendAtt)
   {
      setAxisList();
      std::vector<CAxis*> axisListP = this->getAxis();
      if (!axisListP.empty())
      {
        int idx = 0;
        axisPositionInGrid_.resize(0);
        for (int i = 0; i < axis_domain_order.numElements(); ++i)
        {
          int elementDimension = axis_domain_order(i);
          if (1 == elementDimension)
          {
            axisPositionInGrid_.push_back(idx);
            ++idx;
          }
          else if (2 == elementDimension) idx += 2;
        }

        for (int i = 0; i < axisListP.size(); ++i)
        {
          if (sendAtt)
            axisListP[i]->sendCheckedAttributes(getGlobalDimension(),axisPositionInGrid_[i]);
          else
            axisListP[i]->checkAttributesOnClient();
        }
      }
   }

   //---------------------------------------------------------------

   void CGrid::solveScalarRef(bool sendAtt)
   {
      setScalarList();
      std::vector<CScalar*> scalarListP = this->getScalars();
      if (!scalarListP.empty())
      {
        for (int i = 0; i < scalarListP.size(); ++i)
        {
          /*Nothing to do for now */
//          if (sendAtt) scalarListP[i]->sendCheckedAttributes();
//          else scalarListP[i]->checkAttributesOnClient();
        }
      }
   }

   std::vector<int> CGrid::getAxisPositionInGrid() const
   {
     return axisPositionInGrid_;
   }

   //---------------------------------------------------------------

   /*!
     Compute the global index of grid to send to server as well as the connected server of the current client.
     First of all, from the local data on each element of grid, we can calculate their local index which also allows us to know
     their global index. We can have a map of global index of grid and local index that each client holds
     Then, each client holds a piece of information about the distribution of servers, which permits to compute the connected server(s)
     of the current client.
   */
   void CGrid::computeIndex(void)
   {
     CContext* context = CContext::getCurrent();
     CContextClient* client = context->client;

     // First of all, compute distribution on client side
     clientDistribution_ = new CDistributionClient(client->clientRank, this);
     // Get local data index on client
     storeIndex_client.resize(clientDistribution_->getLocalDataIndexOnClient().size());
     int nbStoreIndex = storeIndex_client.numElements();
     for (int idx = 0; idx < nbStoreIndex; ++idx) storeIndex_client(idx) = (clientDistribution_->getLocalDataIndexOnClient())[idx];
     isDataDistributed_= clientDistribution_->isDataDistributed();

     connectedServerRank_.clear();

     if (!doGridHaveDataDistributed())
     {
        if (client->isServerLeader())
        {
          size_t ssize = clientDistribution_->getLocalDataIndexOnClient().size();
          const std::list<int>& ranks = client->getRanksServerLeader();
          for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
          {
            connectedServerRank_.push_back(*itRank);
            connectedDataSize_[*itRank] = ssize;
          }
        }
        return;
     }

     // Compute mapping between client and server
     std::vector<boost::unordered_map<size_t,std::vector<int> > > indexServerOnElement;
     CServerDistributionDescription serverDistributionDescription(getGlobalDimension(), client->serverSize);
     serverDistributionDescription.computeServerGlobalByElement(indexServerOnElement,
                                                                client->clientRank,
                                                                client->clientSize,
                                                                axis_domain_order,
                                                                getDistributedDimension());
     computeIndexByElement(indexServerOnElement, globalIndexOnServer_);

     const CDistributionClient::GlobalLocalDataMap& globalLocalIndexSendToServer = clientDistribution_->getGlobalLocalDataSendToServer();
     CDistributionClient::GlobalLocalDataMap::const_iterator iteGlobalLocalIndexMap = globalLocalIndexSendToServer.end(), itGlobalLocalIndexMap;
     CClientServerMapping::GlobalIndexMap::const_iterator iteGlobalMap, itbGlobalMap, itGlobalMap;
     itGlobalMap  = itbGlobalMap = globalIndexOnServer_.begin();
     iteGlobalMap = globalIndexOnServer_.end();

     for (; itGlobalMap != iteGlobalMap; ++itGlobalMap)
     {
       int serverRank = itGlobalMap->first;
       int indexSize = itGlobalMap->second.size();
       const std::vector<size_t>& indexVec = itGlobalMap->second;
       for (int idx = 0; idx < indexSize; ++idx)
       {
          itGlobalLocalIndexMap = globalLocalIndexSendToServer.find(indexVec[idx]);
          if (iteGlobalLocalIndexMap != itGlobalLocalIndexMap)
          {
             if (connectedDataSize_.end() == connectedDataSize_.find(serverRank))
               connectedDataSize_[serverRank] = 1;
             else
               ++connectedDataSize_[serverRank];
          }
       }
     }

     for (itGlobalMap = itbGlobalMap; itGlobalMap != iteGlobalMap; ++itGlobalMap) {
       connectedServerRank_.push_back(itGlobalMap->first);
     }

     nbSenders = clientServerMap_->computeConnectedClients(client->serverSize, client->clientSize, client->intraComm, connectedServerRank_);
   }

   /*!
      Compute the global of (client) grid to send to server with the global index of each element of grid
      Each element of grid has its own global index associated to a groups of server. We only search for the global index of each element whose
      server is the same, then calculate the global index of grid. This way can reduce so much the time for executing DHT, which only needs to run
      on each element whose size is much smaller than one of whole grid.
      \param [in] indexServerOnElement global index of each element and the rank of server associated with these index
      \param [out] globalIndexOnServer global index of grid and its corresponding rank of server.
   */
   void CGrid::computeIndexByElement(const std::vector<boost::unordered_map<size_t,std::vector<int> > >& indexServerOnElement,
                                     CClientServerMapping::GlobalIndexMap& globalIndexOnServer)
   {
     CContext* context = CContext::getCurrent();
     CContextClient* client = context->client;
     int serverSize = client->serverSize;
     std::vector<CDomain*> domList = getDomains();
     std::vector<CAxis*> axisList = getAxis();

     // Some pre-calculations of global index on each element of current grid.
     int nbElement = axis_domain_order.numElements();
     std::vector<CArray<size_t,1> > globalIndexElement(nbElement);
     int domainIdx = 0, axisIdx = 0, scalarIdx = 0;
     std::vector<size_t> elementNGlobal(nbElement);
     elementNGlobal[0] = 1;
     size_t globalSize = 1;
     for (int idx = 0; idx < nbElement; ++idx)
     {
       elementNGlobal[idx] = globalSize;
       size_t elementSize;
       size_t elementGlobalSize = 1;
       if (2 == axis_domain_order(idx)) // This is domain
       {
         elementSize = domList[domainIdx]->i_index.numElements();
         globalIndexElement[idx].resize(elementSize);
         for (int jdx = 0; jdx < elementSize; ++jdx)
         {
           globalIndexElement[idx](jdx) = (domList[domainIdx]->i_index)(jdx) + domList[domainIdx]->ni_glo * (domList[domainIdx]->j_index)(jdx);
         }
         elementGlobalSize = domList[domainIdx]->ni_glo.getValue() * domList[domainIdx]->nj_glo.getValue();
         ++domainIdx;
       }
       else if (1 == axis_domain_order(idx))  // This is axis
       {
         elementSize = axisList[axisIdx]->index.numElements();
         globalIndexElement[idx].resize(elementSize);
         for (int jdx = 0; jdx < elementSize; ++jdx)
         {
           globalIndexElement[idx](jdx) = (axisList[axisIdx]->index)(jdx);
         }
         elementGlobalSize = axisList[axisIdx]->n_glo.getValue();
         ++axisIdx;
       }
       else  // Of course, this is scalar
       {
         globalIndexElement[idx].resize(1);
         globalIndexElement[idx](0) = 0;
         elementGlobalSize = 1;
       }
       globalSize *= elementGlobalSize;
     }

     std::vector<std::vector<bool> > elementOnServer(nbElement, std::vector<bool>(serverSize, false));
     std::vector<boost::unordered_map<int,std::vector<size_t> > > globalElementIndexOnServer(nbElement);
     CArray<int,1> nbIndexOnServer(serverSize); // Number of distributed global index held by each client for each server
     // Number of temporary distributed global index held by each client for each server
     // We have this variable for the case of non-distributed element (often axis) to check the duplicate server rank
     CArray<int,1> nbIndexOnServerTmp(serverSize);
     for (int idx = 0; idx < nbElement; ++idx)
     {
       nbIndexOnServer = 0;
       const boost::unordered_map<size_t,std::vector<int> >& indexServerElement = indexServerOnElement[idx];
       const CArray<size_t,1>& globalIndexElementOnClient = globalIndexElement[idx];
       CClientClientDHTInt clientClientDHT(indexServerElement, client->intraComm);
       clientClientDHT.computeIndexInfoMapping(globalIndexElementOnClient);
       const CClientClientDHTInt::Index2VectorInfoTypeMap& globalIndexElementOnServerMap = clientClientDHT.getInfoIndexMap();
       CClientClientDHTInt::Index2VectorInfoTypeMap::const_iterator itb = globalIndexElementOnServerMap.begin(),
                                                                    ite = globalIndexElementOnServerMap.end(), it;
       for (it = itb; it != ite; ++it)
       {
         const std::vector<int>& tmp = it->second;
         nbIndexOnServerTmp = 0;
         for (int i = 0; i < tmp.size(); ++i)
         {
           if (0 == nbIndexOnServerTmp(tmp[i])) ++nbIndexOnServerTmp(tmp[i]);
         }
         nbIndexOnServer += nbIndexOnServerTmp;
       }

       for (int i = 0; i < serverSize; ++i)
       {
         if (0 != nbIndexOnServer(i))
         {
           globalElementIndexOnServer[idx][i].resize(nbIndexOnServer(i));
           elementOnServer[idx][i] = true;
         }
       }

       nbIndexOnServer = 0;
       for (it = itb; it != ite; ++it)
       {
         const std::vector<int>& tmp = it->second;
         nbIndexOnServerTmp = 0;
         for (int i = 0; i < tmp.size(); ++i)
         {
           if (0 == nbIndexOnServerTmp(tmp[i]))
           {
             globalElementIndexOnServer[idx][tmp[i]][nbIndexOnServer(tmp[i])] = it->first;
             ++nbIndexOnServerTmp(tmp[i]);
           }
         }
         nbIndexOnServer += nbIndexOnServerTmp;
       }
     }

    // Determine server which contain global source index
    std::vector<bool> intersectedProc(serverSize, true);
    for (int idx = 0; idx < nbElement; ++idx)
    {
      std::transform(elementOnServer[idx].begin(), elementOnServer[idx].end(),
                     intersectedProc.begin(), intersectedProc.begin(),
                     std::logical_and<bool>());
    }

    std::vector<int> srcRank;
    for (int idx = 0; idx < serverSize; ++idx)
    {
      if (intersectedProc[idx]) srcRank.push_back(idx);
    }

    // Compute the global index of grid from global index of each element.
    for (int i = 0; i < srcRank.size(); ++i)
    {
      size_t ssize = 1;
      int rankSrc = srcRank[i];
      std::vector<std::vector<size_t>* > globalIndexOfElementTmp(nbElement);
      std::vector<size_t> currentIndex(nbElement,0);
      for (int idx = 0; idx < nbElement; ++idx)
      {
        ssize *= (globalElementIndexOnServer[idx][rankSrc]).size();
        globalIndexOfElementTmp[idx] = &(globalElementIndexOnServer[idx][rankSrc]);
      }
      globalIndexOnServer[rankSrc].resize(ssize);

      std::vector<int> idxLoop(nbElement,0);
      int innnerLoopSize = (globalIndexOfElementTmp[0])->size();
      size_t idx = 0;
      while (idx < ssize)
      {
        for (int ind = 0; ind < nbElement; ++ind)
        {
          if (idxLoop[ind] == (globalIndexOfElementTmp[ind])->size())
          {
            idxLoop[ind] = 0;
            ++idxLoop[ind+1];
          }

          currentIndex[ind] = (*(globalIndexOfElementTmp[ind]))[idxLoop[ind]];
        }

        for (int ind = 0; ind < innnerLoopSize; ++ind)
        {
          currentIndex[0] = (*globalIndexOfElementTmp[0])[ind];
          size_t globalSrcIndex = 0;
          for (int idxElement = 0; idxElement < nbElement; ++idxElement)
          {
            globalSrcIndex += currentIndex[idxElement] * elementNGlobal[idxElement];
          }
          globalIndexOnServer[rankSrc][idx] = globalSrcIndex;
          ++idx;
          ++idxLoop[0];
        }
      }
    }
   }
   //----------------------------------------------------------------

   CGrid* CGrid::createGrid(CDomain* domain)
   {
      std::vector<CDomain*> vecDom(1, domain);
      std::vector<CAxis*> vecAxis;

      return createGrid(vecDom, vecAxis);
   }

   CGrid* CGrid::createGrid(CDomain* domain, CAxis* axis)
   {
      std::vector<CDomain*> vecDom(1, domain);
      std::vector<CAxis*> vecAxis(1, axis);

      return createGrid(vecDom, vecAxis);
   }

   CGrid* CGrid::createGrid(const std::vector<CDomain*>& domains, const std::vector<CAxis*>& axis,
                            const CArray<int,1>& axisDomainOrder)
   {
     std::vector<CScalar*> vecScalar;
     return createGrid(generateId(domains, axis, vecScalar, axisDomainOrder), domains, axis, vecScalar, axisDomainOrder);
   }

   CGrid* CGrid::createGrid(const std::vector<CDomain*>& domains, const std::vector<CAxis*>& axis,
                            const std::vector<CScalar*>& scalars, const CArray<int,1>& axisDomainOrder)
   {
     return createGrid(generateId(domains, axis, scalars, axisDomainOrder), domains, axis, scalars, axisDomainOrder);
   }

   CGrid* CGrid::createGrid(StdString id, const std::vector<CDomain*>& domains, const std::vector<CAxis*>& axis,
                            const std::vector<CScalar*>& scalars, const CArray<int,1>& axisDomainOrder)
   {
      if (axisDomainOrder.numElements() > 0 && axisDomainOrder.numElements() != (domains.size() + axis.size() + scalars.size()))
        ERROR("CGrid* CGrid::createGrid(...)",
              << "The size of axisDomainOrder (" << axisDomainOrder.numElements()
              << ") is not coherent with the number of elements (" << domains.size() + axis.size() <<").");

      CGrid* grid = CGridGroup::get("grid_definition")->createChild(id);
      grid->setDomainList(domains);
      grid->setAxisList(axis);
      grid->setScalarList(scalars);

      // By default, domains are always the first elements of a grid
      if (0 == axisDomainOrder.numElements())
      {
        int size = domains.size() + axis.size() + scalars.size();
        int nb = 0;
        grid->axis_domain_order.resize(size);
        for (int i = 0; i < size; ++i)
        {
          if (i < domains.size()) {
            grid->axis_domain_order(i) = 2;

          }
          else if ((scalars.size() < (size-nb)) < size) {
            grid->axis_domain_order(i) = 1;
          }
          else
            grid->axis_domain_order(i) = 0;
          ++nb;
        }
      }
      else
      {
        grid->axis_domain_order.resize(axisDomainOrder.numElements());
        grid->axis_domain_order = axisDomainOrder;
      }

      grid->solveDomainAxisRefInheritance(true);

      return grid;
   }

   CGrid* CGrid::cloneGrid(const StdString& idNewGrid, CGrid* gridSrc)
   {
     std::vector<CDomain*> domainSrcTmp = gridSrc->getDomains(), domainSrc;
     std::vector<CAxis*> axisSrcTmp = gridSrc->getAxis(), axisSrc;
     std::vector<CScalar*> scalarSrcTmp = gridSrc->getScalars(), scalarSrc;

     for (int idx = 0; idx < domainSrcTmp.size(); ++idx)
     {
       CDomain* domain = CDomain::createDomain();
       domain->duplicateAttributes(domainSrcTmp[idx]);
       domain->duplicateTransformation(domainSrcTmp[idx]);
       domain->solveRefInheritance(true);
       domain->solveInheritanceTransformation();
       domainSrc.push_back(domain);
     }

     for (int idx = 0; idx < axisSrcTmp.size(); ++idx)
     {
       CAxis* axis = CAxis::createAxis();
       axis->duplicateAttributes(axisSrcTmp[idx]);
       axis->duplicateTransformation(axisSrcTmp[idx]);
       axis->solveRefInheritance(true);
       axis->solveInheritanceTransformation();
       axisSrc.push_back(axis);
     }

     for (int idx = 0; idx < scalarSrcTmp.size(); ++idx)
     {
       CScalar* scalar = CScalar::createScalar();
       scalar->duplicateAttributes(scalarSrcTmp[idx]);
       scalar->duplicateTransformation(scalarSrcTmp[idx]);
       scalar->solveRefInheritance(true);
       scalar->solveInheritanceTransformation();
       scalarSrc.push_back(scalar);
     }

      CGrid* grid = CGrid::createGrid(idNewGrid, domainSrc, axisSrc, scalarSrc, gridSrc->axis_domain_order);

      return grid;
   }

   StdString CGrid::generateId(const std::vector<CDomain*>& domains, const std::vector<CAxis*>& axis,
                               const std::vector<CScalar*>& scalars, const CArray<int,1>& axisDomainOrder)
   {
      if (axisDomainOrder.numElements() > 0 && axisDomainOrder.numElements() != (domains.size() + axis.size() + scalars.size()))
        ERROR("CGrid* CGrid::generateId(...)",
              << "The size of axisDomainOrder (" << axisDomainOrder.numElements()
              << ") is not coherent with the number of elements (" << domains.size() + axis.size() <<").");

      std::ostringstream id;

      if (domains.empty() && axis.empty() && !scalars.empty())
        id << "__scalar_";

      if (0 != (domains.size() + axis.size() + scalars.size()))
      {
        id << "__grid";

        if (0 == axisDomainOrder.numElements())
        {
          for (size_t i = 0; i < domains.size(); ++i) id << "_" << domains[i]->getId();
          for (size_t i = 0; i < axis.size(); ++i) id << "_" << axis[i]->getId();
          for (size_t i = 0; i < scalars.size(); ++i) id << "_" << scalars[i]->getId();
        }
        else
        {
          size_t iDomain = 0, iAxis = 0, iScalar = 0;
          for (size_t i = 0; i < axisDomainOrder.numElements(); ++i)
          {
            if (2 == axisDomainOrder(i))
              id << "_" << domains[iDomain++]->getId();
            else if (1 == axisDomainOrder(i))
              id << "_" << axis[iAxis++]->getId();
            else
              id << "_" << scalars[iScalar++]->getId();
          }
        }

        id << "__";
      }

      return id.str();
   }

   StdString CGrid::generateId(const CGrid* gridSrc, const CGrid* gridDest)
   {
     StdString idSrc  = gridSrc->getId();
     StdString idDest = gridDest->getId();

     std::ostringstream id;
     id << idSrc << "__" << idDest;

     return id.str();
   }

   //----------------------------------------------------------------

   CDomainGroup* CGrid::getVirtualDomainGroup() const
   {
     return this->vDomainGroup_;
   }

   CAxisGroup* CGrid::getVirtualAxisGroup() const
   {
     return this->vAxisGroup_;
   }

   CScalarGroup* CGrid::getVirtualScalarGroup() const
   {
     return this->vScalarGroup_;
   }

   void CGrid::outputField(int rank, const CArray<double, 1>& stored, double* field)
   {
     const CArray<size_t,1>& out_i = outIndexFromClient[rank];
     StdSize numElements = stored.numElements();
     for (StdSize n = 0; n < numElements; ++n)
     {
       field[out_i(n)] = stored(n);
     }
   }

   void CGrid::inputField(int rank, const double* const field, CArray<double,1>& stored)
   {
     const CArray<size_t,1>& out_i = outIndexFromClient[rank];
     StdSize numElements = stored.numElements();
     for (StdSize n = 0; n < numElements; ++n)
     {
       stored(n) = field[out_i(n)];
     }
   }

   void CGrid::outputCompressedField(int rank, const CArray<double,1>& stored, double* field)
   {
     const CArray<size_t,1>& out_i = compressedOutIndexFromClient[rank];
     StdSize numElements = stored.numElements();
     for (StdSize n = 0; n < numElements; ++n)
     {
       field[out_i(n)] = stored(n);
     }
   }

   //----------------------------------------------------------------

   void CGrid::storeField_arr(const double* const data, CArray<double, 1>& stored) const
   {
      const StdSize size = storeIndex_client.numElements();

      stored.resize(size);
      for(StdSize i = 0; i < size; i++) stored(i) = data[storeIndex_client(i)];
   }

   void CGrid::restoreField_arr(const CArray<double, 1>& stored, double* const data) const
   {
      const StdSize size = storeIndex_client.numElements();

      for(StdSize i = 0; i < size; i++) data[storeIndex_client(i)] = stored(i);
   }

  void CGrid::computeIndexScalarGrid()
  {
    CContext* context = CContext::getCurrent();
    CContextClient* client=context->client;

    storeIndex_client.resize(1);
    storeIndex_client(0) = 0;

    connectedServerRank_.clear();

    if (0 == client->clientRank)
    {
      for (int rank = 0; rank < client->serverSize; ++rank)
      {
        connectedServerRank_.push_back(rank);
        connectedDataSize_[rank] = 1;
        nbSenders[rank] = 1;
      }
    }
    isDataDistributed_ = false;
  }

  void CGrid::computeCompressedIndex()
  {
    std::map<size_t, size_t> indexes;

    {
      std::map<int, CArray<size_t,1> >::const_iterator it = outIndexFromClient.begin();
      std::map<int, CArray<size_t,1> >::const_iterator itEnd = outIndexFromClient.end();
      for (; it != itEnd; ++it)
      {
        for (int i = 0; i < it->second.numElements(); ++i)
          indexes.insert(std::make_pair(it->second(i), 0));

        compressedOutIndexFromClient[it->first].resize(it->second.numElements());
      }
    }

    {
      std::map<size_t, size_t>::iterator it = indexes.begin();
      std::map<size_t, size_t>::iterator itEnd = indexes.end();
      for (size_t i = 0; it != itEnd; ++it, ++i)
        it->second = i;
    }

    {
      std::map<int, CArray<size_t,1> >::iterator it = compressedOutIndexFromClient.begin();
      std::map<int, CArray<size_t,1> >::iterator itEnd = compressedOutIndexFromClient.end();
      for (; it != itEnd; ++it)
      {
        const CArray<size_t,1>& outIndex = outIndexFromClient[it->first];
        for (int i = 0; i < it->second.numElements(); ++i)
          it->second(i) = indexes[outIndex(i)];
      }
    }
  }

  void CGrid::sendIndexScalarGrid()
  {
    CContext* context = CContext::getCurrent();
    CContextClient* client = context->client;

    CEventClient event(getType(), EVENT_ID_INDEX);
    list<CMessage> listMsg;
    list<CArray<size_t,1> > listOutIndex;

    if (client->isServerLeader())
    {
      const std::list<int>& ranks = client->getRanksServerLeader();
      for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
      {
        int rank = *itRank;
        int nb = 1;
        storeIndex_toSrv.insert(std::make_pair(rank, CArray<int,1>(nb)));        
        listOutIndex.push_back(CArray<size_t,1>(nb));

        CArray<int, 1>& outLocalIndexToServer = storeIndex_toSrv[rank];
        CArray<size_t, 1>& outGlobalIndexOnServer = listOutIndex.back();

        for (int k = 0; k < nb; ++k)
        {
          outGlobalIndexOnServer(k) = 0;
          outLocalIndexToServer(k)  = 0;
        }

        storeIndex_fromSrv.insert(std::make_pair(rank, CArray<int,1>(outLocalIndexToServer)));
        listMsg.push_back(CMessage());
        listMsg.back() << getId( )<< isDataDistributed_ << isCompressible_ << listOutIndex.back();

        event.push(rank, 1, listMsg.back());
      }
      client->sendEvent(event);
    }
    else
    {
      const std::list<int>& ranks = client->getRanksServerNotLeader();
      for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
      {
        int rank = *itRank;
        int nb = 1;        
        storeIndex_fromSrv.insert(std::make_pair(rank, CArray<int,1>(nb)));
        CArray<int, 1>& outLocalIndexToServer = storeIndex_fromSrv[rank];
        for (int k = 0; k < nb; ++k)
        {          
          outLocalIndexToServer(k)  = 0;
        }
      }
      client->sendEvent(event);
    }
  }

  void CGrid::sendIndex(void)
  {
    CContext* context = CContext::getCurrent();
    CContextClient* client = context->client;

    CEventClient event(getType(), EVENT_ID_INDEX);
    int rank;
    list<CMessage> listMsg;
    list<CArray<size_t,1> > listOutIndex;
    const CDistributionClient::GlobalLocalDataMap& globalLocalIndexSendToServer = clientDistribution_->getGlobalLocalDataSendToServer();
    CDistributionClient::GlobalLocalDataMap::const_iterator itIndex = globalLocalIndexSendToServer.begin(),
                                                           iteIndex = globalLocalIndexSendToServer.end();

    if (!doGridHaveDataDistributed())
    {
      if (client->isServerLeader())
      {
        int indexSize = globalLocalIndexSendToServer.size();
        CArray<size_t,1> outGlobalIndexOnServer(indexSize);
        CArray<int,1> outLocalIndexToServer(indexSize);
        for (int idx = 0; itIndex != iteIndex; ++itIndex, ++idx)
        {
          outGlobalIndexOnServer(idx) = itIndex->first;
          outLocalIndexToServer(idx) = itIndex->second;
        }
        
        const std::list<int>& ranks = client->getRanksServerLeader();
        for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
        {
          storeIndex_toSrv.insert(std::make_pair(*itRank, CArray<int,1>(outLocalIndexToServer)));
          storeIndex_fromSrv.insert(std::make_pair(*itRank, CArray<int,1>(outLocalIndexToServer)));
          listOutIndex.push_back(CArray<size_t,1>(outGlobalIndexOnServer));

          listMsg.push_back(CMessage());
          listMsg.back() << getId() << isDataDistributed_ << isCompressible_ << listOutIndex.back();

          event.push(*itRank, 1, listMsg.back());
        }
        client->sendEvent(event);
      }
      else 
      {
        int indexSize = globalLocalIndexSendToServer.size();        
        CArray<int,1> outLocalIndexToServer(indexSize);
        for (int idx = 0; itIndex != iteIndex; ++itIndex, ++idx)
        {          
          outLocalIndexToServer(idx) = itIndex->second;
        }
        
        const std::list<int>& ranks = client->getRanksServerNotLeader();
        for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
        {          
          storeIndex_fromSrv.insert(std::make_pair(*itRank, CArray<int,1>(outLocalIndexToServer)));
        }
        client->sendEvent(event);
      }
    }
    else
    {
      CClientServerMapping::GlobalIndexMap::const_iterator iteGlobalMap, itGlobalMap;
      itGlobalMap = globalIndexOnServer_.begin();
      iteGlobalMap = globalIndexOnServer_.end();

      std::map<int,std::vector<int> >localIndexTmp;
      std::map<int,std::vector<size_t> > globalIndexTmp;
      for (; itGlobalMap != iteGlobalMap; ++itGlobalMap)
      {
        int serverRank = itGlobalMap->first;
        int indexSize = itGlobalMap->second.size();
        const std::vector<size_t>& indexVec = itGlobalMap->second;
        for (int idx = 0; idx < indexSize; ++idx)
        {
          itIndex = globalLocalIndexSendToServer.find(indexVec[idx]);
          if (iteIndex != itIndex)
          {
            globalIndexTmp[serverRank].push_back(itIndex->first);
            localIndexTmp[serverRank].push_back(itIndex->second);
          }
        }
      }

      for (int ns = 0; ns < connectedServerRank_.size(); ++ns)
      {
        rank = connectedServerRank_[ns];
        int nb = 0;
        if (globalIndexTmp.end() != globalIndexTmp.find(rank))
          nb = globalIndexTmp[rank].size();

        storeIndex_toSrv.insert(make_pair(rank, CArray<int,1>(nb)));        
        listOutIndex.push_back(CArray<size_t,1>(nb));

        CArray<int, 1>& outLocalIndexToServer = storeIndex_toSrv[rank];
        CArray<size_t, 1>& outGlobalIndexOnServer = listOutIndex.back();

        for (int k = 0; k < nb; ++k)
        {
          outGlobalIndexOnServer(k) = globalIndexTmp[rank].at(k);
          outLocalIndexToServer(k)  = localIndexTmp[rank].at(k);
        }

        storeIndex_fromSrv.insert(make_pair(rank, CArray<int,1>(outLocalIndexToServer)));
        listMsg.push_back(CMessage());
        listMsg.back() << getId() << isDataDistributed_ << isCompressible_ << listOutIndex.back();

        event.push(rank, nbSenders[rank], listMsg.back());
      }

      client->sendEvent(event);
    }
  }

  void CGrid::recvIndex(CEventServer& event)
  {
    string gridId;
    vector<int> ranks;
    vector<CBufferIn*> buffers;

    list<CEventServer::SSubEvent>::iterator it;
    for (it = event.subEvents.begin(); it != event.subEvents.end(); ++it)
    {
      ranks.push_back(it->rank);
      CBufferIn* buffer = it->buffer;
      *buffer >> gridId;
      buffers.push_back(buffer);
    }
    get(gridId)->recvIndex(ranks, buffers);
  }

  void CGrid::recvIndex(vector<int> ranks, vector<CBufferIn*> buffers)
  {
    CContext* context = CContext::getCurrent();
    CContextServer* server = context->server;
    numberWrittenIndexes_ = totalNumberWrittenIndexes_ = offsetWrittenIndexes_ = 0;
    connectedServerRank_ = ranks;

    for (int n = 0; n < ranks.size(); n++)
    {
      int rank = ranks[n];
      CBufferIn& buffer = *buffers[n];

      buffer >> isDataDistributed_ >> isCompressible_;
      size_t dataSize = 0;

      if (0 == serverDistribution_)
      {
        int idx = 0, numElement = axis_domain_order.numElements();
        int ssize = numElement;
        std::vector<int> indexMap(numElement);
        for (int i = 0; i < numElement; ++i)
        {
          indexMap[i] = idx;
          if (2 == axis_domain_order(i))
          {
            ++ssize;
            idx += 2;
          }
          else
            ++idx;
        }

        int axisId = 0, domainId = 0, scalarId = 0;
        std::vector<CDomain*> domainList = getDomains();
        std::vector<CAxis*> axisList = getAxis();
        std::vector<int> nZoomBegin(ssize), nZoomSize(ssize), nGlob(ssize), nZoomBeginGlobal(ssize);
        std::vector<CArray<int,1> > globalZoomIndex(numElement);
        for (int i = 0; i < numElement; ++i)
        {
          if (2 == axis_domain_order(i)) //domain
          {
            nZoomBegin[indexMap[i]] = domainList[domainId]->zoom_ibegin_srv;
            nZoomSize[indexMap[i]]  = domainList[domainId]->zoom_ni_srv;
            nZoomBeginGlobal[indexMap[i]] = domainList[domainId]->global_zoom_ibegin;
            nGlob[indexMap[i]] = domainList[domainId]->ni_glo;

            nZoomBegin[indexMap[i] + 1] = domainList[domainId]->zoom_jbegin_srv;
            nZoomSize[indexMap[i] + 1] = domainList[domainId]->zoom_nj_srv;
            nZoomBeginGlobal[indexMap[i] + 1] = domainList[domainId]->global_zoom_jbegin;
            nGlob[indexMap[i] + 1] = domainList[domainId]->nj_glo;

            {
              int count = 0;
              globalZoomIndex[i].resize(nZoomSize[indexMap[i]]*nZoomSize[indexMap[i]+1]);
              for (int jdx = 0; jdx < nZoomSize[indexMap[i]+1]; ++jdx)
                for (int idx = 0; idx < nZoomSize[indexMap[i]]; ++idx)                
                {
                  globalZoomIndex[i](count) = (nZoomBegin[indexMap[i]] + idx) + (nZoomBegin[indexMap[i]+1] + jdx) * nGlob[indexMap[i]];
                  ++count;
                }
            }
            ++domainId;
          }
          else if (1 == axis_domain_order(i)) // axis
          {
            nZoomBegin[indexMap[i]] = axisList[axisId]->zoom_begin_srv;
            nZoomSize[indexMap[i]]  = axisList[axisId]->zoom_size_srv;
            nZoomBeginGlobal[indexMap[i]] = axisList[axisId]->global_zoom_begin_srv;
            nGlob[indexMap[i]] = axisList[axisId]->n_glo;
            if (!axisList[axisId]->global_zoom_index.isEmpty())
            {
              globalZoomIndex[i].reference(axisList[axisId]->zoom_index_srv);                
            }
            else
            {
              globalZoomIndex[i].resize(nZoomSize[indexMap[i]]);
              for (int idx = 0; idx < nZoomSize[indexMap[i]]; ++idx)
                globalZoomIndex[i](idx) = nZoomBegin[indexMap[i]] + idx;
            }
            
            ++axisId;
          }
          else // scalar
          {
            nZoomBegin[indexMap[i]] = 0;
            nZoomSize[indexMap[i]]  = 1;
            nZoomBeginGlobal[indexMap[i]] = 0;
            nGlob[indexMap[i]] = 1;
            globalZoomIndex[i].resize(1);
            globalZoomIndex[i](0) = 0;
            ++scalarId;
          }
        }
        dataSize = 1;
        for (int i = 0; i < nZoomSize.size(); ++i)
          dataSize *= nZoomSize[i];

/*        serverDistribution_ = new CDistributionServer(server->intraCommRank, nZoomBegin, nZoomSize,
                                                      nZoomBeginGlobal, nGlob);*/
        serverDistribution_ = new CDistributionServer(server->intraCommRank, 
                                                      globalZoomIndex, axis_domain_order,
                                                      nZoomBegin, nZoomSize, nZoomBeginGlobal, nGlob);
      }

      CArray<size_t,1> outIndex;
      buffer >> outIndex;
      if (isDataDistributed_)
        serverDistribution_->computeLocalIndex(outIndex);
      else
      {
        dataSize = outIndex.numElements();
        for (int i = 0; i < outIndex.numElements(); ++i) outIndex(i) = i;
      }
      writtenDataSize_ += dataSize;
      
      outIndexFromClient.insert(std::make_pair(rank, outIndex));
      connectedDataSize_[rank] = outIndex.numElements();
      numberWrittenIndexes_ += outIndex.numElements();
    }

    // if (isScalarGrid()) return;

    if (isDataDistributed_)
    {
      MPI_Allreduce(&numberWrittenIndexes_, &totalNumberWrittenIndexes_, 1, MPI_INT, MPI_SUM, server->intraComm);
      MPI_Scan(&numberWrittenIndexes_, &offsetWrittenIndexes_, 1, MPI_INT, MPI_SUM, server->intraComm);
      offsetWrittenIndexes_ -= numberWrittenIndexes_;
    }
    else
      totalNumberWrittenIndexes_ = numberWrittenIndexes_;

    nbSenders = CClientServerMappingDistributed::computeConnectedClients(context->client->serverSize, context->client->clientSize, context->client->intraComm, ranks);
  }

  /*
     Compute on the fly the global dimension of a grid with its elements
     \param[in/out] globalDim global dimension of grid
     \param[in] domains list of its domains
     \param[in] axiss list of its axis
     \param[in] scalars list of its scalars
     \param[in] axisDomainOrder the order of element in a grid (e.g: scalar then axis)
     \return The dimension of which we do distribution (often for server)
  */
  int CGrid::computeGridGlobalDimension(std::vector<int>& globalDim,
                                        const std::vector<CDomain*> domains,
                                        const std::vector<CAxis*> axis,
                                        const std::vector<CScalar*> scalars,
                                        const CArray<int,1>& axisDomainOrder)
  {
    globalDim.resize(domains.size()*2+axis.size()+scalars.size());
    int positionDimensionDistributed = 1;
    int idx = 0, idxDomain = 0, idxAxis = 0, idxScalar = 0;
    for (int i = 0; i < axisDomainOrder.numElements(); ++i)
    {
      if (2 == axisDomainOrder(i))
      {
        if (!(domains[idxDomain]->type.isEmpty()) && (domains[idxDomain]->type==CDomain::type_attr::unstructured))
        {
          positionDimensionDistributed = idx;
        }
        else
        {
          positionDimensionDistributed = idx +1;
        }

        globalDim[idx]   = domains[idxDomain]->ni_glo.getValue();
        globalDim[idx+1] = domains[idxDomain]->nj_glo.getValue();

        ++idxDomain;
        idx += 2;
      }
      else if (1 == axisDomainOrder(i))
      {
        globalDim[idx] = axis[idxAxis]->n_glo.getValue();
        ++idxAxis;
        ++idx;
      }
      else
      {
        globalDim[idx] = 1;
        ++idxScalar;
        ++idx;
      }
    }

    return positionDimensionDistributed;
  }

  // Retrieve the global dimension of grid
  std::vector<int> CGrid::getGlobalDimension()
  {
    std::vector<int> globalDim;
    computeGridGlobalDimension(globalDim, getDomains(), getAxis(), getScalars(), axis_domain_order);

    return globalDim;
  }

  // Retrieve dimension on which we do distribution (Very often, it should be 2nd dimension)
  int CGrid::getDistributedDimension()
  {
    std::vector<int> globalDim;
    return computeGridGlobalDimension(globalDim, getDomains(), getAxis(), getScalars(), axis_domain_order);    
  }

  bool CGrid::isScalarGrid() const
  {
    return (axisList_.empty() && domList_.empty());
  }

  /*!
    Verify whether one server need to write data
    There are some cases on which one server has nodata to write. For example, when we
    just only want to zoom on a domain.
  */
  bool CGrid::doGridHaveDataToWrite()
  {
     return (0 != writtenDataSize_);
  }

  /*!
    Return size of data which is written on each server
    Whatever dimension of a grid, data which are written on server must be presented as
    an one dimension array.
    \return size of data written on server
  */
  size_t CGrid::getWrittenDataSize() const
  {
    return writtenDataSize_;
  }

  /*!
    Returns the number of indexes written by each server.
    \return the number of indexes written by each server
  */
  int CGrid::getNumberWrittenIndexes() const
  {
    return numberWrittenIndexes_;
  }

  /*!
    Returns the total number of indexes written by the servers.
    \return the total number of indexes written by the servers
  */
  int CGrid::getTotalNumberWrittenIndexes() const
  {
    return totalNumberWrittenIndexes_;
  }

  /*!
    Returns the offset of indexes written by each server.
    \return the offset of indexes written by each server
  */
  int CGrid::getOffsetWrittenIndexes() const
  {
    return offsetWrittenIndexes_;
  }

  CDistributionServer* CGrid::getDistributionServer()
  {
    return serverDistribution_;
  }

  CDistributionClient* CGrid::getDistributionClient()
  {
    return clientDistribution_;
  }

  bool CGrid::doGridHaveDataDistributed()
  {
    if (isScalarGrid()) return false;
    else
      return isDataDistributed_;
  }

   /*!
   \brief Dispatch event received from client
      Whenever a message is received in buffer of server, it will be processed depending on
   its event type. A new event type should be added in the switch list to make sure
   it processed on server side.
   \param [in] event: Received message
   */
  bool CGrid::dispatchEvent(CEventServer& event)
  {

    if (SuperClass::dispatchEvent(event)) return true;
    else
    {
      switch(event.type)
      {
        case EVENT_ID_INDEX :
          recvIndex(event);
          return true;
          break;

         case EVENT_ID_ADD_DOMAIN :
           recvAddDomain(event);
           return true;
           break;

         case EVENT_ID_ADD_AXIS :
           recvAddAxis(event);
           return true;
           break;

         case EVENT_ID_ADD_SCALAR :
           recvAddScalar(event);
           return true;
           break;
        default :
          ERROR("bool CDomain::dispatchEvent(CEventServer& event)",
                << "Unknown Event");
          return false;
      }
    }
  }

   ///---------------------------------------------------------------

   CDomain* CGrid::addDomain(const std::string& id)
   {
     order_.push_back(2);
     axis_domain_order.resize(order_.size());
     for (int idx = 0; idx < order_.size(); ++idx) axis_domain_order(idx)=order_[idx];
     return vDomainGroup_->createChild(id);
   }

   CAxis* CGrid::addAxis(const std::string& id)
   {
     order_.push_back(1);
     axis_domain_order.resize(order_.size());
     for (int idx = 0; idx < order_.size(); ++idx) axis_domain_order(idx)=order_[idx];
     return vAxisGroup_->createChild(id);
   }

   CScalar* CGrid::addScalar(const std::string& id)
   {
     order_.push_back(0);
     axis_domain_order.resize(order_.size());
     for (int idx = 0; idx < order_.size(); ++idx) axis_domain_order(idx)=order_[idx];
     return vScalarGroup_->createChild(id);
   }

   //! Change virtual field group to a new one
   void CGrid::setVirtualDomainGroup(CDomainGroup* newVDomainGroup)
   {
      this->vDomainGroup_ = newVDomainGroup;
   }

   //! Change virtual variable group to new one
   void CGrid::setVirtualAxisGroup(CAxisGroup* newVAxisGroup)
   {
      this->vAxisGroup_ = newVAxisGroup;
   }

   //! Change virtual variable group to new one
   void CGrid::setVirtualScalarGroup(CScalarGroup* newVScalarGroup)
   {
      this->vScalarGroup_ = newVScalarGroup;
   }

   /*!
   \brief Send a message to create a domain on server side
   \param[in] id String identity of domain that will be created on server
   */
   void CGrid::sendAddDomain(const string& id)
   {
    CContext* context=CContext::getCurrent();

    if (! context->hasServer )
    {
       CContextClient* client=context->client;

       CEventClient event(this->getType(),EVENT_ID_ADD_DOMAIN);
       if (client->isServerLeader())
       {
         CMessage msg;
         msg<<this->getId();
         msg<<id;
         const std::list<int>& ranks = client->getRanksServerLeader();
         for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
           event.push(*itRank,1,msg);
         client->sendEvent(event);
       }
       else client->sendEvent(event);
    }
   }

   /*!
   \brief Send a message to create an axis on server side
   \param[in] id String identity of axis that will be created on server
   */
   void CGrid::sendAddAxis(const string& id)
   {
    CContext* context=CContext::getCurrent();

    if (! context->hasServer )
    {
       CContextClient* client=context->client;

       CEventClient event(this->getType(),EVENT_ID_ADD_AXIS);
       if (client->isServerLeader())
       {
         CMessage msg;
         msg<<this->getId();
         msg<<id;
         const std::list<int>& ranks = client->getRanksServerLeader();
         for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
           event.push(*itRank,1,msg);
         client->sendEvent(event);
       }
       else client->sendEvent(event);
    }
   }

   /*!
   \brief Send a message to create a scalar on server side
   \param[in] id String identity of scalar that will be created on server
   */
   void CGrid::sendAddScalar(const string& id)
   {
    CContext* context=CContext::getCurrent();

    if (! context->hasServer )
    {
       CContextClient* client=context->client;

       CEventClient event(this->getType(),EVENT_ID_ADD_SCALAR);
       if (client->isServerLeader())
       {
         CMessage msg;
         msg<<this->getId();
         msg<<id;
         const std::list<int>& ranks = client->getRanksServerLeader();
         for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
           event.push(*itRank,1,msg);
         client->sendEvent(event);
       }
       else client->sendEvent(event);
    }
   }

   /*!
   \brief Receive a message annoucing the creation of a domain on server side
   \param[in] event Received event
   */
   void CGrid::recvAddDomain(CEventServer& event)
   {

      CBufferIn* buffer = event.subEvents.begin()->buffer;
      string id;
      *buffer >> id;
      get(id)->recvAddDomain(*buffer);
   }

   /*!
   \brief Receive a message annoucing the creation of a domain on server side
   \param[in] buffer Buffer containing message
   */
   void CGrid::recvAddDomain(CBufferIn& buffer)
   {
      string id;
      buffer >> id;
      addDomain(id);
   }

   /*!
   \brief Receive a message annoucing the creation of an axis on server side
   \param[in] event Received event
   */
   void CGrid::recvAddAxis(CEventServer& event)
   {

      CBufferIn* buffer = event.subEvents.begin()->buffer;
      string id;
      *buffer >> id;
      get(id)->recvAddAxis(*buffer);
   }

   /*!
   \brief Receive a message annoucing the creation of an axis on server side
   \param[in] buffer Buffer containing message
   */
   void CGrid::recvAddAxis(CBufferIn& buffer)
   {
      string id;
      buffer >> id;
      addAxis(id);
   }

   /*!
   \brief Receive a message annoucing the creation of an scalar on server side
   \param[in] event Received event
   */
   void CGrid::recvAddScalar(CEventServer& event)
   {

      CBufferIn* buffer = event.subEvents.begin()->buffer;
      string id;
      *buffer >> id;
      get(id)->recvAddScalar(*buffer);
   }

   /*!
   \brief Receive a message annoucing the creation of an scalar on server side
   \param[in] buffer Buffer containing message
   */
   void CGrid::recvAddScalar(CBufferIn& buffer)
   {
      string id;
      buffer >> id;
      addScalar(id);
   }

  /*!
  \brief Solve domain and axis references
  As field, domain and axis can refer to other domains or axis. In order to inherit correctly
  all attributes from their parents, they should be processed with this function
  \param[in] apply inherit all attributes of parents (true)
  */
  void CGrid::solveDomainAxisRefInheritance(bool apply)
  {
    CContext* context = CContext::getCurrent();
    unsigned int vecSize, i;
    std::vector<StdString>::iterator it, itE;
    setDomainList();
    it = domList_.begin(); itE = domList_.end();
    for (; it != itE; ++it)
    {
      CDomain* pDom = CDomain::get(*it);
      if (context->hasClient)
      {
        pDom->solveRefInheritance(apply);
        pDom->solveInheritanceTransformation();
      }
    }

    setAxisList();
    it = axisList_.begin(); itE = axisList_.end();
    for (; it != itE; ++it)
    {
      CAxis* pAxis = CAxis::get(*it);
      if (context->hasClient)
      {
        pAxis->solveRefInheritance(apply);
        pAxis->solveInheritanceTransformation();
      }
    }

    setScalarList();
    it = scalarList_.begin(); itE = scalarList_.end();
    for (; it != itE; ++it)
    {
      CScalar* pScalar = CScalar::get(*it);
      if (context->hasClient)
      {
        pScalar->solveRefInheritance(apply);
        pScalar->solveInheritanceTransformation();
      }
    }
  }

  bool CGrid::isTransformed()
  {
    return isTransformed_;
  }

  void CGrid::setTransformed()
  {
    isTransformed_ = true;
  }

  CGridTransformation* CGrid::getTransformations()
  {
    return transformations_;
  }

  void CGrid::addTransGridSource(CGrid* gridSrc)
  {
    if (gridSrc_.end() == gridSrc_.find(gridSrc))
      gridSrc_.insert(make_pair(gridSrc,make_pair(false,"")));
  }

  std::map<CGrid*,std::pair<bool,StdString> >& CGrid::getTransGridSource()
  {
    return gridSrc_;
  }

  /*!
     Complete all the necessary (and lacking) attributes of a grid
     This function is similar to gridTransformation but works only (till now) on generate_rectilinear_domain transformation
  */
  void CGrid::completeGrid(CGrid* transformGridSrc)
  {
    if (0 != transformGridSrc)
    {
      if (axis_domain_order.numElements() != transformGridSrc->axis_domain_order.numElements())
      {
        ERROR("CGrid::completeGrid(CGrid* transformGridSrc)",
             << "Two grids have different number of elements. " << std::endl
             << "Number of element of grid destination " << this->getId() << " is " << axis_domain_order.numElements() << std::endl
             << "Number of element of grid source " << transformGridSrc->getId() << " is " << transformGridSrc->axis_domain_order.numElements());
      }
    }

    if (isGenerated()) return;
    setGenerated();

    CGridGenerate gridGenerate(this, transformGridSrc);
    gridGenerate.completeGrid();
  }

  bool CGrid::isGenerated()
  {
    return isGenerated_;
  }

  void CGrid::setGenerated()
  {
    isGenerated_ = true;
  }

  void CGrid::transformGrid(CGrid* transformGridSrc)
  {
    if (!transformGridSrc)
      ERROR("CGrid::transformGrid(CGrid* transformGridSrc)",
            << "Impossible to transform grid '" << getId() << "', the source grid is null.");

    if (isTransformed()) return;
    setTransformed();
    if (axis_domain_order.numElements() != transformGridSrc->axis_domain_order.numElements())
    {
      ERROR("CGrid::transformGrid(CGrid* transformGridSrc)",
           << "Two grids have different number of elements. " << std::endl
           << "Number of element of grid destination " << this->getId() << " is " << axis_domain_order.numElements() << std::endl
           << "Number of element of grid source " << transformGridSrc->getId() << " is " << transformGridSrc->axis_domain_order.numElements());
    }
    else
    {
    }

    transformations_ = new CGridTransformation(this, transformGridSrc);
    transformations_->computeAll();
    if (0 < transformations_->getNbAlgo()) hasTransform_ = true;

    // Ok, now need to compute index of grid source
    transformGridSrc->checkMaskIndex(false);
  }

  bool CGrid::hasTransform()
  {
    if (hasTransform_) return hasTransform_;

    std::vector<CDomain*> domList = getDomains();
    std::vector<CAxis*> axisList = getAxis();
    std::vector<CScalar*> scalarList = getScalars();

    for (int idx = 0; idx < domList.size(); ++idx) hasTransform_ |= domList[idx]->hasTransformation();
    for (int idx = 0; idx < axisList.size(); ++idx) hasTransform_ |= axisList[idx]->hasTransformation();
    for (int idx = 0; idx < scalarList.size(); ++idx) hasTransform_ |= scalarList[idx]->hasTransformation();

    return hasTransform_;
  }

  /*!
  \brief Get the list of domain pointers
  \return list of domain pointers
  */
  std::vector<CDomain*> CGrid::getDomains()
  {
    std::vector<CDomain*> domList;
    if (!domList_.empty())
    {
      for (int i = 0; i < domList_.size(); ++i) domList.push_back(CDomain::get(domList_[i]));
    }
    return domList;
  }

  /*!
  \brief Get the list of  axis pointers
  \return list of axis pointers
  */
  std::vector<CAxis*> CGrid::getAxis()
  {
    std::vector<CAxis*> aList;
    if (!axisList_.empty())
      for (int i =0; i < axisList_.size(); ++i) aList.push_back(CAxis::get(axisList_[i]));

    return aList;
  }

  /*!
  \brief Get the list of  axis pointers
  \return list of axis pointers
  */
  std::vector<CScalar*> CGrid::getScalars()
  {
    std::vector<CScalar*> sList;
    if (!scalarList_.empty())
      for (int i =0; i < scalarList_.size(); ++i) sList.push_back(CScalar::get(scalarList_[i]));

    return sList;
  }

  /*!
  \brief Get domain pointer with index
  \return domain pointer
  */
  CDomain* CGrid::getDomain(int domainIndex)
  {
    std::vector<CDomain*> domainListP = this->getDomains();
    if (domainListP.empty())
    {
      ERROR("CGrid::getDomain(int domainIndex)",
            << "No domain associated to this grid. " << std::endl
            << "Grid id = " << this->getId());
    }

    if (domainIndex >= domainListP.size() || (domainIndex < 0))
      ERROR("CGrid::getDomain(int domainIndex)",
            << "Domain with the index doesn't exist " << std::endl
            << "Grid id = " << this->getId() << std::endl
            << "Grid has only " << domainListP.size() << " domain but domain index required is " << domainIndex << std::endl);

    return domainListP[domainIndex];
  }

  /*!
  \brief Get the axis pointer with index
  \return axis pointer
  */
  CAxis* CGrid::getAxis(int axisIndex)
  {
    std::vector<CAxis*> axisListP = this->getAxis();
    if (axisListP.empty())
    {
      ERROR("CGrid::getDomain(int axisIndex)",
            << "No axis associated to this grid. " << std::endl
            << "Grid id = " << this->getId());
    }

    if (axisIndex >= axisListP.size() || (axisIndex < 0))
      ERROR("CGrid::getDomain(int axisIndex)",
            << "Domain with the index doesn't exist " << std::endl
            << "Grid id = " << this->getId() << std::endl
            << "Grid has only " << axisListP.size() << " axis but axis index required is " << axisIndex << std::endl);

    return axisListP[axisIndex];
  }

  /*!
  \brief Get the a scalar pointer
  \return scalar pointer
  */
  CScalar* CGrid::getScalar(int scalarIndex)
  {
    std::vector<CScalar*> scalarListP = this->getScalars();
    if (scalarListP.empty())
    {
      ERROR("CGrid::getScalar(int scalarIndex)",
            << "No scalar associated to this grid. " << std::endl
            << "Grid id = " << this->getId());
    }

    if (scalarIndex >= scalarListP.size() || (scalarIndex < 0))
      ERROR("CGrid::getScalar(int scalarIndex)",
            << "Scalar with the index doesn't exist " << std::endl
            << "Grid id = " << this->getId() << std::endl
            << "Grid has only " << scalarListP.size() << " scalar but scalar index required is " << scalarIndex << std::endl);

    return scalarListP[scalarIndex];
  }

  /*!
  \brief Set domain(s) of a grid from a list
  \param[in] domains list of domains
  */
  void CGrid::setDomainList(const std::vector<CDomain*> domains)
  {
    if (isDomListSet) return;
    std::vector<CDomain*> domList = this->getVirtualDomainGroup()->getAllChildren();
    if (!domains.empty() && domList.empty())
    {
      for (int i = 0; i < domains.size(); ++i)
        this->getVirtualDomainGroup()->addChild(domains[i]);
      domList = this->getVirtualDomainGroup()->getAllChildren();
    }

    if (!domList.empty())
    {
      int sizeDom = domList.size();
      domList_.resize(sizeDom);
      for (int i = 0; i < sizeDom; ++i)
      {
        domList_[i] = domList[i]->getId();
      }
      isDomListSet = true;
    }

  }

  /*!
  \brief Set axis(s) of a grid from a list
  \param[in] axis list of axis
  */
  void CGrid::setAxisList(const std::vector<CAxis*> axis)
  {
    if (isAxisListSet) return;
    std::vector<CAxis*> aList = this->getVirtualAxisGroup()->getAllChildren();
    if (!axis.empty() && aList.empty())
    {
      for (int i = 0; i < axis.size(); ++i)
        this->getVirtualAxisGroup()->addChild(axis[i]);
      aList = this->getVirtualAxisGroup()->getAllChildren();
    }

    if (!aList.empty())
    {
      int sizeAxis = aList.size();
      axisList_.resize(sizeAxis);
      for (int i = 0; i < sizeAxis; ++i)
      {
        axisList_[i] = aList[i]->getId();
      }
      isAxisListSet = true;
    }
  }

  /*!
  \brief Set scalar(s) of a grid from a list
  \param[in] scalars list of scalars
  */
  void CGrid::setScalarList(const std::vector<CScalar*> scalars)
  {
    if (isScalarListSet) return;
    std::vector<CScalar*> sList = this->getVirtualScalarGroup()->getAllChildren();
    if (!scalars.empty() && sList.empty())
    {
      for (int i = 0; i < scalars.size(); ++i)
        this->getVirtualScalarGroup()->addChild(scalars[i]);
      sList = this->getVirtualScalarGroup()->getAllChildren();
    }

    if (!sList.empty())
    {
      int sizeScalar = sList.size();
      scalarList_.resize(sizeScalar);
      for (int i = 0; i < sizeScalar; ++i)
      {
        scalarList_[i] = sList[i]->getId();
      }
      isScalarListSet = true;
    }
  }

  /*!
  \brief Get list of id of domains
  \return id list of domains
  */
  std::vector<StdString> CGrid::getDomainList()
  {
    setDomainList();
    return domList_;
  }

  /*!
  \brief Get list of id of axis
  \return id list of axis
  */
  std::vector<StdString> CGrid::getAxisList()
  {
    setAxisList();
    return axisList_;
  }

  /*!
  \brief Get list of id of scalar
  \return id list of scalar
  */
  std::vector<StdString> CGrid::getScalarList()
  {
    setScalarList();
    return scalarList_;
  }

  /*!
    Send all attributes of domains from client to server
  */
  void CGrid::sendAllDomains()
  {
    std::vector<CDomain*> domList = this->getVirtualDomainGroup()->getAllChildren();
    int dSize = domList.size();
    for (int i = 0; i < dSize; ++i)
    {
      sendAddDomain(domList[i]->getId());
      domList[i]->sendAllAttributesToServer();
    }
  }

  /*!
    Send all attributes of axis from client to server
  */
  void CGrid::sendAllAxis()
  {
    std::vector<CAxis*> aList = this->getVirtualAxisGroup()->getAllChildren();
    int aSize = aList.size();

    for (int i = 0; i < aSize; ++i)
    {
      sendAddAxis(aList[i]->getId());
      aList[i]->sendAllAttributesToServer();
    }
  }

  /*!
    Send all attributes of scalars from client to server
  */
  void CGrid::sendAllScalars()
  {
    std::vector<CScalar*> sList = this->getVirtualScalarGroup()->getAllChildren();
    int sSize = sList.size();

    for (int i = 0; i < sSize; ++i)
    {
      sendAddScalar(sList[i]->getId());
      sList[i]->sendAllAttributesToServer();
    }
  }

  /*!
    Parse a grid, for now, it contains only domain, axis and scalar
  */
  void CGrid::parse(xml::CXMLNode& node)
  {
    SuperClass::parse(node);

    if (node.goToChildElement())
    {
      StdString domainName("domain");
      StdString axisName("axis");
      StdString scalarName("scalar");
      do
      {
        if (node.getElementName() == domainName) {
          order_.push_back(2);
          this->getVirtualDomainGroup()->parseChild(node);
        }
        if (node.getElementName() == axisName) {
          order_.push_back(1);
          this->getVirtualAxisGroup()->parseChild(node);
        }
        if (node.getElementName() == scalarName) {
          order_.push_back(0);
          this->getVirtualScalarGroup()->parseChild(node);
        }
      } while (node.goToNextElement());
      node.goToParentElement();
    }

    if (!order_.empty())
    {
      int sizeOrd = order_.size();
      axis_domain_order.resize(sizeOrd);
      for (int i = 0; i < sizeOrd; ++i)
      {
        axis_domain_order(i) = order_[i];
      }
    }

    setDomainList();
    setAxisList();
    setScalarList();
   }
} // namespace xios
