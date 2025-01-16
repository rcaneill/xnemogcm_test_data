/*!
   \file generic_algorithm_transformation.hpp
   \author Ha NGUYEN
   \since 14 May 2015
   \date 21 Mars 2016

   \brief Interface for all transformation algorithms.
 */
#include "generic_algorithm_transformation.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "client_client_dht_template.hpp"
#include "utils.hpp"

namespace xios {

CGenericAlgorithmTransformation::CGenericAlgorithmTransformation()
 : transformationMapping_(), transformationWeight_(), transformationPosition_(),
   idAuxInputs_(), type_(ELEMENT_NO_MODIFICATION_WITH_DATA)
{
}

void CGenericAlgorithmTransformation::updateData(CArray<double,1>& dataOut)
{

}

void CGenericAlgorithmTransformation::apply(const std::vector<std::pair<int,double> >& localIndex,
                                            const double* dataInput,
                                            CArray<double,1>& dataOut,
                                            std::vector<bool>& flagInitial,
                                            bool ignoreMissingValue, bool firstPass  )
{
  int nbLocalIndex = localIndex.size();   
  double defaultValue = std::numeric_limits<double>::quiet_NaN();
  if (ignoreMissingValue)
  {
    for (int idx = 0; idx < nbLocalIndex; ++idx)
    {
      if (NumTraits<double>::isNan(*(dataInput + idx)))
      {
        flagInitial[localIndex[idx].first] = false;
      }
      else
      {
        dataOut(localIndex[idx].first) += *(dataInput + idx) * localIndex[idx].second;
        flagInitial[localIndex[idx].first] = true; // Reset flag to indicate not all data source are nan
      }
    }

    // If all data source are nan then data destination must be nan
    for (int idx = 0; idx < nbLocalIndex; ++idx)
    {
      if (!flagInitial[localIndex[idx].first])
        dataOut(localIndex[idx].first) = defaultValue;
    }
  }
  else
  {
    for (int idx = 0; idx < nbLocalIndex; ++idx)
    {
      dataOut(localIndex[idx].first) += *(dataInput + idx) * localIndex[idx].second;
    }
  }
}

void CGenericAlgorithmTransformation::computePositionElements(CGrid* dst, CGrid* src)
{
  int idxScalar = 0, idxAxis = 0, idxDomain = 0;
  CArray<int,1> axisDomainOrderDst = dst->axis_domain_order;
  for (int i = 0; i < axisDomainOrderDst.numElements(); ++i)
  {
    int dimElement = axisDomainOrderDst(i);
    if (2 == dimElement)
    {
      elementPositionInGridDst2DomainPosition_[i] = idxDomain;
      ++idxDomain;
    }
    else if (1 == dimElement)
    {
      elementPositionInGridDst2AxisPosition_[i] = idxAxis;
      ++idxAxis;
    }
    else
    {
      elementPositionInGridDst2ScalarPosition_[i] = idxScalar;
      ++idxScalar;
    }
  }

  idxScalar = idxAxis = idxDomain = 0;
  CArray<int,1> axisDomainOrderSrc = src->axis_domain_order;
  for (int i = 0; i < axisDomainOrderSrc.numElements(); ++i)
  {
    int dimElement = axisDomainOrderSrc(i);
    if (2 == dimElement)
    {
      elementPositionInGridSrc2DomainPosition_[i] = idxDomain;
      ++idxDomain;
    }
    else if (1 == dimElement)
    {
      elementPositionInGridSrc2AxisPosition_[i] = idxAxis;
      ++idxAxis;
    }
    else
    {
      elementPositionInGridSrc2ScalarPosition_[i] = idxScalar;
      ++idxScalar;
    }
  }
}

/*!
  This function computes the global indexes of grid source, which the grid destination is in demand.
  \param[in] elementPositionInGrid position of an element in a grid .E.g: if grid is composed of domain and axis (in order),
                then position of axis in grid is 1 and domain is positioned at 0.
  \param[in] gridSrc Grid source
  \param[in] gridDst Grid destination
  \param[in\out] globaIndexWeightFromSrcToDst mapping of each global index source and weight to index destination
*/
void CGenericAlgorithmTransformation::computeGlobalSourceIndex(int elementPositionInGrid,
                                                               CGrid* gridSrc,
                                                               CGrid* gridDst,
                                                               SourceDestinationIndexMap& globaIndexWeightFromSrcToDst)
 {
  CContext* context = CContext::getCurrent();
  CContextClient* client = context->client;
  int nbClient = client->clientSize;

  typedef boost::unordered_map<int, std::vector<std::pair<int,double> > > SrcToDstMap;

  size_t indexSrcSize = 0;
  for (size_t idxTrans = 0; idxTrans < transformationMapping_.size(); ++idxTrans)
  {
    TransformationIndexMap::const_iterator itbTransMap = transformationMapping_[idxTrans].begin(), itTransMap,
                                           iteTransMap = transformationMapping_[idxTrans].end();
    TransformationWeightMap::const_iterator itbTransWeight = transformationWeight_[idxTrans].begin(), itTransWeight;

    itTransWeight = itbTransWeight;
    for (itTransMap = itbTransMap; itTransMap != iteTransMap; ++itTransMap, ++itTransWeight)
    {
       indexSrcSize += (itTransMap->second).size();
    }
  }

  bool isTransPosEmpty = transformationPosition_.empty();
  CArray<size_t,1> transPos;
  if (!isTransPosEmpty) transPos.resize(transformationMapping_.size());
  CArray<size_t,1> indexSrc(indexSrcSize);
  indexSrcSize = 0;
  for (size_t idxTrans = 0; idxTrans < transformationMapping_.size(); ++idxTrans)
  {
    TransformationIndexMap::const_iterator itbTransMap = transformationMapping_[idxTrans].begin(), itTransMap,
                                           iteTransMap = transformationMapping_[idxTrans].end();
    TransformationWeightMap::const_iterator itbTransWeight = transformationWeight_[idxTrans].begin(), itTransWeight;

    // Build mapping between global source element index and global destination element index.
    itTransWeight = itbTransWeight;
    for (itTransMap = itbTransMap; itTransMap != iteTransMap; ++itTransMap, ++itTransWeight)
    {
      const std::vector<int>& srcIndex = itTransMap->second;
      for (int idx = 0; idx < srcIndex.size(); ++idx)
      {
        indexSrc(indexSrcSize) = srcIndex[idx];
        ++indexSrcSize;
      }
    }

    if (!isTransPosEmpty)
    {
      TransformationPositionMap::const_iterator itPosMap = transformationPosition_[idxTrans].begin();
      transPos(idxTrans) = itPosMap->second[0];
    }
  }

  // compute position of elements on grids
  computePositionElements(gridDst, gridSrc);
  std::vector<CScalar*> scalarListDestP = gridDst->getScalars();
  std::vector<CAxis*> axisListDestP = gridDst->getAxis();
  std::vector<CDomain*> domainListDestP = gridDst->getDomains();
  CArray<int,1> axisDomainDstOrder = gridDst->axis_domain_order;
  std::vector<CScalar*> scalarListSrcP  = gridSrc->getScalars();
  std::vector<CAxis*> axisListSrcP = gridSrc->getAxis();
  std::vector<CDomain*> domainListSrcP = gridSrc->getDomains();
  CArray<int,1> axisDomainSrcOrder = gridSrc->axis_domain_order;

  // Find out global index source of transformed element on corresponding process.
  std::vector<boost::unordered_map<int,std::vector<size_t> > > globalElementIndexOnProc(axisDomainDstOrder.numElements());
  CClientClientDHTInt::Index2VectorInfoTypeMap globalIndexOfTransformedElementOnProc;  
  for (int idx = 0; idx < axisDomainDstOrder.numElements(); ++idx)
  {
    if (idx == elementPositionInGrid)
      computeExchangeGlobalIndex(indexSrc, axisDomainSrcOrder(idx), globalIndexOfTransformedElementOnProc); //globalElementIndexOnProc[idx]);
    if (2 == axisDomainDstOrder(idx)) // It's domain
    {
      if (idx != elementPositionInGrid)
        computeExchangeDomainIndex(domainListDestP[elementPositionInGridDst2DomainPosition_[idx]],
                                   domainListSrcP[elementPositionInGridSrc2DomainPosition_[idx]],
                                   transPos,
                                   globalElementIndexOnProc[idx]);      

    }
    else if (1 == axisDomainDstOrder(idx))//it's an axis
    {
      if (idx != elementPositionInGrid)
        computeExchangeAxisIndex(axisListDestP[elementPositionInGridDst2AxisPosition_[idx]],
                                 axisListSrcP[elementPositionInGridSrc2AxisPosition_[idx]],
                                 transPos,
                                 globalElementIndexOnProc[idx]);
    }
    else //it's a scalar
    {
      if (idx != elementPositionInGrid)
        computeExchangeScalarIndex(scalarListDestP[elementPositionInGridDst2ScalarPosition_[idx]],
                                   scalarListSrcP[elementPositionInGridSrc2ScalarPosition_[idx]],
                                   transPos,
                                   globalElementIndexOnProc[idx]);

    }
  }

  if (!isTransPosEmpty)
  {
    for (int idx = 0; idx < globalElementIndexOnProc.size(); ++idx)
    {
      if (idx != elementPositionInGrid)
      {
        boost::unordered_map<int,std::vector<size_t> >::iterator itb = globalElementIndexOnProc[idx].begin(), it,
                                                                 ite = globalElementIndexOnProc[idx].end();
        for (it = itb; it != ite; ++it) it->second.resize(1);
      }
    }
  }

  for (size_t idxTrans = 0; idxTrans < transformationMapping_.size(); ++idxTrans)
  {
    TransformationIndexMap::const_iterator itbTransMap = transformationMapping_[idxTrans].begin(), itTransMap,
                                           iteTransMap = transformationMapping_[idxTrans].end();
    TransformationWeightMap::const_iterator itbTransWeight = transformationWeight_[idxTrans].begin(), itTransWeight;
    SrcToDstMap src2DstMap;
    src2DstMap.rehash(std::ceil(transformationMapping_[idxTrans].size()/src2DstMap.max_load_factor()));

    // Build mapping between global source element index and global destination element index.
    boost::unordered_map<int,std::vector<size_t> >().swap(globalElementIndexOnProc[elementPositionInGrid]);
    boost::unordered_map<int,int> tmpCounter;
    itTransWeight = itbTransWeight;
    for (itTransMap = itbTransMap; itTransMap != iteTransMap; ++itTransMap, ++itTransWeight)
    {
      const std::vector<int>& srcIndex = itTransMap->second;
      const std::vector<double>& weight = itTransWeight->second;
      for (int idx = 0; idx < srcIndex.size(); ++idx)
      {
        src2DstMap[srcIndex[idx]].push_back(make_pair(itTransMap->first, weight[idx]));
        if (1 == globalIndexOfTransformedElementOnProc.count(srcIndex[idx]) && (0 == tmpCounter.count(srcIndex[idx])))
        {
          tmpCounter[srcIndex[idx]] = 1;
          std::vector<int>& srcProc = globalIndexOfTransformedElementOnProc[srcIndex[idx]];
          for (int j = 0; j < srcProc.size(); ++j)
            globalElementIndexOnProc[elementPositionInGrid][srcProc[j]].push_back(srcIndex[idx]);
        }
      }
    }

    if (!isTransPosEmpty)
    {
      for (int idx = 0; idx < globalElementIndexOnProc.size(); ++idx)
      {
        if (idx != elementPositionInGrid)
        {
          boost::unordered_map<int,std::vector<size_t> >::iterator itb = globalElementIndexOnProc[idx].begin(), it,
                                                                   ite = globalElementIndexOnProc[idx].end();
          for (it = itb; it != ite; ++it) it->second[0] = transPos(idxTrans);
        }
      }
    }

    std::vector<std::vector<bool> > elementOnProc(axisDomainDstOrder.numElements(), std::vector<bool>(nbClient, false));
    boost::unordered_map<int,std::vector<size_t> >::const_iterator it, itb, ite;
    for (int idx = 0; idx < globalElementIndexOnProc.size(); ++idx)
    {
      itb = globalElementIndexOnProc[idx].begin();
      ite = globalElementIndexOnProc[idx].end();
      for (it = itb; it != ite; ++it) elementOnProc[idx][it->first] = true;
    }

    // Determine procs which contain global source index
    std::vector<bool> intersectedProc(nbClient, true);
    for (int idx = 0; idx < axisDomainDstOrder.numElements(); ++idx)
    {
      std::transform(elementOnProc[idx].begin(), elementOnProc[idx].end(),
                     intersectedProc.begin(), intersectedProc.begin(),
                     std::logical_and<bool>());
    }

    std::vector<int> srcRank;
    for (int idx = 0; idx < nbClient; ++idx)
    {
      if (intersectedProc[idx]) srcRank.push_back(idx);
    }

    // Ok, now compute global index of grid source and ones of grid destination
    computeGlobalGridIndexMapping(elementPositionInGrid,
                                  srcRank,
                                  src2DstMap,
                                  gridSrc,
                                  gridDst,
                                  globalElementIndexOnProc,
                                  globaIndexWeightFromSrcToDst);
  }
 }

/*!
  Compute mapping of global index of grid source and grid destination
  \param [in] elementPositionInGrid position of element in grid. E.x: grid composed of domain and axis, domain has position 0 and axis 1.
  \param [in] srcRank rank of client from which we demand global index of element source
  \param [in] src2DstMap mapping of global index of element source and global index of element destination
  \param[in] gridSrc Grid source
  \param[in] gridDst Grid destination
  \param[in] globalElementIndexOnProc Global index of element source on different client rank
  \param[out] globaIndexWeightFromSrcToDst Mapping of global index of grid source and grid destination
*/
void CGenericAlgorithmTransformation::computeGlobalGridIndexMapping(int elementPositionInGrid,
                                                                   const std::vector<int>& srcRank,
                                                                   boost::unordered_map<int, std::vector<std::pair<int,double> > >& src2DstMap,
                                                                   CGrid* gridSrc,
                                                                   CGrid* gridDst,
                                                                   std::vector<boost::unordered_map<int,std::vector<size_t> > >& globalElementIndexOnProc,
                                                                   SourceDestinationIndexMap& globaIndexWeightFromSrcToDst)
{
  std::vector<CDomain*> domainListSrcP = gridSrc->getDomains();
  std::vector<CAxis*> axisListSrcP = gridSrc->getAxis();
  std::vector<CScalar*> scalarListSrcP = gridSrc->getScalars();
  CArray<int,1> axisDomainSrcOrder = gridSrc->axis_domain_order;

  size_t nbElement = axisDomainSrcOrder.numElements();
  std::vector<size_t> nGlobSrc(nbElement);
  size_t globalSrcSize = 1;
  int domainIndex = 0, axisIndex = 0, scalarIndex = 0;
  for (int idx = 0; idx < nbElement; ++idx)
  {
    nGlobSrc[idx] = globalSrcSize;
    int elementDimension = axisDomainSrcOrder(idx);

    // If this is a domain
    if (2 == elementDimension)
    {
      globalSrcSize *= domainListSrcP[domainIndex]->nj_glo.getValue() * domainListSrcP[domainIndex]->ni_glo.getValue();
      ++domainIndex;
    }
    else if (1 == elementDimension) // So it's an axis
    {
      globalSrcSize *= axisListSrcP[axisIndex]->n_glo.getValue();
      ++axisIndex;
    }
    else
    {
      globalSrcSize *= 1;
      ++scalarIndex;
    }
  }

  std::vector<CDomain*> domainListDestP = gridDst->getDomains();
  std::vector<CAxis*> axisListDestP = gridDst->getAxis();
  std::vector<CScalar*> scalarListDestP = gridDst->getScalars();
  CArray<int,1> axisDomainDstOrder = gridDst->axis_domain_order;

  std::vector<size_t> nGlobDst(nbElement);
  size_t globalDstSize = 1;
  domainIndex = axisIndex = scalarIndex = 0;
  for (int idx = 0; idx < nbElement; ++idx)
  {
    nGlobDst[idx] = globalDstSize;
    int elementDimension = axisDomainDstOrder(idx);

    // If this is a domain
    if (2 == elementDimension)
    {
      globalDstSize *= domainListDestP[domainIndex]->nj_glo.getValue() * domainListDestP[domainIndex]->ni_glo.getValue();
      ++domainIndex;
    }
    else if (1 == elementDimension) // So it's an axis
    {
      globalDstSize *= axisListDestP[axisIndex]->n_glo.getValue();
      ++axisIndex;
    }
    else
    {
      globalDstSize *= 1;
      ++scalarIndex;
    }
  }

  for (int i = 0; i < srcRank.size(); ++i)
  {
    size_t ssize = 1;
    int rankSrc = srcRank[i];
    for (int idx = 0; idx < nbElement; ++idx)
    {
      ssize *= (globalElementIndexOnProc[idx][rankSrc]).size();
    }

    std::vector<int> idxLoop(nbElement,0);
    std::vector<int> currentIndexSrc(nbElement, 0);
    std::vector<int> currentIndexDst(nbElement, 0);
    int innnerLoopSize = (globalElementIndexOnProc[0])[rankSrc].size();
    size_t idx = 0;
    while (idx < ssize)
    {
      for (int ind = 0; ind < nbElement; ++ind)
      {
        if (idxLoop[ind] == (globalElementIndexOnProc[ind])[rankSrc].size())
        {
          idxLoop[ind] = 0;
          ++idxLoop[ind+1];
        }

        currentIndexDst[ind] = currentIndexSrc[ind] = (globalElementIndexOnProc[ind])[rankSrc][idxLoop[ind]];
      }

      for (int ind = 0; ind < innnerLoopSize; ++ind)
      {
        currentIndexDst[0] = currentIndexSrc[0] = (globalElementIndexOnProc[0])[rankSrc][ind];
        int globalElementDstIndexSize = 0;
        if (1 == src2DstMap.count(currentIndexSrc[elementPositionInGrid]))
        {
          globalElementDstIndexSize = src2DstMap[currentIndexSrc[elementPositionInGrid]].size();
        }

        std::vector<size_t> globalDstVecIndex(globalElementDstIndexSize,0);
        size_t globalSrcIndex = 0;
        for (int idxElement = 0; idxElement < nbElement; ++idxElement)
        {
          if (idxElement == elementPositionInGrid)
          {
            for (int k = 0; k < globalElementDstIndexSize; ++k)
            {
              globalDstVecIndex[k] += src2DstMap[currentIndexSrc[elementPositionInGrid]][k].first * nGlobDst[idxElement];
            }
          }
          else
          {
            for (int k = 0; k < globalElementDstIndexSize; ++k)
            {
              globalDstVecIndex[k] += currentIndexDst[idxElement] * nGlobDst[idxElement];
            }
          }
          globalSrcIndex += currentIndexSrc[idxElement] * nGlobSrc[idxElement];
        }

        for (int k = 0; k < globalElementDstIndexSize; ++k)
        {
          globaIndexWeightFromSrcToDst[rankSrc][globalSrcIndex].push_back(make_pair(globalDstVecIndex[k],src2DstMap[currentIndexSrc[elementPositionInGrid]][k].second ));
        }
        ++idxLoop[0];
      }
      idx += innnerLoopSize;
    }
  }
}

/*!
  Find out proc and global index of axis source which axis destination is on demande
  \param[in] scalar Scalar destination
  \param[in] scalar Scalar source
  \param[in] destGlobalIndexPositionInGrid Relative position of axis corresponds to other element of grid.
  \param[out] globalScalarIndexOnProc Global index of axis source on different procs
*/
void CGenericAlgorithmTransformation::computeExchangeScalarIndex(CScalar* scalarDst,
                                                                 CScalar* scalarSrc,
                                                                 CArray<size_t,1>& destGlobalIndexPositionInGrid,
                                                                 boost::unordered_map<int,std::vector<size_t> >& globalScalarIndexOnProc)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;
  int clientRank = client->clientRank;
  int clientSize = client->clientSize;

  globalScalarIndexOnProc.rehash(std::ceil(clientSize/globalScalarIndexOnProc.max_load_factor()));
  for (int idx = 0; idx < clientSize; ++idx)
  {
    globalScalarIndexOnProc[idx].push_back(0);
  }
}

/*!
  Find out proc and global index of axis source which axis destination is on demande
  \param[in] axisDst Axis destination
  \param[in] axisSrc Axis source
  \param[in] destGlobalIndexPositionInGrid Relative position of axis corresponds to other element of grid.
  \param[out] globalAxisIndexOnProc Global index of axis source on different procs
*/
void CGenericAlgorithmTransformation::computeExchangeAxisIndex(CAxis* axisDst,
                                                               CAxis* axisSrc,
                                                               CArray<size_t,1>& destGlobalIndexPositionInGrid,
                                                               boost::unordered_map<int,std::vector<size_t> >& globalAxisIndexOnProc)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;
  int clientRank = client->clientRank;
  int clientSize = client->clientSize;

  size_t globalIndex;
  int nIndexSize = axisSrc->index.numElements();
  CClientClientDHTInt::Index2VectorInfoTypeMap globalIndex2ProcRank;
  globalIndex2ProcRank.rehash(std::ceil(nIndexSize/globalIndex2ProcRank.max_load_factor()));
  for (int idx = 0; idx < nIndexSize; ++idx)
  {
    globalIndex = axisSrc->index(idx);
    globalIndex2ProcRank[globalIndex].push_back(clientRank);
  }

  CClientClientDHTInt dhtIndexProcRank(globalIndex2ProcRank, client->intraComm);
  CArray<size_t,1> globalAxisIndex(axisDst->index.numElements());
  for (int idx = 0; idx < globalAxisIndex.numElements(); ++idx)
  {
    globalAxisIndex(idx) = axisDst->index(idx);
  }
  dhtIndexProcRank.computeIndexInfoMapping(globalAxisIndex);

  std::vector<int> countIndex(clientSize,0);
  const CClientClientDHTInt::Index2VectorInfoTypeMap& computedGlobalIndexOnProc = dhtIndexProcRank.getInfoIndexMap();
  CClientClientDHTInt::Index2VectorInfoTypeMap::const_iterator itb = computedGlobalIndexOnProc.begin(), it,
                                                               ite = computedGlobalIndexOnProc.end();
  for (it = itb; it != ite; ++it)
  {
    const std::vector<int>& procList = it->second;
    for (int idx = 0; idx < procList.size(); ++idx) ++countIndex[procList[idx]];
  }

  globalAxisIndexOnProc.rehash(std::ceil(clientSize/globalAxisIndexOnProc.max_load_factor()));
  for (int idx = 0; idx < clientSize; ++idx)
  {
    if (0 != countIndex[idx])
    {
      globalAxisIndexOnProc[idx].resize(countIndex[idx]);
      countIndex[idx] = 0;
    }
  }

  for (it = itb; it != ite; ++it)
  {
    const std::vector<int>& procList = it->second;
    for (int idx = 0; idx < procList.size(); ++idx)
    {
      globalAxisIndexOnProc[procList[idx]][countIndex[procList[idx]]] = it->first;
      ++countIndex[procList[idx]];
    }
  }
}

/*!
  Find out proc and global index of domain source which domain destination is on demande
  \param[in] domainDst Domain destination
  \param[in] domainSrc Domain source
  \param[in] destGlobalIndexPositionInGrid Relative position of domain corresponds to other element of grid.
  \param[out] globalDomainIndexOnProc Global index of domain source on different procs
*/
void CGenericAlgorithmTransformation::computeExchangeDomainIndex(CDomain* domainDst,
                                                                 CDomain* domainSrc,
                                                                 CArray<size_t,1>& destGlobalIndexPositionInGrid,
                                                                 boost::unordered_map<int,std::vector<size_t> >& globalDomainIndexOnProc)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;
  int clientRank = client->clientRank;
  int clientSize = client->clientSize;

  int niGlobSrc = domainSrc->ni_glo.getValue();
  size_t globalIndex;
  int i_ind, j_ind;
  int nIndexSize = (destGlobalIndexPositionInGrid.isEmpty()) ? domainSrc->i_index.numElements()
                                                             : destGlobalIndexPositionInGrid.numElements();
  CClientClientDHTInt::Index2VectorInfoTypeMap globalIndex2ProcRank;
  globalIndex2ProcRank.rehash(std::ceil(nIndexSize/globalIndex2ProcRank.max_load_factor()));
  if (destGlobalIndexPositionInGrid.isEmpty())
  {
    for (int idx = 0; idx < nIndexSize; ++idx)
    {
      i_ind=domainSrc->i_index(idx) ;
      j_ind=domainSrc->j_index(idx) ;

      globalIndex = i_ind + j_ind * niGlobSrc;
      globalIndex2ProcRank[globalIndex].resize(1);
      globalIndex2ProcRank[globalIndex][0] = clientRank;
    }
  }
  else
  {
    for (int idx = 0; idx < nIndexSize; ++idx)
    {
      globalIndex2ProcRank[destGlobalIndexPositionInGrid(idx)].push_back(clientRank);
    }
  }

  CArray<size_t,1> globalDomainIndex;
  if (destGlobalIndexPositionInGrid.isEmpty())
  {
    int niGlobDst = domainDst->ni_glo.getValue();
    globalDomainIndex.resize(domainDst->i_index.numElements());
    nIndexSize = domainDst->i_index.numElements();

    for (int idx = 0; idx < nIndexSize; ++idx)
    {
      i_ind=domainDst->i_index(idx) ;
      j_ind=domainDst->j_index(idx) ;

      globalDomainIndex(idx) = i_ind + j_ind * niGlobDst;
    }
  }
  else
  {
    globalDomainIndex.reference(destGlobalIndexPositionInGrid);
  }

  CClientClientDHTInt dhtIndexProcRank(globalIndex2ProcRank, client->intraComm);
  dhtIndexProcRank.computeIndexInfoMapping(globalDomainIndex);

  std::vector<int> countIndex(clientSize,0);
  const CClientClientDHTInt::Index2VectorInfoTypeMap& computedGlobalIndexOnProc = dhtIndexProcRank.getInfoIndexMap();
  CClientClientDHTInt::Index2VectorInfoTypeMap::const_iterator itb = computedGlobalIndexOnProc.begin(), it,
                                                               ite = computedGlobalIndexOnProc.end();
  for (it = itb; it != ite; ++it)
  {
    const std::vector<int>& procList = it->second;
    for (int idx = 0; idx < procList.size(); ++idx) ++countIndex[procList[idx]];
  }

  globalDomainIndexOnProc.rehash(std::ceil(clientSize/globalDomainIndexOnProc.max_load_factor()));
  for (int idx = 0; idx < clientSize; ++idx)
  {
    if (0 != countIndex[idx])
    {
      globalDomainIndexOnProc[idx].resize(countIndex[idx]);
      countIndex[idx] = 0;
    }
  }

  for (it = itb; it != ite; ++it)
  {
    const std::vector<int>& procList = it->second;
    for (int idx = 0; idx < procList.size(); ++idx)
    {
      globalDomainIndexOnProc[procList[idx]][countIndex[procList[idx]]] = it->first;
      ++countIndex[procList[idx]];
    }
  }
}

/*!
  Compute index mapping between element source and element destination with an auxiliary inputs which determine
position of each mapped index in global index of grid destination.
  \param [in] dataAuxInputs auxiliary inputs
*/
void CGenericAlgorithmTransformation::computeIndexSourceMapping(const std::vector<CArray<double,1>* >& dataAuxInputs)
{
  computeIndexSourceMapping_(dataAuxInputs);
}

std::vector<StdString> CGenericAlgorithmTransformation::getIdAuxInputs()
{
  return idAuxInputs_;
}

CGenericAlgorithmTransformation::AlgoTransType CGenericAlgorithmTransformation::type()
{
  return type_;
}

}
