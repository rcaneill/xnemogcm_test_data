/*!
   \file domain_algorithm_zoom.cpp
   \author Ha NGUYEN
   \since 02 Jul 2015
   \date 02 Jul 2015

   \brief Algorithm for zooming on an domain.
 */
#include "domain_algorithm_zoom.hpp"
#include "zoom_domain.hpp"
#include "domain.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"

namespace xios {
CGenericAlgorithmTransformation* CDomainAlgorithmZoom::create(CGrid* gridDst, CGrid* gridSrc,
                                                             CTransformation<CDomain>* transformation,
                                                             int elementPositionInGrid,
                                                             std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                             std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                             std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                             std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                             std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                             std::map<int, int>& elementPositionInGridDst2DomainPosition)
{
  std::vector<CDomain*> domainListDestP = gridDst->getDomains();
  std::vector<CDomain*> domainListSrcP  = gridSrc->getDomains();

  CZoomDomain* zoomDomain = dynamic_cast<CZoomDomain*> (transformation);
  int domainDstIndex = elementPositionInGridDst2DomainPosition[elementPositionInGrid];
  int domainSrcIndex = elementPositionInGridSrc2DomainPosition[elementPositionInGrid];

  return (new CDomainAlgorithmZoom(domainListDestP[domainDstIndex], domainListSrcP[domainSrcIndex], zoomDomain));
}

bool CDomainAlgorithmZoom::registerTrans()
{
  return CGridTransformationFactory<CDomain>::registerTransformation(TRANS_ZOOM_DOMAIN, create);
}

CDomainAlgorithmZoom::CDomainAlgorithmZoom(CDomain* domainDestination, CDomain* domainSource, CZoomDomain* zoomDomain)
: CDomainAlgorithmTransformation(domainDestination, domainSource)
{
  zoomDomain->checkValid(domainSource);
  zoomIBegin_ = zoomDomain->ibegin.getValue();
  zoomJBegin_ = zoomDomain->jbegin.getValue();

  zoomNi_  = zoomDomain->ni.getValue();
  zoomNj_  = zoomDomain->nj.getValue();

  zoomIEnd_ = zoomIBegin_ + zoomNi_ - 1;
  zoomJEnd_ = zoomJBegin_ + zoomNj_ - 1;

  if (zoomNi_ > domainSource->ni_glo.getValue())
  {
    ERROR("CDomainAlgorithmZoom::CDomainAlgorithmZoom(CDomain* domainDestination, CDomain* domainSource, CZoomDomain* zoomDomain)",
           << "Zoom size is greater than size of domain source"
           << "Size ni_glo of domain source " <<domainSource->getId() << " is " << domainSource->ni_glo.getValue()  << std::endl
           << "Zoom size is " << zoomNi_ );
  }

  if (zoomNj_ > domainSource->nj_glo.getValue())
  {
    ERROR("CDomainAlgorithmZoom::CDomainAlgorithmZoom(CDomain* domainDestination, CDomain* domainSource, CZoomDomain* zoomDomain)",
           << "Zoom size is greater than size of domain source"
           << "Size nj_glo of domain source " <<domainSource->getId() << " is " << domainSource->nj_glo.getValue()  << std::endl
           << "Zoom size is " << zoomNj_ );
  }
}

/*!
  Compute the index mapping between domain on grid source and one on grid destination
*/
void CDomainAlgorithmZoom::computeIndexSourceMapping_(const std::vector<CArray<double,1>* >& dataAuxInputs)
{

  int niGlob = domainSrc_->ni_glo.getValue();
  int njGlob = domainSrc_->nj_glo.getValue();

  this->transformationMapping_.resize(1);
  this->transformationWeight_.resize(1);

  TransformationIndexMap& transMap = this->transformationMapping_[0];
  TransformationWeightMap& transWeight = this->transformationWeight_[0];

  int domainGlobalIndex;
  int iglob ;
  int jglob ;
  const CArray<int,1>& i_index = domainSrc_->i_index.getValue() ;
  const CArray<int,1>& j_index = domainSrc_->j_index.getValue() ;

  int nglo = i_index.numElements() ;
  for (size_t i = 0; i < nglo ; ++i)
  {
    iglob=i_index(i) ; jglob=j_index(i) ;
    if (iglob>=zoomIBegin_ && iglob<=zoomIEnd_ && jglob>=zoomJBegin_ && jglob<=zoomJEnd_)
    {
      domainGlobalIndex = jglob*niGlob + iglob;
      transMap[domainGlobalIndex].push_back(domainGlobalIndex);
      transWeight[domainGlobalIndex].push_back(1.0);
    }
  }
  updateZoom();
}

/*!
  After a zoom on domain, it should be certain that (global) zoom begin and (global) zoom size are updated
*/
void CDomainAlgorithmZoom::updateZoom()
{
  domainDest_->global_zoom_ibegin = zoomIBegin_;
  domainDest_->global_zoom_jbegin = zoomJBegin_;
  domainDest_->global_zoom_ni  = zoomNi_;
  domainDest_->global_zoom_nj  = zoomNj_;
}

/*!
  Update mask on domain
  Because only zoomed region on domain is not masked, the remaining must be masked to make sure
correct index be extracted
*/
// void CDomainAlgorithmZoom::updateDomainDestinationMask()
// {
//   int niMask     = domainDest_->ni.getValue();
//   int iBeginMask = domainDest_->ibegin.getValue();
//   int njMask     = domainDest_->nj.getValue();
//   int jBeginMask = domainDest_->jbegin.getValue();
//   int niGlob = domainDest_->ni_glo.getValue();
//   int globalIndexMask = 0;

//   TransformationIndexMap& transMap = this->transformationMapping_[0];
//   TransformationIndexMap::const_iterator ite = (transMap).end();
//   for (int j = 0; j < njMask; ++j)
//   {
//     for (int i = 0; i < niMask; ++i)
//     {
//       globalIndexMask = (j+jBeginMask) * niGlob + (i + iBeginMask);
//       if (transMap.find(globalIndexMask) == ite)
//         (domainDest_->mask_1d)(i+j*niMask) = false;
//     }
//   }
// }

}
