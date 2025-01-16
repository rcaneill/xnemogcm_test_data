/*!
   \file axis_algorithm_zoom.cpp
   \author Ha NGUYEN
   \since 03 June 2015
   \date 12 June 2015

   \brief Algorithm for zooming on an axis.
 */
#include "axis_algorithm_zoom.hpp"
#include "axis.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"
#include "zoom_axis.hpp"

namespace xios {
CGenericAlgorithmTransformation* CAxisAlgorithmZoom::create(CGrid* gridDst, CGrid* gridSrc,
                                                           CTransformation<CAxis>* transformation,
                                                           int elementPositionInGrid,
                                                           std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                           std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                           std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                           std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                           std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                           std::map<int, int>& elementPositionInGridDst2DomainPosition)
{
  std::vector<CAxis*> axisListDestP = gridDst->getAxis();
  std::vector<CAxis*> axisListSrcP  = gridSrc->getAxis();

  CZoomAxis* zoomAxis = dynamic_cast<CZoomAxis*> (transformation);
  int axisDstIndex = elementPositionInGridDst2AxisPosition[elementPositionInGrid];
  int axisSrcIndex = elementPositionInGridSrc2AxisPosition[elementPositionInGrid];

  return (new CAxisAlgorithmZoom(axisListDestP[axisDstIndex], axisListSrcP[axisSrcIndex], zoomAxis));
}
bool CAxisAlgorithmZoom::registerTrans()
{
  return CGridTransformationFactory<CAxis>::registerTransformation(TRANS_ZOOM_AXIS, create);
}

CAxisAlgorithmZoom::CAxisAlgorithmZoom(CAxis* axisDestination, CAxis* axisSource, CZoomAxis* zoomAxis)
: CAxisAlgorithmTransformation(axisDestination, axisSource)
{
  zoomAxis->checkValid(axisSource);
  zoomBegin_ = zoomAxis->begin.getValue();
  zoomSize_  = zoomAxis->n.getValue();
  zoomEnd_   = zoomBegin_ + zoomSize_ - 1;

  if (zoomSize_ > axisSource->n_glo.getValue())
  {
    ERROR("CAxisAlgorithmZoom::CAxisAlgorithmZoom(CAxis* axisDestination, CAxis* axisSource, CZoomAxis* zoomAxis)",
           << "Zoom size is greater than global size of axis source"
           << "Global size of axis source " <<axisSource->getId() << " is " << axisSource->n_glo.getValue()  << std::endl
           << "Zoom size is " << zoomSize_ );
  }

  if (!zoomAxis->index.isEmpty())
  {
    int sz = zoomAxis->index.numElements();
    zoomIndex_.resize(sz);
    for (int i = 0; i < sz; ++i)
      zoomIndex_[i] = zoomAxis->index(i);

    std::sort(zoomIndex_.begin(), zoomIndex_.end());
  }

}

/*!
  Compute the index mapping between axis on grid source and one on grid destination
*/
void CAxisAlgorithmZoom::computeIndexSourceMapping_(const std::vector<CArray<double,1>* >& dataAuxInputs)
{
  // We use all index of source and destination to calculate the mapping index of zoom.
  // The server who receives the "zoomed" fields will decide whether it will forward these fields or write "real zoomed" fields into file
  // That means servers need to change to cover this problem.
  StdSize niSource = axisSrc_->n.getValue();
  StdSize ibeginSource = axisSrc_->begin.getValue();
  StdSize iendSource = ibeginSource + niSource - 1;

  StdSize ibegin = std::max(ibeginSource, zoomBegin_);
  StdSize iend = std::min(iendSource, zoomEnd_);
  StdSize ni = iend + 1 - ibegin;
  if (iend < ibegin) ni = 0;

  this->transformationMapping_.resize(1);
  this->transformationWeight_.resize(1);

  TransformationIndexMap& transMap = this->transformationMapping_[0];
  TransformationWeightMap& transWeight = this->transformationWeight_[0];

  if (!zoomIndex_.empty())
  {
    std::vector<int>::iterator itZoomBegin, itZoomEnd;
    itZoomBegin = std::lower_bound(zoomIndex_.begin(), zoomIndex_.end(), ibeginSource);
    itZoomEnd   = std::upper_bound(zoomIndex_.begin(), zoomIndex_.end(), iendSource);            
    for (; itZoomBegin != itZoomEnd; ++itZoomBegin)
    {
      transMap[*itZoomBegin].push_back(*itZoomBegin);
      transWeight[*itZoomBegin].push_back(1.0);
    }
  }
  else
  {
    for (StdSize idx = 0; idx < ni; ++idx)
    {
      transMap[ibegin+idx].push_back(ibegin+idx);
      transWeight[ibegin+idx].push_back(1.0);
    }
  }

  updateZoom();
  // updateAxisDestinationMask();
}

/*!
  After a zoom on axis, it should be certain that (global) zoom begin and (global) zoom size are updated
*/
void CAxisAlgorithmZoom::updateZoom()
{
  axisDest_->global_zoom_begin = zoomBegin_;
  axisDest_->global_zoom_n  = zoomSize_;
  if (!zoomIndex_.empty())
  {
    axisDest_->global_zoom_index.resize(zoomIndex_.size());
    std::copy(zoomIndex_.begin(), zoomIndex_.end(), axisDest_->global_zoom_index.begin());
  }
}

/*!
  Update mask on axis
  Because only zoomed region on axis is not masked, the remaining must be masked to make sure
correct index be extracted
*/
// void CAxisAlgorithmZoom::updateAxisDestinationMask()
// {
//   StdSize niMask = axisDest_->mask.numElements();
//   StdSize iBeginMask = axisDest_->begin.getValue();
//   StdSize globalIndexMask = 0;
//   TransformationIndexMap& transMap = this->transformationMapping_[0];
//   TransformationIndexMap::const_iterator ite = (transMap).end();
//   for (StdSize idx = 0; idx < niMask; ++idx)
//   {
//     globalIndexMask = iBeginMask + idx;
//     if (transMap.find(globalIndexMask) == ite)
//       (axisDest_->mask)(idx) = false;
//   }
// }

}
