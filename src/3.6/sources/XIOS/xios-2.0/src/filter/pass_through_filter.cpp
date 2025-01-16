#include "pass_through_filter.hpp"

namespace xios
{
  CPassThroughFilter::CPassThroughFilter(CGarbageCollector& gc)
    : CFilter(gc, 1, this)
  { /* Nothing to do */ }

  CDataPacketPtr CPassThroughFilter::apply(std::vector<CDataPacketPtr> data)
  {
    return data[0];
  }
} // namespace xios
