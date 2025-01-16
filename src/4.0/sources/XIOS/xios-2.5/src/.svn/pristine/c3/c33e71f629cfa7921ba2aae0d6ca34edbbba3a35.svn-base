
#ifndef __FIELD_IMPL_HPP__
#define __FIELD_IMPL_HPP__

#include "xios_spl.hpp"
#include "field.hpp"
#include "context.hpp"
#include "grid.hpp"
#include "timer.hpp"
#include "array_new.hpp"
#include "source_filter.hpp"
#include "store_filter.hpp"


namespace xios {

  template <int N>
  void CField::setData(const CArray<double, N>& _data)
  {
    if (clientSourceFilter)
    {
      if (check_if_active.isEmpty() || (!check_if_active.isEmpty() && (!check_if_active) || isActive(true)))
        clientSourceFilter->streamData(CContext::getCurrent()->getCalendar()->getCurrentDate(), _data);
    }
    else if (instantDataFilter)
      ERROR("void CField::setData(const CArray<double, N>& _data)",
            << "Impossible to receive data from the model for a field [ id = " << getId() << " ] with a reference or an arithmetic operation.");
  }

  template <int N>
  void CField::getData(CArray<double, N>& _data) const
  {
    if (storeFilter)
    {
      CDataPacket::StatusCode status = storeFilter->getData(CContext::getCurrent()->getCalendar()->getCurrentDate(), _data);

      if (status == CDataPacket::END_OF_STREAM)
        ERROR("void CField::getData(CArray<double, N>& _data) const",
              << "Impossible to access field data, all the records of the field [ id = " << getId() << " ] have been already read.");
    }
    else
    {
      ERROR("void CField::getData(CArray<double, N>& _data) const",
            << "Impossible to access field data, the field [ id = " << getId() << " ] does not have read access.");
    }
  }
} // namespace xios

#endif
