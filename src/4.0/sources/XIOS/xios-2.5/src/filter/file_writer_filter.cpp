#include "file_writer_filter.hpp"
#include "exception.hpp"
#include "field.hpp"
#include "utils.hpp"

namespace xios
{
  CFileWriterFilter::CFileWriterFilter(CGarbageCollector& gc, CField* field)
    : CInputPin(gc, 1)
    , field(field)
  {
    if (!field)
      ERROR("CFileWriterFilter::CFileWriterFilter(CField* field)",
            "The field cannot be null.");
  }

  void CFileWriterFilter::onInputReady(std::vector<CDataPacketPtr> data)
  {
    const bool detectMissingValue = (!field->detect_missing_value.isEmpty()
                                      && !field->default_value.isEmpty()
                                      && field->detect_missing_value == true);

    CArray<double, 1> dataArray = (detectMissingValue) ? data[0]->data.copy() : data[0]->data;

    if (detectMissingValue)
    {
      const double missingValue = field->default_value;
      const size_t nbData = dataArray.numElements();
      for (size_t idx = 0; idx < nbData; ++idx)
      {
        if (NumTraits<double>::isNan(dataArray(idx)))
          dataArray(idx) = missingValue;
      }
    }

    field->sendUpdateData(dataArray);
  }

  bool CFileWriterFilter::mustAutoTrigger() const
  {
    return true;
  }

  bool CFileWriterFilter::isDataExpected(const CDate& date) const
  {
    return true;
  }
} // namespace xios
