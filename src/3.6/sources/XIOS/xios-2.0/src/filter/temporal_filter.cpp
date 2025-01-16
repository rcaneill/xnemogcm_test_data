#include "temporal_filter.hpp"
#include "functor_type.hpp"
#include "calendar_util.hpp"

namespace xios
{
  static func::CFunctor* createFunctor(const std::string& opId, bool ignoreMissingValue, double missingValue, CArray<double, 1>& tmpData);

  CTemporalFilter::CTemporalFilter(CGarbageCollector& gc, const std::string& opId,
                                   const CDate& initDate, const CDuration samplingFreq, const CDuration samplingOffset, const CDuration opFreq,
                                   bool ignoreMissingValue /*= false*/, double missingValue /*= 0.0*/)
    : CFilter(gc, 1, this)
    , functor(createFunctor(opId, ignoreMissingValue, missingValue, tmpData))
    , isOnceOperation(functor->timeType() == func::CFunctor::once)
    , isInstantOperation(functor->timeType() == func::CFunctor::instant)
    , samplingFreq(samplingFreq)
    , samplingOffset(samplingOffset)
    , opFreq(opFreq)
    , nextSamplingDate(initDate + this->samplingOffset + initDate.getRelCalendar().getTimeStep())
    , nextOperationDate(initDate + this->samplingOffset + opFreq)
    , isFirstOperation(true)
  {
  }

  CDataPacketPtr CTemporalFilter::apply(std::vector<CDataPacketPtr> data)
  {
    CDataPacketPtr packet;

    if (data[0]->status != CDataPacket::END_OF_STREAM)
    {
      bool usePacket, outputResult, copyLess;
      if (isOnceOperation)
        usePacket = outputResult = copyLess = isFirstOperation;
      else
      {
        usePacket = (data[0]->date >= nextSamplingDate);
        outputResult = (data[0]->date + samplingFreq > nextOperationDate);
        copyLess = (isInstantOperation && usePacket && outputResult);
      }

      if (usePacket)
      {
        if (!copyLess)
        {
          if (!tmpData.numElements())
            tmpData.resize(data[0]->data.numElements());

          (*functor)(data[0]->data);
        }

        nextSamplingDate = nextSamplingDate + samplingFreq;
      }

      if (outputResult)
      {
        if (!copyLess)
        {
          functor->final();

          packet = CDataPacketPtr(new CDataPacket);
          packet->date = data[0]->date;
          packet->timestamp = data[0]->timestamp;
          packet->status = data[0]->status;
          packet->data.resize(tmpData.numElements());
          packet->data = tmpData;
        }
        else
          packet = data[0];

        isFirstOperation = false;
        nextOperationDate = nextOperationDate + samplingFreq + opFreq - samplingFreq;
      }
    }

    return packet;
  }

  bool CTemporalFilter::mustAutoTrigger() const
  {
    return true;
  }

  bool CTemporalFilter::isDataExpected(const CDate& date) const
  {
    return isOnceOperation ? isFirstOperation : (date >= nextSamplingDate || date + samplingFreq > nextOperationDate);
  }

  static func::CFunctor* createFunctor(const std::string& opId, bool ignoreMissingValue, double missingValue, CArray<double, 1>& tmpData)
  {
    func::CFunctor* functor = NULL;

    double defaultValue = ignoreMissingValue ? std::numeric_limits<double>::quiet_NaN() : missingValue;

#define DECLARE_FUNCTOR(MType, mtype) \
    if (opId.compare(#mtype) == 0) \
    { \
      if (ignoreMissingValue) \
      { \
        functor = new func::C##MType(tmpData, defaultValue); \
      } \
      else \
      { \
        functor = new func::C##MType(tmpData); \
      } \
    }

#include "functor_type.conf"

    if (!functor)
      ERROR("createFunctor(const std::string& opId, ...)",
            << "\"" << opId << "\" is not a valid operation.");

    return functor;
  }
} // namespace xios
