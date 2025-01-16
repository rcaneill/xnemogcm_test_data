#include "temporal_filter.hpp"
#include "functor_type.hpp"
#include "calendar_util.hpp"

namespace xios
{
  static func::CFunctor* createFunctor(const std::string& opId, bool ignoreMissingValue, CArray<double, 1>& tmpData);

  CTemporalFilter::CTemporalFilter(CGarbageCollector& gc, const std::string& opId,
                                   const CDate& initDate, const CDuration samplingFreq, const CDuration samplingOffset, const CDuration opFreq,
                                   bool ignoreMissingValue /*= false*/)
    : CFilter(gc, 1, this)
    , functor(createFunctor(opId, ignoreMissingValue, tmpData))
    , isOnceOperation(functor->timeType() == func::CFunctor::once)
    , isInstantOperation(functor->timeType() == func::CFunctor::instant)
    , samplingFreq(samplingFreq)
    , samplingOffset(samplingOffset)
    , opFreq(opFreq)
    , offsetMonth(0, this->samplingOffset.month, 0, 0, 0, 0, 0)
    , offsetAllButMonth(this->samplingOffset.year, 0 , this->samplingOffset.day,
                        this->samplingOffset.hour, this->samplingOffset.minute,
                        this->samplingOffset.second, this->samplingOffset.timestep)
    , initDate(initDate)
//    , nextSamplingDate(initDate + (this->samplingOffset + initDate.getRelCalendar().getTimeStep()))
    , nextSamplingDate(initDate + offsetMonth + ( offsetAllButMonth + initDate.getRelCalendar().getTimeStep()))
    , nbOperationDates(1)
    , nbSamplingDates(0)
//    , nextOperationDate(initDate + opFreq + this->samplingOffset)
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
//        outputResult = (data[0]->date + samplingFreq > nextOperationDate);
        outputResult = (data[0]->date  > initDate + nbOperationDates*opFreq - samplingFreq + offsetMonth + offsetAllButMonth);
        copyLess = (isInstantOperation && usePacket && outputResult);
      }

      if (usePacket)
      {
        nbSamplingDates ++;
        if (!copyLess)
        {
          if (!tmpData.numElements())
            tmpData.resize(data[0]->data.numElements());

          (*functor)(data[0]->data);
        }

        nextSamplingDate = ((initDate + offsetMonth) + nbSamplingDates * samplingFreq) + offsetAllButMonth + initDate.getRelCalendar().getTimeStep();
      }

      if (outputResult)
      {
        nbOperationDates ++;
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
//        nextOperationDate = initDate + samplingFreq + nbOperationDates*opFreq - samplingFreq + offsetMonth + offsetAllButMonth;
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
//    return isOnceOperation ? isFirstOperation : (date >= nextSamplingDate || date + samplingFreq > nextOperationDate);
    return isOnceOperation ? isFirstOperation : (date >= nextSamplingDate || date > initDate + nbOperationDates*opFreq - samplingFreq + offsetMonth + offsetAllButMonth);
  }

  static func::CFunctor* createFunctor(const std::string& opId, bool ignoreMissingValue, CArray<double, 1>& tmpData)
  {
    func::CFunctor* functor = NULL;

    double defaultValue = std::numeric_limits<double>::quiet_NaN();

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
