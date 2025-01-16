#ifndef __XIOS_CTemporalFilter__
#define __XIOS_CTemporalFilter__

#include "filter.hpp"
#include <boost/smart_ptr/scoped_ptr.hpp>
#include "functor.hpp"
#include "array_new.hpp"

namespace xios
{
  /*!
   * A generic temporal filter with one input slot wrapping any type of temporal operation.
   */
  class CTemporalFilter : public CFilter, public IFilterEngine
  {
    public:
      /*!
       * Constructs a temporal filter wrapping the specified temporal operation.
       *
       * \param gc the associated garbage collector
       * \param opId the string identifying the temporal operation
       * \param initDate the origin of time
       * \param samplingFreq the sampling frequency, i.e. the frequency at which the input data will be used
       * \param samplingOffset the sampling offset, i.e. the offset after which the input data will be used
       * \param opFreq the operation frequency, i.e. the frequency at which the output data will be computed
       * \param ignoreMissingValue true if and only if the missing value must be ignored
                                   when doing the operation
       */
      CTemporalFilter(CGarbageCollector& gc, const std::string& opId,
                      const CDate& initDate, const CDuration samplingFreq, const CDuration samplingOffset, const CDuration opFreq,
                      bool ignoreMissingValue = false);

      /*!
       * Applies the temporal operation to the input data and returns the result when it is ready.
       *
       * \param data a vector of packets corresponding to each slot (one in this case)
       * \return the result of the temporal operation or null if it is not ready yet
       */
      CDataPacketPtr virtual apply(std::vector<CDataPacketPtr> data);

      /*!
       * Tests if the filter must auto-trigger.
       *
       * \return true if the filter must auto-trigger
       */
      bool virtual mustAutoTrigger() const;

      /*!
       * Tests whether data is expected for the specified date.
       *
       * \param date the date associated to the data
       */
      bool virtual isDataExpected(const CDate& date) const;

    private:
      // Warning the declaration order matters here, double-check the constructor before changing it
      CArray<double, 1> tmpData; //!< The array of data used for temporary storage
      const boost::scoped_ptr<func::CFunctor> functor; //!< The functor corresponding to the temporal operation
      const bool isOnceOperation; //!< True if the operation should be computed just once
      const bool isInstantOperation; //!< True if the operation is instant
      const CDuration samplingFreq; //!< The sampling frequency, i.e. the frequency at which the input data will be used
      const CDuration samplingOffset; //!< The sampling offset, i.e. the offset after which the input data will be used
      const CDuration opFreq; //!< The operation frequency, i.e. the frequency at which the output data will be computed
      const CDuration offsetMonth; //!< The month duration of samplingOffset
      CDuration offsetAllButMonth; //!< All but the month duration of samplingOffset
      const CDate initDate;
      CDate nextSamplingDate; //!< The date of the next sampling
      int nbOperationDates; //!< The number of times an operation is performed
      int nbSamplingDates;
//      CDate nextOperationDate; //!< The date of the next operation
      bool isFirstOperation; //!< True before the first operation was been computed
  }; // class CTemporalFilter
} // namespace xios

#endif //__XIOS_CTemporalFilter__
