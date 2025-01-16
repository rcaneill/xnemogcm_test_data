#ifndef __XIOS_COutputPin__
#define __XIOS_COutputPin__

#include "garbage_collector.hpp"
#include "input_pin.hpp"

namespace xios
{
  /*!
   * An output pin handles the connections with downstream filters.
   */
  class COutputPin : public InvalidableObject
  {
    public:
      /*!
       * Constructs an ouput pin with manual or automatic trigger
       * and an associated garbage collector.
       *
       * \param gc the garbage collector associated with this ouput pin
       * \param slotsCount the number of slots
       */
      COutputPin(CGarbageCollector& gc, bool manualTrigger = false);

      /*!
       * Connects to a specific slot of the input pin of a downstream filter.
       * Note that the output pin holds a reference on the downstream filter.
       *
       * \param inputPin the input pin to connect
       * \param inputSlot the input slot number
       */
      void connectOutput(boost::shared_ptr<CInputPin> inputPin, size_t inputSlot);

      /*!
       * Triggers the output of any buffered packet for the specified timestamp.
       *
       * \param timestamp the timestamp for which we are triggering the output
       */
      void virtual trigger(Time timestamp);

      /*!
       * Tests if the pin can be triggered.
       *
       * \return true if the pin can be triggered
       */
      bool virtual canBeTriggered() const;

      /*!
       * Tests if the pin must auto-trigger.
       *
       * \return true if the pin must auto-trigger
       */
      bool virtual mustAutoTrigger() const;

      /*!
       * Tests whether data is expected for the specified date.
       *
       * \param date the date associated to the data
       */
      bool virtual isDataExpected(const CDate& date) const;

      /*!
       * Removes all pending packets which are older than the specified timestamp.
       *
       * \param timestamp the timestamp used for invalidation
       */
      void virtual invalidate(Time timestamp);

    protected:
      /*!
       * Function triggered when a packet is ready to be delivered.
       *
       * \param packet the packet ready for output
       */
      void onOutputReady(CDataPacketPtr packet);

      /*!
       * Informs the downstream pins that this output pin should be triggered.
       */
      void setOutputTriggers();

    private:
      /*!
       * Delivers an output packet to the downstreams filter.
       *
       * \param packet the packet to output
       */
      void deliverOuput(CDataPacketPtr packet);

      CGarbageCollector& gc; //!< The garbage collector associated to the output pin

      //!< Whether the ouput should be triggered manually
      bool manualTrigger;

      //!< The list of connected filters and the corresponding slot numbers
      std::vector<std::pair<boost::shared_ptr<CInputPin>, size_t> > outputs;

      //! Output buffer, store the packets until the output is triggered
      std::map<Time, CDataPacketPtr> outputPackets;
  }; // class COutputPin
} // namespace xios

#endif //__XIOS_COutputPin__
