#include "store_filter.hpp"
#include "context.hpp"
#include "grid.hpp"
#include "timer.hpp"

namespace xios
{
  CStoreFilter::CStoreFilter(CGarbageCollector& gc, CContext* context, CGrid* grid,
                             bool detectMissingValues /*= false*/, double missingValue /*= 0.0*/)
    : CInputPin(gc, 1)
    , gc(gc)
    , context(context)
    , grid(grid)
    , detectMissingValues(detectMissingValues)
    , missingValue(missingValue)
  {
    if (!context)
      ERROR("CStoreFilter::CStoreFilter(CContext* context, CGrid* grid)",
            "Impossible to construct a store filter without providing a context.");
    if (!grid)
      ERROR("CStoreFilter::CStoreFilter(CContext* context, CGrid* grid)",
            "Impossible to construct a store filter without providing a grid.");
  }

  CConstDataPacketPtr CStoreFilter::getPacket(Time timestamp)
  {
    CTimer timer("CStoreFilter::getPacket");
    CConstDataPacketPtr packet;
    const double timeout = CXios::recvFieldTimeout;

    do
    {
      if (canBeTriggered())
        trigger(timestamp);

      timer.resume();

      std::map<Time, CDataPacketPtr>::const_iterator it = packets.find(timestamp);
      if (it != packets.end())
        packet = it->second;
      else // if the packet is not available yet, check if it can be received
        context->checkBuffersAndListen();

      timer.suspend();
    } while (!packet && timer.getCumulatedTime() < timeout);

    if (!packet)
    {
      std::map<Time, CDataPacketPtr>::const_iterator it ;
      info(0)<<"Impossible to get the packet with timestamp = " << timestamp<<std::endl<<"Available timestamp are : "<<std::endl ;
      for(it=packets.begin();it!=packets.end();++it) info(0)<<it->first<<"  ";
      info(0)<<std::endl ;
      ERROR("CConstDataPacketPtr CStoreFilter::getPacket(Time timestamp) const",
            << "Impossible to get the packet with timestamp = " << timestamp);
    }
    return packet;
  }

  template <int N>
  CDataPacket::StatusCode CStoreFilter::getData(Time timestamp, CArray<double, N>& data)
  {
    CConstDataPacketPtr packet = getPacket(timestamp);

    if (packet->status == CDataPacket::NO_ERROR)
      grid->outputField(packet->data, data);

    return packet->status;
  }

  template CDataPacket::StatusCode CStoreFilter::getData<1>(Time timestamp, CArray<double, 1>& data);
  template CDataPacket::StatusCode CStoreFilter::getData<2>(Time timestamp, CArray<double, 2>& data);
  template CDataPacket::StatusCode CStoreFilter::getData<3>(Time timestamp, CArray<double, 3>& data);
  template CDataPacket::StatusCode CStoreFilter::getData<4>(Time timestamp, CArray<double, 4>& data);
  template CDataPacket::StatusCode CStoreFilter::getData<5>(Time timestamp, CArray<double, 5>& data);
  template CDataPacket::StatusCode CStoreFilter::getData<6>(Time timestamp, CArray<double, 6>& data);
  template CDataPacket::StatusCode CStoreFilter::getData<7>(Time timestamp, CArray<double, 7>& data);

  void CStoreFilter::onInputReady(std::vector<CDataPacketPtr> data)
  {

    CDataPacketPtr packet;
    if (detectMissingValues)
    {
      const size_t nbData = data[0]->data.numElements();

      packet = CDataPacketPtr(new CDataPacket);
      packet->date = data[0]->date;
      packet->timestamp = data[0]->timestamp;
      packet->status = data[0]->status;
      packet->data.resize(nbData);
      packet->data = data[0]->data;

      for (size_t idx = 0; idx < nbData; ++idx)
      {
        if (NumTraits<double>::isNan(packet->data(idx)))
          packet->data(idx) = missingValue;
      }

    }

    else
    {
      packet = data[0];
    }

    packets.insert(std::make_pair(packet->timestamp, packet));
    // The packet is always destroyed by the garbage collector
    // so we register but never unregister
    gc.registerObject(this, packet->timestamp);

  }

  bool CStoreFilter::mustAutoTrigger() const
  {
    return false;
  }

  bool CStoreFilter::isDataExpected(const CDate& date) const
  {
    return true;
  }

  void CStoreFilter::invalidate(Time timestamp)
  {
    CInputPin::invalidate(timestamp);
    packets.erase(packets.begin(), packets.lower_bound(timestamp));
  }
} // namespace xios
