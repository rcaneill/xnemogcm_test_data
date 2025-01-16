#include "unary_arithmetic_filter.hpp"

namespace xios
{
  CUnaryArithmeticFilter::CUnaryArithmeticFilter(CGarbageCollector& gc, const std::string& op)
    : CFilter(gc, 1, this)
    , op(operatorExpr.getOpField(op))
  { /* Nothing to do */ };

  CDataPacketPtr CUnaryArithmeticFilter::apply(std::vector<CDataPacketPtr> data)
  {
    CDataPacketPtr packet(new CDataPacket);
    packet->date = data[0]->date;
    packet->timestamp = data[0]->timestamp;
    packet->status = data[0]->status;

    if (packet->status == CDataPacket::NO_ERROR)
      packet->data.reference(op(data[0]->data));

    return packet;
  }
} // namespace xios
