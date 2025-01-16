#include "binary_arithmetic_filter.hpp"

namespace xios
{
  CScalarFieldArithmeticFilter::CScalarFieldArithmeticFilter(CGarbageCollector& gc, const std::string& op, double value)
    : CFilter(gc, 1, this)
    , op(operatorExpr.getOpScalarField(op))
    , value(value)
  { /* Nothing to do */ };

  CDataPacketPtr CScalarFieldArithmeticFilter::apply(std::vector<CDataPacketPtr> data)
  {
    CDataPacketPtr packet(new CDataPacket);
    packet->date = data[0]->date;
    packet->timestamp = data[0]->timestamp;
    packet->status = data[0]->status;

    if (packet->status == CDataPacket::NO_ERROR)
      packet->data.reference(op(value, data[0]->data));

    return packet;
  }

  CFieldScalarArithmeticFilter::CFieldScalarArithmeticFilter(CGarbageCollector& gc, const std::string& op, double value)
    : CFilter(gc, 1, this)
    , op(operatorExpr.getOpFieldScalar(op))
    , value(value)
  { /* Nothing to do */ };

  CDataPacketPtr CFieldScalarArithmeticFilter::apply(std::vector<CDataPacketPtr> data)
  {
    CDataPacketPtr packet(new CDataPacket);
    packet->date = data[0]->date;
    packet->timestamp = data[0]->timestamp;
    packet->status = data[0]->status;

    if (packet->status == CDataPacket::NO_ERROR)
      packet->data.reference(op(data[0]->data, value));

    return packet;
  }

  CFieldFieldArithmeticFilter::CFieldFieldArithmeticFilter(CGarbageCollector& gc, const std::string& op)
    : CFilter(gc, 2, this)
    , op(operatorExpr.getOpFieldField(op))
  { /* Nothing to do */ };

  CDataPacketPtr CFieldFieldArithmeticFilter::apply(std::vector<CDataPacketPtr> data)
  {
    CDataPacketPtr packet(new CDataPacket);
    packet->date = data[0]->date;
    packet->timestamp = data[0]->timestamp;

    if (data[0]->status != CDataPacket::NO_ERROR)
      packet->status = data[0]->status;
    else if (data[1]->status != CDataPacket::NO_ERROR)
      packet->status = data[1]->status;
    else
    {
      packet->status = CDataPacket::NO_ERROR;
      packet->data.reference(op(data[0]->data, data[1]->data));
    }

    return packet;
  }
} // namespace xios
