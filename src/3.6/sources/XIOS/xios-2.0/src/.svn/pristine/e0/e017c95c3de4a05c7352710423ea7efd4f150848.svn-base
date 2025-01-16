#include "ternary_arithmetic_filter.hpp"

namespace xios
{
  CScalarScalarFieldArithmeticFilter::CScalarScalarFieldArithmeticFilter(CGarbageCollector& gc, const std::string& op, double value1, double value2)
    : CFilter(gc, 1, this)
    , op(operatorExpr.getOpScalarScalarField(op))
    , value1(value1)
    , value2(value2)
  { /* Nothing to do */ };

  CDataPacketPtr CScalarScalarFieldArithmeticFilter::apply(std::vector<CDataPacketPtr> data)
  {
    CDataPacketPtr packet(new CDataPacket);
    packet->date = data[0]->date;
    packet->timestamp = data[0]->timestamp;
    packet->status = data[0]->status;

    if (packet->status == CDataPacket::NO_ERROR)
      packet->data.reference(op(value1,value2, data[0]->data));

    return packet;
  }

  CScalarFieldScalarArithmeticFilter::CScalarFieldScalarArithmeticFilter(CGarbageCollector& gc, const std::string& op, double value1, double value2)
    : CFilter(gc, 1, this)
    , op(operatorExpr.getOpScalarFieldScalar(op))
    , value1(value1)
    , value2(value2)
  { /* Nothing to do */ };

  CDataPacketPtr CScalarFieldScalarArithmeticFilter::apply(std::vector<CDataPacketPtr> data)
  {
    CDataPacketPtr packet(new CDataPacket);
    packet->date = data[0]->date;
    packet->timestamp = data[0]->timestamp;
    packet->status = data[0]->status;

    if (packet->status == CDataPacket::NO_ERROR)
      packet->data.reference(op(value1, data[0]->data,value2));

    return packet;
  }

  CScalarFieldFieldArithmeticFilter::CScalarFieldFieldArithmeticFilter(CGarbageCollector& gc, const std::string& op, double value)
    : CFilter(gc, 2, this)
    , op(operatorExpr.getOpScalarFieldField(op))
    , value(value)
  { /* Nothing to do */ };

  CDataPacketPtr CScalarFieldFieldArithmeticFilter::apply(std::vector<CDataPacketPtr> data)
  {
    CDataPacketPtr packet(new CDataPacket);
    packet->date = data[0]->date;
    packet->timestamp = data[0]->timestamp;
    packet->status = data[0]->status;

    if (data[0]->status != CDataPacket::NO_ERROR)
      packet->status = data[0]->status;
    else if (data[1]->status != CDataPacket::NO_ERROR)
      packet->status = data[1]->status;
    else
    { 
      packet->status = CDataPacket::NO_ERROR;
      packet->data.reference(op(value, data[0]->data, data[1]->data));
    }
    return packet;

  }


  CFieldScalarScalarArithmeticFilter::CFieldScalarScalarArithmeticFilter(CGarbageCollector& gc, const std::string& op, double value1, double value2)
    : CFilter(gc, 1, this)
    , op(operatorExpr.getOpFieldScalarScalar(op))
    , value1(value1)
    , value2(value2)
  { /* Nothing to do */ };

  CDataPacketPtr CFieldScalarScalarArithmeticFilter::apply(std::vector<CDataPacketPtr> data)
  {
    CDataPacketPtr packet(new CDataPacket);
    packet->date = data[0]->date;
    packet->timestamp = data[0]->timestamp;
    packet->status = data[0]->status;

    if (packet->status == CDataPacket::NO_ERROR)
      packet->data.reference(op(data[0]->data, value1, value2));

    return packet;
  }


  CFieldScalarFieldArithmeticFilter::CFieldScalarFieldArithmeticFilter(CGarbageCollector& gc, const std::string& op, double value)
    : CFilter(gc, 2, this)
    , op(operatorExpr.getOpFieldScalarField(op))
    , value(value)
  { /* Nothing to do */ };

  CDataPacketPtr CFieldScalarFieldArithmeticFilter::apply(std::vector<CDataPacketPtr> data)
  {
    CDataPacketPtr packet(new CDataPacket);
    packet->date = data[0]->date;
    packet->timestamp = data[0]->timestamp;
    packet->status = data[0]->status;

    if (data[0]->status != CDataPacket::NO_ERROR)
      packet->status = data[0]->status;
    else if (data[1]->status != CDataPacket::NO_ERROR)
      packet->status = data[1]->status;
    else
    { 
      packet->status = CDataPacket::NO_ERROR;
      packet->data.reference(op(data[0]->data, value, data[1]->data));
    }
    return packet;
  }
  
   CFieldFieldScalarArithmeticFilter::CFieldFieldScalarArithmeticFilter(CGarbageCollector& gc, const std::string& op, double value)
    : CFilter(gc, 2, this)
    , op(operatorExpr.getOpFieldFieldScalar(op))
    , value(value)
  { /* Nothing to do */ };

  CDataPacketPtr CFieldFieldScalarArithmeticFilter::apply(std::vector<CDataPacketPtr> data)
  {
    CDataPacketPtr packet(new CDataPacket);
    packet->date = data[0]->date;
    packet->timestamp = data[0]->timestamp;
    packet->status = data[0]->status;

    if (data[0]->status != CDataPacket::NO_ERROR)
      packet->status = data[0]->status;
    else if (data[1]->status != CDataPacket::NO_ERROR)
      packet->status = data[1]->status;
    else
    { 
      packet->status = CDataPacket::NO_ERROR;
      packet->data.reference(op(data[0]->data, data[1]->data, value));
    }
    return packet;
  } 
  
 
  CFieldFieldFieldArithmeticFilter::CFieldFieldFieldArithmeticFilter(CGarbageCollector& gc, const std::string& op)
    : CFilter(gc, 3, this)
    , op(operatorExpr.getOpFieldFieldField(op))
  { /* Nothing to do */ };

  CDataPacketPtr CFieldFieldFieldArithmeticFilter::apply(std::vector<CDataPacketPtr> data)
  {
    CDataPacketPtr packet(new CDataPacket);
    packet->date = data[0]->date;
    packet->timestamp = data[0]->timestamp;
    packet->status = data[0]->status;

    if (data[0]->status != CDataPacket::NO_ERROR)
      packet->status = data[0]->status;
    else if (data[1]->status != CDataPacket::NO_ERROR)
      packet->status = data[1]->status;
    else if (data[2]->status != CDataPacket::NO_ERROR)
      packet->status = data[2]->status;
    else
    { 
      packet->status = CDataPacket::NO_ERROR;
      packet->data.reference(op(data[0]->data, data[1]->data, data[2]->data));
    }
    return packet;
  } 
  
} // namespace xios

