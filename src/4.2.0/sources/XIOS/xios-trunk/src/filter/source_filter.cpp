#include "source_filter.hpp"
#include "grid.hpp"
#include "exception.hpp"
#include "calendar_util.hpp"
#include <limits> 
#include "workflow_graph.hpp"

namespace xios
{
  CSourceFilter::CSourceFilter(CGarbageCollector& gc, CGrid* grid,
                               bool compression /*= true*/, bool mask /*= false*/,
                               const CDuration offset /*= NoneDu*/, bool manualTrigger /*= false*/,
                               bool hasMissingValue /*= false*/,
                               double defaultValue /*= 0.0*/)
    : COutputPin(gc, manualTrigger)
    , grid(grid)
    , compression(compression)
    , mask(mask)
    , offset(offset)
    , hasMissingValue(hasMissingValue), defaultValue(defaultValue)
    , ntiles(0)
    , storedTileData()
  {
    if (!grid)
      ERROR("CSourceFilter::CSourceFilter(CGrid* grid)",
            "Impossible to construct a source filter without providing a grid.");
  }

  void CSourceFilter::buildGraph(CDataPacketPtr packet)
  {
    bool filter_interval=false;
    if (this->field)
    {
      if(this->field->field_graph_start == -1 && this->field->field_graph_end == -1) filter_interval = true;
      else filter_interval = packet->timestamp >= this->field->field_graph_start && packet->timestamp <= this->field->field_graph_end;
    }
    bool building_graph = this->tag ? filter_interval : false;
    if(building_graph)
    {
      this->filterID = InvalidableObject::filterIdGenerator++;  
      packet->src_filterID=this->filterID;
      packet->field = this->field;
      packet->distance = 1;
      
      CWorkflowGraph::allocNodeEdge();
      
      CWorkflowGraph::addNode(this->filterID, "Source Filter ", 1, 1, 0, packet);
      (*CWorkflowGraph::mapFilters_ptr_with_info)[this->filterID].attributes = this->field->record4graphXiosAttributes();
      (*CWorkflowGraph::mapFilters_ptr_with_info)[this->filterID].field_id = this->field->getId();
      (*CWorkflowGraph::mapFilters_ptr_with_info)[this->filterID].distance = 1;

      CWorkflowGraph::build_begin = true;
    }

  }

  template <int N>
  void CSourceFilter::streamTile(CDate date, const CArray<double, N>& tileData, int tileId)
  {
    if (ntiles==0)
    {
      const double nanValue = std::numeric_limits<double>::quiet_NaN();
//      storedTileData.resize(grid->storeIndex_client.numElements());
      storedTileData.resize(grid->getDataSize());
      storedTileData = nanValue;
    }
    grid->copyTile(tileData, storedTileData, tileId);
    ++ntiles;
    if (ntiles==grid->getNTiles())
    {
      // Data entering workflow will be exactly of size ni*nj for a grid 2d or ni*nj*n for a grid 3d
      streamData(date, storedTileData, true);
      ntiles = 0;
    }
  }

  template <int N>
  void CSourceFilter::streamData(CDate date, const CArray<double, N>& data, bool isTiled)
  {
    date = date + offset; // this is a temporary solution, it should be part of a proper temporal filter

    CDataPacketPtr packet(new CDataPacket);
    packet->date = date;
    packet->timestamp = date;
    packet->status = CDataPacket::NO_ERROR;

    packet->data.resize(grid->storeIndex_client.numElements());    
    
    if (compression)
    {
      packet->data = defaultValue;
      grid->uncompressField(data, packet->data);    
    }
    else
    {
      if (mask)
        if (isTiled)
          grid->maskField(data, packet->data, isTiled);
        else
          grid->maskField(data, packet->data);
      else
        grid->inputField(data, packet->data);
    }
    // Convert missing values to NaN
    if (hasMissingValue)
    {
      const double nanValue = std::numeric_limits<double>::quiet_NaN();
      const size_t nbData = packet->data.numElements();
      for (size_t idx = 0; idx < nbData; ++idx)
      {
        if (defaultValue == packet->data(idx))
          packet->data(idx) = nanValue;
      }
    }

    if(CXios::isClient) buildGraph(packet);
    


    onOutputReady(packet);
  }

  template void CSourceFilter::streamData<1>(CDate date, const CArray<double, 1>& data, bool isTiled);
  template void CSourceFilter::streamData<2>(CDate date, const CArray<double, 2>& data, bool isTiled);
  template void CSourceFilter::streamData<3>(CDate date, const CArray<double, 3>& data, bool isTiled);
  template void CSourceFilter::streamData<4>(CDate date, const CArray<double, 4>& data, bool isTiled);
  template void CSourceFilter::streamData<5>(CDate date, const CArray<double, 5>& data, bool isTiled);
  template void CSourceFilter::streamData<6>(CDate date, const CArray<double, 6>& data, bool isTiled);
  template void CSourceFilter::streamData<7>(CDate date, const CArray<double, 7>& data, bool isTiled);

  template void CSourceFilter::streamTile<1>(CDate date, const CArray<double, 1>& data, int ntile);
  template void CSourceFilter::streamTile<2>(CDate date, const CArray<double, 2>& data, int ntile);
  template void CSourceFilter::streamTile<3>(CDate date, const CArray<double, 3>& data, int ntile);
  template void CSourceFilter::streamTile<4>(CDate date, const CArray<double, 4>& data, int ntile);
  template void CSourceFilter::streamTile<5>(CDate date, const CArray<double, 5>& data, int ntile);
  template void CSourceFilter::streamTile<6>(CDate date, const CArray<double, 6>& data, int ntile);
  template void CSourceFilter::streamTile<7>(CDate date, const CArray<double, 7>& data, int ntile);

  void CSourceFilter::streamDataFromServer(CDate date, const std::map<int, CArray<double, 1> >& data)
  {
    date = date + offset; // this is a temporary solution, it should be part of a proper temporal filter

    CDataPacketPtr packet(new CDataPacket);
    packet->date = date;
    packet->timestamp = date;
    packet->status = CDataPacket::NO_ERROR;
    
    if (data.size() != grid->storeIndex_fromSrv.size())
      ERROR("CSourceFilter::streamDataFromServer(CDate date, const std::map<int, CArray<double, 1> >& data)",
            << "Incoherent data received from servers,"
            << " expected " << grid->storeIndex_fromSrv.size() << " chunks but " << data.size() << " were given.");

    packet->data.resize(grid->storeIndex_client.numElements());
    std::map<int, CArray<double, 1> >::const_iterator it, itEnd = data.end();
    for (it = data.begin(); it != itEnd; it++)
    {      
      CArray<int,1>& index = grid->storeIndex_fromSrv[it->first];
      for (int n = 0; n < index.numElements(); n++)
        packet->data(index(n)) = it->second(n);
    }

    // Convert missing values to NaN
    if (hasMissingValue)
    {
      const double nanValue = std::numeric_limits<double>::quiet_NaN();
      const size_t nbData = packet->data.numElements();
      for (size_t idx = 0; idx < nbData; ++idx)
      {
        if (defaultValue == packet->data(idx))
          packet->data(idx) = nanValue;
      }
    }
    if(CXios::isClient) buildGraph(packet);
    onOutputReady(packet);
  }

  void CSourceFilter::signalEndOfStream(CDate date)
  {
    date = date + offset; // this is a temporary solution, it should be part of a proper temporal filter

    CDataPacketPtr packet(new CDataPacket);
    packet->date = date;
    packet->timestamp = date;
    packet->status = CDataPacket::END_OF_STREAM; 
    onOutputReady(packet);
  }
} // namespace xios
