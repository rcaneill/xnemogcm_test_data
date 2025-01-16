#include "nc4_data_output.hpp"

#include <boost/lexical_cast.hpp>
#include "attribute_template.hpp"
#include "group_template.hpp"

#include "file.hpp"
#include "calendar.hpp"
#include "context.hpp"
#include "context_server.hpp"
#include "netCdfException.hpp"
#include "exception.hpp"
#include "timer.hpp"
#include "uuid.hpp"
namespace xios
{
      /// ////////////////////// Dfinitions ////////////////////// ///
      CNc4DataOutput::CNc4DataOutput
         (CFile* file, const StdString & filename, bool exist)
            : SuperClass()
            , SuperClassWriter(filename, exist)
            , filename(filename)
            , file(file),hasTimeInstant(false),hasTimeCentered(false), timeCounterType(none)
      {
        SuperClass::type = MULTI_FILE;
      }

      CNc4DataOutput::CNc4DataOutput
         (CFile* file, const StdString & filename, bool exist, bool useClassicFormat, bool useCFConvention,
          MPI_Comm comm_file, bool multifile, bool isCollective, const StdString& timeCounterName)
            : SuperClass()
            , SuperClassWriter(filename, exist, useClassicFormat, useCFConvention, &comm_file, multifile, timeCounterName)
            , comm_file(comm_file)
            , filename(filename)
            , isCollective(isCollective)
            , file(file),hasTimeInstant(false),hasTimeCentered(false), timeCounterType(none)
      {
        SuperClass::type = (multifile) ? MULTI_FILE : ONE_FILE;
      }

      CNc4DataOutput::~CNc4DataOutput(void)
    { /* Ne rien faire de plus */ }

      ///--------------------------------------------------------------

      const StdString & CNc4DataOutput::getFileName(void) const
      {
         return (this->filename);
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeDomain_(CDomain* domain)
      {
        if (domain->type == CDomain::type_attr::unstructured)
        {
          if (SuperClassWriter::useCFConvention)
            writeUnstructuredDomain(domain) ;
          else
            writeUnstructuredDomainUgrid(domain) ;
          return ;
        }

         CContext* context = CContext::getCurrent() ;
         CContextServer* server=context->server ;

         if (domain->IsWritten(this->filename)) return;
         domain->checkAttributes();

         if (domain->isEmpty())
           if (SuperClass::type==MULTI_FILE) return;

         std::vector<StdString> dim0, dim1;
         StdString domid = domain->getDomainOutputName();
         StdString appendDomid  = (singleDomain) ? "" : "_"+domid ;
         if (isWrittenDomain(domid)) return ;
         else setWrittenDomain(domid);


        StdString dimXid, dimYid ;

        nc_type typePrec ;
        if (domain->prec.isEmpty()) typePrec =  NC_FLOAT ;
        else if (domain->prec==4)  typePrec =  NC_FLOAT ;
        else if (domain->prec==8)   typePrec =  NC_DOUBLE ;
         
         bool isRegularDomain = (domain->type == CDomain::type_attr::rectilinear);
         switch (domain->type)
         {
           case CDomain::type_attr::curvilinear :
             dimXid     = StdString("x").append(appendDomid);
             dimYid     = StdString("y").append(appendDomid);
             break ;
           case CDomain::type_attr::rectilinear :
             dimXid     = StdString("lon").append(appendDomid);
             dimYid     = StdString("lat").append(appendDomid);
             break;
         }

         StdString dimVertId = StdString("nvertex").append(appendDomid);

         string lonid,latid,bounds_lonid,bounds_latid ;
         string areaId = "area" + appendDomid;
/*
         StdString lonid_loc = (server->intraCommSize > 1)
                             ? StdString("lon").append(appendDomid).append("_local")
                             : lonid;
         StdString latid_loc = (server->intraCommSize > 1)
                             ? StdString("lat").append(appendDomid).append("_local")
                             : latid;
*/

         try
         {
           switch (SuperClass::type)
           {
              case (MULTI_FILE) :
              {
  //               if (domain->isEmpty()) return;

                 if (server->intraCommSize > 1)
                 {
  //                 SuperClassWriter::addDimension(lonid, domain->zoom_ni.getValue());
  //                 SuperClassWriter::addDimension(latid, domain->zoom_nj.getValue());
                 }

                 switch (domain->type)
                 {
                   case CDomain::type_attr::curvilinear :
                     dim0.push_back(dimYid); dim0.push_back(dimXid);
                     lonid = StdString("nav_lon").append(appendDomid);
                     latid = StdString("nav_lat").append(appendDomid);
                     break ;
                   case CDomain::type_attr::rectilinear :
                     lonid = StdString("lon").append(appendDomid);
                     latid = StdString("lat").append(appendDomid);
                     dim0.push_back(dimYid);
                     dim1.push_back(dimXid);
                     break;
                 }

                 bounds_lonid = StdString("bounds_lon").append(appendDomid);
                 bounds_latid = StdString("bounds_lat").append(appendDomid);

                 SuperClassWriter::addDimension(dimXid, domain->zoom_ni_srv);
                 SuperClassWriter::addDimension(dimYid, domain->zoom_nj_srv);

                 if (domain->hasBounds)
                   SuperClassWriter::addDimension(dimVertId, domain->nvertex);

                 if (server->intraCommSize > 1)
                 {
                   this->writeLocalAttributes(domain->zoom_ibegin_srv,
                                              domain->zoom_ni_srv,
                                              domain->zoom_jbegin_srv,
                                              domain->zoom_nj_srv,
                                              appendDomid);

                   if (singleDomain)
                    this->writeLocalAttributes_IOIPSL(dimXid, dimYid,
                                                      domain->zoom_ibegin_srv,
                                                      domain->zoom_ni_srv,
                                                      domain->zoom_jbegin_srv,
                                                      domain->zoom_nj_srv,
                                                      domain->ni_glo,domain->nj_glo,
                                                      server->intraCommRank,server->intraCommSize);
                 }

                 if (domain->hasLonLat)
                 {
                   switch (domain->type)
                   {
                     case CDomain::type_attr::curvilinear :
                       SuperClassWriter::addVariable(latid, typePrec, dim0);
                       SuperClassWriter::addVariable(lonid, typePrec, dim0);
                       break ;
                      case CDomain::type_attr::rectilinear :
                        SuperClassWriter::addVariable(latid, typePrec, dim0);
                        SuperClassWriter::addVariable(lonid, typePrec, dim1);
                        break ;
                   }

                   this->writeAxisAttributes(lonid, isRegularDomain ? "X" : "", "longitude", "Longitude", "degrees_east", domid);
                   this->writeAxisAttributes(latid, isRegularDomain ? "Y" : "", "latitude", "Latitude", "degrees_north", domid);

                   if (domain->hasBounds)
                   {
                     SuperClassWriter::addAttribute("bounds", bounds_lonid, &lonid);
                     SuperClassWriter::addAttribute("bounds", bounds_latid, &latid);

                     dim0.clear();
                     dim0.push_back(dimYid);
                     dim0.push_back(dimXid);
                     dim0.push_back(dimVertId);
                     SuperClassWriter::addVariable(bounds_lonid, typePrec, dim0);
                     SuperClassWriter::addVariable(bounds_latid, typePrec, dim0);
                   }
                 }

                 dim0.clear();
                 dim0.push_back(dimYid);
                 dim0.push_back(dimXid);


  // supress mask               if (server->intraCommSize > 1)
  // supress mask               {
  // supress mask                  SuperClassWriter::addVariable(maskid, NC_INT, dim0);
  // supress mask
  // supress mask                  this->writeMaskAttributes(maskid,
  // supress mask                     domain->data_dim.getValue()/*,
  // supress mask                     domain->data_ni.getValue(),
  // supress mask                     domain->data_nj.getValue(),
  // supress mask                     domain->data_ibegin.getValue(),
  // supress mask                     domain->data_jbegin.getValue()*/);
  // supress mask               }

                 //SuperClassWriter::setDefaultValue(maskid, &dvm);

                 if (domain->hasArea)
                 {
                   SuperClassWriter::addVariable(areaId, typePrec, dim0);
                   SuperClassWriter::addAttribute("standard_name", StdString("cell_area"), &areaId);
                   SuperClassWriter::addAttribute("units", StdString("m2"), &areaId);
                 }

                 SuperClassWriter::definition_end();

                 if (domain->hasLonLat)
                 {
                   switch (domain->type)
                   {
                     case CDomain::type_attr::curvilinear :
                       SuperClassWriter::writeData(domain->latvalue_srv, latid, isCollective, 0);
                       SuperClassWriter::writeData(domain->lonvalue_srv, lonid, isCollective, 0);
                       break;
                     case CDomain::type_attr::rectilinear :
                       CArray<double,1> lat = domain->latvalue_srv(Range(fromStart,toEnd,domain->zoom_ni_srv)) ;
                       SuperClassWriter::writeData(CArray<double,1>(lat.copy()), latid, isCollective, 0);
                       CArray<double,1> lon=domain->lonvalue_srv(Range(0,domain->zoom_ni_srv-1)) ;
                       SuperClassWriter::writeData(CArray<double,1>(lon.copy()), lonid, isCollective, 0);
                       break;
                   }

                   if (domain->hasBounds)
                   {
                     SuperClassWriter::writeData(domain->bounds_lon_srv, bounds_lonid, isCollective, 0);
                     SuperClassWriter::writeData(domain->bounds_lat_srv, bounds_latid, isCollective, 0);
                   }
                 }

                 if (domain->hasArea)
                   SuperClassWriter::writeData(domain->area_srv, areaId, isCollective, 0);

                 SuperClassWriter::definition_start();

                 break;
              }
              case (ONE_FILE) :
              {
                 SuperClassWriter::addDimension(dimXid, domain->global_zoom_ni);
                 SuperClassWriter::addDimension(dimYid, domain->global_zoom_nj);

                 if (domain->hasBounds)
                   SuperClassWriter::addDimension(dimVertId, domain->nvertex);

                 if (domain->hasLonLat)
                 {
                   switch (domain->type)
                   {
                     case CDomain::type_attr::curvilinear :
                       dim0.push_back(dimYid); dim0.push_back(dimXid);
                       lonid = StdString("nav_lon").append(appendDomid);
                       latid = StdString("nav_lat").append(appendDomid);
                       SuperClassWriter::addVariable(latid, typePrec, dim0);
                       SuperClassWriter::addVariable(lonid, typePrec, dim0);
                       break;

                     case CDomain::type_attr::rectilinear :
                       dim0.push_back(dimYid);
                       dim1.push_back(dimXid);
                       lonid = StdString("lon").append(appendDomid);
                       latid = StdString("lat").append(appendDomid);
                       SuperClassWriter::addVariable(latid, typePrec, dim0);
                       SuperClassWriter::addVariable(lonid, typePrec, dim1);
                       break;
                   }

                   bounds_lonid = StdString("bounds_lon").append(appendDomid);
                   bounds_latid = StdString("bounds_lat").append(appendDomid);

                   this->writeAxisAttributes
                      (lonid, isRegularDomain ? "X" : "", "longitude", "Longitude", "degrees_east", domid);
                   this->writeAxisAttributes
                      (latid, isRegularDomain ? "Y" : "", "latitude", "Latitude", "degrees_north", domid);

                   if (domain->hasBounds)
                   {
                     SuperClassWriter::addAttribute("bounds", bounds_lonid, &lonid);
                     SuperClassWriter::addAttribute("bounds", bounds_latid, &latid);

                     dim0.clear();
                     dim0.push_back(dimYid);
                     dim0.push_back(dimXid);
                     dim0.push_back(dimVertId);
                     SuperClassWriter::addVariable(bounds_lonid, typePrec, dim0);
                     SuperClassWriter::addVariable(bounds_latid, typePrec, dim0);
                   }
                 }

                 if (domain->hasArea)
                 {
                   dim0.clear();
                   dim0.push_back(dimYid); dim0.push_back(dimXid);
                   SuperClassWriter::addVariable(areaId, typePrec, dim0);
                   SuperClassWriter::addAttribute("standard_name", StdString("cell_area"), &areaId);
                   SuperClassWriter::addAttribute("units", StdString("m2"), &areaId);
                   dim0.clear();
                 }

                 SuperClassWriter::definition_end();

                 switch (domain->type)
                 {
                   case CDomain::type_attr::curvilinear :
                   {
                     std::vector<StdSize> start(2) ;
                     std::vector<StdSize> count(2) ;
                     if (domain->isEmpty())
                     {
                       start[0]=0 ; start[1]=0 ;
                       count[0]=0 ; count[1]=0 ;
                     }
                     else
                     {
                       start[1]=domain->zoom_ibegin_srv-domain->global_zoom_ibegin;
                       start[0]=domain->zoom_jbegin_srv-domain->global_zoom_jbegin;
                       count[1]=domain->zoom_ni_srv ; count[0]=domain->zoom_nj_srv ;
                     }

                     if (domain->hasLonLat)
                     {
                       SuperClassWriter::writeData(domain->latvalue_srv, latid, isCollective, 0,&start,&count);
                       SuperClassWriter::writeData(domain->lonvalue_srv, lonid, isCollective, 0,&start,&count);
                     }
                     break;
                   }
                   case CDomain::type_attr::rectilinear :
                   {
                     if (domain->hasLonLat)
                     {
                       std::vector<StdSize> start(1) ;
                       std::vector<StdSize> count(1) ;
                       if (domain->isEmpty())
                       {
                         start[0]=0 ;
                         count[0]=0 ;
                         SuperClassWriter::writeData(domain->latvalue_srv, latid, isCollective, 0,&start,&count);
                         SuperClassWriter::writeData(domain->lonvalue_srv, lonid, isCollective, 0,&start,&count);

                       }
                       else
                       {
                         start[0]=domain->zoom_jbegin_srv-domain->global_zoom_jbegin;
                         count[0]=domain->zoom_nj_srv ;
                         CArray<double,1> lat = domain->latvalue_srv(Range(fromStart,toEnd,domain->zoom_ni_srv)) ;
                         SuperClassWriter::writeData(CArray<double,1>(lat.copy()), latid, isCollective, 0,&start,&count);

                         start[0]=domain->zoom_ibegin_srv-domain->global_zoom_ibegin;
                         count[0]=domain->zoom_ni_srv ;
                         CArray<double,1> lon=domain->lonvalue_srv(Range(0,domain->zoom_ni_srv-1)) ;
                         SuperClassWriter::writeData(CArray<double,1>(lon.copy()), lonid, isCollective, 0,&start,&count);
                       }
                     }
                     break;
                   }
                 }

                 if (domain->hasBounds)
                 {
                   std::vector<StdSize> start(3);
                   std::vector<StdSize> count(3);
                   if (domain->isEmpty())
                   {
                     start[2] = start[1] = start[0] = 0;
                     count[2] = count[1] = count[0] = 0;
                   }
                   else
                   {
                     start[2] = 0;
                     start[1] = domain->zoom_ibegin_srv - domain->global_zoom_ibegin;
                     start[0] = domain->zoom_jbegin_srv - domain->global_zoom_jbegin;
                     count[2] = domain->nvertex;
                     count[1] = domain->zoom_ni_srv;
                     count[0] = domain->zoom_nj_srv;
                   }

                 SuperClassWriter::writeData(domain->bounds_lon_srv, bounds_lonid, isCollective, 0, &start, &count);
                 SuperClassWriter::writeData(domain->bounds_lat_srv, bounds_latid, isCollective, 0, &start, &count);
                 }

                 if (domain->hasArea)
                 {
                   std::vector<StdSize> start(2);
                   std::vector<StdSize> count(2);

                   if (domain->isEmpty())
                   {
                     start[0] = 0; start[1] = 0;
                     count[0] = 0; count[1] = 0;
                   }
                   else
                   {
                     start[1] = domain->zoom_ibegin_srv - domain->global_zoom_ibegin;
                     start[0] = domain->zoom_jbegin_srv - domain->global_zoom_jbegin;
                     count[1] = domain->zoom_ni_srv;
                     count[0] = domain->zoom_nj_srv;
                   }

                   SuperClassWriter::writeData(domain->area_srv, areaId, isCollective, 0, &start, &count);
                 }

                 SuperClassWriter::definition_start();
                 break;
              }
              default :
                 ERROR("CNc4DataOutput::writeDomain(domain)",
                       << "[ type = " << SuperClass::type << "]"
                       << " not implemented yet !");
           }
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing the domain : ");
           msg.append(domid); msg.append("\n");
           msg.append("In the context : ");
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeDomain_(CDomain* domain)", << msg);
         }

         domain->addRelFile(this->filename);
      }

    //--------------------------------------------------------------

    void CNc4DataOutput::writeUnstructuredDomainUgrid(CDomain* domain)
    {
      CContext* context = CContext::getCurrent() ;
      CContextServer* server=context->server ;

      if (domain->IsWritten(this->filename)) return;
      domain->checkAttributes();
      if (domain->isEmpty())
        if (SuperClass::type==MULTI_FILE) return ;

     nc_type typePrec ;
     if (domain->prec.isEmpty()) typePrec =  NC_FLOAT ;
     else if (domain->prec==4)  typePrec =  NC_FLOAT ;
     else if (domain->prec==8)   typePrec =  NC_DOUBLE ;

      std::vector<StdString> dim0;
      StdString domid = domain->getDomainOutputName();
      StdString domainName = domain->name;
      domain->assignMesh(domainName, domain->nvertex);
      domain->mesh->createMeshEpsilon(server->intraComm, domain->lonvalue_srv, domain->latvalue_srv, domain->bounds_lon_srv, domain->bounds_lat_srv);

      StdString node_x = domainName + "_node_x";
      StdString node_y = domainName + "_node_y";

      StdString edge_x = domainName + "_edge_x";
      StdString edge_y = domainName + "_edge_y";
      StdString edge_nodes = domainName + "_edge_nodes";

      StdString face_x = domainName + "_face_x";
      StdString face_y = domainName + "_face_y";
      StdString face_nodes = domainName + "_face_nodes";
      StdString face_edges = domainName + "_face_edges";
      StdString edge_faces = domainName + "_edge_face_links";
      StdString face_faces = domainName + "_face_links";

      StdString dimNode = "n" + domainName + "_node";
      StdString dimEdge = "n" + domainName + "_edge";
      StdString dimFace = "n" + domainName + "_face";
      StdString dimVertex = "n" + domainName + "_vertex";
      StdString dimTwo = "Two";

      if (!SuperClassWriter::dimExist(dimTwo)) SuperClassWriter::addDimension(dimTwo, 2);
      if (!isWrittenDomain(domid))
      {
        dim0.clear();
        SuperClassWriter::addVariable(domainName, NC_INT, dim0);
        SuperClassWriter::addAttribute("cf_role", StdString("mesh_topology"), &domainName);
        SuperClassWriter::addAttribute("long_name", StdString("Topology data of 2D unstructured mesh"), &domainName);
        SuperClassWriter::addAttribute("topology_dimension", 2, &domainName);
        SuperClassWriter::addAttribute("node_coordinates", node_x + " " + node_y, &domainName);
      }

      try
      {
        switch (SuperClass::type)
        {
          case (ONE_FILE) :
          {
            // Adding nodes
            if (domain->nvertex == 1)
            {
              if (!SuperClassWriter::varExist(node_x) || !SuperClassWriter::varExist(node_y))
              {
                SuperClassWriter::addDimension(dimNode, domain->ni_glo);
                dim0.clear();
                dim0.push_back(dimNode);
                SuperClassWriter::addVariable(node_x, typePrec, dim0);
                SuperClassWriter::addAttribute("standard_name", StdString("longitude"), &node_x);
                SuperClassWriter::addAttribute("long_name", StdString("Longitude of mesh nodes."), &node_x);
                SuperClassWriter::addAttribute("units", StdString("degrees_east"), &node_x);
                SuperClassWriter::addVariable(node_y, typePrec, dim0);
                SuperClassWriter::addAttribute("standard_name", StdString("latitude"), &node_y);
                SuperClassWriter::addAttribute("long_name", StdString("Latitude of mesh nodes."), &node_y);
                SuperClassWriter::addAttribute("units", StdString("degrees_north"), &node_y);
              }
            } // domain->nvertex == 1

            // Adding edges and nodes, if nodes have not been defined previously
            if (domain->nvertex == 2)
            {
              if (!SuperClassWriter::varExist(node_x) || !SuperClassWriter::varExist(node_y))
              {
                SuperClassWriter::addDimension(dimNode, domain->mesh->nbNodesGlo);
                dim0.clear();
                dim0.push_back(dimNode);
                SuperClassWriter::addVariable(node_x, typePrec, dim0);
                SuperClassWriter::addAttribute("standard_name", StdString("longitude"), &node_x);
                SuperClassWriter::addAttribute("long_name", StdString("Longitude of mesh nodes."), &node_x);
                SuperClassWriter::addAttribute("units", StdString("degrees_east"), &node_x);
                SuperClassWriter::addVariable(node_y, typePrec, dim0);
                SuperClassWriter::addAttribute("standard_name", StdString("latitude"), &node_y);
                SuperClassWriter::addAttribute("long_name", StdString("Latitude of mesh nodes."), &node_y);
                SuperClassWriter::addAttribute("units", StdString("degrees_north"), &node_y);
              }
              SuperClassWriter::addAttribute("edge_node_connectivity", edge_nodes, &domainName);
              SuperClassWriter::addAttribute("edge_coordinates", edge_x + " " + edge_y, &domainName);
              SuperClassWriter::addDimension(dimEdge, domain->ni_glo);
              dim0.clear();
              dim0.push_back(dimEdge);
              SuperClassWriter::addVariable(edge_x, typePrec, dim0);
              SuperClassWriter::addAttribute("standard_name", StdString("longitude"), &edge_x);
              SuperClassWriter::addAttribute("long_name", StdString("Characteristic longitude of mesh edges."), &edge_x);
              SuperClassWriter::addAttribute("units", StdString("degrees_east"), &edge_x);
              SuperClassWriter::addVariable(edge_y, typePrec, dim0);
              SuperClassWriter::addAttribute("standard_name", StdString("latitude"), &edge_y);
              SuperClassWriter::addAttribute("long_name", StdString("Characteristic latitude of mesh edges."), &edge_y);
              SuperClassWriter::addAttribute("units", StdString("degrees_north"), &edge_y);
              dim0.clear();
              dim0.push_back(dimEdge);
              dim0.push_back(dimTwo);
              SuperClassWriter::addVariable(edge_nodes, NC_INT, dim0);
              SuperClassWriter::addAttribute("cf_role", StdString("edge_node_connectivity"), &edge_nodes);
              SuperClassWriter::addAttribute("long_name", StdString("Maps every edge/link to two nodes that it connects."), &edge_nodes);
              SuperClassWriter::addAttribute("start_index", 0, &edge_nodes);
            } // domain->nvertex == 2

            // Adding faces, edges, and nodes, if edges and nodes have not been defined previously
            if (domain->nvertex > 2)
            {
              // Nodes
              if (!SuperClassWriter::varExist(node_x) || !SuperClassWriter::varExist(node_y))
              {
                SuperClassWriter::addDimension(dimNode, domain->mesh->nbNodesGlo);
                dim0.clear();
                dim0.push_back(dimNode);
                SuperClassWriter::addVariable(node_x, typePrec, dim0);
                SuperClassWriter::addAttribute("standard_name", StdString("longitude"), &node_x);
                SuperClassWriter::addAttribute("long_name", StdString("Longitude of mesh nodes."), &node_x);
                SuperClassWriter::addAttribute("units", StdString("degrees_east"), &node_x);
                SuperClassWriter::addVariable(node_y, typePrec, dim0);
                SuperClassWriter::addAttribute("standard_name", StdString("latitude"), &node_y);
                SuperClassWriter::addAttribute("long_name", StdString("Latitude of mesh nodes."), &node_y);
                SuperClassWriter::addAttribute("units", StdString("degrees_north"), &node_y);
              }
              if (!SuperClassWriter::varExist(edge_x) || !SuperClassWriter::varExist(edge_y))
              {
                SuperClassWriter::addAttribute("edge_coordinates", edge_x + " " + edge_y, &domainName);
                SuperClassWriter::addAttribute("edge_node_connectivity", edge_nodes, &domainName);
                SuperClassWriter::addDimension(dimEdge, domain->mesh->nbEdgesGlo);
                dim0.clear();
                dim0.push_back(dimEdge);
                SuperClassWriter::addVariable(edge_x, typePrec, dim0);
                SuperClassWriter::addAttribute("standard_name", StdString("longitude"), &edge_x);
                SuperClassWriter::addAttribute("long_name", StdString("Characteristic longitude of mesh edges."), &edge_x);
                SuperClassWriter::addAttribute("units", StdString("degrees_east"), &edge_x);
                SuperClassWriter::addVariable(edge_y, typePrec, dim0);
                SuperClassWriter::addAttribute("standard_name", StdString("latitude"), &edge_y);
                SuperClassWriter::addAttribute("long_name", StdString("Characteristic latitude of mesh edges."), &edge_y);
                SuperClassWriter::addAttribute("units", StdString("degrees_north"), &edge_y);
                dim0.clear();
                dim0.push_back(dimEdge);
                dim0.push_back(dimTwo);
                SuperClassWriter::addVariable(edge_nodes, NC_INT, dim0);
                SuperClassWriter::addAttribute("cf_role", StdString("edge_node_connectivity"), &edge_nodes);
                SuperClassWriter::addAttribute("long_name", StdString("Maps every edge/link to two nodes that it connects."), &edge_nodes);
                SuperClassWriter::addAttribute("start_index", 0, &edge_nodes);
              }
              SuperClassWriter::addAttribute("face_coordinates", face_x + " " + face_y, &domainName);
              SuperClassWriter::addAttribute("face_node_connectivity", face_nodes, &domainName);
              SuperClassWriter::addDimension(dimFace, domain->ni_glo);
              SuperClassWriter::addDimension(dimVertex, domain->nvertex);
              dim0.clear();
              dim0.push_back(dimFace);
              SuperClassWriter::addVariable(face_x, typePrec, dim0);
              SuperClassWriter::addAttribute("standard_name", StdString("longitude"), &face_x);
              SuperClassWriter::addAttribute("long_name", StdString("Characteristic longitude of mesh faces."), &face_x);
              SuperClassWriter::addAttribute("units", StdString("degrees_east"), &face_x);
              SuperClassWriter::addVariable(face_y, typePrec, dim0);
              SuperClassWriter::addAttribute("standard_name", StdString("latitude"), &face_y);
              SuperClassWriter::addAttribute("long_name", StdString("Characteristic latitude of mesh faces."), &face_y);
              SuperClassWriter::addAttribute("units", StdString("degrees_north"), &face_y);
              dim0.clear();
              dim0.push_back(dimFace);
              dim0.push_back(dimVertex);
              SuperClassWriter::addVariable(face_nodes, NC_INT, dim0);
              SuperClassWriter::addAttribute("cf_role", StdString("face_node_connectivity"), &face_nodes);
              SuperClassWriter::addAttribute("long_name", StdString("Maps every face to its corner nodes."), &face_nodes);
              SuperClassWriter::addAttribute("start_index", 0, &face_nodes);
              dim0.clear();
              dim0.push_back(dimFace);
              dim0.push_back(dimVertex);
              SuperClassWriter::addVariable(face_edges, NC_INT, dim0);
              SuperClassWriter::addAttribute("cf_role", StdString("face_edge_connectivity"), &face_edges);
              SuperClassWriter::addAttribute("long_name", StdString("Maps every face to its edges."), &face_edges);
              SuperClassWriter::addAttribute("start_index", 0, &face_edges);
              SuperClassWriter::addAttribute("_FillValue", 999999, &face_edges);
              dim0.clear();
              dim0.push_back(dimEdge);
              dim0.push_back(dimTwo);
              SuperClassWriter::addVariable(edge_faces, NC_INT, dim0);
              SuperClassWriter::addAttribute("cf_role", StdString("edge_face connectivity"), &edge_faces);
              SuperClassWriter::addAttribute("long_name", StdString("neighbor faces for edges"), &edge_faces);
              SuperClassWriter::addAttribute("start_index", 0, &edge_faces);
              SuperClassWriter::addAttribute("_FillValue", -999, &edge_faces);
              SuperClassWriter::addAttribute("comment", StdString("missing neighbor faces are indicated using _FillValue"), &edge_faces);
              dim0.clear();
              dim0.push_back(dimFace);
              dim0.push_back(dimVertex);
              SuperClassWriter::addVariable(face_faces, NC_INT, dim0);
              SuperClassWriter::addAttribute("cf_role", StdString("face_face connectivity"), &face_faces);
              SuperClassWriter::addAttribute("long_name", StdString("Indicates which other faces neighbor each face"), &face_faces);
              SuperClassWriter::addAttribute("start_index", 0, &face_faces);
              SuperClassWriter::addAttribute("_FillValue", 999999, &face_faces);
              SuperClassWriter::addAttribute("flag_values", -1, &face_faces);
              SuperClassWriter::addAttribute("flag_meanings", StdString("out_of_mesh"), &face_faces);
            } // domain->nvertex > 2

            SuperClassWriter::definition_end();

            std::vector<StdSize> startEdges(1) ;
            std::vector<StdSize> countEdges(1) ;
            std::vector<StdSize> startNodes(1) ;
            std::vector<StdSize> countNodes(1) ;
            std::vector<StdSize> startFaces(1) ;
            std::vector<StdSize> countFaces(1) ;
            std::vector<StdSize> startEdgeNodes(2) ;
            std::vector<StdSize> countEdgeNodes(2) ;
            std::vector<StdSize> startEdgeFaces(2) ;
            std::vector<StdSize> countEdgeFaces(2) ;
            std::vector<StdSize> startFaceConctv(2) ;
            std::vector<StdSize> countFaceConctv(2) ;

            if (!isWrittenDomain(domid))
            {
              if (domain->nvertex == 1)
              {
                if (domain->isEmpty())
                 {
                   startNodes[0]=0 ;
                   countNodes[0]=0 ;
                 }
                 else
                 {
                   startNodes[0] = domain->zoom_ibegin_srv-domain->global_zoom_ibegin;
                   countNodes[0] = domain->zoom_ni_srv ;
                 }

                SuperClassWriter::writeData(domain->mesh->node_lat, node_y, isCollective, 0, &startNodes, &countNodes);
                SuperClassWriter::writeData(domain->mesh->node_lon, node_x, isCollective, 0, &startNodes, &countNodes);
              }
              else if (domain->nvertex == 2)
              {
                if (domain->isEmpty())
                 {
                  startEdges[0]=0 ;
                  countEdges[0]=0 ;
                  startNodes[0]=0 ;
                  countNodes[0]=0 ;
                  startEdgeNodes[0]=0;
                  startEdgeNodes[1]=0;
                  countEdgeNodes[0]=0;
                  countEdgeNodes[1]=0;

                 }
                 else
                 {
                   startEdges[0] = domain->zoom_ibegin_srv-domain->global_zoom_ibegin;
                   countEdges[0] = domain->zoom_ni_srv ;
                   startNodes[0] = domain->mesh->node_start;
                   countNodes[0] = domain->mesh->node_count;
                   startEdgeNodes[0] = domain->zoom_ibegin_srv-domain->global_zoom_ibegin;
                   startEdgeNodes[1] = 0;
                   countEdgeNodes[0] = domain->zoom_ni_srv;
                   countEdgeNodes[1]= 2;
                 }
                SuperClassWriter::writeData(domain->mesh->node_lat, node_y, isCollective, 0, &startNodes, &countNodes);
                SuperClassWriter::writeData(domain->mesh->node_lon, node_x, isCollective, 0, &startNodes, &countNodes);
                SuperClassWriter::writeData(domain->mesh->edge_lat, edge_y, isCollective, 0, &startEdges, &countEdges);
                SuperClassWriter::writeData(domain->mesh->edge_lon, edge_x, isCollective, 0, &startEdges, &countEdges);
                SuperClassWriter::writeData(domain->mesh->edge_nodes, edge_nodes, isCollective, 0, &startEdgeNodes, &countEdgeNodes);
              }
              else
              {
                if (domain->isEmpty())
                 {
                   startFaces[0] = 0 ;
                   countFaces[0] = 0 ;
                   startNodes[0] = 0;
                   countNodes[0] = 0;
                   startEdges[0] = 0;
                   countEdges[0] = 0;
                   startEdgeFaces[0] = 0;
                   startEdgeFaces[1] = 0;
                   countEdgeFaces[0] = 0;
                   countEdgeFaces[1] = 0;
                   startFaceConctv[0] = 0;
                   startFaceConctv[1] = 0;
                   countFaceConctv[0] = 0;
                   countFaceConctv[1] = 0;
                 }
                 else
                 {
                   startFaces[0] = domain->zoom_ibegin_srv-domain->global_zoom_ibegin;
                   countFaces[0] = domain->zoom_ni_srv ;
                   startNodes[0] = domain->mesh->node_start;
                   countNodes[0] = domain->mesh->node_count;
                   startEdges[0] = domain->mesh->edge_start;
                   countEdges[0] = domain->mesh->edge_count;
                   startEdgeNodes[0] = domain->mesh->edge_start;
                   startEdgeNodes[1] = 0;
                   countEdgeNodes[0] = domain->mesh->edge_count;
                   countEdgeNodes[1]= 2;
                   startEdgeFaces[0] = domain->mesh->edge_start;
                   startEdgeFaces[1]= 0;
                   countEdgeFaces[0] = domain->mesh->edge_count;
                   countEdgeFaces[1]= 2;
                   startFaceConctv[0] = domain->zoom_ibegin_srv-domain->global_zoom_ibegin;
                   startFaceConctv[1] = 0;
                   countFaceConctv[0] = domain->zoom_ni_srv;
                   countFaceConctv[1] = domain->nvertex;
                 }
                SuperClassWriter::writeData(domain->mesh->node_lat, node_y, isCollective, 0, &startNodes, &countNodes);
                SuperClassWriter::writeData(domain->mesh->node_lon, node_x, isCollective, 0, &startNodes, &countNodes);
                SuperClassWriter::writeData(domain->mesh->edge_lat, edge_y, isCollective, 0, &startEdges, &countEdges);
                SuperClassWriter::writeData(domain->mesh->edge_lon, edge_x, isCollective, 0, &startEdges, &countEdges);
                SuperClassWriter::writeData(domain->mesh->edge_nodes, edge_nodes, isCollective, 0, &startEdgeNodes, &countEdgeNodes);
                SuperClassWriter::writeData(domain->mesh->face_lat, face_y, isCollective, 0, &startFaces, &countFaces);
                SuperClassWriter::writeData(domain->mesh->face_lon, face_x, isCollective, 0, &startFaces, &countFaces);
                SuperClassWriter::writeData(domain->mesh->face_nodes, face_nodes, isCollective, 0, &startFaceConctv, &countFaceConctv);
                SuperClassWriter::writeData(domain->mesh->face_edges, face_edges, isCollective, 0, &startFaceConctv, &countFaceConctv);
                SuperClassWriter::writeData(domain->mesh->edge_faces, edge_faces, isCollective, 0, &startEdgeFaces, &countEdgeFaces);
                SuperClassWriter::writeData(domain->mesh->face_faces, face_faces, isCollective, 0, &startFaceConctv, &countFaceConctv);
              }
              setWrittenDomain(domid);
            } // !isWrittenDomain
            else
            {
              if (domain->nvertex == 2)
              {
                startEdges[0] = domain->zoom_ibegin_srv-domain->global_zoom_ibegin;
                countEdges[0] = domain->zoom_ni_srv ;
                startEdgeNodes[0] = domain->zoom_ibegin_srv-domain->global_zoom_ibegin;
                startEdgeNodes[1] = 0;
                countEdgeNodes[0] = domain->zoom_ni_srv;
                countEdgeNodes[1]= 2;
                SuperClassWriter::writeData(domain->mesh->edge_lat, edge_y, isCollective, 0, &startEdges, &countEdges);
                SuperClassWriter::writeData(domain->mesh->edge_lon, edge_x, isCollective, 0, &startEdges, &countEdges);
                SuperClassWriter::writeData(domain->mesh->edge_nodes, edge_nodes, isCollective, 0, &startEdgeNodes, &countEdgeNodes);
              }  
              if (domain->nvertex > 2)
              {

                if (!domain->mesh->edgesAreWritten)
                {
                  startEdges[0] = domain->mesh->edge_start;
                  countEdges[0] = domain->mesh->edge_count;
                  startEdgeNodes[0] = domain->mesh->edge_start;
                  startEdgeNodes[1] = 0;
                  countEdgeNodes[0] = domain->mesh->edge_count;
                  countEdgeNodes[1]= 2;
                  SuperClassWriter::writeData(domain->mesh->edge_lat, edge_y, isCollective, 0, &startEdges, &countEdges);
                  SuperClassWriter::writeData(domain->mesh->edge_lon, edge_x, isCollective, 0, &startEdges, &countEdges);
                  SuperClassWriter::writeData(domain->mesh->edge_nodes, edge_nodes, isCollective, 0, &startEdgeNodes, &countEdgeNodes);
                }
                startFaces[0] = domain->zoom_ibegin_srv-domain->global_zoom_ibegin;
                countFaces[0] = domain->zoom_ni_srv;
                startEdgeFaces[0] = domain->mesh->edge_start;
                startEdgeFaces[1]= 0;
                countEdgeFaces[0] = domain->mesh->edge_count;
                countEdgeFaces[1]= 2;
                startFaceConctv[0] = domain->zoom_ibegin_srv-domain->global_zoom_ibegin;
                startFaceConctv[1] = 0;
                countFaceConctv[0] = domain->zoom_ni_srv;
                countFaceConctv[1]= domain->nvertex;
                SuperClassWriter::writeData(domain->mesh->face_lat, face_y, isCollective, 0, &startFaces, &countFaces);
                SuperClassWriter::writeData(domain->mesh->face_lon, face_x, isCollective, 0, &startFaces, &countFaces);
                SuperClassWriter::writeData(domain->mesh->face_nodes, face_nodes, isCollective, 0, &startFaceConctv, &countFaceConctv);
                SuperClassWriter::writeData(domain->mesh->face_edges, face_edges, isCollective, 0, &startFaceConctv, &countFaceConctv);
                SuperClassWriter::writeData(domain->mesh->edge_faces, edge_faces, isCollective, 0, &startEdgeFaces, &countEdgeFaces);
                SuperClassWriter::writeData(domain->mesh->face_faces, face_faces, isCollective, 0, &startFaceConctv, &countFaceConctv);
              }
            }// isWrittenDomain

            SuperClassWriter::definition_start();

            break;
          } // ONE_FILE

          case (MULTI_FILE) :
          {
            break;
          }

          default :
          ERROR("CNc4DataOutput::writeDomain(domain)",
          << "[ type = " << SuperClass::type << "]"
          << " not implemented yet !");
          } // switch
        } // try

        catch (CNetCdfException& e)
        {
          StdString msg("On writing the domain : ");
          msg.append(domid); msg.append("\n");
          msg.append("In the context : ");
          msg.append(context->getId()); msg.append("\n");
          msg.append(e.what());
          ERROR("CNc4DataOutput::writeUnstructuredDomainUgrid(CDomain* domain)", << msg);
        }

  domain->addRelFile(this->filename);
  }

    //--------------------------------------------------------------

    void CNc4DataOutput::writeUnstructuredDomain(CDomain* domain)
      {
         CContext* context = CContext::getCurrent() ;
         CContextServer* server=context->server ;

         if (domain->IsWritten(this->filename)) return;
         domain->checkAttributes();

         if (domain->isEmpty())
           if (SuperClass::type==MULTI_FILE) return ;

         std::vector<StdString> dim0, dim1;
         StdString domid = domain->getDomainOutputName();
         if (isWrittenDomain(domid)) return ;
         else setWrittenDomain(domid);

         StdString appendDomid  = (singleDomain) ? "" : "_"+domid ;

         StdString dimXid = StdString("cell").append(appendDomid);
         StdString dimVertId = StdString("nvertex").append(appendDomid);

         string lonid,latid,bounds_lonid,bounds_latid ;
         string areaId = "area" + appendDomid;

         nc_type typePrec ;
         if (domain->prec.isEmpty()) typePrec =  NC_FLOAT ;
         else if (domain->prec==4)  typePrec =  NC_FLOAT ;
         else if (domain->prec==8)   typePrec =  NC_DOUBLE ;

         try
         {
           switch (SuperClass::type)
           {
              case (MULTI_FILE) :
              {
                 dim0.push_back(dimXid);
                 SuperClassWriter::addDimension(dimXid, domain->zoom_ni_srv);

                 lonid = StdString("lon").append(appendDomid);
                 latid = StdString("lat").append(appendDomid);
                 bounds_lonid = StdString("bounds_lon").append(appendDomid);
                 bounds_latid = StdString("bounds_lat").append(appendDomid);
                 if (domain->hasLonLat)
                 {
                   SuperClassWriter::addVariable(latid, typePrec, dim0);
                   SuperClassWriter::addVariable(lonid, typePrec, dim0);
                   this->writeAxisAttributes(lonid, "", "longitude", "Longitude", "degrees_east", domid);
                   if (domain->hasBounds) SuperClassWriter::addAttribute("bounds",bounds_lonid, &lonid);
                   this->writeAxisAttributes(latid, "", "latitude", "Latitude", "degrees_north", domid);
                   if (domain->hasBounds) SuperClassWriter::addAttribute("bounds",bounds_latid, &latid);
                   if (domain->hasBounds) SuperClassWriter::addDimension(dimVertId, domain->nvertex);
                 }
                 dim0.clear();
                 if (domain->hasBounds)
                 {
                   dim0.push_back(dimXid);
                   dim0.push_back(dimVertId);
                   SuperClassWriter::addVariable(bounds_lonid, typePrec, dim0);
                   SuperClassWriter::addVariable(bounds_latid, typePrec, dim0);
                 }

                 dim0.clear();
                 dim0.push_back(dimXid);
                 if (domain->hasArea)
                 {
                   SuperClassWriter::addVariable(areaId, typePrec, dim0);
                   SuperClassWriter::addAttribute("standard_name", StdString("cell_area"), &areaId);
                   SuperClassWriter::addAttribute("units", StdString("m2"), &areaId);
                 }

                 SuperClassWriter::definition_end();

                 if (domain->hasLonLat)
                 {
                   SuperClassWriter::writeData(domain->latvalue_srv, latid, isCollective, 0);
                   SuperClassWriter::writeData(domain->lonvalue_srv, lonid, isCollective, 0);
                   if (domain->hasBounds)
                   {
                     SuperClassWriter::writeData(domain->bounds_lon_srv, bounds_lonid, isCollective, 0);
                     SuperClassWriter::writeData(domain->bounds_lat_srv, bounds_latid, isCollective, 0);
                   }
                 }

                 if (domain->hasArea)
                   SuperClassWriter::writeData(domain->area_srv, areaId, isCollective, 0);

                 SuperClassWriter::definition_start();
                 break ;
              }

              case (ONE_FILE) :
              {
                 lonid = StdString("lon").append(appendDomid);
                 latid = StdString("lat").append(appendDomid);
                 bounds_lonid = StdString("bounds_lon").append(appendDomid);
                 bounds_latid = StdString("bounds_lat").append(appendDomid);
                 dim0.push_back(dimXid);
                 SuperClassWriter::addDimension(dimXid, domain->ni_glo);
                 if (domain->hasLonLat)
                 {
                   SuperClassWriter::addVariable(latid, typePrec, dim0);
                   SuperClassWriter::addVariable(lonid, typePrec, dim0);

                   this->writeAxisAttributes(lonid, "", "longitude", "Longitude", "degrees_east", domid);
                   if (domain->hasBounds) SuperClassWriter::addAttribute("bounds",bounds_lonid, &lonid);
                   this->writeAxisAttributes(latid, "", "latitude", "Latitude", "degrees_north", domid);
                   if (domain->hasBounds) SuperClassWriter::addAttribute("bounds",bounds_latid, &latid);
                   if (domain->hasBounds) SuperClassWriter::addDimension(dimVertId, domain->nvertex);
                 }
                 dim0.clear();

                 if (domain->hasBounds)
                 {
                   dim0.push_back(dimXid);
                   dim0.push_back(dimVertId);
                   SuperClassWriter::addVariable(bounds_lonid, typePrec, dim0);
                   SuperClassWriter::addVariable(bounds_latid, typePrec, dim0);
                 }

                 if (domain->hasArea)
                 {
                   dim0.clear();
                   dim0.push_back(dimXid);
                   SuperClassWriter::addVariable(areaId, typePrec, dim0);
                   SuperClassWriter::addAttribute("standard_name", StdString("cell_area"), &areaId);
                   SuperClassWriter::addAttribute("units", StdString("m2"), &areaId);
                 }

                 SuperClassWriter::definition_end();

                 std::vector<StdSize> start(1), startBounds(2) ;
                 std::vector<StdSize> count(1), countBounds(2) ;
                 if (domain->isEmpty())
                 {
                   start[0]=0 ;
                   count[0]=0 ;
                   startBounds[1]=0 ;
                   countBounds[1]=domain->nvertex ;
                   startBounds[0]=0 ;
                   countBounds[0]=0 ;
                 }
                 else
                 {
                   start[0]=domain->zoom_ibegin_srv-domain->global_zoom_ibegin;
                   count[0]=domain->zoom_ni_srv ;
                   startBounds[0]=domain->zoom_ibegin_srv-domain->global_zoom_ibegin;
                   startBounds[1]=0 ;
                   countBounds[0]=domain->zoom_ni_srv ;
                   countBounds[1]=domain->nvertex ;
                 }

                 if (domain->hasLonLat)
                 {
                   SuperClassWriter::writeData(domain->latvalue_srv, latid, isCollective, 0,&start,&count);
                   SuperClassWriter::writeData(domain->lonvalue_srv, lonid, isCollective, 0,&start,&count);
                   if (domain->hasBounds)
                   {
                     SuperClassWriter::writeData(domain->bounds_lon_srv, bounds_lonid, isCollective, 0,&startBounds,&countBounds);
                     SuperClassWriter::writeData(domain->bounds_lat_srv, bounds_latid, isCollective, 0,&startBounds,&countBounds);
                   }
                 }

                 if (domain->hasArea)
                   SuperClassWriter::writeData(domain->area_srv, areaId, isCollective, 0, &start, &count);

                 SuperClassWriter::definition_start();

                 break;
              }
              default :
                 ERROR("CNc4DataOutput::writeDomain(domain)",
                       << "[ type = " << SuperClass::type << "]"
                       << " not implemented yet !");
           }
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing the domain : ");
           msg.append(domid); msg.append("\n");
           msg.append("In the context : ");
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeUnstructuredDomain(CDomain* domain)", << msg);
         }
         domain->addRelFile(this->filename);
      }
      //--------------------------------------------------------------

      void CNc4DataOutput::writeAxis_(CAxis* axis)
      {
        if (axis->IsWritten(this->filename)) return;
        axis->checkAttributes();
        int zoom_size_srv  = axis->zoom_size_srv;
        int zoom_begin_srv = axis->zoom_begin_srv;
        int zoom_size  = (MULTI_FILE == SuperClass::type) ? zoom_size_srv
                                                          : axis->global_zoom_size_srv;

        if ((0 == zoom_size_srv) && (MULTI_FILE == SuperClass::type)) return;

        std::vector<StdString> dims;
        StdString axisid = axis->getAxisOutputName();
        if (isWrittenAxis(axisid)) return ;
        else setWrittenAxis(axisid);

        nc_type typePrec ;
        if (axis->prec.isEmpty()) typePrec =  NC_FLOAT ;
        else if (axis->prec==4)  typePrec =  NC_FLOAT ;
        else if (axis->prec==8)   typePrec =  NC_DOUBLE ;
         
        if (!axis->label.isEmpty()) typePrec = NC_CHAR ;
        string strId="str_len" ;
        try
        {
          SuperClassWriter::addDimension(axisid, zoom_size);
          if (!axis->label.isEmpty()) SuperClassWriter::addDimension(strId, stringArrayLen);
          if (axis->hasValue)
          {
            dims.push_back(axisid);
            if (!axis->label.isEmpty()) dims.push_back(strId);
            SuperClassWriter::addVariable(axisid, typePrec, dims);

            if (!axis->name.isEmpty())
              SuperClassWriter::addAttribute("name", axis->name.getValue(), &axisid);

            if (!axis->standard_name.isEmpty())
              SuperClassWriter::addAttribute("standard_name", axis->standard_name.getValue(), &axisid);

            if (!axis->long_name.isEmpty())
              SuperClassWriter::addAttribute("long_name", axis->long_name.getValue(), &axisid);

            if (!axis->unit.isEmpty())
              SuperClassWriter::addAttribute("units", axis->unit.getValue(), &axisid);

            if (!axis->positive.isEmpty())
            {
              SuperClassWriter::addAttribute("axis", string("Z"), &axisid);
              SuperClassWriter::addAttribute("positive",
                                             (axis->positive == CAxis::positive_attr::up) ? string("up") : string("down"),
                                             &axisid);
            }

            StdString axisBoundsId = axisid + "_bounds";
            if (!axis->bounds.isEmpty())
            {
              dims.push_back("axis_nbounds");
              SuperClassWriter::addVariable(axisBoundsId, typePrec, dims);
              SuperClassWriter::addAttribute("bounds", axisBoundsId, &axisid);
            }

            SuperClassWriter::definition_end();

            switch (SuperClass::type)
            {
              case MULTI_FILE:
              {
                CArray<double,1> axis_value(zoom_size_srv);
                for (int i = 0; i < zoom_size_srv; i++) axis_value(i) = axis->value_srv(i);
                if (axis->label.isEmpty())  SuperClassWriter::writeData(axis_value, axisid, isCollective, 0);

                if (!axis->bounds.isEmpty() && axis->label.isEmpty())
                  SuperClassWriter::writeData(axis->bound_srv, axisBoundsId, isCollective, 0);

                if (! axis->label.isEmpty())  SuperClassWriter::writeData(axis->label_srv, axisid, isCollective, 0);
 
                SuperClassWriter::definition_start();
                break;
              }
              case ONE_FILE:
              {
                CArray<double,1> axis_value(zoom_size_srv);
                axis_value = axis->value_srv;

                std::vector<StdSize> start(1), startBounds(2) ;
                std::vector<StdSize> count(1), countBounds(2) ;
                start[0] = startBounds[0] = zoom_begin_srv-axis->global_zoom_begin_srv;
                count[0] = countBounds[0] = zoom_size_srv;
                startBounds[1] = 0;
                countBounds[1] = 2;
                if (axis->label.isEmpty()) SuperClassWriter::writeData(axis_value, axisid, isCollective, 0, &start, &count);

                if (!axis->bounds.isEmpty()&& axis->label.isEmpty())
                  SuperClassWriter::writeData(axis->bound_srv, axisBoundsId, isCollective, 0, &startBounds, &countBounds);

                if (! axis->label.isEmpty())  SuperClassWriter::writeData(axis->label_srv, axisid, isCollective, 0);

                SuperClassWriter::definition_start();

                break;
              }
              default :
                ERROR("CNc4DataOutput::writeAxis_(CAxis* axis)",
                      << "[ type = " << SuperClass::type << "]"
                      << " not implemented yet !");
            }
          }
        }
        catch (CNetCdfException& e)
        {
          StdString msg("On writing the axis : ");
          msg.append(axisid); msg.append("\n");
          msg.append("In the context : ");
          CContext* context = CContext::getCurrent() ;
          msg.append(context->getId()); msg.append("\n");
          msg.append(e.what());
          ERROR("CNc4DataOutput::writeAxis_(CAxis* axis)", << msg);
        }
        axis->addRelFile(this->filename);
     }

      void CNc4DataOutput::writeScalar_(CScalar* scalar)
      {
        if (scalar->IsWritten(this->filename)) return;
        scalar->checkAttributes();
        int scalarSize = 1;

        StdString scalaId = scalar->getScalarOutputName();
        if (isWrittenAxis(scalaId)) return ;
        else setWrittenAxis(scalaId);

        nc_type typePrec ;
        if (scalar->prec.isEmpty()) typePrec =  NC_FLOAT ;
        else if (scalar->prec==4)  typePrec =  NC_FLOAT ;
        else if (scalar->prec==8)   typePrec =  NC_DOUBLE ;

        
        try
        {
          if (!scalar->value.isEmpty())
          {
//            dims.push_back(scalaId);
            std::vector<StdString> dims;
            SuperClassWriter::addVariable(scalaId, typePrec, dims);

            if (!scalar->name.isEmpty())
              SuperClassWriter::addAttribute("name", scalar->name.getValue(), &scalaId);

            if (!scalar->standard_name.isEmpty())
              SuperClassWriter::addAttribute("standard_name", scalar->standard_name.getValue(), &scalaId);

            if (!scalar->long_name.isEmpty())
              SuperClassWriter::addAttribute("long_name", scalar->long_name.getValue(), &scalaId);

            if (!scalar->unit.isEmpty())
              SuperClassWriter::addAttribute("units", scalar->unit.getValue(), &scalaId);

            SuperClassWriter::definition_end();

            switch (SuperClass::type)
            {
              case MULTI_FILE:
              {
                CArray<double,1> scalarValue(scalarSize);
                scalarValue(0) = scalar->value;
                SuperClassWriter::writeData(scalarValue, scalaId, isCollective, 0);
                SuperClassWriter::definition_start();

                break;
              }
              case ONE_FILE:
              {
                CArray<double,1> scalarValue(scalarSize);
                scalarValue(0) = scalar->value;

                std::vector<StdSize> start(1);
                std::vector<StdSize> count(1);
                start[0] = 0;
                count[0] = 1;
                SuperClassWriter::writeData(scalarValue, scalaId, isCollective, 0, &start, &count);
                SuperClassWriter::definition_start();

                break;
              }
              default :
                ERROR("CNc4DataOutput::writeAxis_(CAxis* scalar)",
                      << "[ type = " << SuperClass::type << "]"
                      << " not implemented yet !");
            }
          }
        }
        catch (CNetCdfException& e)
        {
          StdString msg("On writing the scalar : ");
          msg.append(scalaId); msg.append("\n");
          msg.append("In the context : ");
          CContext* context = CContext::getCurrent() ;
          msg.append(context->getId()); msg.append("\n");
          msg.append(e.what());
          ERROR("CNc4DataOutput::writeScalar_(CScalar* scalar)", << msg);
        }
        scalar->addRelFile(this->filename);
     }

     //--------------------------------------------------------------

     void CNc4DataOutput::writeGridCompressed_(CGrid* grid)
     {
       if (grid->isScalarGrid() || grid->isWrittenCompressed(this->filename)) return;

       try
       {
         CArray<int,1> axisDomainOrder = grid->axis_domain_order;
         std::vector<StdString> domainList = grid->getDomainList();
         std::vector<StdString> axisList   = grid->getAxisList();
         std::vector<StdString> scalarList = grid->getScalarList();
         int numElement = axisDomainOrder.numElements(), idxDomain = 0, idxAxis = 0, idxScalar = 0;

         std::vector<StdString> dims;

         if (grid->isCompressible())
         {
           StdString varId = grid->getId() + "_points";

           int nbIndexes = (SuperClass::type == MULTI_FILE) ? grid->getNumberWrittenIndexes() : grid->getTotalNumberWrittenIndexes();
           SuperClassWriter::addDimension(varId, nbIndexes);

           dims.push_back(varId);
           SuperClassWriter::addVariable(varId, NC_INT, dims);

           StdOStringStream compress;
           for (int i = numElement - 1; i >= 0; --i)
           {
             if (2 == axisDomainOrder(i))
             {
               CDomain* domain = CDomain::get(domainList[domainList.size() - idxDomain - 1]);
               StdString domId = domain->getDomainOutputName();
               StdString appendDomId  = singleDomain ? "" : "_" + domId;

               switch (domain->type)
               {
                 case CDomain::type_attr::curvilinear:
                   compress << "y" << appendDomId << " x" << appendDomId;
                   break;
                 case CDomain::type_attr::rectilinear:
                   compress << "lat" << appendDomId << " lon" << appendDomId;
                   break;
                 case CDomain::type_attr::unstructured:
                   compress << "cell" << appendDomId;
                   break;
               }
               ++idxDomain;
             }
             else if (1 == axisDomainOrder(i))
             {
               CAxis* axis = CAxis::get(axisList[axisList.size() - idxAxis - 1]);
               compress << axis->getAxisOutputName();
               ++idxAxis;
             }
             else
             {
               CScalar* scalar = CScalar::get(scalarList[scalarList.size() - idxScalar - 1]);
               compress << scalar->getScalarOutputName();
               ++idxScalar;
             }

             if (i != 0) compress << ' ';
           }
           SuperClassWriter::addAttribute("compress", compress.str(), &varId);

           grid->computeCompressedIndex();

           CArray<int, 1> indexes(grid->getNumberWrittenIndexes());
           std::map<int, CArray<size_t, 1> >::const_iterator it;
           for (it = grid->outIndexFromClient.begin(); it != grid->outIndexFromClient.end(); ++it)
           {
             const CArray<size_t, 1> compressedIndexes = grid->compressedOutIndexFromClient[it->first];
             for (int i = 0; i < it->second.numElements(); i++)
               indexes(compressedIndexes(i)) = it->second(i);
           }

           switch (SuperClass::type)
           {
             case (MULTI_FILE):
             {
               SuperClassWriter::writeData(indexes, varId, isCollective, 0);
               break;
             }
             case (ONE_FILE):
             {
               if (grid->doGridHaveDataDistributed())
                 grid->getDistributionServer()->computeGlobalIndex(indexes);

               std::vector<StdSize> start, count;
               start.push_back(grid->getOffsetWrittenIndexes());
               count.push_back(grid->getNumberWrittenIndexes());

               SuperClassWriter::writeData(indexes, varId, isCollective, 0, &start, &count);
               break;
             }
           }
         }
         else
         {
           for (int i = 0; i < numElement; ++i)
           {
             StdString varId, compress;
             CArray<int, 1> indexes;
             bool isDistributed;
             StdSize nbIndexes, totalNbIndexes, offset;
             int firstGlobalIndex;

             if (2 == axisDomainOrder(i))
             {
               CDomain* domain = CDomain::get(domainList[idxDomain]);
               StdString domId = domain->getDomainOutputName();

               if (!domain->isCompressible()
                    || domain->type == CDomain::type_attr::unstructured
                    || domain->isWrittenCompressed(this->filename)
                    || isWrittenCompressedDomain(domId))
                 continue;

               StdString appendDomId  = singleDomain ? "" : "_" + domId;

               varId = domId + "_points";
               switch (domain->type)
               {
                 case CDomain::type_attr::curvilinear:
                   compress = "y" + appendDomId + " x" + appendDomId;
                   break;
                 case CDomain::type_attr::rectilinear:
                   compress = "lat" + appendDomId + " lon" + appendDomId;
                   break;
               }

               const std::vector<int>& indexesToWrite = domain->getIndexesToWrite();
               indexes.resize(indexesToWrite.size());
               for (int n = 0; n < indexes.numElements(); ++n)
                 indexes(n) = indexesToWrite[n];

               isDistributed = domain->isDistributed();
               nbIndexes = domain->getNumberWrittenIndexes();
               totalNbIndexes = domain->getTotalNumberWrittenIndexes();
               offset = domain->getOffsetWrittenIndexes();
               firstGlobalIndex = domain->ibegin + domain->jbegin * domain->ni_glo;

               domain->addRelFileCompressed(this->filename);
               setWrittenCompressedDomain(domId);
               ++idxDomain;
             }
             else if (1 == axisDomainOrder(i))
             {
               CAxis* axis = CAxis::get(axisList[idxAxis]);
               StdString axisId = axis->getAxisOutputName();

               if (!axis->isCompressible()
                    || axis->isWrittenCompressed(this->filename)
                    || isWrittenCompressedAxis(axisId))
                 continue;

               varId = axisId + "_points";
               compress = axisId;

               const std::vector<int>& indexesToWrite = axis->getIndexesToWrite();
               indexes.resize(indexesToWrite.size());
               for (int n = 0; n < indexes.numElements(); ++n)
                 indexes(n) = indexesToWrite[n];

               isDistributed = axis->isDistributed();
               nbIndexes = axis->getNumberWrittenIndexes();
               totalNbIndexes = axis->getTotalNumberWrittenIndexes();
               offset = axis->getOffsetWrittenIndexes();
               firstGlobalIndex = axis->begin;

               axis->addRelFileCompressed(this->filename);
               setWrittenCompressedAxis(axisId);
               ++idxAxis;
             }
             else
             {
             }

             if (!varId.empty())
             {
               SuperClassWriter::addDimension(varId, (SuperClass::type == MULTI_FILE) ? nbIndexes : totalNbIndexes);

               dims.clear();
               dims.push_back(varId);
               SuperClassWriter::addVariable(varId, NC_INT, dims);

               SuperClassWriter::addAttribute("compress", compress, &varId);

               switch (SuperClass::type)
               {
                 case (MULTI_FILE):
                 {
                   indexes -= firstGlobalIndex;
                   SuperClassWriter::writeData(indexes, varId, isCollective, 0);
                   break;
                 }
                 case (ONE_FILE):
                 {
                   std::vector<StdSize> start, count;
                   start.push_back(offset);
                   count.push_back(nbIndexes);

                   SuperClassWriter::writeData(indexes, varId, isCollective, 0, &start, &count);
                   break;
                 }
               }
             }
           }

           if (!dims.empty())
             grid->computeCompressedIndex();
         }

         grid->addRelFileCompressed(this->filename);
       }
       catch (CNetCdfException& e)
       {
         StdString msg("On writing compressed grid : ");
         msg.append(grid->getId()); msg.append("\n");
         msg.append("In the context : ");
         CContext* context = CContext::getCurrent();
         msg.append(context->getId()); msg.append("\n");
         msg.append(e.what());
         ERROR("CNc4DataOutput::writeGridCompressed_(CGrid* grid)", << msg);
       }
     }

     //--------------------------------------------------------------

     void CNc4DataOutput::writeTimeDimension_(void)
     {
       try
       {
        SuperClassWriter::addDimension(getTimeCounterName());
       }
       catch (CNetCdfException& e)
       {
         StdString msg("On writing time dimension : time_couter\n");
         msg.append("In the context : ");
         CContext* context = CContext::getCurrent() ;
         msg.append(context->getId()); msg.append("\n");
         msg.append(e.what());
         ERROR("CNc4DataOutput::writeTimeDimension_(void)", << msg);
       }
     }

      //--------------------------------------------------------------

      void CNc4DataOutput::writeField_(CField* field)
      {
        CContext* context = CContext::getCurrent() ;
        CContextServer* server=context->server ;

        std::vector<StdString> dims, coodinates;
        CGrid* grid = field->grid;
        if (!grid->doGridHaveDataToWrite())
          if (SuperClass::type==MULTI_FILE) return ;

        CArray<int,1> axisDomainOrder = grid->axis_domain_order;
        int numElement = axisDomainOrder.numElements(), idxDomain = 0, idxAxis = 0, idxScalar = 0;
        std::vector<StdString> domainList = grid->getDomainList();
        std::vector<StdString> axisList   = grid->getAxisList();
        std::vector<StdString> scalarList = grid->getScalarList();        

        StdString timeid  = getTimeCounterName();
        StdString dimXid,dimYid;
        std::deque<StdString> dimIdList, dimCoordList;
        bool hasArea = false;
        StdString cellMeasures = "area:";
        bool compressedOutput = !field->indexed_output.isEmpty() && field->indexed_output;

        for (int i = 0; i < numElement; ++i)
        {
          if (2 == axisDomainOrder(i))
          {
            CDomain* domain = CDomain::get(domainList[idxDomain]);
            StdString domId = domain->getDomainOutputName();
            StdString appendDomId  = singleDomain ? "" : "_" + domId ;

            if (compressedOutput && domain->isCompressible() && domain->type != CDomain::type_attr::unstructured)
            {
              dimIdList.push_back(domId + "_points");
              field->setUseCompressedOutput();
            }

            switch (domain->type)
            {
              case CDomain::type_attr::curvilinear:
                if (!compressedOutput || !domain->isCompressible())
                {
                  dimXid     = StdString("x").append(appendDomId);
                  dimIdList.push_back(dimXid);
                  dimYid     = StdString("y").append(appendDomId);
                  dimIdList.push_back(dimYid);
                }
                dimCoordList.push_back(StdString("nav_lon").append(appendDomId));
                dimCoordList.push_back(StdString("nav_lat").append(appendDomId));
              break ;
              case CDomain::type_attr::rectilinear:
                if (!compressedOutput || !domain->isCompressible())
                {
                  dimXid     = StdString("lon").append(appendDomId);
                  dimIdList.push_back(dimXid);
                  dimYid     = StdString("lat").append(appendDomId);
                  dimIdList.push_back(dimYid);
                }
              break ;
              case CDomain::type_attr::unstructured:
              {
                if (SuperClassWriter::useCFConvention)
                {
                  dimXid     = StdString("cell").append(appendDomId);
                  dimIdList.push_back(dimXid);
                  dimCoordList.push_back(StdString("lon").append(appendDomId));
                  dimCoordList.push_back(StdString("lat").append(appendDomId));
                }
                else
                {
                  StdString domainName = domain->name;
                  if (domain->nvertex == 1)
                  {
                    dimXid     = "n" + domainName + "_node";
                    dimIdList.push_back(dimXid);
                    dimCoordList.push_back(StdString(domainName + "_node_x"));
                    dimCoordList.push_back(StdString(domainName + "_node_y"));
                  }
                  else if (domain->nvertex == 2)
                  {
                    dimXid     = "n" + domainName + "_edge";
                    dimIdList.push_back(dimXid);
                    dimCoordList.push_back(StdString(domainName + "_edge_x"));
                    dimCoordList.push_back(StdString(domainName + "_edge_y"));
                  }
                  else
                  {
                    dimXid     = "n" + domainName + "_face";
                    dimIdList.push_back(dimXid);
                    dimCoordList.push_back(StdString(domainName + "_face_x"));
                    dimCoordList.push_back(StdString(domainName + "_face_y"));
                  }
                }  // ugrid convention
              }  // case unstructured domain
            }

            if (domain->hasArea)
            {
              hasArea = true;
              cellMeasures += " area" + appendDomId;
            }
            ++idxDomain;
          }
          else if (1 == axisDomainOrder(i))
          {
            CAxis* axis = CAxis::get(axisList[idxAxis]);
            StdString axisId = axis->getAxisOutputName();

            if (compressedOutput && axis->isCompressible())
            {
              dimIdList.push_back(axisId + "_points");
              field->setUseCompressedOutput();
            }
            else
              dimIdList.push_back(axisId);

            dimCoordList.push_back(axisId);
            ++idxAxis;
          }
          else // scalar
          {
             /* Do nothing here */
          }
        }

        StdString fieldid = field->getFieldOutputName();

        nc_type type ;
        if (field->prec.isEmpty()) type =  NC_FLOAT ;
        else
        {
          if (field->prec==2) type = NC_SHORT ;
          else if (field->prec==4)  type =  NC_FLOAT ;
          else if (field->prec==8)   type =  NC_DOUBLE ;
        }

        bool wtime   = !(!field->operation.isEmpty() && field->getOperationTimeType() == func::CFunctor::once);

        if (wtime)
        {
          if (field->hasTimeInstant && hasTimeInstant) coodinates.push_back(string("time_instant"));
          else if (field->hasTimeCentered && hasTimeCentered)  coodinates.push_back(string("time_centered"));
          dims.push_back(timeid);
        }

        if (compressedOutput && grid->isCompressible())
        {
          dims.push_back(grid->getId() + "_points");
          field->setUseCompressedOutput();
        }
        else
        {
          while (!dimIdList.empty())
          {
            dims.push_back(dimIdList.back());
            dimIdList.pop_back();
          }
        }

        while (!dimCoordList.empty())
        {
          coodinates.push_back(dimCoordList.back());
          dimCoordList.pop_back();
        }

        try
        {
           SuperClassWriter::addVariable(fieldid, type, dims);

           if (!field->standard_name.isEmpty())
              SuperClassWriter::addAttribute
                 ("standard_name",  field->standard_name.getValue(), &fieldid);

           if (!field->long_name.isEmpty())
              SuperClassWriter::addAttribute
                 ("long_name", field->long_name.getValue(), &fieldid);

           if (!field->unit.isEmpty())
              SuperClassWriter::addAttribute
                 ("units", field->unit.getValue(), &fieldid);

           // Ugrid field attributes "mesh" and "location"
           if (!SuperClassWriter::useCFConvention)
           {
            if (!domainList.empty())
            {
              CDomain* domain = CDomain::get(domainList[0]); // Suppose that we have only domain
              StdString mesh = domain->name;
              SuperClassWriter::addAttribute("mesh", mesh, &fieldid);
              StdString location;
              if (domain->nvertex == 1)
                location = "node";
              else if (domain->nvertex == 2)
                location = "edge";
              else if (domain->nvertex > 2)
                location = "face";
              SuperClassWriter::addAttribute("location", location, &fieldid);
            }

           }

           if (!field->valid_min.isEmpty())
              SuperClassWriter::addAttribute
                 ("valid_min", field->valid_min.getValue(), &fieldid);

           if (!field->valid_max.isEmpty())
              SuperClassWriter::addAttribute
                 ("valid_max", field->valid_max.getValue(), &fieldid);

            if (!field->scale_factor.isEmpty())
              SuperClassWriter::addAttribute
                 ("scale_factor", field->scale_factor.getValue(), &fieldid);

             if (!field->add_offset.isEmpty())
              SuperClassWriter::addAttribute
                 ("add_offset", field->add_offset.getValue(), &fieldid);

           SuperClassWriter::addAttribute
                 ("online_operation", field->operation.getValue(), &fieldid);

          // write child variables as attributes


           vector<CVariable*> listVars = field->getAllVariables() ;
           for (vector<CVariable*>::iterator it = listVars.begin() ;it != listVars.end(); it++) writeAttribute_(*it, fieldid) ;

           bool alreadyAddCellMethod = false;
           StdString cellMethodsPrefix(""), cellMethodsSuffix("");
           if (!field->cell_methods.isEmpty())
           {
              StdString cellMethodString = field->cell_methods;
              if (field->cell_methods_mode.isEmpty() ||
                 (CField::cell_methods_mode_attr::overwrite == field->cell_methods_mode))
              {
                SuperClassWriter::addAttribute("cell_methods", cellMethodString, &fieldid);
                alreadyAddCellMethod = true;
              }
              else
              {
                switch (field->cell_methods_mode)
                {
                  case (CField::cell_methods_mode_attr::prefix):
                    cellMethodsPrefix = cellMethodString;
                    cellMethodsPrefix += " ";
                    break;
                  case (CField::cell_methods_mode_attr::suffix):
                    cellMethodsSuffix = " ";
                    cellMethodsSuffix += cellMethodString;
                    break;
                  case (CField::cell_methods_mode_attr::none):
                    break;
                  default:
                    break;
                }
              }
           }


           if (wtime)
           {
              CDuration freqOp = field->freq_op.getValue();
              freqOp.solveTimeStep(*context->calendar);
              StdString freqOpStr = freqOp.toStringUDUnits();
              SuperClassWriter::addAttribute("interval_operation", freqOpStr, &fieldid);

              CDuration freqOut = field->getRelFile()->output_freq.getValue();
              freqOut.solveTimeStep(*context->calendar);
              SuperClassWriter::addAttribute("interval_write", freqOut.toStringUDUnits(), &fieldid);

              StdString cellMethods(cellMethodsPrefix + "time: ");
              if (field->operation.getValue() == "instant") cellMethods += "point";
              else if (field->operation.getValue() == "average") cellMethods += "mean";
              else if (field->operation.getValue() == "accumulate") cellMethods += "sum";
              else cellMethods += field->operation;
              if (freqOp.resolve(*context->calendar) != freqOut.resolve(*context->calendar))
                cellMethods += " (interval: " + freqOpStr + ")";
              cellMethods += cellMethodsSuffix;
              if (!alreadyAddCellMethod)
                SuperClassWriter::addAttribute("cell_methods", cellMethods, &fieldid);
           }

           if (hasArea)
             SuperClassWriter::addAttribute("cell_measures", cellMeasures, &fieldid);

           if (!field->default_value.isEmpty())
           {
              double default_value = field->default_value.getValue();
              float fdefault_value = (float)default_value;
              if (type == NC_DOUBLE)
                 SuperClassWriter::setDefaultValue(fieldid, &default_value);
              else
                 SuperClassWriter::setDefaultValue(fieldid, &fdefault_value);
           }
           else
              SuperClassWriter::setDefaultValue(fieldid, (double*)NULL);

            if (field->compression_level.isEmpty())
              field->compression_level = field->file->compression_level.isEmpty() ? 0 : field->file->compression_level;
            SuperClassWriter::setCompressionLevel(fieldid, field->compression_level);

           {  // Ecriture des coordonnes

              StdString coordstr; //boost::algorithm::join(coodinates, " ")
              std::vector<StdString>::iterator
                 itc = coodinates.begin(), endc = coodinates.end();

              for (; itc!= endc; itc++)
              {
                 StdString & coord = *itc;
                 if (itc+1 != endc)
                       coordstr.append(coord).append(" ");
                 else  coordstr.append(coord);
              }

              SuperClassWriter::addAttribute("coordinates", coordstr, &fieldid);

           }
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing field : ");
           msg.append(fieldid); msg.append("\n");
           msg.append("In the context : ");
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeField_(CField* field)", << msg);
         }
      } // writeField_()

      //--------------------------------------------------------------

      void CNc4DataOutput::writeFile_ (CFile* file)
      {
         StdString filename = file->getFileOutputName();
         StdString description = (!file->description.isEmpty())
                               ? file->description.getValue()
                               : StdString("Created by xios");

         singleDomain = (file->nbDomains == 1);

         StdString conv_str ;
         if (file->convention_str.isEmpty())
         {
            if (SuperClassWriter::useCFConvention) conv_str="CF-1.6" ;
            else conv_str="UGRID" ;
         }
         else conv_str=file->convention_str ;
           
         try
         {
           this->writeFileAttributes(filename, description,
                                      conv_str,
                                      StdString("An IPSL model"),
                                      this->getTimeStamp());

           if (!appendMode)
             SuperClassWriter::addDimension("axis_nbounds", 2);
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing file : ");
           msg.append(filename); msg.append("\n");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeFile_ (CFile* file)", << msg);
         }
      }

      void CNc4DataOutput::writeAttribute_ (CVariable* var, const string& fieldId)
      {
        StdString name = var->getVariableOutputName();

        try
        {
          if (var->type.getValue() == CVariable::type_attr::t_int || var->type.getValue() == CVariable::type_attr::t_int32)
            addAttribute(name, var->getData<int>(), &fieldId);
          else if (var->type.getValue() == CVariable::type_attr::t_int16)
            addAttribute(name, var->getData<short int>(), &fieldId);
          else if (var->type.getValue() == CVariable::type_attr::t_float)
            addAttribute(name, var->getData<float>(), &fieldId);
          else if (var->type.getValue() == CVariable::type_attr::t_double)
            addAttribute(name, var->getData<double>(), &fieldId);
          else if (var->type.getValue() == CVariable::type_attr::t_string)
            addAttribute(name, var->getData<string>(), &fieldId);
          else
            ERROR("CNc4DataOutput::writeAttribute_ (CVariable* var, const string& fieldId)",
                  << "Unsupported variable of type " << var->type.getStringValue());
        }
       catch (CNetCdfException& e)
       {
         StdString msg("On writing attributes of variable with name : ");
         msg.append(name); msg.append("in the field "); msg.append(fieldId); msg.append("\n");
         msg.append("In the context : ");
         CContext* context = CContext::getCurrent() ;
         msg.append(context->getId()); msg.append("\n");
         msg.append(e.what());
         ERROR("CNc4DataOutput::writeAttribute_ (CVariable* var, const string& fieldId)", << msg);
       }
     }

     void CNc4DataOutput::writeAttribute_ (CVariable* var)
     {
        StdString name = var->getVariableOutputName();

        try
        {
          if (var->type.getValue() == CVariable::type_attr::t_int || var->type.getValue() == CVariable::type_attr::t_int32)
            addAttribute(name, var->getData<int>());
          else if (var->type.getValue() == CVariable::type_attr::t_int16)
            addAttribute(name, var->getData<short int>());
          else if (var->type.getValue() == CVariable::type_attr::t_float)
            addAttribute(name, var->getData<float>());
          else if (var->type.getValue() == CVariable::type_attr::t_double)
            addAttribute(name, var->getData<double>());
          else if (var->type.getValue() == CVariable::type_attr::t_string)
            addAttribute(name, var->getData<string>());
          else
            ERROR("CNc4DataOutput::writeAttribute_ (CVariable* var)",
                  << "Unsupported variable of type " << var->type.getStringValue());
        }
       catch (CNetCdfException& e)
       {
         StdString msg("On writing attributes of variable with name : ");
         msg.append(name); msg.append("\n");
         msg.append("In the context : ");
         CContext* context = CContext::getCurrent() ;
         msg.append(context->getId()); msg.append("\n");
         msg.append(e.what());
         ERROR("CNc4DataOutput::writeAttribute_ (CVariable* var)", << msg);
       }
     }

      void CNc4DataOutput::syncFile_ (void)
      {
        try
        {
          SuperClassWriter::sync() ;
        }
        catch (CNetCdfException& e)
        {
         StdString msg("On synchronizing the write among processes");
         msg.append("In the context : ");
         CContext* context = CContext::getCurrent() ;
         msg.append(context->getId()); msg.append("\n");
         msg.append(e.what());
         ERROR("CNc4DataOutput::syncFile_ (void)", << msg);
        }
      }

      void CNc4DataOutput::closeFile_ (void)
      {
        try
        {
          SuperClassWriter::close() ;
        }
        catch (CNetCdfException& e)
        {
         StdString msg("On closing file");
         msg.append("In the context : ");
         CContext* context = CContext::getCurrent() ;
         msg.append(context->getId()); msg.append("\n");
         msg.append(e.what());
         ERROR("CNc4DataOutput::syncFile_ (void)", << msg);
        }

      }

      //---------------------------------------------------------------

      StdString CNc4DataOutput::getTimeStamp(void) const
      {
         const int buffer_size = 100;
         time_t rawtime;
         struct tm * timeinfo = NULL;
         char buffer [buffer_size];
         StdString formatStr;
         if (file->time_stamp_format.isEmpty()) formatStr="%Y-%b-%d %H:%M:%S %Z" ;
         else formatStr=file->time_stamp_format;

//         time ( &rawtime );
//         timeinfo = localtime ( &rawtime );
         time ( &rawtime );
         timeinfo = gmtime ( &rawtime );
         strftime (buffer, buffer_size, formatStr.c_str(), timeinfo);

         return (StdString(buffer));
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeFieldData_ (CField*  field)
      {
        CContext* context = CContext::getCurrent();
        CContextServer* server = context->server;
        CGrid* grid = field->grid;

        if (field->getNStep()<1) 
        {
          return;
        }
        
        if (!grid->doGridHaveDataToWrite())
          if (SuperClass::type == MULTI_FILE || !isCollective)
          {
            return;
          }

          
        StdString fieldid = field->getFieldOutputName();

        StdOStringStream oss;
        string timeAxisId;
        if (field->hasTimeInstant) timeAxisId = "time_instant";
        else if (field->hasTimeCentered) timeAxisId = "time_centered";

        StdString timeBoundId = getTimeCounterName() + "_bounds";

        StdString timeAxisBoundId;
        if (field->hasTimeInstant) timeAxisBoundId = "time_instant_bounds";
        else if (field->hasTimeCentered) timeAxisBoundId = "time_centered_bounds";

        if (!field->wasWritten())
        {
          if (appendMode && field->file->record_offset.isEmpty() && 
              field->getOperationTimeType() != func::CFunctor::once)
          {
            double factorUnit;
            if (!field->file->time_units.isEmpty() && field->file->time_units==CFile::time_units_attr::days)
            factorUnit=context->getCalendar()->getDayLengthInSeconds() ;
            else factorUnit=1 ;
            field->resetNStep(getRecordFromTime(field->last_Write_srv,factorUnit) + 1);
          }

          field->setWritten();
        }


        CArray<double,1> time_data(1);
        CArray<double,1> time_data_bound(2);
        CArray<double,1> time_counter(1);
        CArray<double,1> time_counter_bound(2);

        bool wtime = (field->getOperationTimeType() != func::CFunctor::once);
        bool wtimeCounter =false ;
        bool wtimeData =false ;
        

        if (wtime)
        {

          Time lastWrite = field->last_Write_srv;
          Time lastLastWrite = field->lastlast_Write_srv;

          
          if (field->hasTimeInstant)
          {
            time_data(0) = time_data_bound(1) = lastWrite;
            time_data_bound(0) = time_data_bound(1) = lastWrite;
            if (timeCounterType==instant)
            {
              time_counter(0) = time_data(0);
              time_counter_bound(0) = time_data_bound(0);
              time_counter_bound(1) = time_data_bound(1);
              wtimeCounter=true ;
            }
            if (hasTimeInstant) wtimeData=true ;
          }
          else if (field->hasTimeCentered)
          {
            time_data(0) = (lastWrite + lastLastWrite) / 2;
            time_data_bound(0) = lastLastWrite;
            time_data_bound(1) = lastWrite;
            if (timeCounterType==centered)
            {
              time_counter(0) = time_data(0) ;
              time_counter_bound(0) = time_data_bound(0) ;
              time_counter_bound(1) = time_data_bound(1) ;
              wtimeCounter=true ;
            }
            if (hasTimeCentered) wtimeData=true ;
          }

          if (timeCounterType==record)
          {
            time_counter(0) = field->getNStep() - 1;
            time_counter_bound(0) = time_counter_bound(1) = field->getNStep() - 1;
            wtimeCounter=true ;
          }

          if (!field->file->time_units.isEmpty() && field->file->time_units==CFile::time_units_attr::days)
          {
            double secByDay=context->getCalendar()->getDayLengthInSeconds() ;
            time_data/=secByDay;
            time_data_bound/=secByDay;
            time_counter/=secByDay;
            time_counter_bound/=secByDay;
          }
        }

         bool isRoot = (server->intraCommRank == 0);

         if (!field->scale_factor.isEmpty() || !field->add_offset.isEmpty())
         {
           double scaleFactor = 1.0;
           double addOffset = 0.0;
           if (!field->scale_factor.isEmpty()) scaleFactor = field->scale_factor;
           if (!field->add_offset.isEmpty()) addOffset = field->add_offset;
           field->scaleFactorAddOffset(scaleFactor, addOffset);
         }

         try
         {
           size_t writtenSize;
           if (field->getUseCompressedOutput())
             writtenSize = grid->getNumberWrittenIndexes();
           else
             writtenSize = grid->getWrittenDataSize();

           CArray<double,1> fieldData(writtenSize);
           if (!field->default_value.isEmpty()) fieldData = field->default_value;

           if (field->getUseCompressedOutput())
             field->outputCompressedField(fieldData);
           else
             field->outputField(fieldData);

           if (!field->prec.isEmpty() && field->prec == 2) fieldData = round(fieldData);

           switch (SuperClass::type)
           {
              case (MULTI_FILE) :
              {
                 CTimer::get("Files : writing data").resume();
                 SuperClassWriter::writeData(fieldData, fieldid, isCollective, field->getNStep() - 1);
                 CTimer::get("Files : writing data").suspend();
                 if (wtime)
                 {
                   CTimer::get("Files : writing time axis").resume();
                   if ( wtimeData)
                   {
//                     SuperClassWriter::writeData(time_data, timeAxisId, isCollective, field->getNStep() - 1);
//                     SuperClassWriter::writeData(time_data_bound, timeAxisBoundId, isCollective, field->getNStep() - 1);
                       SuperClassWriter::writeTimeAxisData(time_data, timeAxisId, isCollective, field->getNStep() - 1, isRoot);
                       SuperClassWriter::writeTimeAxisDataBounds(time_data_bound, timeAxisBoundId, isCollective, field->getNStep() - 1, isRoot);
                  }
                   if (wtimeCounter)
                   {
//                     SuperClassWriter::writeData(time_counter, getTimeCounterName(), isCollective, field->getNStep() - 1);
//                     if (timeCounterType!=record) SuperClassWriter::writeData(time_counter_bound, timeBoundId, isCollective, field->getNStep() - 1);
                     SuperClassWriter::writeTimeAxisData(time_counter, getTimeCounterName(), isCollective, field->getNStep() - 1,isRoot);
                     if (timeCounterType!=record) SuperClassWriter::writeTimeAxisDataBounds(time_counter_bound, timeBoundId, isCollective, field->getNStep() - 1, isRoot);
                   }
                   CTimer::get("Files : writing time axis").suspend();
                 }
                 break;
              }
              case (ONE_FILE) :
              {
                const std::vector<int>& nZoomBeginGlobal = grid->getDistributionServer()->getZoomBeginGlobal();
                const std::vector<int>& nZoomBeginServer = grid->getDistributionServer()->getZoomBeginServer();
                const std::vector<int>& nZoomSizeServer  = grid->getDistributionServer()->getZoomSizeServer();

                std::vector<StdSize> start, count;

                if (field->getUseCompressedOutput())
                {
                  if (grid->isCompressible())
                  {
                    start.push_back(grid->getOffsetWrittenIndexes());
                    count.push_back(grid->getNumberWrittenIndexes());
                  }
                  else
                  {
                    CArray<int,1> axisDomainOrder = grid->axis_domain_order;
                    std::vector<StdString> domainList = grid->getDomainList();
                    std::vector<StdString> axisList   = grid->getAxisList();
                    int numElement = axisDomainOrder.numElements();
                    int idxDomain = domainList.size() - 1, idxAxis = axisList.size() - 1;
                    int idx = nZoomBeginGlobal.size() - 1;

                    start.reserve(nZoomBeginGlobal.size());
                    count.reserve(nZoomBeginGlobal.size());


                    for (int i = numElement - 1; i >= 0; --i)
                    {
                      if (2 == axisDomainOrder(i))
                      {
                        CDomain* domain = CDomain::get(domainList[idxDomain]);

                        if (domain->isCompressible())
                        {
                          start.push_back(domain->getOffsetWrittenIndexes());
                          count.push_back(domain->getNumberWrittenIndexes());
                          idx -= 2;
                        }
                        else
                        {
                          if ((domain->type) != CDomain::type_attr::unstructured)
                          {
                            start.push_back(nZoomBeginServer[idx] - nZoomBeginGlobal[idx]);
                            count.push_back(nZoomSizeServer[idx]);
                          }
                          --idx;
                          start.push_back(nZoomBeginServer[idx] - nZoomBeginGlobal[idx]);
                          count.push_back(nZoomSizeServer[idx]);
                          --idx;
                        }
                        --idxDomain;
                      }
                      else if (1 == axisDomainOrder(i))
                      {
                        CAxis* axis = CAxis::get(axisList[idxAxis]);

                        if (axis->isCompressible())
                        {
                          start.push_back(axis->getOffsetWrittenIndexes());
                          count.push_back(axis->getNumberWrittenIndexes());
                        }
                        else
                        {
                          start.push_back(nZoomBeginServer[idx] - nZoomBeginGlobal[idx]);
                          count.push_back(nZoomSizeServer[idx]);
                        }

                        --idxAxis;
                        --idx;
                      }
                    }
                  }
                }
                else
                {

                  CArray<int,1> axisDomainOrder = grid->axis_domain_order;
                  std::vector<StdString> domainList = grid->getDomainList();
                  std::vector<StdString> axisList   = grid->getAxisList();
                  int numElement = axisDomainOrder.numElements();
                  int idxDomain = domainList.size() - 1, idxAxis = axisList.size() - 1;
                  int idx = nZoomBeginGlobal.size() - 1;

                  start.reserve(nZoomBeginGlobal.size());
                  count.reserve(nZoomBeginGlobal.size());

                  for (int i = numElement - 1; i >= 0; --i)
                  {
                    if (2 == axisDomainOrder(i))
                    {
                      CDomain* domain = CDomain::get(domainList[idxDomain]);
                      if ((domain->type) != CDomain::type_attr::unstructured)
                      {
                        start.push_back(nZoomBeginServer[idx] - nZoomBeginGlobal[idx]);
                        count.push_back(nZoomSizeServer[idx]);
                      }
                      --idx ;
                      start.push_back(nZoomBeginServer[idx] - nZoomBeginGlobal[idx]);
                      count.push_back(nZoomSizeServer[idx]);
                      --idx ;
                      --idxDomain;
                    }
                    else if (1 == axisDomainOrder(i))
                    {
                      start.push_back(nZoomBeginServer[idx] - nZoomBeginGlobal[idx]);
                      count.push_back(nZoomSizeServer[idx]);
                      --idx;
                    }
                    else
                    {
                      if (1 == axisDomainOrder.numElements())
                      {
                        start.push_back(0);
                        count.push_back(1);
                      }
                      --idx;
                    }
                  }
                }


                CTimer::get("Files : writing data").resume();
                SuperClassWriter::writeData(fieldData, fieldid, isCollective, field->getNStep() - 1, &start, &count);
                CTimer::get("Files : writing data").suspend();

                 if (wtime)
                 {
                   CTimer::get("Files : writing time axis").resume();
                   if ( wtimeData)
                   {
//                     SuperClassWriter::writeData(time_data, timeAxisId, isCollective, field->getNStep() - 1);
//                     SuperClassWriter::writeData(time_data_bound, timeAxisBoundId, isCollective, field->getNStep() - 1);
                     SuperClassWriter::writeTimeAxisData(time_data, timeAxisId, isCollective, field->getNStep() - 1, isRoot);
                     SuperClassWriter::writeTimeAxisDataBounds(time_data_bound, timeAxisBoundId, isCollective, field->getNStep() - 1, isRoot);
                   }
                   if (wtimeCounter)
                   {
//                     SuperClassWriter::writeData(time_counter, getTimeCounterName(), isCollective, field->getNStep() - 1);
//                     if (timeCounterType!=record) SuperClassWriter::writeData(time_counter_bound, timeBoundId, isCollective, field->getNStep() - 1);
                     SuperClassWriter::writeTimeAxisData(time_counter, getTimeCounterName(), isCollective, field->getNStep() - 1,isRoot);
                     if (timeCounterType!=record) SuperClassWriter::writeTimeAxisDataBounds(time_counter_bound, timeBoundId, isCollective, field->getNStep() - 1, isRoot);

                   }
                   CTimer::get("Files : writing time axis").suspend();  
                 }

                break;
              }
            }
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing field data: ");
           msg.append(fieldid); msg.append("\n");
           msg.append("In the context : ");
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeFieldData_ (CField*  field)", << msg);
         }
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeTimeAxis_
                  (CField*    field,
                   const boost::shared_ptr<CCalendar> cal)
      {
         StdOStringStream oss;
         bool createInstantAxis=false ;
         bool createCenteredAxis=false ;
         bool createTimeCounterAxis=false ;
         
         if (field->getOperationTimeType() == func::CFunctor::once) return ;


         StdString axisId ;
         StdString axisBoundId;
         StdString timeid(getTimeCounterName());
         StdString timeBoundId("axis_nbounds");

         StdString strTimeUnits ;
         if (!field->file->time_units.isEmpty() && field->file->time_units==CFile::time_units_attr::days) strTimeUnits="days since " ;
         else  strTimeUnits="seconds since " ;
 
         if (field->getOperationTimeType() == func::CFunctor::instant) field->hasTimeInstant = true;
         if (field->getOperationTimeType() == func::CFunctor::centered) field->hasTimeCentered = true;


         if (field->file->time_counter.isEmpty())
         {
           if (timeCounterType==none) createTimeCounterAxis=true ;
           if (field->hasTimeCentered)
           {
             timeCounterType=centered ;
             if (!hasTimeCentered) createCenteredAxis=true ;
           }
           if (field->hasTimeInstant)
           {
             if (timeCounterType==none) timeCounterType=instant ;
             if (!hasTimeInstant) createInstantAxis=true ;
           }
         }
         else if (field->file->time_counter==CFile::time_counter_attr::instant)
         {
           if (field->hasTimeCentered)
           {
             if (!hasTimeCentered) createCenteredAxis=true ;
           }
           if (field->hasTimeInstant)
           {
             if (timeCounterType==none) createTimeCounterAxis=true ;
             timeCounterType=instant ;
             if (!hasTimeInstant) createInstantAxis=true ;
           }
         }
         else if (field->file->time_counter==CFile::time_counter_attr::centered)
         {
           if (field->hasTimeCentered)
           {
             if (timeCounterType==none) createTimeCounterAxis=true ;
             timeCounterType=centered ;
             if (!hasTimeCentered) createCenteredAxis=true ;
           }
           if (field->hasTimeInstant)
           {
             if (!hasTimeInstant) createInstantAxis=true ;
           }
         }
         else if (field->file->time_counter==CFile::time_counter_attr::instant_exclusive)
         {
           if (field->hasTimeCentered)
           {
             if (!hasTimeCentered) createCenteredAxis=true ;
           }
           if (field->hasTimeInstant)
           {
             if (timeCounterType==none) createTimeCounterAxis=true ;
             timeCounterType=instant ;
           }
         }
         else if (field->file->time_counter==CFile::time_counter_attr::centered_exclusive)
         {
           if (field->hasTimeCentered)
           {
             if (timeCounterType==none) createTimeCounterAxis=true ;
             timeCounterType=centered ;
           }
           if (field->hasTimeInstant)
           {
             if (!hasTimeInstant) createInstantAxis=true ;
           }
         }
         else if (field->file->time_counter==CFile::time_counter_attr::exclusive)
         {
           if (field->hasTimeCentered)
           {
             if (timeCounterType==none) createTimeCounterAxis=true ;
             if (timeCounterType==instant) createInstantAxis=true ;
             timeCounterType=centered ;
           }
           if (field->hasTimeInstant)
           {
             if (timeCounterType==none)
             {
               createTimeCounterAxis=true ;
               timeCounterType=instant ;
             }
             if (timeCounterType==centered)
             {
               if (!hasTimeInstant) createInstantAxis=true ;
             }
           }
         }
         else if (field->file->time_counter==CFile::time_counter_attr::none)
         {
           if (field->hasTimeCentered)
           {
             if (!hasTimeCentered) createCenteredAxis=true ;
           }
           if (field->hasTimeInstant)
           {
             if (!hasTimeInstant) createInstantAxis=true ;
           }
         }
         else if (field->file->time_counter==CFile::time_counter_attr::record)
         {
           if (timeCounterType==none) createTimeCounterAxis=true ;
           timeCounterType=record ;
           if (field->hasTimeCentered)
           {
             if (!hasTimeCentered) createCenteredAxis=true ;
           }
           if (field->hasTimeInstant)
           {
             if (!hasTimeInstant) createInstantAxis=true ;
           }
         }
         
         if (createInstantAxis)
         {
           axisId="time_instant" ;
           axisBoundId="time_instant_bounds";
           hasTimeInstant=true ;
         }

         if (createCenteredAxis)
         {
           axisId="time_centered" ;
           axisBoundId="time_centered_bounds";
           hasTimeCentered=true ;
         }

         
         try
         {
            std::vector<StdString> dims;
            
            if (createInstantAxis || createCenteredAxis)
            {
              // Adding time_instant or time_centered
              dims.push_back(timeid);
              if (!SuperClassWriter::varExist(axisId))
              {
                SuperClassWriter::addVariable(axisId, NC_DOUBLE, dims);

                CDate timeOrigin=cal->getTimeOrigin() ;
                StdOStringStream oss2;
                StdString strInitdate=oss2.str() ;
                StdString strTimeOrigin=timeOrigin.toString() ;
                this->writeTimeAxisAttributes(axisId, cal->getType(),strTimeUnits+strTimeOrigin,
                                              strTimeOrigin, axisBoundId);
             }

             // Adding time_instant_bounds or time_centered_bounds variables
             if (!SuperClassWriter::varExist(axisBoundId))
             {
                dims.clear() ;
                dims.push_back(timeid);
                dims.push_back(timeBoundId);
                SuperClassWriter::addVariable(axisBoundId, NC_DOUBLE, dims);
             }
           }

           if (createTimeCounterAxis)
           {
             // Adding time_counter
             axisId = getTimeCounterName();
             axisBoundId = getTimeCounterName() + "_bounds";
             dims.clear();
             dims.push_back(timeid);
             if (!SuperClassWriter::varExist(axisId))
             {
                SuperClassWriter::addVariable(axisId, NC_DOUBLE, dims);
                SuperClassWriter::addAttribute("axis", string("T"), &axisId);

                if (field->file->time_counter.isEmpty() || 
                   (field->file->time_counter != CFile::time_counter_attr::record))
                {
                  CDate timeOrigin = cal->getTimeOrigin();
                  StdString strTimeOrigin = timeOrigin.toString();

                  this->writeTimeAxisAttributes(axisId, cal->getType(),
                                                strTimeUnits+strTimeOrigin,
                                                strTimeOrigin, axisBoundId);
                }
             }

             // Adding time_counter_bound dimension
             if (field->file->time_counter.isEmpty() || (field->file->time_counter != CFile::time_counter_attr::record))
             {
                if (!SuperClassWriter::varExist(axisBoundId))
                {
                  dims.clear();
                  dims.push_back(timeid);
                  dims.push_back(timeBoundId);
                  SuperClassWriter::addVariable(axisBoundId, NC_DOUBLE, dims);
                }
             }
           }
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing time axis data: ");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeTimeAxis_ (CField*    field, \
                  const boost::shared_ptr<CCalendar> cal)", << msg);
         }
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeTimeAxisAttributes(const StdString & axis_name,
                                                   const StdString & calendar,
                                                   const StdString & units,
                                                   const StdString & time_origin,
                                                   const StdString & time_bounds,
                                                   const StdString & standard_name,
                                                   const StdString & long_name)
      {
         try
         {
           SuperClassWriter::addAttribute("standard_name", standard_name, &axis_name);
           SuperClassWriter::addAttribute("long_name",     long_name    , &axis_name);
           SuperClassWriter::addAttribute("calendar",      calendar     , &axis_name);
           SuperClassWriter::addAttribute("units",         units        , &axis_name);
           SuperClassWriter::addAttribute("time_origin",   time_origin  , &axis_name);
           SuperClassWriter::addAttribute("bounds",        time_bounds  , &axis_name);
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing time axis Attribute: ");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeTimeAxisAttributes(const StdString & axis_name, \
                                                          const StdString & calendar,\
                                                          const StdString & units, \
                                                          const StdString & time_origin, \
                                                          const StdString & time_bounds, \
                                                          const StdString & standard_name, \
                                                          const StdString & long_name)", << msg);
         }
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeAxisAttributes(const StdString & axis_name,
                                               const StdString & axis,
                                               const StdString & standard_name,
                                               const StdString & long_name,
                                               const StdString & units,
                                               const StdString & nav_model)
      {
         try
         {
          if (!axis.empty())
            SuperClassWriter::addAttribute("axis"       , axis         , &axis_name);

          SuperClassWriter::addAttribute("standard_name", standard_name, &axis_name);
          SuperClassWriter::addAttribute("long_name"    , long_name    , &axis_name);
          SuperClassWriter::addAttribute("units"        , units        , &axis_name);
//          SuperClassWriter::addAttribute("nav_model"    , nav_model    , &axis_name);
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing Axis Attribute: ");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeAxisAttributes(const StdString & axis_name, \
                                                      const StdString & axis, \
                                                      const StdString & standard_name, \
                                                      const StdString & long_name, \
                                                      const StdString & units, \
                                                      const StdString & nav_model)", << msg);
         }
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeLocalAttributes
         (int ibegin, int ni, int jbegin, int nj, StdString domid)
      {
        try
        {
         SuperClassWriter::addAttribute(StdString("ibegin").append(domid), ibegin);
         SuperClassWriter::addAttribute(StdString("ni"    ).append(domid), ni);
         SuperClassWriter::addAttribute(StdString("jbegin").append(domid), jbegin);
         SuperClassWriter::addAttribute(StdString("nj"    ).append(domid), nj);
        }
        catch (CNetCdfException& e)
        {
           StdString msg("On writing Local Attributes: ");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeLocalAttributes \
                  (int ibegin, int ni, int jbegin, int nj, StdString domid)", << msg);
        }

      }

      void CNc4DataOutput::writeLocalAttributes_IOIPSL(const StdString& dimXid, const StdString& dimYid,
                                                       int ibegin, int ni, int jbegin, int nj, int ni_glo, int nj_glo, int rank, int size)
      {
         CArray<int,1> array(2) ;

         try
         {
           SuperClassWriter::addAttribute("DOMAIN_number_total",size ) ;
           SuperClassWriter::addAttribute("DOMAIN_number", rank) ;
           array = SuperClassWriter::getDimension(dimXid) + 1, SuperClassWriter::getDimension(dimYid) + 1;
           SuperClassWriter::addAttribute("DOMAIN_dimensions_ids",array) ;
           array=ni_glo,nj_glo ;
           SuperClassWriter::addAttribute("DOMAIN_size_global", array) ;
           array=ni,nj ;
           SuperClassWriter::addAttribute("DOMAIN_size_local", array) ;
           array=ibegin+1,jbegin+1 ;
           SuperClassWriter::addAttribute("DOMAIN_position_first", array) ;
           array=ibegin+ni-1+1,jbegin+nj-1+1 ;
           SuperClassWriter::addAttribute("DOMAIN_position_last",array) ;
           array=0,0 ;
           SuperClassWriter::addAttribute("DOMAIN_halo_size_start", array) ;
           SuperClassWriter::addAttribute("DOMAIN_halo_size_end", array);
           SuperClassWriter::addAttribute("DOMAIN_type",string("box")) ;
  /*
           SuperClassWriter::addAttribute("DOMAIN_DIM_N001",string("x")) ;
           SuperClassWriter::addAttribute("DOMAIN_DIM_N002",string("y")) ;
           SuperClassWriter::addAttribute("DOMAIN_DIM_N003",string("axis_A")) ;
           SuperClassWriter::addAttribute("DOMAIN_DIM_N004",string("time_counter")) ;
  */
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing Local Attributes IOIPSL \n");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeLocalAttributes_IOIPSL \
                  (int ibegin, int ni, int jbegin, int nj, int ni_glo, int nj_glo, int rank, int size)", << msg);
         }
      }
      //---------------------------------------------------------------

      void CNc4DataOutput:: writeFileAttributes(const StdString & name,
                                                const StdString & description,
                                                const StdString & conventions,
                                                const StdString & production,
                                                const StdString & timeStamp)
      {
         try
         {
           SuperClassWriter::addAttribute("name"       , name);
           SuperClassWriter::addAttribute("description", description);
           SuperClassWriter::addAttribute("title"      , description);
           SuperClassWriter::addAttribute("Conventions", conventions);
           // SuperClassWriter::addAttribute("production" , production);

           StdString timeStampStr ;
           if (file->time_stamp_name.isEmpty()) timeStampStr="timeStamp" ;
           else timeStampStr=file->time_stamp_name ;
           SuperClassWriter::addAttribute(timeStampStr, timeStamp);

           StdString uuidName ;
           if (file->uuid_name.isEmpty()) uuidName="uuid" ;
           else uuidName=file->uuid_name ;

           if (file->uuid_format.isEmpty()) SuperClassWriter::addAttribute(uuidName, getUuidStr());
           else SuperClassWriter::addAttribute(uuidName, getUuidStr(file->uuid_format));
          
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing File Attributes \n ");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput:: writeFileAttributes(const StdString & name, \
                                                const StdString & description, \
                                                const StdString & conventions, \
                                                const StdString & production, \
                                                const StdString & timeStamp)", << msg);
         }
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeMaskAttributes(const StdString & mask_name,
                                               int data_dim,
                                               int data_ni,
                                               int data_nj,
                                               int data_ibegin,
                                               int data_jbegin)
      {
         try
         {
           SuperClassWriter::addAttribute("data_dim"   , data_dim   , &mask_name);
           SuperClassWriter::addAttribute("data_ni"    , data_ni    , &mask_name);
           SuperClassWriter::addAttribute("data_nj"    , data_nj    , &mask_name);
           SuperClassWriter::addAttribute("data_ibegin", data_ibegin, &mask_name);
           SuperClassWriter::addAttribute("data_jbegin", data_jbegin, &mask_name);
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing Mask Attributes \n ");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeMaskAttributes(const StdString & mask_name, \
                                               int data_dim, \
                                               int data_ni, \
                                               int data_nj, \
                                               int data_ibegin, \
                                               int data_jbegin)", << msg);
         }
      }

      ///--------------------------------------------------------------

      StdSize CNc4DataOutput::getRecordFromTime(Time time, double factorUnit)
      {
        std::map<Time, StdSize>::const_iterator it = timeToRecordCache.find(time);
        if (it == timeToRecordCache.end())
        {
          StdString timeAxisBoundsId(getTimeCounterName() + "_bounds");
          if (!SuperClassWriter::varExist(timeAxisBoundsId)) timeAxisBoundsId = "time_centered_bounds";
          if (!SuperClassWriter::varExist(timeAxisBoundsId)) timeAxisBoundsId = "time_instant_bounds";

          CArray<double,2> timeAxisBounds;
          std::vector<StdSize> dimSize(SuperClassWriter::getDimensions(timeAxisBoundsId)) ;
          
          StdSize record = 0;
          double dtime(time);
          for (int n = dimSize[0] - 1; n >= 0; n--)
          {
            SuperClassWriter::getTimeAxisBounds(timeAxisBounds, timeAxisBoundsId, isCollective, n);
            timeAxisBounds*=factorUnit ;
            if (timeAxisBounds(1, 0) < dtime)
            {
              record = n + 1;
             break;
            }
          }
          it = timeToRecordCache.insert(std::make_pair(time, record)).first;
        }
         return it->second;
      }

      ///--------------------------------------------------------------

      bool CNc4DataOutput::isWrittenDomain(const std::string& domainName) const
      {
        return (this->writtenDomains.find(domainName) != this->writtenDomains.end());
      }

      bool CNc4DataOutput::isWrittenCompressedDomain(const std::string& domainName) const
      {
        return (this->writtenCompressedDomains.find(domainName) != this->writtenCompressedDomains.end());
      }

      bool CNc4DataOutput::isWrittenAxis(const std::string& axisName) const
      {
        return (this->writtenAxis.find(axisName) != this->writtenAxis.end());
      }

      bool CNc4DataOutput::isWrittenCompressedAxis(const std::string& axisName) const
      {
        return (this->writtenCompressedAxis.find(axisName) != this->writtenCompressedAxis.end());
      }

      bool CNc4DataOutput::isWrittenScalar(const std::string& scalarName) const
      {
        return (this->writtenScalar.find(scalarName) != this->writtenScalar.end());
      }

      void CNc4DataOutput::setWrittenDomain(const std::string& domainName)
      {
        this->writtenDomains.insert(domainName);
      }

      void CNc4DataOutput::setWrittenCompressedDomain(const std::string& domainName)
      {
        this->writtenCompressedDomains.insert(domainName);
      }

      void CNc4DataOutput::setWrittenAxis(const std::string& axisName)
      {
        this->writtenAxis.insert(axisName);
      }

      void CNc4DataOutput::setWrittenCompressedAxis(const std::string& axisName)
      {
        this->writtenCompressedAxis.insert(axisName);
      }

      void CNc4DataOutput::setWrittenScalar(const std::string& scalarName)
      {
        this->writtenScalar.insert(scalarName);
      }
} // namespace xios
