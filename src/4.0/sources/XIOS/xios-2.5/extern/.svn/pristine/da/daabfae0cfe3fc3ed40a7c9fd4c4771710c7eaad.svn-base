#include "intersection_ym.hpp"
#include "elt.hpp"
#include "clipper.hpp"
#include "gridRemap.hpp"
#include "triple.hpp"
#include "polyg.hpp"
#include <vector>
#include <stdlib.h>
#include <limits>

#define epsilon 1e-3  // epsilon distance ratio over side lenght for approximate small circle by great circle
#define fusion_vertex 1e-13

namespace sphereRemap {

using namespace std;
using namespace ClipperLib ;

double intersect_ym(Elt *a, Elt *b)
{

// transform small circle into piece of great circle if necessary

  vector<Coord> srcPolygon ;
  createGreatCirclePolygon(*b, srcGrid.pole, srcPolygon) ;
//  b->area=polygonarea(&srcPolygon[0],srcPolygon.size()) ;
  vector<Coord> dstPolygon ;
  createGreatCirclePolygon(*a, tgtGrid.pole, dstPolygon) ;
  a->area=polygonarea(&dstPolygon[0],dstPolygon.size()) ; // just for target

// compute coordinates of the polygons into the gnomonique plane tangent to barycenter C of dst polygon
// transform system coordinate : Z axis along OC
  int na=dstPolygon.size() ;
  Coord *a_gno   = new Coord[na];
  int nb=srcPolygon.size() ;
  Coord *b_gno   = new Coord[nb];

  Coord OC=barycentre(a->vertex,a->n) ;
  Coord Oz=OC ;
  Coord Ox=crossprod(Coord(0,0,1),Oz) ;
// choose Ox not too small to avoid rounding error
  if (norm(Ox)< 0.1) Ox=crossprod(Coord(0,1,0),Oz) ;
  Ox=Ox*(1./norm(Ox)) ;
  Coord Oy=crossprod(Oz,Ox) ;
  double cos_alpha;

  for(int n=0; n<na;n++)
  {
    cos_alpha=scalarprod(OC,dstPolygon[n]) ;
    a_gno[n].x=scalarprod(dstPolygon[n],Ox)/cos_alpha ;
    a_gno[n].y=scalarprod(dstPolygon[n],Oy)/cos_alpha ;
    a_gno[n].z=scalarprod(dstPolygon[n],Oz)/cos_alpha ; // must be equal to 1
  }

  for(int n=0; n<nb;n++)
  {
    cos_alpha=scalarprod(OC,srcPolygon[n]) ;
    b_gno[n].x=scalarprod(srcPolygon[n],Ox)/cos_alpha ;
    b_gno[n].y=scalarprod(srcPolygon[n],Oy)/cos_alpha ;
    b_gno[n].z=scalarprod(srcPolygon[n],Oz)/cos_alpha ; // must be equal to 1
  }



// Compute intersections using clipper
// 1) Compute offset and scale factor to rescale polygon

  double xmin, xmax, ymin,ymax ;
  xmin=xmax=a_gno[0].x ;
  ymin=ymax=a_gno[0].y ;

  for(int n=0; n<na;n++)
  {
    if (a_gno[n].x< xmin) xmin=a_gno[n].x ;
    else if (a_gno[n].x > xmax) xmax=a_gno[n].x ;

    if (a_gno[n].y< ymin) ymin=a_gno[n].y ;
    else if (a_gno[n].y > ymax) ymax=a_gno[n].y ;
  }

  for(int n=0; n<nb;n++)
  {
    if (b_gno[n].x< xmin) xmin=b_gno[n].x ;
    else if (b_gno[n].x > xmax) xmax=b_gno[n].x ;

    if (b_gno[n].y< ymin) ymin=b_gno[n].y ;
    else if (b_gno[n].y > ymax) ymax=b_gno[n].y ;
  }

  double xoffset=(xmin+xmax)*0.5 ;
  double yoffset=(ymin+ymax)*0.5 ;
  double xscale= 1e-4*0.5*hiRange/(xmax-xoffset) ;
  double yscale= 1e-4*0.5*hiRange/(ymax-yoffset) ;
// Problem with numerical precision if using larger scaling factor

// 2) Compute intersection with clipper
//    clipper use only long integer value for vertex => offset and rescale

  Paths src(1), dst(1), intersection;

  for(int n=0; n<na;n++)
     src[0]<<IntPoint((a_gno[n].x-xoffset)*xscale,(a_gno[n].y-yoffset)*yscale) ;

  for(int n=0; n<nb;n++)
     dst[0]<<IntPoint((b_gno[n].x-xoffset)*xscale,(b_gno[n].y-yoffset)*yscale) ;

  Clipper clip ;
  clip.AddPaths(src, ptSubject, true);
  clip.AddPaths(dst, ptClip, true);
  clip.Execute(ctIntersection, intersection);

  double area=0 ;

  for(int ni=0;ni<intersection.size(); ni++)
  {
    // go back into real coordinate on the sphere
    Coord* intersectPolygon=new Coord[intersection[ni].size()] ;
    for(int n=0; n < intersection[ni].size(); n++)
    {
      double x=intersection[ni][n].X/xscale+xoffset ;
      double y=intersection[ni][n].Y/yscale+yoffset ;

      intersectPolygon[n]=Ox*x+Oy*y+Oz ;
      intersectPolygon[n]=intersectPolygon[n]*(1./norm(intersectPolygon[n])) ;
    }

// remove redondants vertex
    int nv=0 ;
    for(int n=0; n < intersection[ni].size(); n++)
    {
      if (norm(intersectPolygon[n]-intersectPolygon[(n+1)%intersection[ni].size()])>fusion_vertex)
      {
        intersectPolygon[nv]=intersectPolygon[n] ;
        nv++ ;
      }
    }


    if (nv>2)
    {
//     assign intersection to source and destination polygons
       Polyg *is = new Polyg;
       is->x = exact_barycentre(intersectPolygon,nv);
       is->area = polygonarea(intersectPolygon,nv) ;
//        if (is->area < 1e-12) cout<<"Small intersection : "<<is->area<<endl ;
       if (is->area==0.) delete is ;
       else
       {  
         is->id = b->id; /* intersection holds id of corresponding source element (see Elt class definition for details about id) */
         is->src_id = b->src_id;
         is->n = nv;
         (a->is).push_back(is);
         (b->is).push_back(is);
         area=is->area ;
       }
    }
    delete[] intersectPolygon ;
  }

  delete[] a_gno ;
  delete[] b_gno ;
  return area ;
}

void createGreatCirclePolygon(const Elt& element, const Coord& pole, vector<Coord>& coordinates)
{
  int nv = element.n;

  double z,r ;
  int north ;
  int iterations ;

  Coord xa,xb,xi,xc ;
  Coord x1,x2,x ;

  for(int i=0;i < nv ;i++)
  {
    north = (scalarprod(element.edge[i], pole) < 0) ? -1 : 1;
    z=north*element.d[i] ;

    if (z != 0.0)
    {

      xa=element.vertex[i] ;
      xb=element.vertex[(i+1)%nv] ;
      iterations=0 ;

// compare max distance (at mid-point) between small circle and great circle
// if greater the epsilon refine the small circle by dividing it recursively.

      do
      {
        xc = pole * z ;
        r=sqrt(1-z*z) ;
        xi=(xa+xb)*0.5 ;
        x1=xc+(xi-xc)*(r/norm(xi-xc)) ;
        x2= xi*(1./norm(xi)) ;
        ++iterations;
        xb=x1 ;
      } while(norm(x1-x2)/norm(xa-xb)>epsilon) ;

      iterations = 1 << (iterations-1) ;

// small circle divided in "iterations" great circle arc
      Coord delta=(element.vertex[(i+1)%nv]-element.vertex[i])*(1./iterations);
      x=xa ;
      for(int j=0; j<iterations ; j++)
      {
        //xc+(x-xc)*r/norm(x-xc)
        coordinates.push_back(xc+(x-xc)*(r/norm(x-xc))) ;
        x=x+delta ;
      }
    }
    else coordinates.push_back(element.vertex[i]) ;
  }
}

}
