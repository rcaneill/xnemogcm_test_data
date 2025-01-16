#include <list>
#include "elt.hpp"
#include "polyg.hpp"

namespace sphereRemap {

using namespace std;

void cptEltGeom(Elt& elt, const Coord &pole)
{
  orient(elt.n, elt.vertex, elt.edge, elt.d, elt.x);
  normals(elt, pole);
  Coord gg;
  elt.area = airbar(elt.n, elt.vertex, elt.edge, elt.d, pole, gg);
  elt.x = gg;
}

void cptAllEltsGeom(Elt *elt, int N, const Coord &pole)
{
  for (int ne=0; ne<N; ne++)
    cptEltGeom(elt[ne], pole);
}

/* for all elements of size-N-array `elt`,
   make centre areaweighted average centres of super mesh elements */
void update_baryc(Elt *elt, int N)
{
  for (int ne=0; ne<N; ne++)
  {
    Elt &e = elt[ne];
    int ns = e.is.size();  // sous-elements
    Coord *sx = new Coord[ns];
    int i=0;
    for (list<Polyg*>::iterator it = e.is.begin(); it != e.is.end(); i++, it++)
    {
      sx[i] = (*it)->x * (*it)->area;
    }
    e.x = barycentre(sx, ns);
  }
}


Coord gradient_old(Elt& elt, Elt **neighElts)
{
  Coord grad = ORIGIN;
  Coord *neighBaryc = new Coord[elt.n];
  for (int j = 0; j < elt.n; j++)
  {
    int k = (j + 1) % elt.n;
    neighBaryc[j] = neighElts[j]->x;
    Coord edgeNormal = crossprod(neighElts[k]->x, neighElts[j]->x);

    // use nomenclauture form paper 
    double f_i = elt.val;
    double f_j = neighElts[j]->val;
    double f_k = neighElts[k]->val;
    grad = grad + edgeNormal * (0.5*(f_j + f_k) - f_i);
  }
  // area of the polygon whoes vertices are the barycentres the neighbours 
  grad = grad * (1./polygonarea(neighBaryc, elt.n));
  delete[] neighBaryc;

  return grad - elt.x * scalarprod(elt.x, grad);
}



Coord gradient(Elt& elt, Elt **neighElts)
{
    
  Coord grad = ORIGIN;
  Coord neighBaryc[3] ;

  double f_i ;
  double f_j ;
  double f_k ;
 
  Coord edgeNormal ;
  double area=0 ;
  int k ;
  int count=0 ;
  
  for (int j = 0; j < elt.n; j++)
  {
    f_i = elt.val;
    k = (j + 1) % elt.n;
    if (neighElts[j]==NULL || neighElts[k]==NULL) continue ;

    // use nomenclauture form paper 
    f_j = neighElts[j]->val;
    f_k = neighElts[k]->val;

    
   
    neighBaryc[0] = elt.x;
    neighBaryc[1] = neighElts[j]->x;
    neighBaryc[2] = neighElts[k]->x;

    edgeNormal = crossprod(neighElts[k]->x, neighElts[j]->x);
    grad = grad + edgeNormal * (0.5*(f_k + f_j) - f_i);

    edgeNormal = crossprod(neighElts[j]->x, elt.x);
    grad = grad + edgeNormal * (0.5*(f_j + f_i) - f_i);

    edgeNormal = crossprod(elt.x, neighElts[k]->x);
    grad = grad + edgeNormal * (0.5*(f_i + f_k) - f_i);

  // area of the polygon whoes vertices are the barycentres the neighbours 

    area+=polygonarea(neighBaryc, 3) ;
    count++ ;

  }
  if (count>0) 
  {
    grad=grad*(1./area) ;
    return grad - elt.x * scalarprod(elt.x, grad);
  }
  else return grad ;
}




void computeGradients(Elt **elts, int N)
{
  
  for (int j = 0; j < N; j++)
    elts[j]->val = 0;

  Elt *neighbours[NMAX];
  for (int j = 0; j < N; j++)
  {
    for (int i = 0; i < elts[j]->n; i++) 
      if ( elts[j]->neighbour[i]== NOT_FOUND) neighbours[i]=NULL ;
      else  neighbours[i] = elts[elts[j]->neighbour[i]];

    for (int i = 0; i < elts[j]->n; i++)
      if (neighbours[i]!=NULL) neighbours[i]->val = 0;
      
    for (int i = 0; i < elts[j]->n; i++)
    {
      if (neighbours[i]!=NULL)
      {
        elts[j]->neighId[i] = neighbours[i]->src_id;
        /* for weight computation all values are always kept zero and only set to one when used .. */
        neighbours[i]->val = 1;
        elts[j]->gradNeigh[i] = gradient(*(elts[j]), neighbours);
        /* .. and right after zeroed again */
        neighbours[i]->val = 0;
      }
      else
      {
        elts[j]->neighId[i].rank = -1; // mark end
        elts[j]->neighId[i].ind = -1; // mark end
      }
    }

    for(int i = elts[j]->n ; i < NMAX; i++)
    {
      elts[j]->neighId[i].rank = -1; // mark end
      elts[j]->neighId[i].ind = -1; // mark end
    }
    /* For the most naive algorithm the case where the element itself is one must also be considered.
       Thomas says this can later be optimized out. */
    elts[j]->val = 1;
    elts[j]->grad = gradient(*(elts[j]), neighbours);
    elts[j]->val = 0;
  }
}

}
