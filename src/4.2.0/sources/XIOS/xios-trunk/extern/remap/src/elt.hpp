#ifndef __ELT_H__
#define __ELT_H__
#include <list>
#include "triple.hpp"
#include <vector>
#include <array>

#define NMAX 0 /**< maximum number of vertices for polygons */

#define NOT_FOUND -1

namespace sphereRemap {

using namespace std;

Coord barycentre(const Coord *x, int n);

/** Two great or small circles (or mixed) have two or zero intersections.
    The coordinates of the intersections are stored in `pt` and `nb` holds the number of intersections (0 or 2).
    */
struct Ipt
{
	int nb;
	Coord pt[2];
};

struct Sgm // edge
{
	Coord n; // normal
	Coord xt[2]; // endpoints
	double d; // (see Elt)
};

struct GloId
{
	int rank;
	int ind; /* local id */
  long globalId ;

	bool operator<(const GloId& other) const {
		return (rank == other.rank) ? (ind < other.ind) : (rank < other.rank);
	}
};

struct Polyg 
{ 
	/* Note:  for the target grid elements the id (rank and local id) depends on the order of the target grid elements as read from the nc-file whereas for source grid elements it depends on the SS-tree (i.e. super mesh distribution, not the order in the nc-file) */
	struct GloId id;
	struct GloId src_id;
	int n; /* number of vertices */
	double area;
  double given_area ;
	Coord x; /* barycentre */
};

struct Elt : Polyg
{
	Elt() {}
	Elt(const double *bounds_lon, const double *bounds_lat, int max_num_vert)
	{
		int k = 0;
                vertex.resize(max_num_vert) ;
		vertex[k++] = xyz(bounds_lon[0], bounds_lat[0]);
		for (int i = 1; i < max_num_vert; i++)
		{
			vertex[k] = xyz(bounds_lon[i], bounds_lat[i]);
			/* netCDF convention: if first vertex repeats element is finished (at least three vertices == triagle) */
			if (k >= 3 && squaredist(vertex[k], vertex[0]) < EPS*EPS) 
				break;
			/* eliminate zero edges: move to next vertex only if it is different */
			if (squaredist(vertex[k], vertex[k-1]) > EPS*EPS)
				k++;
			else
				/* cout << "Removed edge " << k << " due to zero length (coinciding endpoints)." << endl */ ;
		}
		n = k;
    vertex.resize(n) ;
    vertex.shrink_to_fit();
	  allocate() ;

		x = barycentre(vertex.data(), n);
	}
  void allocate(void)
  {
    vertex.resize(n) ;
    neighbour.resize(n) ;
    d.resize(n) ;
    edge.resize(n) ;
    gradNeigh.resize(n) ;
    neighId.resize(n) ;
  }
	Elt& operator=(const Elt& rhs)
	{
		id    = rhs.id;
		src_id    = rhs.src_id;
		n    = rhs.n;
		area = rhs.area;
		given_area = rhs.given_area;
		x    = rhs.x;
		val  = rhs.val;
		grad = rhs.grad;
		is   = rhs.is;

         	neighbour = rhs.neighbour;
		d         = rhs.d;
		edge      = rhs.edge;
		vertex    = rhs.vertex;
		gradNeigh = rhs.gradNeigh;
		return *this;
	}

	void delete_intersections()
	{
		for (list<Polyg*>::iterator it = this->is.begin(); it != this->is.end(); it++)
		{
			Polyg* poly = *it;
			delete poly;
		}
	}

  void insert_vertex(int i, const Coord& v)
  {
    vertex.resize(n+1) ;
    edge.resize(n+1) ;
    d.resize(n+1) ;
    neighbour.resize(n+1) ;
    gradNeigh.resize(n+1) ;
    neighId.resize(n+1) ;

    for(int j=n; j > i ; j--)
    {
      vertex[j]=vertex[j-1] ;
      edge[j]=edge[j-1] ;
      d[j]=d[j-1] ;
      neighbour[j]=neighbour[j-1] ;
    }
    vertex[i+1]=v ;
    n++ ;
  }
  
	std::vector<int> neighbour;
	std::vector<double> d; /**< distance of centre of small circle to origin, zero if great circle */
	double val;     /**< value (sample if src element, interpolated if dest element) */
	std::vector<Coord> vertex;
	std::vector<Coord> edge; /**< edge normals: if great circle tangential to sphere, if small circle parallel to pole */
	Coord grad;    /**< gradient of the reconstructed linear function over this element */
	std::vector<Coord> gradNeigh; /**< for weight computation: gradients for val=1 at individual neighbours */
	std::vector<struct GloId> neighId; /**< weight computation needs to know global IDs for all sources with "link" */
	std::list<Polyg*> is; /**< intersections */
};

static double normals(Elt &elt, const Coord &pole)
{
	double nmin = 17.;
	for (int i = 0; i < elt.n; i++)  // supposed oriented
	{
		int j = (i+1) % elt.n;
		elt.edge[i] = crossprod(elt.vertex[j], elt.vertex[i]); 
		Coord t = elt.vertex[j] - elt.vertex[i];
		/* polygonal grid || vertices not on same latitude */
		if (pole == ORIGIN || fabs(scalarprod(t, pole)) > EPS)  // great circle
		{
			double n = norm(elt.edge[i]);
			//assert(n > 0);
			if (n < nmin) nmin = n;
			elt.edge[i] = proj(elt.edge[i]);
			elt.d[i] = 0.0;
		}
		else /* lan lot grid && vertices on same latitude => small circle */
		{
			int north = (scalarprod(elt.edge[i], pole) < 0) ? -1 : 1;
			elt.edge[i] = pole * north;
			elt.d[i] = scalarprod(elt.vertex[i], elt.edge[i]);
		}
	}
	return nmin;
}

}

#endif 
