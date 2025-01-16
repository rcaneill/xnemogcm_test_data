/* ************************************************************************** *
 *               Interface auto generated - do not modify                     *
 * ************************************************************************** */

#include <boost/multi_array.hpp>
#include <boost/shared_ptr.hpp>
#include "xios.hpp"
#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "icutil.hpp"
#include "icdate.hpp"
#include "timer.hpp"
#include "node_type.hpp"

extern "C"
{
  typedef xios::CDomainGroup* domaingroup_Ptr;

  void cxios_set_domaingroup_area(domaingroup_Ptr domaingroup_hdl, double* area, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<double,2> tmp(area, shape(extent[0], extent[1]), neverDeleteData);
    domaingroup_hdl->area.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_area(domaingroup_Ptr domaingroup_hdl, double* area, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<double,2> tmp(area, shape(extent[0], extent[1]), neverDeleteData);
    tmp=domaingroup_hdl->area.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_area(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->area.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_bounds_lat_1d(domaingroup_Ptr domaingroup_hdl, double* bounds_lat_1d, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<double,2> tmp(bounds_lat_1d, shape(extent[0], extent[1]), neverDeleteData);
    domaingroup_hdl->bounds_lat_1d.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_bounds_lat_1d(domaingroup_Ptr domaingroup_hdl, double* bounds_lat_1d, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<double,2> tmp(bounds_lat_1d, shape(extent[0], extent[1]), neverDeleteData);
    tmp=domaingroup_hdl->bounds_lat_1d.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_bounds_lat_1d(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->bounds_lat_1d.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_bounds_lat_2d(domaingroup_Ptr domaingroup_hdl, double* bounds_lat_2d, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<double,3> tmp(bounds_lat_2d, shape(extent[0], extent[1], extent[2]), neverDeleteData);
    domaingroup_hdl->bounds_lat_2d.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_bounds_lat_2d(domaingroup_Ptr domaingroup_hdl, double* bounds_lat_2d, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<double,3> tmp(bounds_lat_2d, shape(extent[0], extent[1], extent[2]), neverDeleteData);
    tmp=domaingroup_hdl->bounds_lat_2d.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_bounds_lat_2d(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->bounds_lat_2d.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_bounds_lon_1d(domaingroup_Ptr domaingroup_hdl, double* bounds_lon_1d, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<double,2> tmp(bounds_lon_1d, shape(extent[0], extent[1]), neverDeleteData);
    domaingroup_hdl->bounds_lon_1d.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_bounds_lon_1d(domaingroup_Ptr domaingroup_hdl, double* bounds_lon_1d, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<double,2> tmp(bounds_lon_1d, shape(extent[0], extent[1]), neverDeleteData);
    tmp=domaingroup_hdl->bounds_lon_1d.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_bounds_lon_1d(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->bounds_lon_1d.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_bounds_lon_2d(domaingroup_Ptr domaingroup_hdl, double* bounds_lon_2d, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<double,3> tmp(bounds_lon_2d, shape(extent[0], extent[1], extent[2]), neverDeleteData);
    domaingroup_hdl->bounds_lon_2d.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_bounds_lon_2d(domaingroup_Ptr domaingroup_hdl, double* bounds_lon_2d, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<double,3> tmp(bounds_lon_2d, shape(extent[0], extent[1], extent[2]), neverDeleteData);
    tmp=domaingroup_hdl->bounds_lon_2d.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_bounds_lon_2d(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->bounds_lon_2d.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_data_dim(domaingroup_Ptr domaingroup_hdl, int data_dim)
  {
    CTimer::get("XIOS").resume();
    domaingroup_hdl->data_dim.setValue(data_dim);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_data_dim(domaingroup_Ptr domaingroup_hdl, int* data_dim)
  {
    CTimer::get("XIOS").resume();
    *data_dim = domaingroup_hdl->data_dim.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_data_dim(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->data_dim.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_data_i_index(domaingroup_Ptr domaingroup_hdl, int* data_i_index, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(data_i_index, shape(extent[0]), neverDeleteData);
    domaingroup_hdl->data_i_index.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_data_i_index(domaingroup_Ptr domaingroup_hdl, int* data_i_index, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(data_i_index, shape(extent[0]), neverDeleteData);
    tmp=domaingroup_hdl->data_i_index.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_data_i_index(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->data_i_index.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_data_ibegin(domaingroup_Ptr domaingroup_hdl, int data_ibegin)
  {
    CTimer::get("XIOS").resume();
    domaingroup_hdl->data_ibegin.setValue(data_ibegin);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_data_ibegin(domaingroup_Ptr domaingroup_hdl, int* data_ibegin)
  {
    CTimer::get("XIOS").resume();
    *data_ibegin = domaingroup_hdl->data_ibegin.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_data_ibegin(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->data_ibegin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_data_j_index(domaingroup_Ptr domaingroup_hdl, int* data_j_index, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(data_j_index, shape(extent[0]), neverDeleteData);
    domaingroup_hdl->data_j_index.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_data_j_index(domaingroup_Ptr domaingroup_hdl, int* data_j_index, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(data_j_index, shape(extent[0]), neverDeleteData);
    tmp=domaingroup_hdl->data_j_index.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_data_j_index(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->data_j_index.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_data_jbegin(domaingroup_Ptr domaingroup_hdl, int data_jbegin)
  {
    CTimer::get("XIOS").resume();
    domaingroup_hdl->data_jbegin.setValue(data_jbegin);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_data_jbegin(domaingroup_Ptr domaingroup_hdl, int* data_jbegin)
  {
    CTimer::get("XIOS").resume();
    *data_jbegin = domaingroup_hdl->data_jbegin.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_data_jbegin(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->data_jbegin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_data_ni(domaingroup_Ptr domaingroup_hdl, int data_ni)
  {
    CTimer::get("XIOS").resume();
    domaingroup_hdl->data_ni.setValue(data_ni);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_data_ni(domaingroup_Ptr domaingroup_hdl, int* data_ni)
  {
    CTimer::get("XIOS").resume();
    *data_ni = domaingroup_hdl->data_ni.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_data_ni(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->data_ni.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_data_nj(domaingroup_Ptr domaingroup_hdl, int data_nj)
  {
    CTimer::get("XIOS").resume();
    domaingroup_hdl->data_nj.setValue(data_nj);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_data_nj(domaingroup_Ptr domaingroup_hdl, int* data_nj)
  {
    CTimer::get("XIOS").resume();
    *data_nj = domaingroup_hdl->data_nj.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_data_nj(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->data_nj.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_domain_ref(domaingroup_Ptr domaingroup_hdl, const char * domain_ref, int domain_ref_size)
  {
    std::string domain_ref_str;
    if (!cstr2string(domain_ref, domain_ref_size, domain_ref_str)) return;
    CTimer::get("XIOS").resume();
    domaingroup_hdl->domain_ref.setValue(domain_ref_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_domain_ref(domaingroup_Ptr domaingroup_hdl, char * domain_ref, int domain_ref_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(domaingroup_hdl->domain_ref.getInheritedValue(), domain_ref, domain_ref_size))
      ERROR("void cxios_get_domaingroup_domain_ref(domaingroup_Ptr domaingroup_hdl, char * domain_ref, int domain_ref_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_domain_ref(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->domain_ref.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_group_ref(domaingroup_Ptr domaingroup_hdl, const char * group_ref, int group_ref_size)
  {
    std::string group_ref_str;
    if (!cstr2string(group_ref, group_ref_size, group_ref_str)) return;
    CTimer::get("XIOS").resume();
    domaingroup_hdl->group_ref.setValue(group_ref_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_group_ref(domaingroup_Ptr domaingroup_hdl, char * group_ref, int group_ref_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(domaingroup_hdl->group_ref.getInheritedValue(), group_ref, group_ref_size))
      ERROR("void cxios_get_domaingroup_group_ref(domaingroup_Ptr domaingroup_hdl, char * group_ref, int group_ref_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_group_ref(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->group_ref.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_i_index(domaingroup_Ptr domaingroup_hdl, int* i_index, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(i_index, shape(extent[0]), neverDeleteData);
    domaingroup_hdl->i_index.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_i_index(domaingroup_Ptr domaingroup_hdl, int* i_index, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(i_index, shape(extent[0]), neverDeleteData);
    tmp=domaingroup_hdl->i_index.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_i_index(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->i_index.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_ibegin(domaingroup_Ptr domaingroup_hdl, int ibegin)
  {
    CTimer::get("XIOS").resume();
    domaingroup_hdl->ibegin.setValue(ibegin);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_ibegin(domaingroup_Ptr domaingroup_hdl, int* ibegin)
  {
    CTimer::get("XIOS").resume();
    *ibegin = domaingroup_hdl->ibegin.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_ibegin(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->ibegin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_j_index(domaingroup_Ptr domaingroup_hdl, int* j_index, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(j_index, shape(extent[0]), neverDeleteData);
    domaingroup_hdl->j_index.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_j_index(domaingroup_Ptr domaingroup_hdl, int* j_index, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(j_index, shape(extent[0]), neverDeleteData);
    tmp=domaingroup_hdl->j_index.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_j_index(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->j_index.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_jbegin(domaingroup_Ptr domaingroup_hdl, int jbegin)
  {
    CTimer::get("XIOS").resume();
    domaingroup_hdl->jbegin.setValue(jbegin);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_jbegin(domaingroup_Ptr domaingroup_hdl, int* jbegin)
  {
    CTimer::get("XIOS").resume();
    *jbegin = domaingroup_hdl->jbegin.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_jbegin(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->jbegin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_latvalue_1d(domaingroup_Ptr domaingroup_hdl, double* latvalue_1d, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<double,1> tmp(latvalue_1d, shape(extent[0]), neverDeleteData);
    domaingroup_hdl->latvalue_1d.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_latvalue_1d(domaingroup_Ptr domaingroup_hdl, double* latvalue_1d, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<double,1> tmp(latvalue_1d, shape(extent[0]), neverDeleteData);
    tmp=domaingroup_hdl->latvalue_1d.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_latvalue_1d(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->latvalue_1d.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_latvalue_2d(domaingroup_Ptr domaingroup_hdl, double* latvalue_2d, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<double,2> tmp(latvalue_2d, shape(extent[0], extent[1]), neverDeleteData);
    domaingroup_hdl->latvalue_2d.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_latvalue_2d(domaingroup_Ptr domaingroup_hdl, double* latvalue_2d, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<double,2> tmp(latvalue_2d, shape(extent[0], extent[1]), neverDeleteData);
    tmp=domaingroup_hdl->latvalue_2d.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_latvalue_2d(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->latvalue_2d.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_long_name(domaingroup_Ptr domaingroup_hdl, const char * long_name, int long_name_size)
  {
    std::string long_name_str;
    if (!cstr2string(long_name, long_name_size, long_name_str)) return;
    CTimer::get("XIOS").resume();
    domaingroup_hdl->long_name.setValue(long_name_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_long_name(domaingroup_Ptr domaingroup_hdl, char * long_name, int long_name_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(domaingroup_hdl->long_name.getInheritedValue(), long_name, long_name_size))
      ERROR("void cxios_get_domaingroup_long_name(domaingroup_Ptr domaingroup_hdl, char * long_name, int long_name_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_long_name(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->long_name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_lonvalue_1d(domaingroup_Ptr domaingroup_hdl, double* lonvalue_1d, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<double,1> tmp(lonvalue_1d, shape(extent[0]), neverDeleteData);
    domaingroup_hdl->lonvalue_1d.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_lonvalue_1d(domaingroup_Ptr domaingroup_hdl, double* lonvalue_1d, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<double,1> tmp(lonvalue_1d, shape(extent[0]), neverDeleteData);
    tmp=domaingroup_hdl->lonvalue_1d.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_lonvalue_1d(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->lonvalue_1d.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_lonvalue_2d(domaingroup_Ptr domaingroup_hdl, double* lonvalue_2d, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<double,2> tmp(lonvalue_2d, shape(extent[0], extent[1]), neverDeleteData);
    domaingroup_hdl->lonvalue_2d.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_lonvalue_2d(domaingroup_Ptr domaingroup_hdl, double* lonvalue_2d, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<double,2> tmp(lonvalue_2d, shape(extent[0], extent[1]), neverDeleteData);
    tmp=domaingroup_hdl->lonvalue_2d.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_lonvalue_2d(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->lonvalue_2d.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_mask_1d(domaingroup_Ptr domaingroup_hdl, bool* mask_1d, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,1> tmp(mask_1d, shape(extent[0]), neverDeleteData);
    domaingroup_hdl->mask_1d.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_mask_1d(domaingroup_Ptr domaingroup_hdl, bool* mask_1d, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,1> tmp(mask_1d, shape(extent[0]), neverDeleteData);
    tmp=domaingroup_hdl->mask_1d.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_mask_1d(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->mask_1d.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_mask_2d(domaingroup_Ptr domaingroup_hdl, bool* mask_2d, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,2> tmp(mask_2d, shape(extent[0], extent[1]), neverDeleteData);
    domaingroup_hdl->mask_2d.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_mask_2d(domaingroup_Ptr domaingroup_hdl, bool* mask_2d, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,2> tmp(mask_2d, shape(extent[0], extent[1]), neverDeleteData);
    tmp=domaingroup_hdl->mask_2d.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_mask_2d(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->mask_2d.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_name(domaingroup_Ptr domaingroup_hdl, const char * name, int name_size)
  {
    std::string name_str;
    if (!cstr2string(name, name_size, name_str)) return;
    CTimer::get("XIOS").resume();
    domaingroup_hdl->name.setValue(name_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_name(domaingroup_Ptr domaingroup_hdl, char * name, int name_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(domaingroup_hdl->name.getInheritedValue(), name, name_size))
      ERROR("void cxios_get_domaingroup_name(domaingroup_Ptr domaingroup_hdl, char * name, int name_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_name(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_ni(domaingroup_Ptr domaingroup_hdl, int ni)
  {
    CTimer::get("XIOS").resume();
    domaingroup_hdl->ni.setValue(ni);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_ni(domaingroup_Ptr domaingroup_hdl, int* ni)
  {
    CTimer::get("XIOS").resume();
    *ni = domaingroup_hdl->ni.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_ni(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->ni.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_ni_glo(domaingroup_Ptr domaingroup_hdl, int ni_glo)
  {
    CTimer::get("XIOS").resume();
    domaingroup_hdl->ni_glo.setValue(ni_glo);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_ni_glo(domaingroup_Ptr domaingroup_hdl, int* ni_glo)
  {
    CTimer::get("XIOS").resume();
    *ni_glo = domaingroup_hdl->ni_glo.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_ni_glo(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->ni_glo.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_nj(domaingroup_Ptr domaingroup_hdl, int nj)
  {
    CTimer::get("XIOS").resume();
    domaingroup_hdl->nj.setValue(nj);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_nj(domaingroup_Ptr domaingroup_hdl, int* nj)
  {
    CTimer::get("XIOS").resume();
    *nj = domaingroup_hdl->nj.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_nj(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->nj.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_nj_glo(domaingroup_Ptr domaingroup_hdl, int nj_glo)
  {
    CTimer::get("XIOS").resume();
    domaingroup_hdl->nj_glo.setValue(nj_glo);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_nj_glo(domaingroup_Ptr domaingroup_hdl, int* nj_glo)
  {
    CTimer::get("XIOS").resume();
    *nj_glo = domaingroup_hdl->nj_glo.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_nj_glo(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->nj_glo.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_nvertex(domaingroup_Ptr domaingroup_hdl, int nvertex)
  {
    CTimer::get("XIOS").resume();
    domaingroup_hdl->nvertex.setValue(nvertex);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_nvertex(domaingroup_Ptr domaingroup_hdl, int* nvertex)
  {
    CTimer::get("XIOS").resume();
    *nvertex = domaingroup_hdl->nvertex.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_nvertex(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->nvertex.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_prec(domaingroup_Ptr domaingroup_hdl, int prec)
  {
    CTimer::get("XIOS").resume();
    domaingroup_hdl->prec.setValue(prec);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_prec(domaingroup_Ptr domaingroup_hdl, int* prec)
  {
    CTimer::get("XIOS").resume();
    *prec = domaingroup_hdl->prec.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_prec(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->prec.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_standard_name(domaingroup_Ptr domaingroup_hdl, const char * standard_name, int standard_name_size)
  {
    std::string standard_name_str;
    if (!cstr2string(standard_name, standard_name_size, standard_name_str)) return;
    CTimer::get("XIOS").resume();
    domaingroup_hdl->standard_name.setValue(standard_name_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_standard_name(domaingroup_Ptr domaingroup_hdl, char * standard_name, int standard_name_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(domaingroup_hdl->standard_name.getInheritedValue(), standard_name, standard_name_size))
      ERROR("void cxios_get_domaingroup_standard_name(domaingroup_Ptr domaingroup_hdl, char * standard_name, int standard_name_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_standard_name(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->standard_name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domaingroup_type(domaingroup_Ptr domaingroup_hdl, const char * type, int type_size)
  {
    std::string type_str;
    if (!cstr2string(type, type_size, type_str)) return;
    CTimer::get("XIOS").resume();
    domaingroup_hdl->type.fromString(type_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domaingroup_type(domaingroup_Ptr domaingroup_hdl, char * type, int type_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(domaingroup_hdl->type.getInheritedStringValue(), type, type_size))
      ERROR("void cxios_get_domaingroup_type(domaingroup_Ptr domaingroup_hdl, char * type, int type_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domaingroup_type(domaingroup_Ptr domaingroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domaingroup_hdl->type.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }
}
