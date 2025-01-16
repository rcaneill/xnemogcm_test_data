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
  typedef xios::CAxisGroup* axisgroup_Ptr;

  void cxios_set_axisgroup_axis_ref(axisgroup_Ptr axisgroup_hdl, const char * axis_ref, int axis_ref_size)
  {
    std::string axis_ref_str;
    if (!cstr2string(axis_ref, axis_ref_size, axis_ref_str)) return;
    CTimer::get("XIOS").resume();
    axisgroup_hdl->axis_ref.setValue(axis_ref_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_axis_ref(axisgroup_Ptr axisgroup_hdl, char * axis_ref, int axis_ref_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(axisgroup_hdl->axis_ref.getInheritedValue(), axis_ref, axis_ref_size))
      ERROR("void cxios_get_axisgroup_axis_ref(axisgroup_Ptr axisgroup_hdl, char * axis_ref, int axis_ref_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_axis_ref(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->axis_ref.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_begin(axisgroup_Ptr axisgroup_hdl, int begin)
  {
    CTimer::get("XIOS").resume();
    axisgroup_hdl->begin.setValue(begin);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_begin(axisgroup_Ptr axisgroup_hdl, int* begin)
  {
    CTimer::get("XIOS").resume();
    *begin = axisgroup_hdl->begin.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_begin(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->begin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_bounds(axisgroup_Ptr axisgroup_hdl, double* bounds, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<double,2> tmp(bounds, shape(extent[0], extent[1]), neverDeleteData);
    axisgroup_hdl->bounds.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_bounds(axisgroup_Ptr axisgroup_hdl, double* bounds, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<double,2> tmp(bounds, shape(extent[0], extent[1]), neverDeleteData);
    tmp=axisgroup_hdl->bounds.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_bounds(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->bounds.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_data_begin(axisgroup_Ptr axisgroup_hdl, int data_begin)
  {
    CTimer::get("XIOS").resume();
    axisgroup_hdl->data_begin.setValue(data_begin);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_data_begin(axisgroup_Ptr axisgroup_hdl, int* data_begin)
  {
    CTimer::get("XIOS").resume();
    *data_begin = axisgroup_hdl->data_begin.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_data_begin(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->data_begin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_data_index(axisgroup_Ptr axisgroup_hdl, int* data_index, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(data_index, shape(extent[0]), neverDeleteData);
    axisgroup_hdl->data_index.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_data_index(axisgroup_Ptr axisgroup_hdl, int* data_index, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(data_index, shape(extent[0]), neverDeleteData);
    tmp=axisgroup_hdl->data_index.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_data_index(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->data_index.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_data_n(axisgroup_Ptr axisgroup_hdl, int data_n)
  {
    CTimer::get("XIOS").resume();
    axisgroup_hdl->data_n.setValue(data_n);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_data_n(axisgroup_Ptr axisgroup_hdl, int* data_n)
  {
    CTimer::get("XIOS").resume();
    *data_n = axisgroup_hdl->data_n.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_data_n(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->data_n.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_group_ref(axisgroup_Ptr axisgroup_hdl, const char * group_ref, int group_ref_size)
  {
    std::string group_ref_str;
    if (!cstr2string(group_ref, group_ref_size, group_ref_str)) return;
    CTimer::get("XIOS").resume();
    axisgroup_hdl->group_ref.setValue(group_ref_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_group_ref(axisgroup_Ptr axisgroup_hdl, char * group_ref, int group_ref_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(axisgroup_hdl->group_ref.getInheritedValue(), group_ref, group_ref_size))
      ERROR("void cxios_get_axisgroup_group_ref(axisgroup_Ptr axisgroup_hdl, char * group_ref, int group_ref_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_group_ref(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->group_ref.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_index(axisgroup_Ptr axisgroup_hdl, int* index, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(index, shape(extent[0]), neverDeleteData);
    axisgroup_hdl->index.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_index(axisgroup_Ptr axisgroup_hdl, int* index, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(index, shape(extent[0]), neverDeleteData);
    tmp=axisgroup_hdl->index.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_index(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->index.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_label(axisgroup_Ptr axisgroup_hdl, char* label, int str_len, int* str_size, int* extent)
  {
    CTimer::get("XIOS").resume();
    axisgroup_hdl->label.resize(shape(extent[0]));
    Array<StdString,1>::iterator it, itb=axisgroup_hdl->label.begin(), ite=axisgroup_hdl->label.end() ;
    int i, n ;
    for(it=itb, i=0, n=0 ; it!=ite ; ++it,n+=str_len,++i) *it=StdString(&label[n],str_size[i]) ;
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_label(axisgroup_Ptr axisgroup_hdl, char* label, int str_size, int* extent)
  {
    CTimer::get("XIOS").resume();
    Array<StdString,1>::const_iterator it, itb=axisgroup_hdl->label.getInheritedValue().begin(), ite=axisgroup_hdl->label.getInheritedValue().end() ;
    int n ;
    for(it=itb, n=0 ; it!=ite ; ++it, n+=str_size) it->copy(&label[n],it->size()) ; 
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_label(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->label.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_long_name(axisgroup_Ptr axisgroup_hdl, const char * long_name, int long_name_size)
  {
    std::string long_name_str;
    if (!cstr2string(long_name, long_name_size, long_name_str)) return;
    CTimer::get("XIOS").resume();
    axisgroup_hdl->long_name.setValue(long_name_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_long_name(axisgroup_Ptr axisgroup_hdl, char * long_name, int long_name_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(axisgroup_hdl->long_name.getInheritedValue(), long_name, long_name_size))
      ERROR("void cxios_get_axisgroup_long_name(axisgroup_Ptr axisgroup_hdl, char * long_name, int long_name_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_long_name(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->long_name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_mask(axisgroup_Ptr axisgroup_hdl, bool* mask, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,1> tmp(mask, shape(extent[0]), neverDeleteData);
    axisgroup_hdl->mask.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_mask(axisgroup_Ptr axisgroup_hdl, bool* mask, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,1> tmp(mask, shape(extent[0]), neverDeleteData);
    tmp=axisgroup_hdl->mask.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_mask(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->mask.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_n(axisgroup_Ptr axisgroup_hdl, int n)
  {
    CTimer::get("XIOS").resume();
    axisgroup_hdl->n.setValue(n);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_n(axisgroup_Ptr axisgroup_hdl, int* n)
  {
    CTimer::get("XIOS").resume();
    *n = axisgroup_hdl->n.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_n(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->n.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_n_distributed_partition(axisgroup_Ptr axisgroup_hdl, int n_distributed_partition)
  {
    CTimer::get("XIOS").resume();
    axisgroup_hdl->n_distributed_partition.setValue(n_distributed_partition);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_n_distributed_partition(axisgroup_Ptr axisgroup_hdl, int* n_distributed_partition)
  {
    CTimer::get("XIOS").resume();
    *n_distributed_partition = axisgroup_hdl->n_distributed_partition.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_n_distributed_partition(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->n_distributed_partition.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_n_glo(axisgroup_Ptr axisgroup_hdl, int n_glo)
  {
    CTimer::get("XIOS").resume();
    axisgroup_hdl->n_glo.setValue(n_glo);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_n_glo(axisgroup_Ptr axisgroup_hdl, int* n_glo)
  {
    CTimer::get("XIOS").resume();
    *n_glo = axisgroup_hdl->n_glo.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_n_glo(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->n_glo.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_name(axisgroup_Ptr axisgroup_hdl, const char * name, int name_size)
  {
    std::string name_str;
    if (!cstr2string(name, name_size, name_str)) return;
    CTimer::get("XIOS").resume();
    axisgroup_hdl->name.setValue(name_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_name(axisgroup_Ptr axisgroup_hdl, char * name, int name_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(axisgroup_hdl->name.getInheritedValue(), name, name_size))
      ERROR("void cxios_get_axisgroup_name(axisgroup_Ptr axisgroup_hdl, char * name, int name_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_name(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_positive(axisgroup_Ptr axisgroup_hdl, const char * positive, int positive_size)
  {
    std::string positive_str;
    if (!cstr2string(positive, positive_size, positive_str)) return;
    CTimer::get("XIOS").resume();
    axisgroup_hdl->positive.fromString(positive_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_positive(axisgroup_Ptr axisgroup_hdl, char * positive, int positive_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(axisgroup_hdl->positive.getInheritedStringValue(), positive, positive_size))
      ERROR("void cxios_get_axisgroup_positive(axisgroup_Ptr axisgroup_hdl, char * positive, int positive_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_positive(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->positive.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_prec(axisgroup_Ptr axisgroup_hdl, int prec)
  {
    CTimer::get("XIOS").resume();
    axisgroup_hdl->prec.setValue(prec);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_prec(axisgroup_Ptr axisgroup_hdl, int* prec)
  {
    CTimer::get("XIOS").resume();
    *prec = axisgroup_hdl->prec.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_prec(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->prec.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_standard_name(axisgroup_Ptr axisgroup_hdl, const char * standard_name, int standard_name_size)
  {
    std::string standard_name_str;
    if (!cstr2string(standard_name, standard_name_size, standard_name_str)) return;
    CTimer::get("XIOS").resume();
    axisgroup_hdl->standard_name.setValue(standard_name_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_standard_name(axisgroup_Ptr axisgroup_hdl, char * standard_name, int standard_name_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(axisgroup_hdl->standard_name.getInheritedValue(), standard_name, standard_name_size))
      ERROR("void cxios_get_axisgroup_standard_name(axisgroup_Ptr axisgroup_hdl, char * standard_name, int standard_name_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_standard_name(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->standard_name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_unit(axisgroup_Ptr axisgroup_hdl, const char * unit, int unit_size)
  {
    std::string unit_str;
    if (!cstr2string(unit, unit_size, unit_str)) return;
    CTimer::get("XIOS").resume();
    axisgroup_hdl->unit.setValue(unit_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_unit(axisgroup_Ptr axisgroup_hdl, char * unit, int unit_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(axisgroup_hdl->unit.getInheritedValue(), unit, unit_size))
      ERROR("void cxios_get_axisgroup_unit(axisgroup_Ptr axisgroup_hdl, char * unit, int unit_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_unit(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->unit.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_value(axisgroup_Ptr axisgroup_hdl, double* value, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<double,1> tmp(value, shape(extent[0]), neverDeleteData);
    axisgroup_hdl->value.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_value(axisgroup_Ptr axisgroup_hdl, double* value, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<double,1> tmp(value, shape(extent[0]), neverDeleteData);
    tmp=axisgroup_hdl->value.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_value(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->value.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }
}
