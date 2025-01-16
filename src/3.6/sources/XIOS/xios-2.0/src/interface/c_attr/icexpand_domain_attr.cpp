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
  typedef xios::CExpandDomain* expand_domain_Ptr;

  void cxios_set_expand_domain_i_periodic(expand_domain_Ptr expand_domain_hdl, bool i_periodic)
  {
    CTimer::get("XIOS").resume();
    expand_domain_hdl->i_periodic.setValue(i_periodic);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_expand_domain_i_periodic(expand_domain_Ptr expand_domain_hdl, bool* i_periodic)
  {
    CTimer::get("XIOS").resume();
    *i_periodic = expand_domain_hdl->i_periodic.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_expand_domain_i_periodic(expand_domain_Ptr expand_domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = expand_domain_hdl->i_periodic.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_expand_domain_j_periodic(expand_domain_Ptr expand_domain_hdl, bool j_periodic)
  {
    CTimer::get("XIOS").resume();
    expand_domain_hdl->j_periodic.setValue(j_periodic);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_expand_domain_j_periodic(expand_domain_Ptr expand_domain_hdl, bool* j_periodic)
  {
    CTimer::get("XIOS").resume();
    *j_periodic = expand_domain_hdl->j_periodic.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_expand_domain_j_periodic(expand_domain_Ptr expand_domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = expand_domain_hdl->j_periodic.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_expand_domain_order(expand_domain_Ptr expand_domain_hdl, int order)
  {
    CTimer::get("XIOS").resume();
    expand_domain_hdl->order.setValue(order);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_expand_domain_order(expand_domain_Ptr expand_domain_hdl, int* order)
  {
    CTimer::get("XIOS").resume();
    *order = expand_domain_hdl->order.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_expand_domain_order(expand_domain_Ptr expand_domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = expand_domain_hdl->order.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_expand_domain_type(expand_domain_Ptr expand_domain_hdl, const char * type, int type_size)
  {
    std::string type_str;
    if (!cstr2string(type, type_size, type_str)) return;
    CTimer::get("XIOS").resume();
    expand_domain_hdl->type.fromString(type_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_expand_domain_type(expand_domain_Ptr expand_domain_hdl, char * type, int type_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(expand_domain_hdl->type.getInheritedStringValue(), type, type_size))
      ERROR("void cxios_get_expand_domain_type(expand_domain_Ptr expand_domain_hdl, char * type, int type_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_expand_domain_type(expand_domain_Ptr expand_domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = expand_domain_hdl->type.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }
}
