/* ************************************************************************** *
 *      Copyright © IPSL/LSCE, xios, Avril 2010 - Octobre 2011         *
 * ************************************************************************** */

#include <boost/multi_array.hpp>
#include <boost/shared_ptr.hpp>

#include "xios.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"

#include "icutil.hpp"
#include "timer.hpp"
#include "field.hpp"

extern "C"
{
// /////////////////////////////// Définitions ////////////////////////////// //

   // ----------------------- Redéfinition de types ----------------------------
   
   typedef xios::CField      * XFieldPtr;
   typedef xios::CFieldGroup * XFieldGroupPtr;
   
   typedef xios::CDomain     * XDomainPtr;
   typedef xios::CAxis       * XAxisPtr;
   typedef xios::CScalar     * XScalarPtr;

// --------------------------------------------------------------------------   
// ------------------------ Création des handle -----------------------------
// --------------------------------------------------------------------------   
   
   void cxios_field_handle_create (XFieldPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;
      CTimer::get("XIOS").resume() ;
      *_ret = CField::get(id);
      CTimer::get("XIOS").suspend() ;
   }
   
   void cxios_fieldgroup_handle_create (XFieldGroupPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;
      CTimer::get("XIOS").resume() ;
      *_ret = CFieldGroup::get(id);
      CTimer::get("XIOS").suspend() ;
   }


   // -------------------- Vérification des identifiants -----------------------

   void cxios_field_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;
      CTimer::get("XIOS").resume() ;
      *_ret = CField::has(id);
      CTimer::get("XIOS").suspend() ;
   }

   void cxios_fieldgroup_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;
      CTimer::get("XIOS").resume() ;
      *_ret = CFieldGroup::has(id);
      CTimer::get("XIOS").suspend() ;
   }

// -----------------------------------------------------------------------------------------------------   
// ------------------------- Other functions
// -----------------------------------------------------------------------------------------------------   

  void cxios_field_is_active (XFieldPtr field_hdl, bool at_current_timestep, bool* ret)
  {
    CTimer::get("XIOS").resume() ;
    *ret = field_hdl->isActive(at_current_timestep);
    CTimer::get("XIOS").suspend() ;
  }

// -----------------------------------------------------------------------------------------------------
// ------------------------- Retrieving information of grid associated to field
// -----------------------------------------------------------------------------------------------------
  void cxios_field_get_domain_handle(XDomainPtr * domain_hdl_ret, XFieldPtr field_hdl, int domainIndex)
  {
     CTimer::get("XIOS").resume() ;
     *domain_hdl_ret = field_hdl->grid->getDomain(domainIndex);
     CTimer::get("XIOS").suspend();
  }

  void cxios_field_get_axis_handle(XAxisPtr * axis_hdl_ret, XFieldPtr field_hdl, int axisIndex)
  {
     CTimer::get("XIOS").resume() ;
     *axis_hdl_ret = field_hdl->grid->getAxis(axisIndex);
     CTimer::get("XIOS").suspend();
  }

  void cxios_field_get_scalar_handle(XScalarPtr * scalar_hdl_ret, XFieldPtr field_hdl, int scalarIndex)
  {
     CTimer::get("XIOS").resume() ;
     *scalar_hdl_ret = field_hdl->grid->getScalar(scalarIndex);
     CTimer::get("XIOS").suspend();
  }
} // extern "C"
