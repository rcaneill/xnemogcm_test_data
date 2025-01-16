/* ************************************************************************** *
 *      Copyright © IPSL/LSCE, xios, Avril 2010 - Octobre 2011         *
 * ************************************************************************** */

#include <boost/multi_array.hpp>
#include <boost/shared_ptr.hpp>

#include "xios.hpp"

#include "object_template.hpp"
#include "group_template.hpp"
#include "attribute_template.hpp"

#include "icutil.hpp"
#include "timer.hpp"
#include "extract_axis_to_scalar.hpp"

extern "C"
{
// /////////////////////////////// Définitions ////////////////////////////// //

   // ----------------------- Redéfinition de types ----------------------------

   typedef xios::CExtractAxisToScalar      * XExtractAxisToScalarPtr;
   
   // ------------------------ Création des handle -----------------------------
   void cxios_extract_axis_to_scalar_handle_create(XExtractAxisToScalarPtr * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;
      CTimer::get("XIOS").resume() ;
      *_ret = xios::CExtractAxisToScalar::get(id);
      CTimer::get("XIOS").suspend() ;
   }

   // -------------------- Vérification des identifiants -----------------------
   void cxios_extract_axis_to_scalar_valid_id(bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      CTimer::get("XIOS").resume() ;
      *_ret = xios::CExtractAxisToScalar::has(id);
      CTimer::get("XIOS").suspend() ;
   }

} // extern "C"
