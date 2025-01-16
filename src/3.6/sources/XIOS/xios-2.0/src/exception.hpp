#ifndef __XIOS_CException__
#define __XIOS_CException__

/// XIOS headers ///
#include "xios_spl.hpp"
#include "object.hpp"

namespace xios
{
   /// ////////////////////// Déclarations ////////////////////// ///
   class CException
      : private CObject, public StdOStringStream
   {
         typedef CObject SuperClass;

      public :

         /// Constructeurs ///
         CException(void);
         explicit CException(const StdString & id);
         CException(const CException & exception);
         CException(const CException * const exception); // Not implemented.

         /// Accesseurs ///
         StdString getMessage(void) const;
         StdOStringStream & getStream(void);

         /// Destructeur ///
         virtual ~CException(void);

         /// Autre ///
         virtual StdString toString(void) const;
         virtual void fromString(const StdString & str);

      private :

         /// Propriétés ///
         bool desc_rethrow; // throw destructor

   }; // CException
} // namespace xios

/// //////////////////////////// Macros //////////////////////////// ///

#define INFO(x) \
   "In file \'" __FILE__ "\', line " << __LINE__ << " -> " x << std::endl;

#ifdef __XIOS_DEBUG
#  define DEBUG(x) std::clog << "> Debug " << INFO(x)
#else
#  define DEBUG(x)
#endif

#define ERROR(id, x) xios::CException(id).getStream() << INFO(x)

#endif // __XIOS_CException__
