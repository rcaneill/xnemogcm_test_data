#include "object_factory_impl.hpp"
#include "node_type.hpp"

namespace xios
{
#define macro(U) \
  template shared_ptr<U> CObjectFactory::GetObject<U>(const StdString& id);  \
  template shared_ptr<U> CObjectFactory::GetObject<U>(const StdString& context,const StdString& id); \
  template shared_ptr<U> CObjectFactory::GetObject<U>(const U* const object); \
  template int CObjectFactory::GetObjectNum<U>(void); \
  template int CObjectFactory::GetObjectIdNum<U>(void); \
  template const std::vector<shared_ptr<U> >& CObjectFactory::GetObjectVector<U>(const StdString& context ); \
  template bool CObjectFactory::HasObject<U>(const StdString& id); \
  template bool CObjectFactory::HasObject<U>(const StdString& context,const StdString& id); \
  template boost::shared_ptr<U> CObjectFactory::CreateObject<U>(const StdString& id ); \
  template const StdString& CObjectFactory::GetUIdBase<U>(void); \
  template StdString CObjectFactory::GenUId<U>(void); \
  template bool CObjectFactory::IsGenUId<U>(const StdString& id); \

  macro(CField)
  macro(CFile)
  macro(CGrid)
  macro(CAxis)
  macro(CDomain)
  macro(CContext)
  macro(CCalendarWrapper)
  macro(CVariable)
  macro(CInverseAxis)
  macro(CZoomAxis)
  macro(CInterpolateAxis)
  macro(CZoomDomain)
  macro(CInterpolateDomain)
  macro(CGenerateRectilinearDomain)
  macro(CScalar)
  macro(CReduceAxisToScalar)
  macro(CReduceDomainToAxis)
  macro(CExtractDomainToAxis)
  macro(CComputeConnectivityDomain)
  macro(CExpandDomain)
  macro(CExtractAxisToScalar)
  macro(CReduceDomainToScalar)

  macro(CFieldGroup)
  macro(CFileGroup)
  macro(CGridGroup)
  macro(CAxisGroup)
  macro(CDomainGroup)
  macro(CContextGroup)
  macro(CVariableGroup)
  macro(CInverseAxisGroup)
  macro(CZoomAxisGroup)
  macro(CInterpolateAxisGroup)
  macro(CZoomDomainGroup)
  macro(CInterpolateDomainGroup)
  macro(CGenerateRectilinearDomainGroup)
  macro(CScalarGroup)
  macro(CReduceAxisToScalarGroup)
  macro(CReduceDomainToAxisGroup)
  macro(CExtractDomainToAxisGroup)
  macro(CComputeConnectivityDomainGroup)
  macro(CExpandDomainGroup)
  macro(CExtractAxisToScalarGroup)
  macro(CReduceDomainToScalarGroup)
}
