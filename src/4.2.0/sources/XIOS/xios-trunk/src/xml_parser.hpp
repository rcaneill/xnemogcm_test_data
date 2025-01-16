#ifndef __XIOS_CXMLParser__
#define __XIOS_CXMLParser__

/// XIOS headers ///
#include "xios_spl.hpp"
#include "exception.hpp"
#include "cxios.hpp"
#include "xml_node.hpp"


namespace xios
{
   namespace xml
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CXMLParser
      {
         public :

            static void ParseFile(const StdString & filename, const std::set<StdString>& parseList = std::set<StdString>());
            static void ParseString(const StdString & xmlContent);
            static void ParseStream(StdIStream & stream, const string& fluxId, const std::set<StdString>& parseList);
            
            template <class T>
            static void ParseInclude(const string& fluxId, T & object);
            
            template <class T>
            static void ParseInclude(const string& fluxId, T & object, const std::set<StdString>& parseContextList);
            
            static string updateIncludePath(const string& filePath) ;

         private:
            static string currentIncludePath_ ; // current include path for XML file
      }; //class CXMLParser
   }// namespace xml
} // namespace xios

#endif // __XIOS_CXMLParser__
