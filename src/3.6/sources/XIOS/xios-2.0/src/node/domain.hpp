#ifndef __XIOS_CDomain__
#define __XIOS_CDomain__

/// XIOS headers ///
#include "xios_spl.hpp"
#include "group_factory.hpp"

#include "declare_group.hpp"
#include "declare_ref_func.hpp"
#include "event_client.hpp"
#include "event_server.hpp"
#include "buffer_in.hpp"
#include "array_new.hpp"
#include "attribute_array.hpp"
#include "attribute_enum.hpp"
#include "transformation.hpp"
#include "transformation_enum.hpp"

#include "mesh.hpp"

namespace xios {

   /// ////////////////////// Déclarations ////////////////////// ///

   class CDomainGroup;
   class CDomainAttributes;
   class CDomain;
   class CFile;

   ///--------------------------------------------------------------

   // Declare/Define CDomainAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CDomain)
#  include "domain_attribute.conf"
#  include "domain_attribute_private.conf"
   END_DECLARE_ATTRIBUTE_MAP(CDomain)

   ///--------------------------------------------------------------

   class CDomain
      : public CObjectTemplate<CDomain>
      , public CDomainAttributes
   {

         enum EEventId
         {
           EVENT_ID_SERVER_ATTRIBUT, EVENT_ID_INDEX, EVENT_ID_LON, EVENT_ID_LAT, EVENT_ID_AREA
         } ;

         /// typedef ///
         typedef CObjectTemplate<CDomain>   SuperClass;
         typedef CDomainAttributes SuperClassAttribute;

      public:

         typedef CDomainAttributes RelAttributes;
         typedef CDomainGroup      RelGroup;
         typedef CTransformation<CDomain>::TransformationMapTypes TransMapTypes;

         /// Constructeurs ///
         CDomain(void);
         explicit CDomain(const StdString & id);
         CDomain(const CDomain & domain);       // Not implemented yet.
         CDomain(const CDomain * const domain); // Not implemented yet.

         static CDomain* createDomain();
         
         CMesh* mesh;
         void assignMesh(const StdString, const int);
        
         virtual void parse(xml::CXMLNode & node);

         /// Vérifications ///
         void checkAttributes(void);

         void checkAttributesOnClient();
         void checkAttributesOnClientAfterTransformation();

         void checkEligibilityForCompressedOutput(void);

         void sendCheckedAttributes();

         bool hasTransformation();
         void solveInheritanceTransformation();
         TransMapTypes getAllTransformations();
         void redistribute(int nbLocalDomain);
         void duplicateTransformation(CDomain*);
         CTransformation<CDomain>* addTransformation(ETranformationType transType, const StdString& id="");

      public:
         const std::set<StdString> & getRelFiles(void) const;
         bool IsWritten(const StdString & filename) const;
         bool isWrittenCompressed(const StdString& filename) const;

         const std::vector<int>& getIndexesToWrite(void) const;
         int getNumberWrittenIndexes() const;
         int getTotalNumberWrittenIndexes() const;
         int getOffsetWrittenIndexes() const;

         std::map<int, StdSize> getAttributesBufferSize();

         bool isEmpty(void) const;
         bool isDistributed(void) const;
         bool isCompressible(void) const;
         bool distributionAttributesHaveValue() const;

         int ni_srv,ibegin_srv,iend_srv ;
         int zoom_ni_srv,zoom_ibegin_srv,zoom_iend_srv ;

         int nj_srv,jbegin_srv,jend_srv ;
         int zoom_nj_srv,zoom_jbegin_srv,zoom_jend_srv ;

         CArray<double, 1> lonvalue_srv, latvalue_srv ;
         CArray<double, 2> bounds_lon_srv, bounds_lat_srv ;
         CArray<double, 1> lonvalue_client, latvalue_client;
         CArray<double, 2> bounds_lon_client, bounds_lat_client;
         CArray<double, 1> area_srv;

        vector<int> connectedServer ; // list of connected server
        vector<int> nbSenders ; // for each communication with a server, number of communicating client
        vector<int> nbDataSrv ; // size of data to send to each server
        vector< vector<int> > i_indSrv ; // for each server, i global index to send
        vector< vector<int> > j_indSrv ; // for each server, j global index to send

         std::vector<int> getNbGlob();
         bool isEqual(CDomain* domain);
      public:
         /// Mutateur ///
         void addRelFile(const StdString & filename);
         void addRelFileCompressed(const StdString& filename);
         void completeLonLatClient(void);
         void sendServerAttribut(void) ;
         void sendLonLatArea(void);
         void computeConnectedServer(void) ;
         void computeLocalMask(void) ;
         
         void AllgatherRectilinearLonLat(CArray<double,1>& lon, CArray<double,1>& lat,
                                         CArray<double,1>& lon_g, CArray<double,1>& lat_g);

         void fillInRectilinearBoundLonLat(CArray<double,1>& lon, CArray<double,1>& lat,
                                           CArray<double,2>& boundsLon, CArray<double,2>& boundsLat);
         
         void fillInLonLat();

         static bool dispatchEvent(CEventServer& event);
         static void recvServerAttribut(CEventServer& event);
         static void recvIndex(CEventServer& event);
         static void recvLon(CEventServer& event);
         static void recvLat(CEventServer& event);
         static void recvArea(CEventServer& event);
         void recvServerAttribut(CBufferIn& buffer);
         void recvIndex(int rank, CBufferIn& buffer);
         void recvLon(int rank, CBufferIn& buffer);
         void recvLat(int rank, CBufferIn& buffer);
         void recvArea(int rank, CBufferIn& buffer);

         /// Destructeur ///
         virtual ~CDomain(void);

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);

         static ENodeType GetType(void);
         const std::map<int, vector<size_t> >& getIndexServer() const;
         CArray<bool, 1> localMask;
         bool isCurvilinear ;
         bool hasBounds ;
         bool hasArea;
         bool hasLonLat;
         bool hasPole ;

      private:
         void checkDomain(void);
         void checkLocalIDomain(void);
         void checkLocalJDomain(void);

         void checkMask(void);
         void checkDomainData(void);
         void checkCompression(void);

         void checkBounds(void);
         void checkArea(void);
         void checkLonLat();
         void checkZoom(void);    
         
         void setTransformations(const TransMapTypes&);
         void computeNGlobDomain();

         void sendIndex();
         void sendArea();
         void sendLonLat();

         void fillInRectilinearLonLat();
         void fillInCurvilinearLonLat();
         void fillInUnstructuredLonLat();
       private:
         bool isChecked;
         std::set<StdString> relFiles, relFilesCompressed;
         bool isClientChecked; // Verify whether all attributes of domain on the client side are good
         bool isClientAfterTransformationChecked;
         std::map<int, CArray<int,1> > indiSrv, indjSrv;
         std::map<int,int> nbConnectedClients_; // Mapping of number of communicating client to a server
         std::map<int, vector<size_t> > indSrv_; // Global index of each client sent to server
         std::map<int, vector<int> > indWrittenSrv_; // Global written index of each client sent to server
         std::vector<int> indexesToWrite;
         int numberWrittenIndexes_, totalNumberWrittenIndexes_, offsetWrittenIndexes_;
         std::vector<int> connectedServerRank_;
         bool isDistributed_;
         //! True if and only if the data defined on the domain can be outputted in a compressed way
         bool isCompressible_;
         bool isRedistributed_;
         TransMapTypes transformationMap_;         
         bool isUnstructed_;
       
       private:
         static bool initializeTransformationMap(std::map<StdString, ETranformationType>& m);
         static std::map<StdString, ETranformationType> transformationMapList_;
         static bool _dummyTransformationMapList;

         DECLARE_REF_FUNC(Domain,domain)

   }; // class CDomain

   ///--------------------------------------------------------------

   // Declare/Define CDomainGroup and CDomainDefinition
   DECLARE_GROUP(CDomain);

   ///--------------------------------------------------------------

} // namespace xios

#endif //__XIOS_CDomain__
