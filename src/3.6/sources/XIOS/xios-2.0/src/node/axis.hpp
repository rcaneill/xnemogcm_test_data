#ifndef __XIOS_CAxis__
#define __XIOS_CAxis__

/// XIOS headers ///
#include "xios_spl.hpp"
#include "group_factory.hpp"
#include "virtual_node.hpp"

#include "declare_group.hpp"
#include "declare_ref_func.hpp"
#include "declare_virtual_node.hpp"
#include "attribute_array.hpp"
#include "attribute_enum.hpp"
#include "attribute_enum_impl.hpp"
#include "server_distribution_description.hpp"
#include "transformation.hpp"
#include "transformation_enum.hpp"

namespace xios {
   /// ////////////////////// Déclarations ////////////////////// ///

   class CAxisGroup;
   class CAxisAttributes;
   class CAxis;

   ///--------------------------------------------------------------

   // Declare/Define CAxisAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CAxis)
#  include "axis_attribute.conf"
#  include "axis_attribute_private.conf"
   END_DECLARE_ATTRIBUTE_MAP(CAxis)

   ///--------------------------------------------------------------

   class CAxis
      : public CObjectTemplate<CAxis>
      , public CAxisAttributes
   {
         enum EEventId
         {
           EVENT_ID_SERVER_ATTRIBUT,
           EVENT_ID_INDEX,
           EVENT_ID_DISTRIBUTED_VALUE,
           EVENT_ID_NON_DISTRIBUTED_VALUE
         } ;

         /// typedef ///
         typedef CObjectTemplate<CAxis>   SuperClass;
         typedef CAxisAttributes SuperClassAttribute;

      public :

         typedef CAxisAttributes RelAttributes;
         typedef CAxisGroup      RelGroup;
         typedef CTransformation<CAxis>::TransformationMapTypes TransMapTypes;

      public:
         /// Constructeurs ///
         CAxis(void);
         explicit CAxis(const StdString & id);
         CAxis(const CAxis & axis);       // Not implemented yet.
         CAxis(const CAxis * const axis); // Not implemented yet.

         static CAxis* createAxis();

         /// Accesseurs ///
         const std::set<StdString> & getRelFiles(void) const;

         const std::vector<int>& getIndexesToWrite(void) const;
         int getNumberWrittenIndexes() const;
         int getTotalNumberWrittenIndexes() const;
         int getOffsetWrittenIndexes() const;

         std::map<int, StdSize> getAttributesBufferSize();

         /// Test ///
         bool IsWritten(const StdString & filename) const;
         bool isWrittenCompressed(const StdString& filename) const;
         bool isDistributed(void) const;
         bool isCompressible(void) const;

         /// Mutateur ///
         void addRelFile(const StdString & filename);
         void addRelFileCompressed(const StdString& filename);

         /// Vérifications ///
         void checkAttributes(void);

         /// Destructeur ///
         virtual ~CAxis(void);

         virtual void parse(xml::CXMLNode & node);

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);
         static ENodeType GetType(void);

         void sendServerAttribut(const std::vector<int>& globalDim, int orderPositionInGrid,
                                 CServerDistributionDescription::ServerDistributionType distType);
         static bool dispatchEvent(CEventServer& event);
         static void recvServerAttribut(CEventServer& event);
         void recvServerAttribut(CBufferIn& buffer) ;
         void checkAttributesOnClient();
         void checkAttributesOnClientAfterTransformation(const std::vector<int>& globalDim, int orderPositionInGrid,
                                                         CServerDistributionDescription::ServerDistributionType distType = CServerDistributionDescription::BAND_DISTRIBUTION);
         void sendCheckedAttributes(const std::vector<int>& globalDim, int orderPositionInGrid,
                                    CServerDistributionDescription::ServerDistributionType disType = CServerDistributionDescription::BAND_DISTRIBUTION);

         void checkEligibilityForCompressedOutput();

         bool hasTransformation();
         void solveInheritanceTransformation();
         TransMapTypes getAllTransformations();
         void fillInValues(const CArray<double,1>& values);
         void duplicateTransformation(CAxis*);
         CTransformation<CAxis>* addTransformation(ETranformationType transType, const StdString& id="");
         bool isEqual(CAxis* axis);

      public:
        int zoom_begin_srv, zoom_end_srv, zoom_size_srv;
        int ni_srv, begin_srv, end_srv;
        int global_zoom_begin_srv, global_zoom_end_srv, global_zoom_size_srv;
        CArray<double,1> value_srv;
        CArray<double,2> bound_srv;
        CArray<StdString,1> label_srv;
        CArray<int,1> zoom_index_srv;
        bool hasValue;

      private:
         void checkData();
         void checkMask();
         void checkZoom();
         void checkBounds();
         void checkLabel();         
         void sendValue();
         void computeConnectedServer(const std::vector<int>& globalDim, int orderPositionInGrid,
                                     CServerDistributionDescription::ServerDistributionType distType);
         void sendDistributedValue();
         void sendNonDistributedValue();
         bool zoomByIndex();

         static void recvIndex(CEventServer& event);
         static void recvDistributedValue(CEventServer& event);
         static void recvNonDistributedValue(CEventServer& event);
         void recvIndex(int rank, CBufferIn& buffer);
         void recvDistributedValue(int rank, CBufferIn& buffer);
         void recvNonDistributedValue(int rank, CBufferIn& buffer);

         void setTransformations(const TransMapTypes&);

      private:
         bool isChecked;
         bool areClientAttributesChecked_;
         bool isClientAfterTransformationChecked;
         std::set<StdString> relFiles, relFilesCompressed;
         TransMapTypes transformationMap_;
         bool isDistributed_;
         //! True if and only if the data defined on the axis can be outputted in a compressed way
         bool isCompressible_;
         std::map<int,int> nbConnectedClients_; // Mapping of number of communicating client to a server
         std::map<int, vector<size_t> > indSrv_; // Global index of each client sent to server
         std::map<int, vector<int> > indWrittenSrv_; // Global written index of each client sent to server
         std::vector<int> indexesToWrite;
         int numberWrittenIndexes_, totalNumberWrittenIndexes_, offsetWrittenIndexes_;
         std::vector<int> connectedServerRank_;
         std::map<int, CArray<int,1> > indiSrv_;
         bool hasBounds_;
         bool hasLabel;

       private:
         static bool initializeTransformationMap(std::map<StdString, ETranformationType>& m);
         static std::map<StdString, ETranformationType> transformationMapList_;
         static bool dummyTransformationMapList_;

         DECLARE_REF_FUNC(Axis,axis)
   }; // class CAxis

   ///--------------------------------------------------------------

   // Declare/Define CAxisGroup and CAxisDefinition
   DECLARE_GROUP(CAxis);
} // namespace xios

#endif // __XIOS_CAxis__
