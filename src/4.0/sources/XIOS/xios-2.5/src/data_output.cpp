#include "data_output.hpp"

#include "attribute_template.hpp"
#include "group_template.hpp"
#include "context.hpp"

namespace xios
{
      /// ////////////////////// Définitions ////////////////////// ///

      CDataOutput::~CDataOutput(void)
      { /* Ne rien faire de plus */ }

      //----------------------------------------------------------------

      void CDataOutput::writeGrid(CGrid* grid, bool allowCompressedOutput /*= false*/)
      {
        this->writeGrid(grid->getDomains(), grid->getAxis(), grid->getScalars());

        if (allowCompressedOutput)
          writeGridCompressed_(grid);
      }

      //----------------------------------------------------------------

      void CDataOutput::writeFile(CFile*  file)
      {
         this->writeFile_(file);
      }

      void CDataOutput::writeAttribute(CVariable*  var)
      {
         this->writeAttribute_(var) ;
      }

      void CDataOutput::syncFile(void)
      {
         this->syncFile_();
      }

      void CDataOutput::closeFile(void)
      {
         this->closeFile_();
      }

      //----------------------------------------------------------------

      void CDataOutput::writeGrid(CDomain* domain, CAxis* axis)
      {
         this->writeDomain_(domain);
         this->writeAxis_(axis);
      }

      void CDataOutput::writeGrid(std::vector<CDomain*> domains, std::vector<CAxis*> axis)
      {
        int domSize = domains.size();
        int aSize = axis.size();
        for (int i = 0; i < domSize; ++i) this->writeDomain_(domains[i]);
        for (int i = 0; i < aSize; ++i) this->writeAxis_(axis[i]);
      }

      void CDataOutput::writeGrid(std::vector<CDomain*> domains, std::vector<CAxis*> axis, std::vector<CScalar*> scalars)
      {
        int domSize = domains.size();
        int aSize = axis.size();
        int sSize = scalars.size();
        for (int i = 0; i < domSize; ++i) this->writeDomain_(domains[i]);
        for (int i = 0; i < aSize; ++i) this->writeAxis_(axis[i]);
        for (int i = 0; i < sSize; ++i) this->writeScalar_(scalars[i]);
      }

      //----------------------------------------------------------------

      void CDataOutput::writeGrid(CDomain* domain)
      {
         this->writeDomain_(domain);
      }

      void CDataOutput::writeTimeDimension(void)
      {
         this->writeTimeDimension_();
      }

      //----------------------------------------------------------------

      void CDataOutput::writeFieldTimeAxis(CField* field)
      {
         CContext* context = CContext::getCurrent() ;
         boost::shared_ptr<CCalendar> calendar = context->getCalendar();

         this->writeTimeAxis_(field, calendar);
      }
      
      void CDataOutput::writeField(CField* field)
      {
         this->writeField_(field);
      }

      //----------------------------------------------------------------

      void CDataOutput::writeFieldGrid(CField* field)
      {
         this->writeGrid(field->getRelGrid(),
                         !field->indexed_output.isEmpty() && field->indexed_output);
      }

      //----------------------------------------------------------------

      void CDataOutput::writeFieldData(CField* field)
      {
//         CGrid* grid = CGrid::get(field->grid_ref.getValue());
//         CDomain* domain = CDomain::get(grid->domain_ref.getValue());
         this->writeFieldData_(field);
      }

      ///----------------------------------------------------------------

} // namespace xios
