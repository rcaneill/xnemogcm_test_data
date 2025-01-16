#ifndef __XIOS_CSpatialTransformFilter__
#define __XIOS_CSpatialTransformFilter__

#include "filter.hpp"

namespace xios
{
  class CGrid;
  class CGridTransformation;
  class CSpatialTransformFilterEngine;

  /*!
   * A generic filter with multiple input slots wrapping any type of spatial transformations.
   */
  class CSpatialTransformFilter : public CFilter
  {
    public:
      /*!
       * Constructs a filter wrapping the specified spatial transformation.
       *
       * \param gc the associated garbage collector
       * \param engine the engine defining the spatial transformation
       * \param outputValue default value of output pin
       * \param [in] inputSlotsCount number of input, by default there is only one for field src
       */
      CSpatialTransformFilter(CGarbageCollector& gc, CSpatialTransformFilterEngine* engine, double outputValue, size_t inputSlotsCount = 1);

      /*!
       * Builds the filter graph needed to transform the specified source grid into the specified destination grid.
       *
       * \param gc the associated garbage collector
       * \param srcGrid the source grid
       * \param destGrid the destination grid
       * \param hasMissingValue whether field source has missing value
       * \param defaultValue default value
       * \return the first and the last filters of the filter graph
       */
      static std::pair<boost::shared_ptr<CSpatialTransformFilter>, boost::shared_ptr<CSpatialTransformFilter> >
      buildFilterGraph(CGarbageCollector& gc, CGrid* srcGrid, CGrid* destGrid, bool hasMissingValue, double defaultValue);

    protected:
      /*!
        Overriding this function to process transformations with auxillary inputs
      */
      void virtual onInputReady(std::vector<CDataPacketPtr> data);

    protected:
      //! Default value of output pin
      double outputDefaultValue;
  }; // class CSpatialTransformFilter


 /*!
   * A specific spatial filter for the temporal_splitting transformation scalar -> axis. An incoming flux will be stored in an aditional dimension given by the destination axis.
   * At each flux received, the storing index (record) is increased. When it reach the size of the axis (nrecords) a new flux is generated and the record is reset to 0
   */

 class CSpatialTemporalFilter : public CSpatialTransformFilter
  {
    public:
      /*!
       * Constructs a filter wrapping the specified spatial transformation.
       *
       * \param gc the associated garbage collector
       * \param engine the engine defining the spatial transformation
       * \param [in] gridTransformation the associated transformations
       * \param outputValue default value of output pin
       * \param [in] inputSlotsCount number of input, by default there is only one for field src
       */
      CSpatialTemporalFilter(CGarbageCollector& gc, CSpatialTransformFilterEngine* engine, CGridTransformation* gridTransformation, double outputValue, size_t inputSlotsCount = 1);


    protected:
      /*!
        Overriding this function to process transformations with auxillary inputs
      */
      void virtual onInputReady(std::vector<CDataPacketPtr> data);
      //! Current record in the filter
      int record ;
      //! Maximum number of records
      int nrecords;
      //! Temporary storage for output flux
      CArray<double, 1> tmpData; 


  }; // class CSpatialTemporalFilter


  /*!
   * A generic filter engine wrapping a grid transformation.
   */
  class CSpatialTransformFilterEngine : public IFilterEngine
  {
    public:
      /*!
       * Returns the engine wrapping the specified grid transformation.
       * If the engine already exists it is reused, otherwise it is created.
       *
       * \param gridTransformation the grid transformation the engine will use
       * \return the engine wrapping the specified grid transformation
       */
      static CSpatialTransformFilterEngine* get(CGridTransformation* gridTransformation);

      /*!
       * Applies the grid transformation to the input data and returns the result.
       *
       * \param data a vector of packets corresponding to each slot (one in this case)
       * \param [in] defaultValue default value of output data
       * \return the result of the grid transformation
       */
      CDataPacketPtr applyFilter(std::vector<CDataPacketPtr> data, double defaultValue = 0);

       /*!
       * Applies the grid transformation to the input data and returns the result.
       *
       * \param data a vector of packets corresponding to each slot (one in this case)
       * \return the result of the grid transformation
       */
      CDataPacketPtr virtual apply(std::vector<CDataPacketPtr> data);



    protected:
      /*!
       * Constructs a filter engine wrapping the specified grid transformation.
       *
       * \param gridTransformation the grid transformation the engine will use
       */
      CSpatialTransformFilterEngine(CGridTransformation* gridTransformation);

      /*!
       * Applies the grid transformation to the input data and returns the result.
       * This helper function handles all the communications.
       *
       * \param dataSrc the source data
       * \param dataDest the resulting transformed data
       */
      void apply(const CArray<double, 1>& dataSrc, CArray<double,1>& dataDest);

      CGridTransformation* gridTransformation; //!< The grid transformation used by the engine

      //! The allocated engines
      static std::map<CGridTransformation*, boost::shared_ptr<CSpatialTransformFilterEngine> > engines;
  }; // class CSpatialTransformFilterEngine
} // namespace xios

#endif //__XIOS_CSpatialTransformFilter__
