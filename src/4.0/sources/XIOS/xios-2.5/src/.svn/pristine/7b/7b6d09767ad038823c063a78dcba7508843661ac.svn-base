#ifndef __XIOS_CDataPacket__
#define __XIOS_CDataPacket__

#include <boost/shared_ptr.hpp>

#include "array_new.hpp"
#include "date.hpp"

namespace xios
{
  /*!
   * A packet corresponds to a timestamped array of data.
   */
  struct CDataPacket
  {
    /*!
     * Describes the status associated to a packet
     */
    enum StatusCode {
      NO_ERROR,     //!< No error were encountered when handling the packet
      END_OF_STREAM //!< Last packet of the stream, does not have data
    };

    CArray<double, 1> data; //!< Array containing the data
    CDate date;             //!< Date associated to the data
    Time timestamp;         //!< Timestamp of the data
    StatusCode status;      //!< Status of the packet

    /*!
     * Creates a deep copy of the packet.
     *
     * \return a deep copy of the packet
     */
    CDataPacket* copy() const {
      CDataPacket* p = new CDataPacket;
      p->data.resize(data.shape());
      p->data = data;
      p->date = date;
      p->timestamp = timestamp;
      p->status = status;
      return p;
    };
  }; // struct CDataPacket

  typedef boost::shared_ptr<CDataPacket> CDataPacketPtr;
  typedef boost::shared_ptr<const CDataPacket> CConstDataPacketPtr;
} // namespace xios

#endif //__XIOS_CDataPacket__
