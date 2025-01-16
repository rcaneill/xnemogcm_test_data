#ifndef __XIOS_MESH_VALUES_HPP__
#define __XIOS_MESH_VALUES_HPP__

namespace xios
{
  /// constants for use as fill values in mesh connectivity arrays

  inline int fill_value_face_faces()
  {
    return -999;
  }

  inline int fill_value_face_edges()
  {
    return -999;
  }

  inline int fill_value_edge_faces()
  {
    return -999;
  }
}

#endif // __XIOS_MESH_VALUES_HPP__
