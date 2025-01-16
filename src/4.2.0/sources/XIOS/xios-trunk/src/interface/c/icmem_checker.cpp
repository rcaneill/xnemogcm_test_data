
#include "mem_checker.hpp"
#include <string>
#include <iostream>


using namespace xios;

extern "C"
{
  void cxios_mem_checker_get(double* mem)
  {
    std::vector<double> memories = CMemChecker::getMem();
    if (memories.size()>1)
    {
      *mem=memories[1]; //rss
    }
    else
    {
      *mem = 0.;
    }
  }

  void cxios_mem_checker_log(const char* mem_id, int len_mem_id, bool* finalize)
  {
    std::string str(mem_id,len_mem_id);
    CMemChecker::logMem(str, *finalize);
  }

}


