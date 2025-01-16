#ifndef __XIOS_MEM_CHECKER_HPP__
#define __XIOS_MEM_CHECKER_HPP__

#include <string>
#include <map>
#include <fstream>
#include <vector>

namespace xios
{
  class CMemChecker
  {
    public:
      CMemChecker(const std::string& name);
      void suspend(void);
      void resume(void);
      void reset(void);
      std::vector<double> getCumulatedMem(void);
      static std::vector<double> getMem(void);
      static std::vector<double> getMemories(void);
      static void logMem( std::string id, bool finalizeLog = false );
      static CMemChecker& get(std::string name);
      static std::string getAllCumulatedMem(void) ;
      static void disable(void) { enabled_=false ;}
      static void enable(void) {enabled_=true ;}
      static void release(void) {allMemChecker_.clear();}
    private:
      static void check(void) ;
      std::vector<double> cumulatedMem_;
      std::vector<double> lastMem_;
      bool suspended_;
      std::string name_;

      static std::map<std::string,CMemChecker> allMemChecker_;
      static CMemChecker dummy_ ;
      static bool first_ ;
      static bool enabled_ ;

      static double vsize_init_;
      static double rss_init_;
      static double vmhwm_init_;
      static double time_init_;
      static std::ofstream fout_;
      static int flush_counter_;
  };
}



#endif
