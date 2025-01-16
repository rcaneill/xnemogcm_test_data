#include "mem_checker.hpp"
#include "cxios.hpp"
#include "mpi.hpp"
#include <string>
#include <map>
#include <iostream>
#include <sstream>

#include <fcntl.h>
#include <iomanip>
#include <unistd.h>
#include <cstring>

namespace xios
{
  CMemChecker CMemChecker::dummy_("") ;
  std::map<std::string,CMemChecker> CMemChecker::allMemChecker_;
  bool CMemChecker::enabled_=true;
  bool CMemChecker::first_=true;
  double CMemChecker::vsize_init_=0;
  double CMemChecker::rss_init_=0;
  double CMemChecker::vmhwm_init_=0;
  double CMemChecker::time_init_=0;
  std::ofstream CMemChecker::fout_;
  int CMemChecker::flush_counter_=1;
  
  CMemChecker::CMemChecker(const std::string& name) : name_(name)
  { 
    if (first_) check() ;
    reset();
  }

  void CMemChecker::check(void)
  {
    std::ifstream statStream("/proc/self/stat",std::ios_base::in);
    enabled_ &= statStream.good() ;
    first_=false ;
  }
  
  // Return vector of memories amount :
  //   - vector[0] : virtual memory (vsize) from /proc/self/stat
  //   - vector[1] : resident set size (rss) from /proc/self/stat
  std::vector<double> CMemChecker::getMem(void)
  {
    std::vector<double> memories(0);
    if ( !CXios::reportMemory ) return memories ;
    memories.clear();
    
    if (first_) check() ;
    if (!enabled_) return memories;
    std::ifstream statStream("/proc/self/stat",std::ios_base::in);
    std::string dummy ;
    for(int i=1;i<=22;i++) statStream>>dummy ;
    unsigned long vsize; 
    statStream>>vsize ;
    memories.push_back(vsize);
    unsigned long rss;
    statStream>>rss ;
    memories.push_back(rss*4096);
    return memories ;
  }
  
  // Return vector of memories amount, register values (see _init) as first call as reference :
  //   - vector[0] : virtual memory (vsize) from /proc/self/stat
  //   - vector[1] : resident set size (rss) from /proc/self/stat
  //   - vector[2] : Peak resident set size (VmHWM) from /proc/self/status
  std::vector<double> CMemChecker::getMemories(void)
  {
    std::vector<double> memories;

    if (first_) check() ;
    if (!enabled_) return memories;
    std::ifstream statStream("/proc/self/stat",std::ios_base::in);
    std::string dummy ;
    for(int i=1;i<=22;i++) statStream>>dummy ;

    // 1 - Virtual
    unsigned long vsize; 
    statStream>>vsize ;
    if (vsize_init_==0) {
      vsize_init_ = vsize;
      time_init_=MPI_Wtime();
    }
    vsize -= vsize_init_;
    memories.push_back(vsize);

    // 2 - RSS
    unsigned long rss;
    statStream>>rss ;
    if (rss_init_==0) {
      rss_init_ = rss;
    }
    rss -= rss_init_;
    rss *= 4096; //getconf("PAGE_SIZE");    
    memories.push_back(rss);

    // 3 - Peak
    char sbuf[1024];
    std::ifstream file( "/proc/self/status" );
    if( file.fail() ) {
      return memories;
    }
    int fd = open( "/proc/self/status", O_RDONLY, 0 );
    int num_read=read( fd, sbuf, ( sizeof sbuf )-1 );
    close( fd );
    if( !num_read ) {
      return memories;             
    }
    // Peak resident set size
    char *S=strstr( sbuf, "VmHWM:" )+6;
    double vmhwm = ( int )atoi( S );
    if (vmhwm_init_==0) {
      vmhwm_init_ = vmhwm;
    }
    vmhwm -= vmhwm_init_;
    memories.push_back(vmhwm);
    
    return memories;
  }
  
  void CMemChecker::logMem( std::string id, bool finalizeLog )
  {
    if ( !CXios::logMemory ) return ;

    int rk = 0;
    MPI_Comm_rank( MPI_COMM_WORLD, &rk );
    std::string logName("xios_memory_"+std::to_string(rk)+".csv");
    vector<double> mem = getMemories();
    if (!mem[0]) {
      fout_.open( logName );
      fout_ << "time,event,vsize,rss,VmHWM" << std::endl;
    }

    fout_.precision(4);
    // Time format : YYYY-MM-DD HH:MM:SS.XXX -> seconds * 1000.
    fout_ << (MPI_Wtime()-time_init_) << "," << id
          << "," << mem[0]/1024./1024.
          << "," << mem[1]/1024./1024.
          << "," << mem[2]/1024.
          << std::endl;

    if ((MPI_Wtime()-time_init_)>flush_counter_*600.)
    {
      fout_.flush();
      flush_counter_++;
    }
    
    if (finalizeLog)
    {
      fout_.close();
    }
  }

  
  void CMemChecker::suspend(void)
  {
    if (first_) check() ;
    if (!enabled_) return ;
    if (!suspended_)
    {
      vector<double> mem = getMem();
      cumulatedMem_.resize( mem.size() );
      for (int i=0;i<mem.size() ; i++)
      {
        cumulatedMem_[i] += mem[i] - lastMem_[i];
      }
    }
    suspended_ = true;
  }
  
  void CMemChecker::resume(void)
  {
    if (first_) check() ;
    if (!enabled_) return ;
    if (suspended_)
    {
      vector<double> mem = getMem();
      lastMem_.resize( mem.size() );
      for (int i=0;i<mem.size() ; i++)
      {
        lastMem_[i] = mem[i];
      }
    }
    suspended_ = false;
  }

  void CMemChecker::reset(void)
  {
    if (first_) check() ;
    if (!enabled_) return ;
    cumulatedMem_.clear();
    suspended_ = true;
  }
  
  std::vector<double> CMemChecker::getCumulatedMem(void)
  {
    std::vector<double> memories;
    memories.clear();
    if (first_) check() ;
    if (!enabled_) return memories;
    return cumulatedMem_;
  }
  
  CMemChecker& CMemChecker::get(const std::string name)
  {
    if (first_) check() ;
    if (!enabled_) return dummy_ ;
    else
    {
      std::map<std::string,CMemChecker>::iterator it = allMemChecker_.find(name);
      if (it == allMemChecker_.end())
        it = allMemChecker_.insert(std::make_pair(name, CMemChecker(name))).first;
      return it->second;
    }
  }

  std::string CMemChecker::getAllCumulatedMem(void)
  {
    if (first_) check() ;
    if (!enabled_) return std::string(" MemChecker : memory consumption report not available") ; 
    std::ostringstream strOut ;
    const double Kb=1024 ;
    const double Mb=Kb*1024 ;
    const double Gb=Mb*1024 ;
    const double Tb=Gb*1024 ;
    for(std::map<std::string,CMemChecker>::iterator it=allMemChecker_.begin();it!=allMemChecker_.end();++it)
    {  
      vector<double> mem=it->second.getCumulatedMem() ;
      if (mem.size()>0)
      {
        strOut<<"MemChecker : "<<it->first<<"    -->   consumed memory : " ;      
        if ((fabs(mem[0])>=Tb)||(fabs(mem[1])>=Tb)) strOut<< mem[0] / Tb<<" Tb - "<<mem[1] / Tb<<" Tb"<<std::endl ;
        else if ((fabs(mem[0])>=Gb)||(fabs(mem[1])>=Gb)) strOut<< mem[0] / Gb<<" Gb - "<<mem[1] / Gb<<" Gb"<<std::endl ;
        else if ((fabs(mem[0])>=Mb)||(fabs(mem[1])>=Mb)) strOut<< mem[0] / Mb<<" Mb - "<<mem[1] / Mb<<" Mb"<<std::endl ;
        else if ((fabs(mem[0])>=Kb)||(fabs(mem[1])>=Kb)) strOut<< mem[0] / Kb<<" Kb - "<<mem[1] / Kb<<" Kb"<<std::endl ;
        else strOut<< mem[0] <<" bytes - "<<mem[1] <<" bytes"<<std::endl ;
      }
    }
    return strOut.str() ;
  }
}
