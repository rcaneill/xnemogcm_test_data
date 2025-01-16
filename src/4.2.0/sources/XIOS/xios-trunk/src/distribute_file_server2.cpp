#include "distribute_file_server2.hpp"
#include <fstream>
#include <vector>
#include <string>
#include <map>
#include <iostream>
#include <cstdlib>
#include <cmath>

namespace xios
{


using namespace std ;


void distributeFileOverServer2(int nbServers, int nGrids, SDistGrid* grids, int nFiles, SDistFile* files)
{

  double maxBandwith=0 ;
  for(int i=0; i<nFiles; i++) if (maxBandwith<files[i].bandwith_) maxBandwith=files[i].bandwith_ ;
  double maxMemory=0 ;
  for(int i=0; i<nGrids; i++) if (maxMemory<grids[i].size_) maxMemory=grids[i].size_ ;
  double T0=maxBandwith+maxMemory ;
  
  // initialise
  vector<SDistServer> servers(nbServers) ;
  for(int i=0;i<nbServers;++i) servers[i].initialize(i,nGrids, grids, nFiles,files) ;
  
  double p ;
  double Em, energy, newEnergy;
  double T, deltaE ;
  vector<int> listFile(nFiles);
    
  for(int i=0; i<nFiles; ++i) servers[i%nbServers].addFile(i) ;

// compute energy
  energy=0;
  for(int i=0;i<nbServers;++i) if (energy<servers[i].energy_) energy=servers[i].energy_ ;

  int nf1,nf2,si,sj ;

//  T=energy ;
  T=T0 ;
  int itMax=10*nFiles*nFiles ;

  srand(314159) ;
  for(int it=0;it<itMax;++it)
  { 
     T=T/(1+10./itMax) ;
  // metropolis step i->j

    nf1=rand()%nFiles ;
    si=files[nf1].assignedServer_ ;
    sj=rand()%nbServers ;

    servers[si].removeFile(nf1) ;
    servers[sj].addFile(nf1) ;

    newEnergy=0;
    for(int i=0;i<nbServers;++i) if (newEnergy<servers[i].energy_) newEnergy=servers[i].energy_ ;
    deltaE=newEnergy-energy ;
    
    if (deltaE>0) 
    {
      p=rand()*1./RAND_MAX ;
      if ( p > exp(-deltaE/T)) // don't accept configuration
      {
        servers[sj].removeFile(nf1) ;
        servers[si].addFile(nf1) ;
      }
      else energy=newEnergy ;
    }
    else energy=newEnergy ;

// metropolis step i<->j

    nf1=rand()%nFiles ;
    nf2=rand()%nFiles ;

    si=files[nf1].assignedServer_ ;
    sj=files[nf2].assignedServer_ ;
    
    servers[si].removeFile(nf1) ;
    servers[sj].removeFile(nf2) ;
    servers[si].addFile(nf2) ;
    servers[sj].addFile(nf1) ;

    newEnergy=0;
    for(int i=0;i<nbServers;++i) if (newEnergy<servers[i].energy_) newEnergy=servers[i].energy_  ;
    deltaE=newEnergy-energy ;
    
    if (deltaE>0) 
    {
      p=rand()*1./RAND_MAX ;
      if ( p > exp(-deltaE/T)) // don't accept configuration
      {

        servers[si].removeFile(nf2) ;
        servers[sj].removeFile(nf1) ;
        servers[si].addFile(nf1) ;
        servers[sj].addFile(nf2) ;
      }
      else energy=newEnergy ;
    }
    else energy=newEnergy ;

// metropolis switch grid
    if (it%10==0)
    {
      int sum=0 ;
      while(sum==0)
      {
        si=rand()%nbServers ;
        for(int i=0;i<nGrids;++i) sum+=servers[si].assignedGrid_[i] ;
      }    
      sj=rand()%nbServers ;
      int numGrid=rand()%nGrids ;
      while (servers[si].assignedGrid_[numGrid]==0) numGrid=rand()%nGrids ;  
      
      int nListFile=0 ;
      for(int i=0;i<nFiles; i++)
      {
        if (files[i].assignedServer_==si)
        {
          for (int j=0;j<files[i].nbGrids_;++j)
          {
            if (files[i].assignedGrid_[j]==numGrid)
            {
              servers[si].removeFile(i) ;
              servers[sj].addFile(i) ;
              listFile[nListFile]=i ;
              nListFile++ ;
              break ;
            }
          }
        }
      }

      newEnergy=0;
      for(int i=0;i<nbServers;++i) if (newEnergy<servers[i].energy_) newEnergy=servers[i].energy_ ;
      deltaE=newEnergy-energy ;
    
      if (deltaE>0) 
      {
        p=rand()*1./RAND_MAX ;
        if ( p > exp(-deltaE/T)) // don't accept configuration
        {

          for(int i=0;i<nListFile;++i)
          {
            servers[sj].removeFile(listFile[i]) ;
            servers[si].addFile(listFile[i]) ;
          }
        }
        else energy=newEnergy ;
      }
      else energy=newEnergy ;
    }  
    
    if (it%1000==0) cout<<"T "<<T<<"  ;  energy "<<energy<<endl ;
  }
}


void SDistServer::initialize(int num, int nMaxGrid, SDistGrid* grids, int nMaxFile, SDistFile* files)
{
  
   num_=num ;
   nMaxGrid_=nMaxGrid;
   grids_=grids ;
   nMaxFile=nMaxFile;
   files_=files ; 
   energy_=0. ;
   bandwith_=0.;
   assignedGrid_=new int[nMaxGrid] ;
   for(int i=0;i<nMaxGrid;++i) assignedGrid_[i]=0 ;
}

void SDistServer::finalize()
{
  delete [] assignedGrid_ ;
}



double SDistServer::addFile(int numFile)
{
  int numGrid ;
  bandwith_+=files_[numFile].bandwith_ ;
  files_[numFile].assignedServer_=num_ ;
  
  for(int i=0;i<files_[numFile].nbGrids_;i++)
  {
    numGrid=files_[numFile].assignedGrid_[i] ;
    if (assignedGrid_[numGrid]==0) memory_+=grids_[numGrid].size_ ;
    assignedGrid_[numGrid]++ ;
  }
  energy_=bandwith_+memory_ ; 
  return energy_ ;
}


double SDistServer::removeFile(int numFile)
{
  int numGrid ;
  bandwith_-=files_[numFile].bandwith_ ;
  for(int i=0;i<files_[numFile].nbGrids_;i++)
  {
    numGrid=files_[numFile].assignedGrid_[i] ;
    if (assignedGrid_[numGrid]==1) memory_-=grids_[numGrid].size_ ;
    assignedGrid_[numGrid]-- ;
  }
  energy_=bandwith_+memory_ ; 
  return energy_ ;
}

}
