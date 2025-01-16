#ifndef __SERVER_HPP__
#define __SERVER_HPP__

#include "xios_spl.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "mpi.hpp"
#include "event_scheduler.hpp"

namespace xios
{
    class CServer
    {
      public:
        static void initialize(void);
        static void finalize(void);
        static void eventLoop(void);
        static void contextEventLoop(void);
        static void listenContext(void);
        static void listenFinalize(void);
        static void recvContextMessage(void* buff,int count);
        static void listenRootContext(void);
        static void listenRootFinalize(void);
        static void registerContext(void* buff,int count, int leaderRank=0);

        static MPI_Comm intraComm;
        static list<MPI_Comm> interComm;
        static std::list<MPI_Comm> contextInterComms;
        static CEventScheduler* eventScheduler;
        
        struct contextMessage
        {
          int nbRecv;
          int leaderRank;
        };

        static bool isRoot;

        static map<string,CContext*> contextList;
        static bool finished;
        static bool is_MPI_Initialized;

      public:
        //! Get rank of the current process
        static int getRank();

        //! Open a file stream to write the info logs
        static void openInfoStream(const StdString& fileName);
        //! Write the info logs to standard output
        static void openInfoStream();
        //! Close the info logs file if it opens
        static void closeInfoStream();

        //! Open a file stream to write the error log
        static void openErrorStream(const StdString& fileName);
        //! Write the error log to standard error output
        static void openErrorStream();
        //! Close the error log file if it opens
        static void closeErrorStream();

      private:
        static int rank;
        static StdOFStream m_infoStream;
        static StdOFStream m_errorStream;

        static void openStream(const StdString& fileName, const StdString& ext, std::filebuf* fb);
    };
}

#endif
