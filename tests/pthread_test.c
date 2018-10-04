/*
	Tests LMDB for process shared mutex functionality.
	If this program returns 95, then LMDB must be recompiled
  using the -DMDB_USE_POSIX_SEM cppflag.
*/
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/stat.h>
#include "lmdb.h"

int 
main(int argc,char * argv[])
{
  int rc = 0;
  MDB_env *env;
  mkdir("/tmp/tmpdb", S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH); 
  mdb_env_create(&env);
  rc = mdb_env_open(env, "/tmp/tmpdb", 0, 0664);
  remove("/tmp/tmpdb/data.mdb");
  remove("/tmp/tmpdb/lock.mdb");
  rmdir("/tmp/tmpdb");
  return rc;
}

