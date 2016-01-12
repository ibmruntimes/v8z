// TODO: from ISL 3.14, add license here.

#include <sys/sem.h>
#include <errno.h>
#include <pthread.h>

#if __WORDSIZE == 64
# define __SIZEOF_SEM_T 32
#else
# define __SIZEOF_SEM_T 16
#endif

#define SEM_FAILED ((sem_t *) 0)

typedef union
{
    char __size[__SIZEOF_SEM_T];
      long int __align;
} sem_t;

int initsem(key_t key, int nsems);

int sem_create(key_t key);

int sem_initialize(int *semid, int value);

int sem_init(int *semid,int pshared,unsigned int value);

int sem_destroy(int *semid);

int sem_wait(int *semid);

int sem_trywait(int *semid);

int sem_post(int *semid);

int sem_timedwait(int *semid, struct timespec *timeout);
