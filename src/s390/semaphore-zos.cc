// Copyright 2012 the V8 project authors. All rights reserved.
//
// Copyright IBM Corp. 2016. All rights reserved.
//
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "semaphore-zos.h"
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/modes.h>


void assignSemInitializeError() {
  switch (errno) {
  case EACCES:
    errno = EPERM;
    break;
  case EINVAL:
    break;
  case EPERM:
    break;
  case ERANGE:
    errno = EINVAL;
    break;
  }
}


void assignSemDestroyError() {
  switch (errno) {
  case EACCES:
    errno = EINVAL;
    break;
  case EINVAL:
    break;
  case EPERM:
    errno = EINVAL;
    break;
  case ERANGE:
    break;
  }
}


void assignSemgetError() {
  switch (errno) {
  case EACCES:
    errno = EPERM;
    break;
  case EINVAL:
    break;
  case ENOENT:
    errno = EINVAL;
    break;
  case ENOSPC:
    break;
  default:
    break;
  }
}


void assignSemopErrorCode() {
  switch (errno) {
  case EACCES:
    errno = EINVAL;
    break;
  case EINVAL:
    break;
  case EFAULT:
    errno = EINVAL;
    break;
  case EFBIG:
    errno = EINVAL;
    break;
  case EIDRM:
    errno = EINTR;
    break;
  case ERANGE:
    break;
  case ENOSPC:
    break;
  case EINTR:
    break;
  case EAGAIN:
    break;
  default:
    errno = EINVAL;
    break;
  }
}

static std::vector<sem_t> system_ipc;

// On success returns 0. On error returns -1 and errno is set.
int sem_init(sem_t *sem, int pshared, unsigned int value) {
  if ((sem->__semid = semget(IPC_PRIVATE, 1, S_IRUSR | S_IWUSR)) == -1) {
    assignSemgetError();
    return -1;
  }
  // Assign value to the semaphore.
  union {
    int              val;
    struct semid_ds *buf;
    unsigned short  *array;
  } arg;
  arg.val = value;

  if (-1 == semctl(sem->__semid, 0, SETVAL, arg)) {
    assignSemInitializeError();
    return -1;
  }
  sem->__removed = false;
  system_ipc.push_back(*sem);
  return 0;
}


/* sem_destroy -- destroys the semaphore using semctl() */
int sem_destroy(sem_t *sem) {
  int ret = semctl(sem->__semid, 0, IPC_RMID);
  if (ret == -1) {
    assignSemDestroyError();  /* assign err code for semctl*/
  }
  sem->__removed = true;
  return ret;
}

/* sem_wait -- it gets a lock on semaphore and implemented using semop() */
int sem_wait(sem_t *sem) {
  struct sembuf sb;

  if (sem->__removed == true) {
    errno = EINTR;
    return -1;
  }

  sb.sem_num = 0;
  sb.sem_op = -1;
  sb.sem_flg = 0;
  if (semop(sem->__semid, &sb, 1) == -1) {
    if (errno == EIDRM) {
      sem->__removed = true;
      errno = EINTR;
    } else
      assignSemopErrorCode();
    return -1;
  }
  return 0;
}


/* sem_timedwait -- it waits for a specific time-period to get a lock on
 * semaphore. Implemented using __semop_timed() */
int sem_timedwait(sem_t *sem, struct timespec *ts) {
  int ret;
  struct sembuf sb;
  sb.sem_num = 0;
  sb.sem_op = -1;
  sb.sem_flg = 0;

  ret = __semop_timed(sem->__semid, &sb, 1, ts);
  if (ret != 0) {
    assignSemopErrorCode();
  }
  return ret;
}


/* sem_post -- it releases lock on semaphore using semop */
int sem_post(sem_t *sem) {
  struct sembuf sb;
  sb.sem_num = 0;
  sb.sem_op = 1;
  sb.sem_flg = 0;
  if (semop(sem->__semid, &sb, 1) == -1) {
    assignSemopErrorCode();
    return -1;
  }
  return 0;
}

void sem_destroy_all() {
  std::vector<sem_t>::iterator i = system_ipc.begin();
  while(i != system_ipc.end()) {
    sem_destroy(&(*i));
    i++;
  }
}
