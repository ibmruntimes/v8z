# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above
#       copyright notice, this list of conditions and the following
#       disclaimer in the documentation and/or other materials provided
#       with the distribution.
#     * Neither the name of Google Inc. nor the names of its
#       contributors may be used to endorse or promote products derived
#       from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import os
import subprocess
import sys
import time
import platform
from threading import Timer

from ..local import utils
from ..objects import output


SEM_INVALID_VALUE = -1
SEM_NOGPFAULTERRORBOX = 0x0002  # Microsoft Platform SDK WinBase.h


def Win32SetErrorMode(mode):
  prev_error_mode = SEM_INVALID_VALUE
  try:
    import ctypes
    prev_error_mode = \
        ctypes.windll.kernel32.SetErrorMode(mode)  #@UndefinedVariable
  except ImportError:
    pass
  return prev_error_mode

def CleanupSemaphores():
  if (platform.system() == 'OS/S390'):
     os.system("for u in $(ipcs -s | grep `whoami` | tr -s ' ' | cut -d ' ' -f2);"
             "do ipcrm -s $u; done")

def RunProcess(verbose, timeout, args, **rest):
  if verbose: print "#", " ".join(args)
  popen_args = args
  prev_error_mode = SEM_INVALID_VALUE
  if utils.IsWindows():
    popen_args = subprocess.list2cmdline(args)
    # Try to change the error mode to avoid dialogs on fatal errors. Don't
    # touch any existing error mode flags by merging the existing error mode.
    # See http://blogs.msdn.com/oldnewthing/archive/2004/07/27/198410.aspx.
    error_mode = SEM_NOGPFAULTERRORBOX
    prev_error_mode = Win32SetErrorMode(error_mode)
    Win32SetErrorMode(error_mode | prev_error_mode)

  try:
    process = subprocess.Popen(
      args=popen_args,
      stdout=subprocess.PIPE,
      stderr=subprocess.PIPE,
      **rest
    )
  except Exception as e:
    sys.stderr.write("Error executing: %s\n" % popen_args)
    raise e

  if (utils.IsWindows() and prev_error_mode != SEM_INVALID_VALUE):
    Win32SetErrorMode(prev_error_mode)

  def kill_process(process):
    try:
      if utils.IsWindows():
        if verbose:
          print "Attempting to kill process %d" % process.pid
          sys.stdout.flush()
        tk = subprocess.Popen(
            'taskkill /T /F /PID %d' % process.pid,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        stdout, stderr = tk.communicate()
        if verbose:
          print "Taskkill results for %d" % process.pid
          print stdout
          print stderr
          print "Return code: %d" % tk.returncode
          sys.stdout.flush()
      else:
        process.kill()
    except OSError:
      sys.stderr.write('Error: Process %s already ended.\n' % process.pid)

  MAX_SLEEP_TIME = 0.1
  INITIAL_SLEEP_TIME = 0.0001
  SLEEP_TIME_FACTOR = 1.25

  try:
    if timeout is None: end_time = None
    else: end_time = time.time() + timeout
    timed_out = False

    (stdout, stderr) = process.communicate()

    # Repeatedly check the exit code from the process in a
    # loop and keep track of whether or not it times out.
    exit_code = None
    sleep_time = INITIAL_SLEEP_TIME
    while exit_code is None:
      if (not end_time is None) and (time.time() >= end_time):
        # Kill the process and wait for it to exit.
        kill_process(process)
        exit_code = process.wait()
        timed_out = True
      else:
        exit_code = process.poll()
        time.sleep(sleep_time)
        sleep_time = sleep_time * SLEEP_TIME_FACTOR
        if sleep_time > MAX_SLEEP_TIME:
          sleep_time = MAX_SLEEP_TIME
  except Exception as e:
    print "Exception: " + str(e)

  CleanupSemaphores()
  return output.Output(
      process.returncode,
      timed_out,
      stdout,
      stderr,
      process.pid,
  )


def Execute(args, verbose=False, timeout=None):
  args = [ c for c in args if c != "" ]
  return RunProcess(verbose, timeout, args=args)

