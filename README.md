v8z
=====

Port of Google V8 javascript engine to Linux on IBM System Z.

## Compile code
### 31-bit
<code>make dependencies; make s390</code>
### 64-bit
<code>make dependencies; make s390x</code>

### z/OS
#### Do this part on a Linux on Z machine since there is no git/svn on z/OS.
<code>git clone this_repo
cd v8z
git checkout ibm-3.28-zos
make dependencies
cd ..
scp -r v8z user@machine:path
</code>
#### Do this part on the z/OS machine where you copied the repo.
<code>// Ensure that paths to xlc 2.2, Make 4.1 and Python 2.7 are setup.
// Create a debug build targetting z/OS.
export GYP_DEFINES="OS=os390 target_arch=s390x v8_target_arch=s390x"
cd v8z
make s390x.debug -j7 i18nsupport=off snapshot=off
</code>

## Test code
<code>tools/run-tests.py -j 12 --progress=dots --no-presubmit --arch-and-mode=s390.debug --junitout v8tests-junit.xml
tools/run-tests.py -j 12 --progress=dots --no-presubmit --arch-and-mode=s390x.debug --junitout v8tests-junit.xml
</code>

### z/OS
<code>// Use -j1 if multiprocessing is not supported in your z/OS Python port.
python tools/run-tests.py -j1 --no-presubmit --arch-and-mode=s390.debug
</code>
