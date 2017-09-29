V8 JavaScript Engine
=============

V8 is Google's open source JavaScript engine.

V8 implements ECMAScript as specified in ECMA-262.

V8 is written in C++ and is used in Google Chrome, the open source
browser from Google.

V8 can run standalone, or can be embedded into any C++ application.

V8 Project page: https://github.com/v8/v8/wiki


Getting the Code
=============

Checkout [depot tools](http://www.chromium.org/developers/how-tos/install-depot-tools), and run

        git clone https://github.com/ibmruntimes/v8z -b 5.1-lkgr.zos v8
        cp v8/gclient_config .gclient
        gclient sync

For fetching all branches, add the following into your remote
configuration in `.git/config`:

        fetch = +refs/branch-heads/*:refs/remotes/branch-heads/*
        fetch = +refs/tags/*:refs/tags/*


Configuring your z/OS build environment
================
 #GMAKE SETUP#
 export PATH=<PATH TO GMAKE>:$PATH
 
 #PYTHON SETUP#
 export PYTHONDIR=<path to folder where python is installed>
 export PYTHONHOME=$PYTHONDIR
 export PYTHONPATH=$PYTHONDIR/lib/python2.7 
 export LIBPATH=$PYTHONDIR/lib:$LIBPATH
 
 #BUILD COMPILER SETUP#
 export PATH=<path to C/C++ compiler>:$PATH
 export CXX=njsc++ (when using the compiler packaged with z/OS Node beta)
 export CXX_host=$CXX
 export LINK=$CXX

 #GYP CONFIG#
 export GYP_DEFINES="OS=zos target_arch=s390x v8_target_arch=s390x"

Building and Testing
=============
cd v8
make s390x.debug i18nsupport=off snapshot=off -j9 (DEBUG build)
make s390x.release i18nsupport=off snapshot=off -j9 (RELEASE build)

For testing release build

python tools/run-tests.py -j1 --arch-and-mode=s390x.release \
    --no-presubmit --no-i18n --no-snap --no-variants --nonetwork \
        --junitout $WORKSPACE/v8a.xml "mjsunit" 2>&1


Contributing
=============

Please follow the instructions mentioned on the
[V8 wiki](https://github.com/v8/v8/wiki/Contributing).
