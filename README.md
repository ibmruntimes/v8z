V8 JavaScript Engine
=============

V8 is Google's open source JavaScript engine.

V8 implements ECMAScript as specified in ECMA-262.

V8 is written in C++ and is used in Google Chrome, the open source
browser from Google.

V8 can run standalone, or can be embedded into any C++ application.

V8 Project page: https://code.google.com/p/v8/


Getting the Code
=============

Checkout [depot tools](http://www.chromium.org/developers/how-tos/install-depot-tools), and run

        fetch v8

This will checkout V8 into the directory `v8` and fetch all of its dependencies.
To stay up to date, run

        git pull origin
        gclient sync

For fetching all branches, add the following into your remote
configuration in `.git/config`:

        fetch = +refs/branch-heads/*:refs/remotes/branch-heads/*
        fetch = +refs/tags/*:refs/tags/*

+++++++++++++++
S390 Specific:
Make code:
<code>
make -j4 s390 i18nsupport=off V=1
make -j4 s390x i18nsupport=off V=1
</code>

Test code:
<code>
tools/run-tests.py -j 12 --progress=dots --noi18n --no-presubmit --arch-and-mode=s390.debug --junitout v8tests-junit.xml
tools/run-tests.py -j 12 --progress=dots --noi18n --no-presubmit --arch-and-mode=s390x.debug --junitout v8tests-junit.xml
</code>

