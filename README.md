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

        fetch v8

This will checkout V8 into the directory `v8` and fetch all of its dependencies.
To stay up to date, run

        git pull origin
        gclient sync

For fetching all branches, add the following into your remote
configuration in `.git/config`:

        fetch = +refs/branch-heads/*:refs/remotes/branch-heads/*
        fetch = +refs/tags/*:refs/tags/*


Contributing
=============

Please follow the instructions mentioned on the
[V8 wiki](https://github.com/v8/v8/wiki/Contributing).

S390 Specific Instructions
=============

To build a driver:

        make s390 i18nsupport=off     (31-bit)
        make s390x i18nsupport=off    (64-bit)

To run the V8 tests:

        tools/run-tests.py --progress=dots --noi18n --no-presubmit --arch-and-mode=s390.release --junitout v8tests-junit.xml     (31-bit)
        tools/run-tests.py --progress=dots --noi18n --no-presubmit --arch-and-mode=s390x.release --junitout v8tests-junit.xml    (64-bit)

