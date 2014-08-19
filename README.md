v8z
=====

Port of Google V8 javascript engine to Linux on IBM System Z.

Compile code:
31-bit:
<code>
make dependencies; make s390
</code>
64-bit:
<code>
make dependencies; make s390x
</code>

Test code:
<code>
tools/run-tests.py -j 12 --progress=dots --no-presubmit --arch-and-mode=s390.debug --junitout v8tests-junit.xml
tools/run-tests.py -j 12 --progress=dots --no-presubmit --arch-and-mode=s390x.debug --junitout v8tests-junit.xml
</code>
