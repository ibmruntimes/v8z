v8z
=====

Port of Google V8 javascript engine to IBM System Z - zLinux. Port based on v8ppc


Compile code:<br><code>
make dependencies; make -j8 s390
</code>

Test code:<br><code>
tools/run-tests.py -j 12 --progress=dots --no-presubmit --arch-and-mode=s390.debug --junitout v8tests-junit.xml
</code>
