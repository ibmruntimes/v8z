v8z
=====

Port of Google V8 javascript engine to IBM's Linux on System Z. Port based on v8ppc.


Compile code:<br/>
31-bit:<br/>
<code>
make dependencies; make s390
</code>
<br/>
64-bit:<br/>
<code>
make dependencies; make s390x
</code>

Test code:<br><code>
tools/run-tests.py --progress=dots --no-presubmit --arch-and-mode=s390.debug --junitout v8tests-junit.xml
</code>
