// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// On z/OS, this test needs a stack size of at least 1.02 mBytes.
// Flags: --allow-natives-syntax --stack-size=1020

function g() {}

var count = 0;
function f() {
  try {
    f();
  } catch(e) {
    print(e.stack);
  }
  if (count < 100) {
    count++;
    %DebugGetLoadedScripts();
  }
}
f();
g();
