// Copyright 2017 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// On z/OS, this test needs a stack size of at least 260 kBytes.
// Flags: --harmony-regexp-named-captures --stack-size=260

function call_replace_close_to_stack_overflow() {
  try {
    call_replace_close_to_stack_overflow();
  } catch(e) {
    "b".replace(/(b)/g);
  }
}

call_replace_close_to_stack_overflow();
