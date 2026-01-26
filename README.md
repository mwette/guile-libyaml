# guile-libyaml
simple yaml module using ffi-helper from www.nongnu.org/nyacc

This version 3.0.0 requires nyacc 3.0 or better.

Build
    install nyacc (https://github.com/mwette/nyacc)
    guild compile-ffi ffi/libyaml.ffi

Run the test via

    guile demo1.scm

# Dependencies
See guix.scm for dependencies.

Install them on Guix via 

    guix environment -l guix.scm
