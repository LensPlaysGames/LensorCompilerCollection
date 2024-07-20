module Foo;

foo :int() 69;
alias foo "Foo_foo";
;; Can now use from C like `extern ssize_t Foo_foo;`
;; Importing this file from Glint will not make `foo` nor `Foo_foo`
;; available in any way.
