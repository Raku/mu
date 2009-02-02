#|
exec sbcl --noinform --load $0 --eval "(quit)" --end-toplevel-options "$@"
|#

(require 'asdf)
#+sbcl  (pushnew #p"lib-cl/for-sbcl/systems/"  asdf:*central-registry*)
#+clisp (pushnew #p"lib-cl/for-clisp/systems/" asdf:*central-registry*)
#+ccl   (pushnew #p"lib-cl/for-ccl/systems/"   asdf:*central-registry*)
(asdf:operate 'asdf:compile-op :trivial-gray-streams)
(asdf:operate 'asdf:compile-op :flexi-streams)
(asdf:operate 'asdf:compile-op :cl-ppcre)
(asdf:operate 'asdf:compile-op :cl-unicode)
(asdf:operate 'asdf:compile-op :cl-interpol)
