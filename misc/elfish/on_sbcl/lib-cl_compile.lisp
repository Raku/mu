#|
exec sbcl --noinform --load $0 --eval "(quit)" --end-toplevel-options "$@"
|#

(require 'asdf)
(pushnew #p"lib-cl/systems/" asdf:*central-registry*)
(asdf:operate 'asdf:compile-op :trivial-gray-streams)
(asdf:operate 'asdf:compile-op :flexi-streams)
(asdf:operate 'asdf:compile-op :cl-ppcre)
(asdf:operate 'asdf:compile-op :cl-unicode)
(asdf:operate 'asdf:compile-op :cl-interpol)
