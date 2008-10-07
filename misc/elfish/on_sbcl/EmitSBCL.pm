
class EmitSBCL {

  method new_emitter($ignore,$compiler,$ignore2,$filename) {
    self.new('compiler',$compiler,'filename',$filename);
  }

  has $.compiler;
  has $.filename;

  method tidy($source) {
    if $source.re_matchp('^CompUnit\(') { private_tidy($source) }
    else { $source }
  }

  method prelude_lexical () {
    "";
  }

  method prelude_oo () {
   '';
  }
  method prelude ($n) {
  '#|
#fasl=`dirname $0`/`basename $0 .lisp`.fasl
#[ $fasl -ot $0 ] && sbcl --noinform --eval "(compile-file \"$0\")" --eval "(quit)"
#exec sbcl --noinform --load $fasl --end-toplevel-options "$@"
exec sbcl --noinform --load $0 --eval "(quit)" --end-toplevel-options "$@"
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require \'asdf)
  (require \'sb-posix)
)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew #p"lib-cl/systems/" asdf:*central-registry*)
  (asdf:operate \'asdf:load-op :cl-ppcre)
)

(declaim (optimize (debug 2)))

;;------------------------------------------------------------------------------
;; Generic-functions without congruence restrictions.
;; http://www.lispworks.com/documentation/HyperSpec/Body/07_fd.htm

(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar *maximum-number-of-dispatch-affecting-variables* 10)

(defun n-gensyms (n) (loop for i from 1 to n collect (gensym)))
)

(defmacro ncgf-funcall (func &rest args)
  (let* ((n (1+ *maximum-number-of-dispatch-affecting-variables*))
         (len (length args))
         (syms (n-gensyms len))
         (dispatch-syms (subseq syms 0 (min n len)))
         (dispatch-padding (make-list (max 0 (- n len))))
         (f (gensym)))
   `(let ((,f ,func))
      (multiple-value-bind (,@syms) (values ,@args)
        (if (not (typep ,f \'standard-generic-function))
            (funcall ,f ,@syms)
          (funcall ,f (list ,@syms) ,@dispatch-syms ,@dispatch-padding))))))

(defgeneric ncgf-apply (func args))
(defmethod ncgf-apply (func args)
  (apply func args))
(defmethod ncgf-apply ((func standard-generic-function) args)
  (let* ((n (1+ *maximum-number-of-dispatch-affecting-variables*))
         (len (length args))
         (pad-args (make-list (max 0 (- n len))))
         (real-args (subseq args 0 (min n len)))
         (dispatch-args (concatenate \'list real-args pad-args)))
    (apply func (cons args dispatch-args))))
         
(defmacro ncgf-defgeneric (name sig)
  (declare (ignore sig))
  (let* ((n (1+ *maximum-number-of-dispatch-affecting-variables*))
         (vars (n-gensyms n)))
    `(defgeneric ,name (args ,@vars))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun parameters-in-lambda-list (sig)
  (let ((not-param
         (lambda(e) (case e
                          (&optional t)
                          (&rest t)
                          ))))
    (remove-if not-param sig))) ;X should stop at &aux, etc.
)

(defmacro ncgf-defmethod (name sig &rest body)
  (let* ((n (1+ *maximum-number-of-dispatch-affecting-variables*))
         (n-1 (1- n))
         (vars (parameters-in-lambda-list sig))
         (len (length vars))
         (real-vars (subseq vars 0 (min n-1 len)))
         (bounds-var (list (if (find \'&rest sig)
                               (gensym)
                             `(,(gensym) ,(class-of nil)))))
         (pad-vars (n-gensyms (max 0 (- n-1 len))))
         (dispatch-vars (concatenate \'list real-vars bounds-var pad-vars))
         (typeless-sig (map \'list (lambda (p) (if (listp p) (car p) p)) sig))
         (def `(defmethod ,name (args ,@dispatch-vars)
                 (declare (ignore ,@pad-vars))
                 (destructuring-bind ,typeless-sig args
                                     ,@body))))
    def))


;;------------------------------------------------------------------------------
;; Classes

(defmacro pkg-init-flag-name (pkg) `(concatenate \'string ,pkg "/initialized"))
(defmacro pkg-clsname (pkg) `(find-symbol (concatenate \'string ,pkg "/cls")))
(defmacro pkg-co (pkg) `(find-symbol (concatenate \'string ,pkg "::/co")))
(defmacro pkg-super (pkg) `(find-symbol (concatenate \'string ,pkg "::/super")))
(defmacro pkg-slots (pkg) `(find-symbol (concatenate \'string ,pkg "::/slots")))
  
(defun cls-sync-definition (pkg)
  (let ((def 
   `(defclass
     ,(pkg-clsname pkg)
     ,(symbol-value (pkg-super pkg))
     ,(symbol-value (pkg-slots pkg)))))
  (eval def)))

(defun pkg-declare (kind pkg base)
  (unless (find-symbol (pkg-init-flag-name pkg))
    (set (intern (pkg-init-flag-name pkg)) t)
    (when (equal kind "class")
      (intern (concatenate \'string pkg "/cls"))
      (set (intern (concatenate \'string pkg "::/super")) (if base (list base) nil))
      (set (intern (concatenate \'string pkg "::/slots")) nil)
      (cls-sync-definition pkg)
      (set (intern (concatenate \'string pkg "::/co"))
        (make-instance (pkg-clsname pkg)))
      (eval `(dm |M::WHAT| ((cls ,(pkg-clsname pkg))) (declare(ignorable cls)) (UP ,pkg)))
      )))

(defun cls-has (pkg new-slot-specifier)
  (let ((slots-symbol (pkg-slots pkg)))
    (set slots-symbol (nconc (symbol-value slots-symbol) (list new-slot-specifier)))
    (cls-sync-definition pkg)))

(defun cls-is (pkg new-super-pkg)
  (let ((pkg-super-symbol (pkg-super pkg))
        (new-super-name (pkg-clsname new-super-pkg)))
    (assert pkg-super-symbol)
    (assert new-super-name)
    (if (not (equal new-super-pkg "Any"))
      (set pkg-super-symbol (cons new-super-name (symbol-value pkg-super-symbol)))
      (cls-sync-definition pkg))))

;;------------------------------------------------------------------------------
;; utility functions

(defun size-n-partition (n seq)
    (do ((parts nil)
         (lst (coerce seq \'list)))
        ((not lst) (reverse parts))
        (let ((part (subseq lst 0 n))
              (rest (subseq lst n)))
          (setq parts (cons part parts))
          (setq lst rest))))

(defun wrapped-index (len k)
  (cond ((and (<= 0 k) (< k len)) k)
        ((> 0 k) (let ((k1 (+ len k)))
                   (if (<= 0 k1) k1 nil)))
        (t nil)))

;;------------------------------------------------------------------------------
;; fake containers

(defmacro rw-v (c)
  `(cadr ,c))

(defmacro rw (c)
  (let ((cv (gensym)))
    `(let ((,cv ,c))
       (if (and (listp ,cv) ,cv (eq (car ,cv) :rw-flag)) (rw-v ,cv) ,cv))))

(defsetf rw (c) (v)
  (let ((cv (gensym)))
    `(let ((,cv ,c))
       (ap (caddr ,cv) (concatenate \'list (cdddr ,cv) (list ,v))))))

(defmacro rw-able (v f &rest args) `(list :rw-flag ,v ,f ,@args))

;;------------------------------------------------------------------------------
;; can

(defmacro def-can (name sig)
  (let* ((arg0-type (cond ((not (listp (car sig))) t)
                          (t (cadar sig))))
         (sym (intern (concatenate \'string "CAN::" (symbol-name name)))))
    `(defmethod ,sym ((x ,arg0-type)) nil)))

(defun can-p (name obj)
  (let* ((sym (intern (concatenate \'string "CAN::" name)))
         (gf (symbol-function sym))
         (argl (list obj)))
    (if (not gf) nil
      (if (compute-applicable-methods gf argl)
          t nil))))

;;------------------------------------------------------------------------------

(defmacro fc (func &rest args)
  `(rw (ncgf-funcall ,func ,@args)))

(defmacro fc-preserving-rw (func &rest args)
  `(ncgf-funcall ,func ,@args))

(defmacro ap (func args)
  `(ncgf-apply ,func ,args))

(defmacro dm-without-can (name sig &rest body)
  `(ncgf-defmethod ,name ,sig ,@body))

(defmacro dm (name sig &rest body)
  `(progn
     (def-can ,name ,sig)
     (ncgf-defmethod ,name ,sig ,@body)))

;;------------------------------------------------------------------------------
;; Prelude & stuff

;; abbreviations
(defgeneric UP (x))
(defmacro S (x) `(to-s ,x))
(defmacro N (x) `(to-n ,x))
(defgeneric to-s (x))
(defgeneric to-n (x))

;; predecls
(defclass |Any/cls| () ())
(defclass |Bool/cls| () ())
(defclass |Int/cls| () ())
(defclass |Num/cls| () ())
(defclass |Str/cls| () ())

;;
(defun set-slots (o argl)
  (let* ((clsname (symbol-name (class-name (class-of o))))
         (realname (subseq clsname 0 (- (length clsname) (length "/cls")))))
    (mapcar (lambda (kv)
              (let* ((k (S (car kv)))
                     (v (cadr kv))
                     (sym (find-symbol (concatenate \'string realname "::." k))))
                (setf (slot-value o sym) v)))
            (size-n-partition 2 argl)))
  o)

;;
(defun flatten-lists (args)
  (reduce #\'append
          (mapcar (lambda (e) (if (and (listp e) (not (null e)))
                                  e (list e)))
                  args)))

;;
(dm |M::new| ((co |Any/cls|) &rest argl)
  (declare (ignorable argl))
  (set-slots (make-instance (class-of co)) argl))

(dm |M::can| (self |$method_name|)
    (let* ((name (concatenate \'string "M::" (S |$method_name|))))
      (UP (can-p name self))))

;; Undef is still being kludged as nil.  And Bools.
(defmacro undef () nil)
(dm |M::make_ir_from_Match_tree| ((self null) ) (block __f__ (let () self)))
(dm |M::isa| ((x null) str) (declare (ignorable x)) (equal (S str) "Undef"))
(dm |M::Str| ((x symbol)) (UP (symbol-name x))) ;X for t

 ;;Array.new is defined here to a avoid cyclic dependency on *@args.
(pkg-declare "class" "Array" \'|Any/cls|)
(dm |M::new| ((co |Array/cls|) &rest argl)
  (declare (ignorable co))
  (let ((inst (make-instance \'|Array/cls|)))
    (setf (slot-value inst \'|Array::._native_|)
          (make-array (length argl) :adjustable t :initial-contents argl))
    inst))

;; Hack until Str, Int, Num, etc are p6 objects.
(dm |M::Str| ((s string) &rest argl) (declare (ignorable argl)) s)
(dm |M::Str| ((n number) &rest argl) (declare (ignorable argl)) (write-to-string n))
(dm |M::WHAT| ((s string) &rest argl) (declare (ignorable s argl)) "str")
(dm |M::WHAT| ((n number) &rest argl) (declare (ignorable n argl)) "num")

(dm |M::Str| ((x null) &rest argl) (declare (ignorable x argl)) "")
(dm |M::WHAT| ((x null) &rest argl) (declare (ignorable x argl)) "nil")
(dm |M::substr| ((s string) from len) (subseq s from (+ from len)))


;; Muffle warnings at compile and runtimes.
;(declaim (sb-ext:muffle-conditions style-warning))
(declaim (sb-ext:muffle-conditions warning))

;(defparameter sb-ext:*muffled-warnings* style-warning) ;In sbcl-1.0.20 .
(if (find-symbol "sb-ext:*muffled-warnings*") ;In sbcl-1.0.20
;  (eval(read-from-string "(defparameter sb-ext:*muffled-warnings* style-warning)"))
 nil) 

;; UP
;(defgeneric UP (x))
(defmethod UP ((x null)) nil)
(defmethod UP ((x symbol)) x)
(defmethod UP ((x integer)) (fc #\'|M::new| |Int::/co| x))
(defmethod UP ((x number)) (fc #\'|M::new| |Num::/co| x))
(defmethod UP ((x string)) (fc #\'|M::new| |Str::/co| x))
(defmethod UP ((bug |Str/cls|)) (write (S bug)) (die))
;; new-
(defun new-Array (lst) (ap #\'|M::new| (cons |Array::/co| lst)))
(defun new-Hash  (lst) (ap #\'|M::new| (cons |Hash::/co| lst)))
(defun new-Pair  (k v) (fc #\'|M::new| |Pair::/co| k v))
;; to-b
(defgeneric to-b (x))
(defmethod to-b (x) t)
(defmethod to-b ((x null)) nil)
(defmethod to-b ((x symbol)) t) ;for t. #X should check.
(defmethod to-b ((x string)) (if (equal x "") nil t))
(defmethod to-b ((x number)) (if (= x 0) nil t))
(defmethod to-b ((x |Bool/cls|)) (slot-value x \'|Bool::._native_|))
(defmethod to-b ((x |Any/cls|)) (to-b (fc #\'|M::Bool| x)))
;; to-n
(defgeneric to-n (x))
(defmethod to-s ((x null)) 0) ;X unboxed Undef
(defmethod to-n ((x number)) x)
(defmethod to-n ((x |Int/cls|)) (slot-value x \'|Int::._native_|))
(defmethod to-n ((x |Num/cls|)) (slot-value x \'|Num::._native_|))
(defmethod to-n ((x |Any/cls|)) (to-n (fc #\'|M::Num| x)))
;; to-s
(defgeneric to-s (x))
(defmethod to-s ((x null)) "") ;X unboxed Undef
(defmethod to-s ((x string)) x)
(defmethod to-s ((x |Str/cls|)) (slot-value x \'|Str::._native_|))
(defmethod to-s ((x |Any/cls|)) (to-s (fc #\'|M::Str| x)))

(defgeneric cl-hash (x))
(defmethod cl-hash (x) x)
(defmethod cl-hash ((x |Str/cls|)) (slot-value x \'|Str::._native_|))
(defmethod cl-hash ((x |Int/cls|)) (slot-value x \'|Int::._native_|))
(defmethod cl-hash ((x |Num/cls|)) (slot-value x \'|Num::._native_|))

;; regexp elf bootstrap primitives

; CL-CPCRE doesn\'t do $1, etc.
(defun parse-re-replacement (rep) ; XXX kludge
  (let* ((pat "(?:[^\\\\\\\\$]|\\\\\\\\.|.\\\\z|\\\\$[^{1])+|\\\\$1|\\\\$\\\\{1}")
         (parts (ppcre::all-matches-as-strings pat rep)))
    (write rep)(write parts)
    (assert (equal (length rep)
                   (length (apply #\'concatenate (cons \'string parts)))))
    (mapcar
     (lambda (part)
       (cond ((equal part "$1") 0)
             ((equal part "${1}") 0)
             (t part)))
     parts)))

;; short-circuiting logicals
(defmacro or6 (&rest args)
  (if (null args)
      \'nil
    (let ((sym (gensym)))
      `(let ((,sym ,(car args))) (if (to-b ,sym) ,sym (or6 ,@(cdr args)))))))
(defmacro and6 (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t (let ((sym (gensym)))
             `(let ((,sym ,(car args))) (if (not (to-b ,sym)) nil (and6 ,@(cdr args))))
             ))))
   

;;------------------------------------------------------------------------------
';
  }

  method e($x) {
    my $ref = $x.WHAT;
    if $ref eq 'Undef' { $x }
    elsif $ref eq 'Str' || $ref eq 'Int' || $ref eq 'Num' { $x }
    elsif $ref eq 'Array' { $x.map(sub($ae){$.e($ae)}) }
    else {$x.callback(self)}
  }


  method cb__CompUnit ($n) {
    $n.do_all_analysis();
    my $decls = $n.notes<lexical_variable_decls>;
    my $code = "";
    my $lexicals = "";
    my $lexicals_foot = "";
    $decls.map(sub($d){if $d.scope eq 'my' {
      $lexicals = $lexicals ~ $.qsym($d.sigil~$d.name)~" ";
    }});
    if $lexicals {
      $code = $code~"(let (\n"~$lexicals ~")\n";
      $lexicals_foot = ")";
    }
    temp $whiteboard::in_package = [];
    temp $whiteboard::emit_pairs_inline = 0;
    temp $whiteboard::declares = [];
    temp $whiteboard::compunit_header = [];
    temp $whiteboard::block_header = [];
    temp $whiteboard::compunit_footer = [];
    my $stmts = $.e($n.statements).join("\n");
    my $declare = $whiteboard::declares.join("");
    my $head = $whiteboard::compunit_header.join("\n")~"\n\n";
    my $foot = $whiteboard::compunit_footer.join("\n");
    my $blk_head = $whiteboard::block_header.join("");
    $code ~ $declare ~ $blk_head ~ $head ~ $stmts ~$foot~"\n"~$lexicals_foot~"\n";
  }
  method cb__Block ($n) {
    temp $whiteboard::emit_pairs_inline = 0;
    temp $whiteboard::declares = [];
    temp $whiteboard::block_header = [];
    my $decls = $n.notes<lexical_variable_decls>;
    my $code = "";
    my $lexicals = "";
    my $lexicals_foot = "";
    $decls.map(sub($d){
       my $scope = $d.scope;
       if $scope eq 'my' {
         $lexicals = $lexicals ~ $.e($d.var)~" ";
       }
       elsif $scope eq 'temp' {
         my $v = $.e($d.var);
         $lexicals = $lexicals ~ "("~$v~" (if (boundp '"~$v~") "~$v~")) ";
       }
    });
    if $lexicals {
      $code = $code~"(let (\n"~$lexicals ~")\n";
      $lexicals_foot = ")";
    }
    my $stmts = $.e($n.statements).join("\n");
    my $declare = $whiteboard::declares.join("");
    my $blk_head = $whiteboard::block_header.join("");
    $code~$declare~$blk_head~$stmts~$lexicals_foot;
  }

  method cb__Use ($n) {
    my $module = $.e($n.module_name);
    my $expr = $.e($n.expr);
    if $module eq 'v6-alpha' { "" }
    elsif $module eq 'v6' { "" }
    elsif $module eq 'lib' {
      my $name = $n.expr.buf;
      if $.compiler.hook_for_use_lib($name) { "" }
      else { "" }
    }
    elsif $.compiler.hook_for_use($module,$expr) { "" }
    else {
      "use " ~$module;
    }
  }
  method cb__ClosureTrait ($n) {
    temp $whiteboard::emit_pairs_inline = 0;
    $n.kind~'{'~$.e($n.block)~'}'
  }

  method cb__PackageDecl ($n) {
    my $in_package = [$whiteboard::in_package.flatten,$n.name];
    my $kind = $n.kind;
    my $name = $in_package.join('::');
    my $base = "'"~$.classname_from_package_name("Any");
    if $name eq 'Any' { $base = 'nil' }
    if $name eq 'Object' { $base = 'nil' }
    if $name eq 'Junction' { $base = 'nil' }
    my $head = "\n(pkg-declare \""~$kind~"\" \""~$name~"\" "~$base~")\n";
    my $foot = "\n";
    if $n.block {
      temp $whiteboard::in_package = $in_package; # my()
      my $decl = $head ~ $.e($n.traits||[]).join("\n");
      $whiteboard::compunit_header.push($decl);
      $.e($n.block) ~ $foot;
    } else {
      $whiteboard::in_package = $in_package; # not my()
      my $decl = $head ~ $.e($n.traits||[]).join("\n");
      $whiteboard::compunit_header.push($decl);
      $whiteboard::compunit_footer.unshift($foot);
      "\n"
    }
  }
  method cb__Trait ($n) {
    if ($n.verb eq 'is' or $n.verb eq 'does') {
      my $pkgname = $whiteboard::in_package.join('::');
      my $super = $whiteboard::in_package.splice(0,-1).join('::');
      if $super { $super = $super ~'::' }
      $super = $super ~ $.e($n.expr);
      "\n(cls-is "~$.qstr($pkgname)~" "~$.qstr($super)~")\n"
    } else {
      say "ERROR: Emitting p5 for Trait verb "~$n.verb~" has not been implemented.\n";
      "***Trait***"
    }
  }

  method emit_array ($contents) {
    "(fc #'|M::new| |Array::/co| "~$contents~')'
  }
  method emit_hash ($contents) {
    "(fc #'|M::new| |Hash::/co| "~$contents~')'
  }

  method cb__VarDecl ($n) {
    temp $whiteboard::emit_pairs_inline = 0;
    my $scope = $n.scope;
    my $sigil = $n.var.sigil;

    my $default_expr = $.e($n.default_expr);
    my $default = "";
    if $n.default_expr {
      if (not($n.var.sigil eq '$') &&
          $n.default_expr.isa('IRx1::Apply') &&
          ($n.default_expr.function eq 'circumfix:( )' ||
           $n.default_expr.function eq 'infix:,'))
      {
        temp $whiteboard::emit_pairs_inline = 1;
        my $default_expr = $.e($n.default_expr);
        if $n.is_array { $default = $.emit_array($default_expr) }
        elsif $n.is_hash { $default = $.emit_hash($default_expr) }
        else { $default = $default_expr; }
      }
      else {
        $default = $default_expr;
      }
    }
    else {
      if ($sigil eq '$') { $default = 'nil' }#X
      if ($sigil eq '@') { $default = $.emit_array('') }
      if ($sigil eq '%') { $default = $.emit_hash('') }
    }

    if $scope eq 'has' {
      my $pkg = $n.var.notes<crnt_package>;
      my $name = $.e($n.var.name);
      my $cls = $.classname_from_package_name($pkg);
      my $slotname = '|'~$pkg~'::.'~$name~'|';
      my $accname = '|M::'~$name~'|';
      my $code =
        ('(dm '~$accname~' ((self '~$cls~'))'~
         ' (let ((setter (lambda (o v) (setf (slot-value o \''~$slotname~') v))))'~
         '   (rw-able (slot-value self \''~$slotname~') setter self)))'~"\n");
      my $slot_specifier = '('~$slotname;
      if $default {
        $slot_specifier = $slot_specifier ~ " :initform "~$default;
      }
      $slot_specifier = $slot_specifier ~')';
      $code = $code ~ "(cls-has \""~$pkg~"\" '"~$slot_specifier~")\n\n";
      $code;
    }
    elsif $scope eq 'temp' {
      my $evar = $.e($n.var);
      my $evar_d = $evar;
      if $default { $evar_d = '(setq '~$evar~' '~$default~')' }
      $whiteboard::declares.push("(declare (special "~$evar~"))\n");
      my $init = "(unless (boundp '"~$evar~") (setq "~$evar~" nil))\n";
      $whiteboard::block_header.push($init);
      $evar_d;
    }
    else {
      my $code = $.e($n.var);
      if $default { $code = '(setq '~$code~' '~$default~')' }
      $code;
    }
  }


  method cb__MethodDecl ($n) {
    my $body;
    if $n.traits && $n.traits[0].expr && $n.traits[0].expr eq 'cl' {
      $body = $n.block.statements[0].buf;
    }
    else {
      $body = $.e($n.block);
    }
    my $cls = $.classname_from_package_name($n.notes<crnt_package>||'Main');
    my $enc_name = $.qsym('M::'~$.e($n.name));
    my $sig = $.e($n.multisig);
    my $decl = ('(dm '~$enc_name~' ((self '~$cls~') '~$sig~
                #' (let () (declare (ignorable self))'~
                ' (block __f__ '~$body~'))');
    $whiteboard::block_header.push($decl);
    "";
  }
  method classname_from_package_name($pkg) {
    '|'~$pkg~'/cls|';
  }
  method classobject_from_package_name($pkg) {
    '|'~$pkg~'::/co|';
  }

  method cb__SubDecl ($n) {
    temp $whiteboard::emit_pairs_inline = 0;
    my $name = $n.name;
    if $name { $name = $.e($name) } else { $name = "" }
    my $sig = $n.multisig;
    if $sig { $sig = $.e($sig) } else { $sig = "" }
    my $body;
    if $n.traits && $n.traits[0].expr && $n.traits[0].expr eq 'cl' {
      $body = $n.block.statements[0].buf;
    } else {
      $body = $.e($n.block);
    }
    my $most = '('~$sig~' (block __f__ '~$body~')';
    my $decl = "";
    my $code = "";
    if $n.scope && $n.scope eq 'our' {
      my $pkg = $n.notes<crnt_package>;
      my $enc_name = $.qsym($pkg~'::&'~$name);
      if $n.plurality && $n.plurality eq 'multi' {
        my $dm_name = $.qsym('MS::'~$pkg~'::&'~$name);
        $decl = ('(dm-without-can '~$dm_name~' '~$most~')'~"\n"~
                 '(defparameter '~$enc_name~' #\''~$dm_name~')');
      } else {
        $decl = '(defparameter '~$enc_name~' (lambda '~$most~'))';
      }
    }
    elsif $name {
      my $enc_name = $.qsym('&'~$name);
      $decl = '(setq '~$enc_name~' (lambda '~$most~'))';
    }
    else {
      $code = '(lambda '~$most~')';
    }
    $whiteboard::block_header.push($decl);
    $code;
  }
  method cb__Signature ($n) {
    if ($n.parameters.elems == 0) { ")" }
    else {
      temp $whiteboard::signature_inits = "";
      my $pl = $.e($n.parameters).join(" ");
      ''~$pl~")"~$whiteboard::signature_inits~"\n";
    }
  }
  method cb__Parameter ($n) {
    my $enc = $.e($n.param_var);
    my $par = $enc;
    if $n.type_constraints {
      my $typ = $.classname_from_package_name($n.type_constraints[0]);
      $par = '('~$par~' '~$typ~')';
    }
    if $n.quant && $n.quant eq '*' {
      my $init = "\n (setq "~$enc~" (ap #'|M::new| (cons |Array::/co| "~$enc~")))";
      $whiteboard::signature_inits = $whiteboard::signature_inits~$init;
      " &rest "~$enc;
    } else {
      $par;
    }
  }
  method cb__ParamVar ($n) {
    my $s = $n.sigil;
    my $t = '';
    my $dsn = $.e($n.name);
    $.encode_param_varname($s,$t,$dsn);
  }

  method cb__Call ($n) {
    my $g;
    temp $whiteboard::emit_pairs_inline = 0;
    my $method = $.e($n.method);
    my $meth = $.fqsym('M::'~$method);
    my $invocant = $.e($n.invocant);
    if $invocant.re_matchp('^[A-Z][\w:]*$') {
      $invocant = ""~$.classobject_from_package_name($invocant);
    }
    my $e_capture = $.e($n.capture);
    if $g = $method.re_groups('^postcircumfix:(.+)') {
      my $op = $g[0];
      if $op eq '< >' {
        my $s = $n.capture.arguments[0];
        my $words = $s.split('\s+');
        my $self = self;
        $e_capture = $words.map(sub($x){$self.qstr($x)}).join(" ");
      }
    }
    if $n.capture.contains_a_list {
      '(ap '~$meth~' (cons '~$invocant~' (flatten-lists '~$e_capture~')))';
    }
    else {
      '(fc '~$meth~' '~$invocant~' '~$e_capture~')';
    }
  }
  method fqsym ($name) {
     "#'|"~$name.re_gsub('\|','\\|')~'|';
  }
  method qsym ($name) {
     "|"~$name.re_gsub('\|','\\|')~'|';
  }
  method qstr ($str) {
     '"'~$str.re_gsub('\\\\','\\\\').re_gsub('"','\"')~'"'
  }

  method cb__Apply ($n) {
    my $g;
    # temp $whiteboard::emit_pairs_inline = 0; #XXX depends on function :/
    my $fun = $.e($n.function);
    my $e_capture; # avoid exponential time on deeply nested ops.
    if $n.notes<lexical_bindings>{'&'~$fun} {
       my $fe = $.qsym('&'~$fun);
       my $decl = $n.notes<lexical_bindings>{'&'~$fun};
       if $decl.scope eq 'our' {
         $fe = $.qsym($decl.notes<crnt_package>~'::&'~$fun);
       }
       return '(fc '~$fe~' '~$.e($n.capture)~')'
    }
    if $g = $fun.re_groups('^infix:(.+)$') {
      my $op = $g[0];
      my $args = $n.capture.arguments;
      if $args.elems == 1 && $args[0].isa('IRx1::Apply') && $args[0].function eq 'infix:,' {
        $args = $args[0].capture.arguments;
      }
      my $a = $.e($args);
      my $l = $a[0];
      my $r = $a[1];
      if ($op eq ',') {
        my $s = $a.shift;
        while $a.elems { $s = $s ~" "~ $a.shift }
        return $s;
      }
      if ($op eq '=') {
        return '(setf '~$l~' '~$r~')';
      }
      if ($op eq 'or' or $op eq '||') {return '(or6 '~$l~' '~$r~')';}
      if ($op eq 'and' or $op eq '&&') {return '(and6 '~$l~' '~$r~')';}
      $e_capture = $a.join(" ");
    }
    elsif $g = $fun.re_groups('^prefix:(.+)$') {
      #my $op = $g[0];
      #my $a = $.e($n.capture.arguments);
      #my $x = $a[0];
      #if $op eq '?' {return '(('~$x~')?1:0)'}
    }
    elsif $g = $fun.re_groups('^statement_prefix:(.+)$') {
      my $op = $g[0];
      if $op eq 'do' {
        return '(progn '~$.e($n.capture.arguments[0])~')'
      #} elsif $op eq 'try' {
      #  return 'eval{'~$.e($n.capture)~'}'
      #} elsif $op eq 'gather' {
      #  return 'gather'~$.e($n.capture)~''
      } else {
        die $fun~': unimplemented';
      }
    }
    elsif $g = $fun.re_groups('^postfix:(.+)$') {
      my $op = $g[0];
      my $a = $.e($n.capture.arguments);
      my $x = $a[0];
      if $op.re_matchp('^(\+\+)$') {
        return "(setq "~$x~" (UP (1+ (N "~$x~"))))"
      }
      $e_capture = $a.join(" ");
    }
    elsif $g = $fun.re_groups('^circumfix:(.+)') {
      my $op = $g[0];
      if $op eq '< >' {
        my $s = $n.capture.arguments[0];
        my $words = $s.split('\s+');
        my $self = self;
        return $.emit_array($words.map(sub($x){$self.qstr($x)}).join(" "));
      }
    }
    elsif ($fun eq 'self') {
      return 'self'
    }
    elsif ($fun eq 'next') {
      return '(return-from __l__)'
    }
    elsif ($fun eq 'last') {
      return '(return)'
    }
    elsif ($fun eq 'return') {
      return '(return-from __f__ '~$.e($n.capture)~')';
    }
    elsif $fun eq 'eval' {
      my $env = ''; #XXX harder in CL
      return '(fc |GLOBAL::&eval| '~$.e($n.capture)~' '~$env~')'
    }

    if not(defined($e_capture)) { $e_capture = $.e($n.capture) }
    my $pre = '(fc ';
    my $mid = ' ';
    my $post = ')';
    if $n.capture.contains_a_list {
      $pre = '(ap ';
      $mid = ' (flatten-lists ';
      $post = '))';
    }

    if $fun.re_matchp('^\w') {
      my $fe = $.qsym('GLOBAL::&'~$fun);
      return $pre~$fe~$mid~$e_capture~$post;
    }
    if $fun.re_matchp('^[$@%&]') {
       return  $pre~$.qsym($fun)~$mid~$e_capture~$post;
    }
    else {
       return $pre~$fun~$mid~$e_capture~$post;
    }
  }
  method cb__Capture ($n) {
    # temp $whiteboard::emit_pairs_inline = 0; XXX?
    my $a = $.e($n.arguments||[]).join(" ");
    if $n.invocant {
      my $inv = $.e($n.invocant);
      if $a { $inv~" "~$a }
      else { $inv }
    }
    else { $a }
  }

  method cb__For ($n) {
    my $e = $.e($n.expr);
    if $n.expr.WHAT ne 'IRx1::Apply' { $e = '(fc #\'|M::flatten| '~$e~')' };
    my $b = $.e($n.block);
    if $n.block.WHAT eq 'IRx1::SubDecl' { $b = '('~$b~' _)' };
    '(loop for |$_| in '~$e~"\n do (block __l__ \n"~$b~"\n))"
  }
  method cb__Cond ($n) {
    my $els = '';
    if $n.default { $els = "(t \n"~$.e($n.default)~"\n)" }
    my $clauses = $.e($n.clauses);
    my $first = $clauses.shift;
    my $first_test = '(to-b '~$first[0]~')';
    if $n.invert_first_test { $first_test = "(not "~$first_test~")" }
    ('(cond ('~$first_test~"\n"~$first[1]~")\n"
    ~$clauses.map(sub($e){'((to-b '~$e[0]~")\n"~$e[1]~"\n)"}).join("")
    ~$els~")\n")
  }
  method cb__Loop ($n) {
    '(loop while (to-b '~$.e($n.pretest)~") do (block __l__ \n"~$.e($n.block)~"\n))"
  }

  method cb__Var ($n) {
    my $s = $n.sigil;
    my $t = $n.twigil||'';
    if $n.is_context { $t = '+' }
    my $dsn = $.e($n.name);
    my $bare = $n.notes<bare_name>;
    my $v = $s~$t~$dsn;
    if $v eq '$?PACKAGE' || $v eq '$?MODULE' || $v eq '$?CLASS' {
      my $pkgname = $n.notes<crnt_package>;
      '"'~$pkgname~'"'
    } elsif $v eq '$?FILE' {
      '"'~$.filename~'"'
    } elsif $v eq '$?LINE' {
      "0" # XXX $n notes needs to provide this.
    } elsif $v eq '$?PERLVER' {
      '"elf / '~ primitive_runtime_version() ~ " / " ~ $.WHAT ~'"'
    } else {
      my $t1 = $t;
      my $decl = $n.notes<decl>;
      my $path = "";
      my $vpkg = $n.package;
      if $vpkg { $path = $vpkg~"::" }
      if $t eq '*' { $path = 'GLOBAL::'~$path; $t1 = ""; }
      if not($path) && ($n.is_temp || ($decl && $decl.scope eq 'our')) {
         my $pkg = $decl.notes<crnt_package>;
         $path = $pkg~"::";
      }
      if $t eq '.' {
        "(fc #'"~$.qsym("M::"~$bare)~" self)";
      }
      else {
        $.qsym($path~$s~$t1~$bare);
      }
    }
  }
  method encode_param_varname($s,$t,$dsn) {
    $.qsym($s~$t~$dsn);
  }


  method cb__NumInt ($n) {
    '(UP '~$.e($n.text)~')'
  }
  method cb__Hash ($n) {
    temp $whiteboard::emit_pairs_inline = 1;
    '(fc #\'|M::new| |Hash::/co| '~$.e($n.hash||[]).join(" ")~')'
  }
  method cb__Buf ($n) {
    my $s = $n.buf;
    '(UP '~$.qstr($s)~')';
  }
  method cb__Rx ($n) {
    my $pat = $n.pat || '';
    'qr/'~$pat~'/'
  }
  method cb__Pair($n) {
    if $whiteboard::emit_pairs_inline {
      temp $whiteboard::emit_pairs_inline = 0;
      ' '~$.e($n.key)~' '~$.e($n.value)~' '
    } else {
       '(new-Pair '~$.e($n.key)~' '~$.e($n.value)~")"
    }
  }

};

if not($*emitter0) { $*emitter0 = EmitSBCL.new}
$*emitter1 = EmitSBCL.new;
