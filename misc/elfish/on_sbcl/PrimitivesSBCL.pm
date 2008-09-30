# This is sort of a hybrid Primitives and Prelude at the moment.

# Class graph
class Bit is Any {}
class Int is Any {}
class Str is Any {}
class Num is Any {}
class Complex is Any {}
class Bool is Any {}
class Code is Any {}
class Block is Code {}
class List is Any {}
class Seq is Any {}
class Range is Any {}
class Set is Any {}
class Bag is Any {}
class Pair is Any {}
class Mapping is Any {}
class Signature is Any {}
class Capture is Any {}
class Blob is Any {}
class Scalar is Any {}
class Array is List {}
class Hash is Any {}
class KeyHash is Any {}
class KeySet is Any {}
class KeyBag is Any {}
class Buf is Any {}
class IO is Any {}
class Routine is Code {}
class Sub is Routine {}
class Method is Routine {}
class Subethod is Routine {}
class Macro is Routine {}
class Regex is Routine {}
class Match is Any {}
class Package is Any {}
class Module is Package {}
class Class is Module {}
class Role is Module {}
class Grammar is Module {}
class Object {}; #XXX does Class 
class Any is Object {}
class Junction is Object {}


package GLOBAL {

  sub say(*@a) { for @a { print $_; }; print "\n";}
  sub print(*@a) { for @a { primitive_print $_.Str }; }
  sub primitive_print ($x) is cl {' (cl:write-string (S |$x|)) '}
  sub primitive_write_to_string ($x) is cl {' (UP (write-to-string |$x|)) '};

  sub undef () is cl {'
    nil ;XX
  '}

  multi infix:<+> ($a,$b) is cl {' (UP (+ (N |$a|) (N |$b|))) '}
  multi infix:<-> ($a,$b) is cl {' (UP (- (N |$a|) (N |$b|))) '}
  multi infix:<*> ($a,$b) is cl {' (UP (* (N |$a|) (N |$b|))) '}
  multi infix:</> ($a,$b) is cl {' (UP (/ (N |$a|) (N |$b|))) '}

  multi infix:<<> ($a,$b) is cl {' (UP (< (N |$a|) (N |$b|))) '}
  multi infix:«>» ($a,$b) is cl {' (UP (> (N |$a|) (N |$b|))) '}
  multi infix:<<=> ($a,$b) is cl {' (UP (<= (N |$a|) (N |$b|))) '}
  multi infix:«>=» ($a,$b) is cl {' (UP (>= (N |$a|) (N |$b|))) '}
  multi infix:<==> ($a,$b) is cl {' (UP (equal (N |$a|) (N |$b|))) '}
  multi infix:«!=» ($a,$b) is cl {' (UP (not (equal (N |$a|) (N |$b|)))) '}
  multi infix:<eq> ($a,$b) is cl {' (UP (equal (S |$a|) (S |$b|))) '}
  multi infix:<ne> ($a,$b) is cl {' (UP (not (equal (S |$a|) (S |$b|)))) '}

  multi infix:<~> ($a,$b) { primitive_strcat($a.Str,$b.Str) }
  multi primitive_strcat ($a,$b) is cl {' (UP (concatenate \'string (S |$a|) (S |$b|))) '}

  multi prefix:<!> ($a) is cl {' (UP (not (to-b |$a|))) '}
  multi prefix:<-> ($a) is cl {' (UP (- 0 (N |$a|))) '}
  multi prefix:<?> ($a) is cl {' (UP (to-b |$a|)) '}

  multi circumfix:«[ ]» (*@a) is cl {' |@a| '}
  multi circumfix:«( )» ($a) is cl {' |$a| '}

  multi slurp ($filename) is cl {'
    (with-open-file (stream (S |$filename|))
      (let ((str (make-string (file-length stream))))
        (read-sequence str stream)
        (UP str)))
  '}
  multi unslurp ($string,$filename) is cl {'
    (with-open-file (stream (S |$filename|) :direction :output :if-exists :supersede)
      (write-sequence (S |$string|) stream))
  '}

  multi exit ($status) is cl {' (sb-unix:unix-exit (N |$status|)) '}
  multi die ($msg) { say $msg; exit(1); }

  multi system ($cmd) is cl {'
    (let ((p (sb-ext:run-program "/bin/sh" (list "-c" (S |$cmd|)) :output t)))
       (sb-ext:process-wait p)
       (UP (sb-ext:process-exit-code p)))
  '}
  multi unlink (*@filenames) { @filenames.map(sub($f){unlink_($f)}) }
  multi unlink_ ($filename) is cl {' (sb-unix:unix-unlink (S |$filename|)) '}
  multi not ($x) { if $x { undef } else { 1 } }
  multi defined ($x) is cl {' (UP (if |$x| 1 nil)) '} #X undef as nil
}

# Elf
package GLOBAL {
  our $compiler0;
  our $compiler1;
  our $parser0;
  our $parser1;
  our $ast2ir_0;
  our $ast2ir_1;
  our $emitter0;
  our $emitter1;
  sub fastundump ($dump_string) is cl {'
    (let ((tree (read-from-string (S |$dump_string|))))
      (labels
       ((undump (node)
           (cond ((listp node)
                  (let ((args (mapcar #\'undump (cdr node))))
                    (ecase (car node)
                           (\'match (ap #\'|M::make_from_rsfth| (cons |Match::/co| args)))
                           (\'array (ap #\'|M::new| (cons |Array::/co| args)))
                           (\'hash  (ap #\'|M::new| (cons |Hash::/co| args))))))
                 (t (UP node)))))
       (undump tree)))
  '}
  sub parser_format () { "cl" }
  sub parser_name () {
    my $e = %*ENV{'ELF_STD_RED_RUN'};
    if $e { $e }
    else {
      # XXX
      # my $f = $0;
      # $f =~ s/[^\/]+$//;
      # # $f."elf_h_src/STD_red/STD_red_run"
      # $f."../STD_red/STD_red_run"
      "../../STD_red/STD_red_run"
    }
  }
}
# regexp elf bootstrap primitives
package Str {
  method re_matchp ($re) is cl {' (UP (ppcre::scan (S |$re|) (S self))) '}
  method re_groups ($re) is cl {'
    (multiple-value-bind (match_str a) (ppcre::scan-to-strings (S |$re|) (S self))
      (declare (ignorable match_str))
      (new-Array (mapcar #\'UP a)))
  '}
  method re_gsub ($re,$replacement_str) is cl {'
    (UP (ppcre::regex-replace-all (S |$re|) (S self) (list (S |$replacement_str|))))
  '}
  method re_gsub_pat ($re,$replacement_pat) is cl {'
     (UP (ppcre::regex-replace-all (S |$re|) (S self)
           (parse-re-replacement (S |$replacement_pat|))))
  '}
}

package Main {
}

class Any {
  method say() { say(self) }
  method print() { say(self) }
}

class Undef {
}

class Pair {
  has $.key; has $.value;
  method new ($k,$v) is cl {'
    (let ((inst (make-instance \'|Pair/cls|)))
      (setf (slot-value inst \'|Pair::.key|) |$k|)
      (setf (slot-value inst \'|Pair::.value|) |$v|)
      inst)
  '}
}

class Int {
  has $._native_;
  method new ($n) is cl {'
    (let ((inst (make-instance \'|Int/cls|)))
      (setf (slot-value inst \'|Int::._native_|) (N |$n|))
      inst)
  '}
}
class Num {
  has $._native_;
  method new ($n) is cl {'
    (let ((inst (make-instance \'|Num/cls|)))
      (setf (slot-value inst \'|Num::._native_|) (N |$n|))
      inst)
  '}
}
class Str {
  has $._native_;
  method new ($s) is cl {'
    (let ((inst (make-instance \'|Str/cls|)))
      (setf (slot-value inst \'|Str::._native_|) (S |$s|))
      inst)
  '}
}

class Array {
  has $._native_;
  method flatten () is cl {'
    (coerce (slot-value self \'|Array::._native_|) \'list)
  '}
  method elems () is cl {'
    (UP (length (slot-value self \'|Array::._native_|)))
  '}
  method push (*@a) is cl {'
    (setf (slot-value self \'|Array::._native_|)
          (concatenate \'vector
            (slot-value self \'|Array::._native_|)
            (fc #\'|M::_native_| |@a|)))
    self
  '}
  method unshift (*@a) is cl {'
    (setf (slot-value self \'|Array::._native_|)
          (concatenate \'vector
            (fc #\'|M::_native_| |@a|)
            (slot-value self \'|Array::._native_|)))
    self
  '}
  method pop () is cl {'
    (let* ((a (slot-value self \'|Array::._native_|))
           (len (length a)))
      (if (> len 0)
        (let ((v (aref a (- len 1))))
          (setf (slot-value self \'|Array::._native_|)
                (subseq a 0 (- len 1)))
          v)
        (fc |M::undef|)))
  '}
  method shift () is cl {'
    (let* ((a (slot-value self \'|Array::._native_|))
           (len (length a)))
      (if (> len 0)
        (let ((v (aref a 0)))
          (setf (slot-value self \'|Array::._native_|)
                (subseq a 1 len))
          v)
        (fc |GLOBAL::&undef|)))
  '}
  method STORE ($k,$v) is cl {'
    (let* ((a (slot-value self \'|Array::._native_|))
           (idx |$k|)) ;XXX no wrapping, expansion, etc.
      (setf (aref a idx) |$v|))
  '}  
  method postcircumfix:<[ ]> ($k) is cl {'
    (let* ((a (slot-value self \'|Array::._native_|))
           (len (length a))
           (idx (wrapped-index len |$k|)))
      (rw-able (if idx (aref a idx) (undef)) #\'|M::STORE| self |$k|))
  '}
  method join ($join_str) is cl {'
    (let* ((a (slot-value self \'|Array::._native_|))
           (len (length a))
           (strs (loop for v across a append (list (S (fc #\'|M::Str| v)) (S |$join_str|)))))
      (UP (apply #\'concatenate (cons \'string (subseq strs 0 (max 0 (1- (* 2 len))))))))
  '}
  method map ($code) is cl {'
    (let* ((a (slot-value self \'|Array::._native_|)))
      (new-Array (loop for v across a collect (fc |$code| v))))
  '}
}

class Hash {
  has $._native_;
  method new (*@a) is cl {'
    (let ((inst (make-instance \'|Hash/cls|))
          (h (make-hash-table :test #\'equal))
          (args (fc #\'|M::_native_| |@a|)))
      (setf (slot-value inst \'|Hash::._native_|) h)
      (loop for kv in (size-n-partition 2 args)
            do (setf (gethash (car kv) h) (cadr kv)))
      inst)
  '}

  method kv () is cl {'
    (let ((h (slot-value self \'|Hash::._native_|)))
      (new-Array (loop for k being the hash-key using (hash-value v) of h
                   append (list k v))))
  '}
  method pairs () is cl {'
    (let ((h (slot-value self \'|Hash::._native_|)))
      (new-Array (loop for k being the hash-key using (hash-value v) of h
                   collect (new-pair k v))))
  '}
  method keys () is cl {'
    (let ((h (slot-value self \'|Hash::._native_|)))
      (new-Array (loop for k being the hash-key of h collect k)))
  '}
  method values () is cl {'
    (let ((h (slot-value self \'|Hash::._native_|)))
      (new-Array (loop for v being the hash-value of h collect v)))
  '}
  method exists ($key) is cl {'
    (let ((h (slot-value self \'|Hash::._native_|)))
      (if (nth-value 1 (gethash |$key| h)) t nil))
  '}
  method delete ($key) is cl {'
    (let ((h (slot-value self \'|Hash::._native_|)))
      (remhash |$key| h))
  '}
  method clear () is cl {'
    (let ((h (slot-value self \'|Hash::._native_|)))
      (clrhash h)
      self)
  '}
  method STORE ($k,$v) is cl {'
    (let ((h (slot-value self \'|Hash::._native_|)))
      (setf (gethash |$k| h) |$v|))
  '}  
  method postcircumfix:<{ }> ($k) is cl {'
    (let ((h (slot-value self \'|Hash::._native_|)))
      (multiple-value-bind (v exists) (gethash |$k| h)
        (rw-able (if exists v (undef)) #\'|M::STORE| self |$k|)))
  '}
  #method postcircumfix:«< >» ($k) { self.{$k} }
  method postcircumfix:«< >» ($k) is cl {'
    (fc #\'|M::postcircumfix:{ }| self |$k|) ;so rw-able isnt lost.
  '}
}

# true
class Any   { method true() { self.Bool }}
# .Bool()
class Any   { method Bool () { 0 == 0 } }
class Bool  { method Bool () { self } }
class Int   { method Bool () { self != 0 } }
class Num   { method Bool () { self != 0 } }
class Str   { method Bool () { self ne "" } }
class Array { method Bool () { self.elems != 0 } }
class Hash  { method Bool () { self.keys.elems != 0 } }

# .Num()
class Int   { method Num () { self } }
class Num   { method Num () { self } }
class Str   { method Num () { self.primitive_Num() } }
class Array { method Num () { self.elems } }
class Hash  { method Num () { self.keys.elems } }
class Pair  { method Num () { 2 } }

# .Str()
class Any   { method Str () { primitive_write_to_string(self) } }
class Int   { method Str () { primitive_write_to_string(self._native_) } }
class Num   { method Str () { primitive_write_to_string(self._native_) } }
class Str   { method Str () { self._native_ } }
class Array { method Str () { self.join('') } }
class Hash  { method Str () { self.keys.map(sub($k){$k~"\t"~self.{$k}}).join("\n") } }
class Pair  { method Str () { $.key~"\t"~$.value } }



package GLOBAL {
  sub _pid is cl {' (UP (sb-posix:getpid)) '}
  our $*PID = _pid();

  our @*INC = ('.');
  sub _init_ () is cl {'
     (setq |GLOBAL::@ARGS|
       (new-Array (mapcar #\'UP (subseq sb-ext:*posix-argv* 1)))) ;skip "sbcl"
     (defun env ()
       (mapcan #\'copy-list
        (mapcar (lambda (str)
                  (let ((pos (position #\= str :test #\'equal)))
                    (list (UP (subseq str 0 pos))
                          (UP (subseq str (1+ pos))))))
                (posix-environ))))
      (setq |GLOBAL::%ENV| (ap #\'|M::new| (cons |Hash::/co| (env))))
  '}
  _init_();
}
