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
  sub primitive_print ($x) is cl {'
   (cl:if (cl:stringp |$x|) (cl:write-string |$x|) (cl:write |$x|))
  '}
  sub primitive_write_to_string ($x) is cl {' (write-to-string |$x|) '}

  sub undef () is cl {'
    nil ;XX
  '}

  sub infix:<+> ($a,$b) is cl {' (+ |$a| |$b|) '}
  sub infix:<-> ($a,$b) is cl {' (- |$a| |$b|) '}
  sub infix:<*> ($a,$b) is cl {' (* |$a| |$b|) '}
  sub infix:</> ($a,$b) is cl {' (/ |$a| |$b|) '}

  sub infix:<<> ($a,$b) is cl {' (< |$a| |$b|) '}
  sub infix:«>» ($a,$b) is cl {' (> |$a| |$b|) '}
  sub infix:<<=> ($a,$b) is cl {' (<= |$a| |$b|) '}
  sub infix:«>=» ($a,$b) is cl {' (>= |$a| |$b|) '}
  sub infix:<==> ($a,$b) is cl {' (equal |$a| |$b|) '}
  sub infix:«!=» ($a,$b) is cl {' (not (equal |$a| |$b|)) '}
  sub infix:<eq> ($a,$b) is cl {' (equal |$a| |$b|) '}
  sub infix:<ne> ($a,$b) is cl {' (not (equal |$a| |$b|)) '}

  sub infix:<||> ($a,$b) is cl {' (or |$a| |$b|) '}
  sub infix:<&&> ($a,$b) is cl {' (and |$a| |$b|) '}
  sub infix:<or> ($a,$b) is cl {' (or |$a| |$b|) '}
  sub infix:<and> ($a,$b) is cl {' (and |$a| |$b|) '}

  sub infix:<~> ($a,$b) { primitive_strcat($a.Str,$b.Str) }
  sub primitive_strcat ($a,$b) is cl {' (concatenate \'string |$a| |$b|) '}

  sub prefix:<!> ($a) is cl {' (not |$a|) '}
  sub prefix:<-> ($a) is cl {' (- 0 |$a|) '}
  sub prefix:<?> ($a) is cl {' nil '}

  sub circumfix:«[ ]» (*@a) is cl {' |@a| '}
  sub circumfix:«( )» ($a) is cl {' |$a| '}

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
}

class Array {
  has $._native_;
  method values () is cl {'
    (coerce (slot-value self \'|Array::._native_|) \'list)
  '}
  method elems () is cl {'
    (length (slot-value self \'|Array::._native_|))
  '}
  method push (*@a) is cl {'
    (setf (slot-value self \'|Array::._native_|)
          (concatenate \'vector
            (slot-value self \'|Array::._native_|)
            (fc |M::_native_| |@a|)))
    self
  '}
  method unshift (*@a) is cl {'
    (setf (slot-value self \'|Array::._native_|)
          (concatenate \'vector
            (fc |M::_native_| |@a|)
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
}

class Hash {
  has $._native_;
}

# .Num()
class Int   { method Num () { self } }
class Num   { method Num () { self } }
class Str   { method Num () { self.primitive_Num() } }
class Array { method Num () { self.elems } }
class Hash  { method Num () { self.keys.elems } }
class Pair  { method Num () { 2 } }; # so says pugs, the only impl working. 2008-May-24

# .Str()
class Int   { method Str () { primitive_write_to_string(self._native_) } }
class Num   { method Str () { primitive_write_to_string(self._native_) } }
class Str   { method Str () { self._native_ } }
class Array { method Str () { self.join('') } }
class Hash  { method Str () { self.keys.map(sub($k){$k~"\t"~self<$k>}).join("\n") } }
class Pair  { method Str () { $.key~"\t"~$.value } }

# truth
class Any { method true() { defined(self) }}


