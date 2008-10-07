
sub header is p5 {'use Math::Trig qw();'}
header();


class Num {
sub pi() is p5 {'Math::Trig::pi();'}
}

$*PID = (sub () is p5 {'$$'}).();

package GLOBAL { # sub *f(){} isn't working yet.

  sub primitive_runtime_version is p5 {'CORE::sprintf "perl %vd", $^V'}

}

class Any {
  method perl() { self.WHAT ~ '.new(!!!)' }
}
class STRING {
  method perl() is p5 {'q{"} . CORE::quotemeta($self) . q{"}'}
}
class INTEGER {
  method perl() { ''~self }
}
class FLOAT {
  method perl() { ''~self }
}
class ARRAY {
  method perl() { '[' ~ self.map(sub($e){$e.perl()}).join(",") ~ ']' }
}
class HASH {
  method perl() {
    '{' ~ self.keys.map(sub($k){$k.perl() ~ ' => ' ~ self.{$k}.perl}).join(", ") ~ '}'
  }
}

class UNDEF {
    method Str() { "" }
}

class Regexp { method Str () { ''~self } }

module Math {
  method rand($max) is p5 {'CORE::rand($max||1)'}
  method srand($seed) is p5 {'CORE::srand($seed)'}
}
module IO {
  method mkdir($dirname) is p5 {'CORE::mkdir($dirname)'}
  method rmdir($dirname) is p5 {'CORE::rmdir($dirname)'}
}
package GLOBAL {
  sub rmdir($dirname) is p5 {'CORE::rmdir($dirname)'}
}


package GLOBAL {


  multi prefix:<?> (Any $a) { $a.Bool }
  multi prefix:<!> (Any $a) is p5 {'!$a'}

  multi prefix:<+> (Any $a) { $a.Num }
  multi prefix:<+> (Num $a) { $a }
  multi prefix:<+> (Array $a) { $a.elems }

  multi prefix:<~> (Any $a) { $a.Str }
  multi prefix:<~> (Str $a) { $a }

  multi prefix:<-> (Num $a) is p5 {'(0-$a)'}

  multi prefix:<++> (Num $a is rw) is p5 {'++$_[0]'}
  multi prefix:<--> (Num $a is rw) is p5 {'--$_[0]'}


  multi postfix:<++> (Num $a is rw) is p5 {'$_[0]++'}
  multi postfix:<--> (Num $a is rw) is p5 {'$_[0]--'}
  multi postfix:<i>  (Num $a) { $a } ;# Need to implement Complex.


  # infix < > == != eq ne + - || && and or = =~   # Not called.

  # XXX fixme...

  multi infix:<+>    ($a,$b) is p5 {'($a + $b)'}
  multi infix:<*>    ($a,$b) is p5 {'($a * $b)'}
  multi infix:<**>   ($a,$b) is p5 {'($a ** $b)'}

  multi infix:<and>  ($a,$b) is p5 {'($a and $b)'}
  multi infix:<or>   ($a,$b) is p5 {'($a or $b)'}
  multi infix:<eq>   ($a,$b) is p5 {'($a eq $b)'}
  multi infix:<ne>   ($a,$b) is p5 {'($a ne $b)'}

  multi infix:<!=>   ($a,$b) is p5 {'($a != $b)'}
  multi infix:<==>   ($a,$b) is p5 {'($a == $b)'}
  multi infix:«<»    ($a,$b) is p5 {'($a < $b)'}
  multi infix:«>»    ($a,$b) is p5 {'($a > $b)'}
  multi infix:«<=»   ($a,$b) is p5 {'($a <= $b)'}
  multi infix:«>=»   ($a,$b) is p5 {'($a >= $b)'}
  multi infix:«<=>»  ($a,$b) is p5 {'($a <=> $b)'}

  multi infix:<ge>   ($a,$b) is p5 {'($a ge $b)'}
  multi infix:<gt>   ($a,$b) is p5 {'($a gt $b)'}
  multi infix:<le>   ($a,$b) is p5 {'($a le $b)'}
  multi infix:<lt>   ($a,$b) is p5 {'($a lt $b)'}
  multi infix:<cmp>  ($a,$b) is p5 {'($a cmp $b)'}

  multi infix:<xor>  ($a,$b) is p5 {'($a xor $b)'}

  multi infix:<===>   ($a,$b) { undef }
  multi infix:<=:=>   ($a,$b) { undef }

  multi infix:<^>    ($a,$b) is p5 {'($a ^ $b)'}
  multi infix:<..>   ($a,$b) is p5 {'($a .. $b)'}
  multi infix:<^..>  ($a,$b) is p5 {'($a .. $b)'}
  multi infix:<..^>  ($a,$b) is p5 {'($a .. $b)'}
  multi infix:<^..^> ($a,$b) is p5 {'($a .. $b)'}

  multi infix:«>>»    ($a,$b) is p5 {'($a >> $b)'}
  multi infix:«<<»    ($a,$b) is p5 {'($a << $b)'}

  multi infix:<|>     ($a,$b) is p5 {'($a | $b)'}
  multi infix:<||>    ($a,$b) is p5 {'($a || $b)'}
  multi infix:<&>     ($a,$b) is p5 {'($a & $b)'}
  multi infix:<&&>    ($a,$b) is p5 {'($a && $b)'}

  multi infix:<:=>    ($a,$b) is p5 {'($a = $b)'} ;# crock

  multi infix:<!~>   (Any $a, Any $b) { undef }
  multi infix:<~~>   (Any $a, Any $b) { undef }
  multi infix:<~~>   (Str $a, Regexp $b) is p5 {'$a =~ $b'}

  multi infix:<x>    ($a, $b) is p5 {'($a x $b)'}
  
}

# Elf
package GLOBAL {
  sub elf_main () { Program.new().main(@*ARGS); }
}
