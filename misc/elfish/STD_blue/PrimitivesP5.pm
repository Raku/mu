# This is a modified copy of ../../elf/elf_h_src/PrimitivesP5.pm r24075.
# gimme5 (r24075) creates a new lexer for each 'multi foofix:<...>' decl,
# using a great deal of memory.
# Ways to workaround:
#  (a) Comment out some decls.  Currently 51.  25 requires 2GB.
#      Code stays correct.  It's not clear what half to dummp.
#  (b) Change foofix:<bar> to foofix:sym<bar>.
#      Code becomes incorrect.  But gimme5 then has no trouble with it,
#      and elf currently treats :sym<foo> and :<foo> the same.
# Going with (b).

sub header is p5 {'use Math::Trig qw();'}
header();


class Num {
sub pi() is p5 {'Math::Trig::pi();'}
}

$*PID = (sub () is p5 {'$$'}).();
$*OS = (sub () is p5 {'$^O'}).();
$*OSVER = (sub () is p5 {'`/bin/uname -a`'}).();
$*EXECUTABLE_NAME = (sub () is p5 {'$0'}).();
sub not_used_only_once { [$*OS,$*OSVER,$*EXECUTABLE_NAME] }

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


  multi prefix:sym<?> (Any $a) { $a.Bool }
  multi prefix:sym<!> (Any $a) is p5 {'!$a'}

  multi prefix:sym<+> (Any $a) { $a.Num }
  multi prefix:sym<+> (Num $a) { $a }
  multi prefix:sym<+> (Array $a) { $a.elems }

  multi prefix:sym<~> (Any $a) { $a.Str }
  multi prefix:sym<~> (Str $a) { $a }

  multi prefix:sym<-> (Num $a) is p5 {'(0-$a)'}

  multi prefix:sym<++> (Num $a is rw) is p5 {'++$_[0]'}
  multi prefix:sym<--> (Num $a is rw) is p5 {'--$_[0]'}


  multi postfix:sym<++> (Num $a is rw) is p5 {'$_[0]++'}
  multi postfix:sym<--> (Num $a is rw) is p5 {'$_[0]--'}
  multi postfix:sym<i>  (Num $a) { $a } ;# Need to implement Complex.


  # infix < > == != eq ne + - || && and or = =~   # Not called.

  # XXX fixme...

  multi infix:sym<+>    ($a,$b) is p5 {'($a + $b)'}
  multi infix:sym<*>    ($a,$b) is p5 {'($a * $b)'}
  multi infix:sym<**>   ($a,$b) is p5 {'($a ** $b)'}

  multi infix:sym<and>  ($a,$b) is p5 {'($a and $b)'}
  multi infix:sym<or>   ($a,$b) is p5 {'($a or $b)'}
  multi infix:sym<eq>   ($a,$b) is p5 {'($a eq $b)'}
  multi infix:sym<ne>   ($a,$b) is p5 {'($a ne $b)'}

  multi infix:sym<!=>   ($a,$b) is p5 {'($a != $b)'}
  multi infix:sym<==>   ($a,$b) is p5 {'($a == $b)'}
  multi infix:sym«<»    ($a,$b) is p5 {'($a < $b)'}
  multi infix:sym«>»    ($a,$b) is p5 {'($a > $b)'}
  multi infix:sym«<=»   ($a,$b) is p5 {'($a <= $b)'}
  multi infix:sym«>=»   ($a,$b) is p5 {'($a >= $b)'}
  multi infix:sym«<=>»  ($a,$b) is p5 {'($a <=> $b)'}

  multi infix:sym<ge>   ($a,$b) is p5 {'($a ge $b)'}
  multi infix:sym<gt>   ($a,$b) is p5 {'($a gt $b)'}
  multi infix:sym<le>   ($a,$b) is p5 {'($a le $b)'}
  multi infix:sym<lt>   ($a,$b) is p5 {'($a lt $b)'}
  multi infix:sym<cmp>  ($a,$b) is p5 {'($a cmp $b)'}

  multi infix:sym<xor>  ($a,$b) is p5 {'($a xor $b)'}

  multi infix:sym<===>   ($a,$b) { undef }
  multi infix:sym<=:=>   ($a,$b) { undef }

  multi infix:sym<^>    ($a,$b) is p5 {'($a ^ $b)'}
  multi infix:sym<..>   ($a,$b) is p5 {'($a .. $b)'}
  multi infix:sym<^..>  ($a,$b) is p5 {'($a .. $b)'}
  multi infix:sym<..^>  ($a,$b) is p5 {'($a .. $b)'}
  multi infix:sym<^..^> ($a,$b) is p5 {'($a .. $b)'}

  multi infix:sym«>>»    ($a,$b) is p5 {'($a >> $b)'}
  multi infix:sym«<<»    ($a,$b) is p5 {'($a << $b)'}

  multi infix:sym<|>     ($a,$b) is p5 {'($a | $b)'}
  multi infix:sym<||>    ($a,$b) is p5 {'($a || $b)'}
  multi infix:sym<&>     ($a,$b) is p5 {'($a & $b)'}
  multi infix:sym<&&>    ($a,$b) is p5 {'($a && $b)'}

  multi infix:sym<:=>    ($a,$b) is p5 {'($a = $b)'} ;# crock

  multi infix:sym<!~>   (Any $a, Any $b) { undef }
  multi infix:sym<~~>   (Any $a, Any $b) { undef }
  multi infix:sym<~~>   (Str $a, Regexp $b) is p5 {'$a =~ $b'}

  multi infix:sym<x>    ($a, $b) is p5 {'($a x $b)'}
  
}

# Elf
package GLOBAL {
  sub elf_main () { Program.new().main(@*ARGS); }
}
