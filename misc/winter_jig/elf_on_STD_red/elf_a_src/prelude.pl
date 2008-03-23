#line 2 prelude.pl
sub Program::prelude {
  #XXX only Main is prelude'ed.
  return "#line ".(__LINE__+1)." elf_a_src/prelude.pl\n".<<'END';
package Moose::Object;
use Perl6::Say;
our @ARGS = @ARGV;
END
}
