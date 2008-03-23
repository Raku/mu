#line 2 prelude.pl
sub Program::prelude {
  #XXX only Main is prelude'ed.
  return "#line ".(__LINE__+1)." elf_a_src/prelude.pl\n".<<'END';
package main;
use Perl6::Say;
use Moose::Autobox;
use autobox::Core;

our @ARGS = @ARGV;
END
}
