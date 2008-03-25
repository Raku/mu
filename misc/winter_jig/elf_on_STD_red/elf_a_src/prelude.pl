#line 2 prelude.pl
sub Program::prelude {
  #XXX only Main is prelude'ed.
  return "#line ".(__LINE__+1)." elf_a_src/prelude.pl\n".<<'END';
package main;
use Perl6::Say;
use Moose::Autobox; use autobox; use autobox::Core;

our $a_ARGS = [@ARGV];

use Carp;
sub slurp{my($file)=@_; `cat $file`;}
sub unslurp{my($text,$file)=@_; open(F,">$file") or die $!; print F $text; close F;}
sub system{system(@_)}
sub eval_perl5{my($p5)=@_;my $res = eval($p5); croak($@) if $@; $res}
sub ::die{croak @_}
sub ::exit{exit(@_)}
END
}
