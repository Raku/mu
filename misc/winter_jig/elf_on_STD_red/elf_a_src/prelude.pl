#line 2 prelude.pl
sub Program::prelude {
  #XXX only Main is prelude'ed.
  return "#line ".(__LINE__+1)." elf_a_src/prelude.pl\n".<<'END';
package main;
use Perl6::Say;
use Moose::Autobox; use autobox; use autobox::Core; use autobox UNDEF => 'UNDEF';

our $a_ARGS = [@ARGV];

{package UNDEF;}
{package UNDEF; sub ref{'UNDEF'}}
{package UNIVERSAL; sub ref{ref($_[0]) || 'SCALAR'} }

sub ::undef{undef}

use Carp;
sub slurp{my($file)=@_; `cat $file`;}
sub unslurp{my($text,$file)=@_; open(F,">$file") or die $!; print F $text; close F;}
sub file_exists{-e $_[0]}
sub system{system(@_)}
sub eval_perl5{my($p5)=@_;my $res = eval($p5); croak($@) if $@; $res}
sub ::die{croak @_}
sub ::exit{exit(@_)}
sub ::defined{defined($_[0])}
sub ::substr ($$$){substr($_[0],$_[1],$_[2])}

{ package SCALAR;
sub re_gsub ($$$) {$_[0] =~ s/$_[1]/$_[2]/g; $_[0]}
sub re_sub  ($$$) {$_[0] =~ s/$_[1]/$_[2]/;  $_[0]}
}

package main;
END
}
