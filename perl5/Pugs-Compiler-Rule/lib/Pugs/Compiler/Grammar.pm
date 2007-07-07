use strict;
use warnings;
#use Smart::Comments;

package Pugs::Compiler::Grammar;

use Pugs::Grammar::Rule;
use Pugs::Emitter::Grammar::Perl5;
use Carp qw(carp croak);

sub compile {
    my ($class, $src) = @_;
    my $ast = Pugs::Grammar::Rule->grammars($src)->();
    if ($ast) {
        ## $ast
        my $perl5;
        for my $g (@$ast) {
            ### Grammar found...
            $g = $g->();
            my ($name) = keys %$g;
            ### Grammar: $name
            $perl5 .= Pugs::Emitter::Grammar::Perl5::emit($g);
        }
        bless {
            source => $src,
            ast => $ast,
            perl5 => $perl5,
        }, $class;
    } else {
        carp "Failed to compile the grammar source";
    }
}

sub perl5 {
    $_[0]->{perl5};
}

1;
__END__


