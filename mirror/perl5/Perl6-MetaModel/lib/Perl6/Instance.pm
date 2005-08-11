
package Perl6::Instance;

use strict;
use warnings;

use Scalar::Util 'blessed';
use Carp 'confess';

sub isa {
    our $AUTOLOAD = 'isa';
    goto &AUTOLOAD;
}

sub can {
    our $AUTOLOAD = 'can';
    goto &AUTOLOAD;
}

sub AUTOLOAD {
    my @AUTOLOAD = split '::', our $AUTOLOAD;
    my $label = $AUTOLOAD[-1];
    my $self = shift;    
    if ($label =~ /DESTROY/) {
        # XXX - hack to avoid destorying Perl6::Class object
        # as that presents some issues for some reason
        return unless blessed($self) ne 'Perl6::Class';
    }    
    return ::dispatch($self, $label, @_);   
}

1;
