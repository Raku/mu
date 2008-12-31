package VAST::variable;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    AST::Call->new(
        identifier=>string 'lookup',
        capture=>AST::Capture->new(invocant=>reg '$scope',positional=>[string varname($m)]),
    );
}

1;
