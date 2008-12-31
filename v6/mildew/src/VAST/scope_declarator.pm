package VAST::scope_declarator;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub VAST::scope_declarator::emit_m0ld {
    my $m = shift;
    if ($m->{'sym'} eq 'my' || $m->{'sym'} eq 'our') {
        if (my $decl = $m->{scoped}{declarator}) {
            if (my $var_decl = $decl->{variable_declarator}) {
		let call(new => FETCH(lookup 'Scalar')), sub {
		    my $value = shift;
		    AST::Seq->new(stmts => [
				      ( $m->{'sym'} eq 'our' ? 
				      call(BIND => (call 'postcircumfix:{ }' => FETCH(lookup '$?PACKAGE'),
						    [ string varname($var_decl->{variable}) ]),[$value])
				      :()),
				      call(BIND => (call 'postcircumfix:{ }' => reg('$scope'),
						    [ string varname($var_decl->{variable}) ]),[$value])
				  ]);

		}
            } elsif (my $routine_decl = $decl->{routine_declarator}) {
		$routine_decl->{routine_def}->emit_m0ld($m->{sym});
            } else {
                XXX('unknown scope declarator');
            }
        } else {
            XXX('scoped declarator without a declarator');
        }
    } else {
        XXX('unknown sym in scope declarator');
    }
}

1;
