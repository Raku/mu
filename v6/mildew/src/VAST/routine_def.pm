package VAST::routine_def;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my ($m, $visibility) = @_;
    $visibility ||= 'our';

    my $name = '&'.$m->{deflongname}[0]{name}{identifier}{TEXT};
    my $rout = routine($m->{block},$m->{multisig}[0]{signature}[0]);

    let $rout, sub {
	my $value = shift;
	my @trait_statement = ();
	if (ref $m->{trait} eq 'ARRAY') {
	    foreach my $trait (@{$m->{trait}}) {
		my $aux = $trait->{trait_auxiliary};
		if ($aux->{sym} eq 'is') {
		    if ($aux->{longname}{name}{identifier}{TEXT} eq 'export') {
			push @trait_statement,
			call(BIND =>
			     (call 'postcircumfix:{ }' =>
			      FETCH(call 'postcircumfix:{ }' =>
				    FETCH(call 'postcircumfix:{ }' =>
					  FETCH(call 'lookup' => reg '$scope', [ string '$?PACKAGE' ]),
					  [ string 'EXPORT::' ]),
				    [ string 'ALL::' ]),
			      [ string $name ]),
			     [ $value ]);
			if (ref $trait->{postcircumfix} eq 'ARRAY') {
			}
		    } else {
			XXX;
		    }
		} else {
		    XXX;
		}
	    }
	}
	AST::Seq->new(stmts => [
			  ( $visibility eq 'our' ? 
			    call(BIND => (call 'postcircumfix:{ }' => FETCH(lookup '$?PACKAGE'),
					  [ string $name ]),[$value])
			    : ()),
			  call(BIND => (call 'postcircumfix:{ }' => reg('$scope'),
					[ string $name ]),[$value]),
			  @trait_statement
		      ]);
    };
}

1;
