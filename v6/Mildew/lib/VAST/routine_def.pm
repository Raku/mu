package VAST::routine_def;
use utf8;
use strict;
use warnings;
use Mildew::AST::Helpers;

sub emit_m0ld {
    my ($m, $visibility) = @_;
    $visibility ||= 'our';

    my ($anon, $name);
    if ($m->{deflongname}[0]) {
        $name = '&'.$m->{deflongname}[0]{name}{identifier}{TEXT};
    } else {
        $anon = 1;
    }
    my $sig = $m->{multisig}[0]{signature}[0];
    my $rout = routine($m->{blockoid},($sig ? $sig->emit_m0ld : empty_sig));

    if ($anon) {
        return $rout;
    }

    let $rout, sub {
	my $value = shift;
	my @trait_statement = ();
	if (ref $m->{trait} eq 'ARRAY') {
	    foreach my $trait (@{$m->{trait}}) {
		my $aux = $trait->{trait_auxiliary};
		if ($aux->{SYM} eq 'is') {
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
	Mildew::AST::Seq->new(stmts => [
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
