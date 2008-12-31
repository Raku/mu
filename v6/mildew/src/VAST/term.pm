package VAST::term;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub VAST::term::emit_m0ld {
    my $m = shift;
    if ($m->{sym} eq 'self') {
        lookup('$¿self');
    } elsif ($m->{identifier} && $m->{args}) {
        my $func = lookup('&'.$m->{identifier}{TEXT});
        call 'postcircumfix:( )' => FETCH($func),[capturize($m->{args}->emit_m0ld)];
    } elsif ($m->{longname}) {
	my $name = $m->{longname}->{name};
	if ($name->{morename} and !$name->{identifier}) {
            lookup join '',map {$_->{identifier}[0]{TEXT}} @{$name->{morename}};
        } elsif ($name->{identifier}{TEXT} eq 'CALLER') {
	    call new => reg '¢SMOP__S1P__FlattenedScope',
	      [ call lexical => (call back => (call continuation => reg '$interpreter')) ];
        } elsif ($name->{identifier}{TEXT} eq 'MY') {
	    call new => reg '¢SMOP__S1P__FlattenedScope',
	      [ reg '$scope' ];
	} elsif ($m->{args}[0]) {
	    my $outer = FETCH(lookup($m->{longname}{name}{identifier}{TEXT}.'::'));
	    my @morenames = @{$m->{longname}{name}{morename}};
	    while (my $new_outer = shift @morenames) {
		my $inner = $outer;
		my $name = $new_outer->{identifier}[0]{TEXT};
		if ($name) {
		    if (@morenames) {
			$name .= '::';
			$outer = FETCH(call( 'postcircumfix:{ }' => $inner, [ string $name ]));
		    } else {
			$name = '&'.$name;
			$outer = call 'postcircumfix:( )' =>
			    FETCH(call( 'postcircumfix:{ }' => $inner, [ string $name ])),
			    [capturize($m->{args}[0]->emit_m0ld)];
		    }
		} else {
		    $outer = $inner;
		    last;
		}
	    }
	    $outer;
	}
    } else {
	XXX;
    }
}

1;
