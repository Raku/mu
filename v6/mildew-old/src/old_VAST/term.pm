package VAST::term;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub VAST::term::emit_m0ld {
    my $m = shift;
    if ($m->{sym} eq 'self') {
        lookup('$¿self');
    } elsif ($m->{sym} eq 'undef') {
        lookupf("undef");
    } elsif ($m->{identifier} && $m->{args}) {
        my $func = lookup('&'.$m->{identifier}{TEXT});
        my @args = $m->{args}->emit_m0ld;
        my @positional = grep { ref $_ ne 'AST::Pair' } @args;
        my @named = map { $_->key, $_->value } grep { ref eq 'AST::Pair' } @args;
        use YAML::XS;
        call 'postcircumfix:( )' => FETCH($func),[capturize(\@positional,\@named)];
    } elsif ($m->{longname}) {
	my $name = $m->{longname}->{name};
        use YAML::XS;
	if ($name->{morename} and !$name->{identifier}) {
            my @name = map {$_->{identifier}[0]{TEXT}} @{$name->{morename}};
            if (scalar @name > 1) {
                my $name = lookup($name[0].'::');
                for my $part (@name[1..-2]) {
                    $name = call('postcircumfix:{ }'=>FETCH($name),[string($part.'::')]);
                }
                 call('postcircumfix:{ }'=>FETCH($name),[string($name[-1])]);
            } else {
                lookup($name[-1]);
            }
        } elsif ($name->{identifier}{TEXT} eq 'CALLER') {
	    call new => reg '¢SMOP__S1P__FlattenedScope',
	      [ call lexical => (call back => (call continuation => reg '$interpreter')) ];
        } elsif ($name->{identifier}{TEXT} eq 'MY') {
	    call new => reg '¢SMOP__S1P__FlattenedScope',
	      [ reg '$scope' ];
	} elsif ($m->{args}) {
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
                        my @args = $m->{args}->emit_m0ld;
                        my @positional = grep { ref $_ ne 'AST::Pair' } @args;
                        my @named = map { $_->key, $_->value } grep { ref eq 'AST::Pair' } @args;
			$outer = call 'postcircumfix:( )' =>
			    FETCH(call( 'postcircumfix:{ }' => $inner, [ string $name ])),
			    [capturize(\@positional,\@named)];
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
