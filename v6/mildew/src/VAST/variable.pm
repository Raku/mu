package VAST::variable;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    my $twigil = $m->{twigil}[0]{sym} || '';
    if ($twigil eq '!') {
	call('postcircumfix:{ }'=>
	     FETCH(call('postcircumfix:{ }',
			FETCH(call('^!instance_storage'=>
				   FETCH(lookup('$¿self')))),
		   [FETCH(call(name=>FETCH(lookup('$?PACKAGE'))))])),
             [string varname($m)])
    } elsif ($twigil eq '.') {
	call($m->{desigilname}{longname}->canonical,FETCH(lookup('$¿self')));
    } else {
        use Data::Dumper;
        my @name = name_components($m);
        if (scalar @name > 1) {
            my $name = lookup($name[0].'::');
            for my $part (@name[1..-2]) {
                $name = call('postcircumfix:{ }'=>FETCH($name),[string($part.'::')]);
            }
            $name = call('postcircumfix:{ }'=>FETCH($name),[string($name[-1])]);
        } else {
            lookup($name[-1]);
        }
    }
}

1;
