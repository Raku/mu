# nfa-perl.pl
#
# a Perl 5 implementation of the algorithm by Russ Cox <rsc@swtch.com>
# described in http://swtch.com/~rsc/regexp/regexp1.html (August 2008)
#
# this version doesn't include the regex compiler, because this is intended 
# to be used with the PCR compiler
#
# this seems to be 30,000 times slower than pure Perl 5 regexes
#
# Copyright (c) 2008 Flavio S. Glock
# Can be distributed under the same license as Pugs.


use strict;
use warnings;
use Data::Dumper;

# the NFA is a linked collection of states
my $start = '8';
my %states = (
    '8' => { id => 8, op => 'LParen', data => 0, out => 3 },
    '3' => { id => 3, op => 'LParen', data => 1, out => 2 },
    '2' => { id => 2, op => 'Split', out => 1, out1 => 4 },
    '1' => { id => 1, op => 'Any', out => 2 },
    '4' => { id => 4, op => 'RParen', data => 1, out => 6 },
    '6' => { id => 6, op => 'LParen', data => 2, out => 5 },
    '5' => { id => 5, op => 'Char', char => 'a', out => 7 },
    '7' => { id => 7, op => 'RParen', data => 2, out => 9 },
    '9' => { id => 9, op => 'RParen', data => 0, out => 10 },
    '10' => { id => 10, op => 'Match' },
);

for ( values %states ) {
    $_->{out}  = $states{ $_->{out}  } if $_->{out};
    $_->{out1} = $states{ $_->{out1} } if $_->{out1};
}
$start = $states{$start};


# debug
sub dumpmatch {
        for ( @{$_[0]} ) {
            if ( defined $_->{paren_start} && defined $_->{paren_end} ) {
                print "($_->{paren_start},$_->{paren_end})";
            }
            elsif ( defined $_->{paren_start}  ) {
                print "($_->{paren_start},?)";
            }
            else {
                print("(?,?)");
            }
        }
        print "\n";
}
sub dumplist {
    my $l = shift;
    for ( @$l ) {
        next unless $_->{state}{op} eq 'Char' || $_->{state}{op} eq 'Any' || $_->{state}{op} eq 'Match';
        print "  id:$_->{state}{id} ";
        dumpmatch($_->{match});
	}
}



{
my $list_id = 1;

sub addstate {
    my ($l, $state, $m, $p) = @_;
    #print "addstate pos=$p state=$state->{id}\n";
    return unless defined $state;

	if ( ($state->{list_last} || 0) == $list_id) {
        return if ++$state->{visits} > 2;
	}
    else {
		$state->{list_last}   = $list_id;
        push @$l, {};
		$state->{thread_last} = $l->[-1];   
		$state->{visits}      = 1;
	}
	if ($state->{visits} == 1) {
        #print "clone\n";
		$state->{thread_last}{state} = $state;
		$state->{thread_last}{match} = [ map { {%{$_}} } @$m ];
	}

	if ($state->{op} eq 'Split' ) {
		addstate($l, $state->{out},  $m, $p);
		addstate($l, $state->{out1}, $m, $p);
	}
    elsif ($state->{op} eq 'LParen' ) {
		my $save = $m->[$state->{data}];
		$m->[$state->{data}] = { paren_start => $p, paren_end => undef };
		addstate($l, $state->{out}, $m, $p);
		$m->[$state->{data}] = $save;
	}
    elsif ($state->{op} eq 'RParen' ) {
		my $save = $m->[$state->{data}];
		$m->[$state->{data}]{paren_end} = $p;
		addstate($l, $state->{out}, $m, $p);
		$m->[$state->{data}] = $save;
	}
}

sub step {
    my ($list_current, $char, $next_pos, $list_next, $match) = @_;
    #print "step pos=$next_pos\n";
    #dumplist( $list_current );
    #print "  char=/$char/ \n";
	$list_id++;
	@$list_next = ();
	for my $t ( @$list_current ){
        #print "step - op:$t->{state}{op}\n";
		if ($t->{state}{op} eq 'Char' ) {
			if ($char eq $t->{state}{char}) {
				addstate($list_next, $t->{state}{out}, $t->{match}, $next_pos);
            }
		}
		elsif ($t->{state}{op} eq 'Any' ) {
			addstate($list_next, $t->{state}{out}, $t->{match}, $next_pos);
		}
		elsif ($t->{state}{op} eq 'Match' ) {
            # print "Matched: ", Dumper($t->{match});
            @$match = @{$t->{match}};
            return;
		}
	}
	if ( !@$match || !defined($match->[0]{paren_start}) ) {
        #print "step - starting\n";
		addstate($list_next, $start, [], $next_pos);
    }
}

}

sub match {
    my ($text, $pos, $match) = @_;
	my $list_current = [];
	my $list_next = [];
    step([], "", $pos, $list_current, []);
    #print "list_current: ", Dumper($list_current);
	for(; $pos < length($text) && scalar @$list_current; $pos++) {
		step($list_current, substr($text, $pos, 1), $pos+1, $list_next, $match);
		($list_current, $list_next) = ($list_next, $list_current);
	}
	step($list_current, "", $pos, $list_next, $match);
	return scalar @$match && defined $match->[0]{paren_start};
}

#print "Start: ", Dumper($start);
my $text  = "a" x 100;
my $pos   = 0;
my $match = [];
if ( match($text, $pos, $match) ) {
    #print "text: $text ";
    print "match: ";
    dumpmatch($match);
}
#print "Start: ", Dumper($start);

use Benchmark ':hireswallclock', ':all';

print "benchmarking...\n";
my $text2 = $text x 30000;
cmpthese( 
  500, 
  {
   'nfa-perl x 1' => sub { 
                my $pos   = 0;
                my $match = [];
                if ( match($text, $pos, $match) ) { 1 }
            },
   'p5regex x 30,000' => sub { 
                my $pos   = 0;
                my $match = [];
                if ( $text2 =~ /^(.*)(a)$/ ) { 1 } 
            },
  }
);
