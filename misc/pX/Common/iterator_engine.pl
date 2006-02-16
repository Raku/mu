# pX/Common/iterator_engine.pl - fglock
#
# status: the implementation uses fast ARRAY operations, 
# but this makes it difficult to write regex compositions
# such as alternations, and so it doesn't scale easily to complex regexes
#
# plan: rewrite using generators instead of ARRAY
# problem: this may be too slow, or difficult to maintain

use strict;
no strict "refs";

# internal composition functions

# NOTE: <rule>+ can be compiled as <rule><rule>*

sub rule::greedy { 
  my $node = shift;
  my $start = 1;  # XXX - iterator initialization don't belong here
  my $n = 0;      # XXX - iterator initialization don't belong here
  return sub {
    my @tail = @_;
    my @matches;
    if ( $start ) {
        $start = 0;
        while (1) {
            my ($match, @new_tail) = $node->(@tail);
            print "  greedy match: ", Dumper( [ $match, @new_tail ] );
            return ( { '_greedy' => [ @matches ] }, @tail ) if ! $match;
            @tail = @new_tail;
            push @matches, $match;
            $n++;
        }
    }
    # XXX - removed 'cache' optimization until this whole thing works
    # TODO - when is it _not_ possible to cache?
    for (1 .. --$n) {
            my ($match, @new_tail) = $node->(@tail);
            return unless $match;  
            @tail = @new_tail;
            push @matches, $match;
    }
    return ( { '_greedy' => [ @matches ] }, @tail );
  }
}

sub rule::non_greedy { 
  my $node = shift;
  my $n = 0;      # XXX - iterator initialization don't belong here
  return sub {
    my @tail = @_;
    my @matches;
    for (1 .. ++$n) {
            my ($match, @new_tail) = $node->(@tail);
            return unless $match;
            @tail = @new_tail;
            push @matches, $match;
    }
    return ( { '_non_greedy' => [ @matches ] }, @tail );
  }
}

sub rule::alternation {
  my @nodes = @_;
  my $n = 0;      # XXX - iterator initialization don't belong here
  return sub {
    for ( @nodes[ $n++ .. $#nodes ] ) {
        my ($match, @tail) = $_->(@_);
        return ( { '_alternation' =>$match }, @tail) if $match;
    }
    return undef;
  }
}

sub rule::concat {
  my @concat = @_;
  return sub {
    my @matches;
    my @tail;
    while (1) {
        ($matches[0], @tail) = $concat[0]->(@_);
        print "  1st match: ", Dumper( [ $matches[0], @tail ] );
        return undef unless $matches[0];  
        ($matches[1], @tail) = $concat[1]->(@tail);
        print "  2nd match: ", Dumper( [ $matches[1], @tail ] );
        if ( $matches[1] ) {
            return ( { '_concat'=>[ @matches ] }, @tail);
        }
        warn "backtracking";
    }
  }
}

# Prelude - precompiled rules, such as <word>, \x, etc.

*{'rule::.'} = sub { 
        @_ ? ( { '.'=> $_[0] }, @_[1..$#_] ) : undef
    };
*{'rule::<ws>'} = sub {
        return unless @_;
        return ( { '<ws>' => $_[0] }, @_[1..$#_] )
            if $_[0] =~ /\s/;
        return;
    };
*{'rule::<slashed_char>'} = sub {
        return if @_ < 2;
        return ( { '<slashed_char>' => [ $_[0], $_[1] ] }, @_[2..$#_] ) 
            if $_[0] eq '\\';
        return;
    };
*{'rule::<word_char>'} = sub { 
        return unless @_;
        return ( { '<word_char>'=> $_[0] }, @_[1..$#_] ) 
            if $_[0] =~ m/[a-zA-Z0-9\_]/;  
        return;
    };
*{'rule::<word>'} = rule::greedy( \&{'rule::<word_char>'} );
  
  # more definitions, just for testing
 
my %rules;
%rules = (
    'a' => sub { 
        return ( { 'a'=> $_[0] }, @_[1..$#_] ) if $_[0] eq 'a';
        return;
    },
    'ab' => sub { 
        return ( { 'ab'=>[ @_[0,1] ] }, @_[2..$#_] ) if $_[0] eq 'a' && $_[1] eq 'b';
        return;
    },
    'cd' => sub { 
        return ( { 'cd'=>[ @_[0,1] ] }, @_[2..$#_] ) if $_[0] eq 'c' && $_[1] eq 'd';
        return;
    },
    'abb' => sub { 
        my ($match, @tail) = $rules{'ab'}(@_);
        return unless $match;
        return ( { 'abb'=>[ $match, 'b' ] }, @tail[1..$#tail] ) if $tail[0] eq 'b'; 
        return;
    },
);

package main;

# TODO: use Test::More

use Data::Dumper;
$Data::Dumper::Indent = 1;

print Dumper( 
  &{'rule::.'}( qw( a b b a b c c d ) ) 
);
print Dumper( 
  $rules{'abb'}( qw( a b b a b c c d ) ) 
);
print Dumper( 
  rule::alternation( $rules{'ab'}, $rules{'cd'} )->( qw(a b c) ) 
);
print Dumper( 
  rule::greedy( $rules{'a'} )->( qw(a a a b c) ) 
);
print Dumper( 
  rule::concat( 
    rule::greedy( $rules{'a'} ),
    \&{'rule::.'} 
  )->( qw(a a a a) ) 
);
print Dumper( 
  rule::concat(
    rule::greedy( $rules{'a'} ),
    \&{'rule::.'}
  )->( qw(b a a a a) ) 
);
print Dumper( 
  &{'rule::<word>'}( qw(b a a ! !) ) 
);
print Dumper( 
  rule::concat( \&{'rule::<word>'}, \&{'rule::<ws>'} )->( qw(b a ),' ' ) 
);
warn "non_greedy + backtracking"; 
print Dumper( 
  rule::concat(
    rule::non_greedy( $rules{'a'} ),
    $rules{'ab'}
  )->( qw(a a a a b) ) 
);
warn "alternation + backtracking";
print Dumper( 
  rule::concat(
    rule::alternation( $rules{'a'}, $rules{'ab'} ),
    $rules{'ab'}
  )->( qw(a b a b) ) 
);
warn "alternation + greedy + backtracking -- (ab,a,ab)(ab)";
print Dumper( 
  rule::concat(
    rule::greedy(
      rule::alternation( $rules{'a'}, $rules{'ab'} )
    ),
    $rules{'ab'}
  )->( qw(a b a a b a b) ) 
);
