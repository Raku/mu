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

# TODO: <rule>+ can be compiled as <rule><rule>*

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
            return ( { '_greedy' => [ @matches ] }, @tail ) if ! $match;
            @tail = @new_tail;
            push @matches, $match;
            $n++;
        }
    }
    # XXX - removed 'cache' optimization until this whole thing works
    for (1 .. --$n) {
            my ($match, @new_tail) = $node->(@tail);
            @tail = @new_tail;
            push @matches, $match;
    }
    return ( { '_greedy' => [ @matches ] }, @tail );
  }
}

sub rule::non_greedy { 
  my $node = shift;
  return sub {
    my ($match, @tail) = $node->(@_);
    return ( { '_non_greedy' => [ $match ] }, @tail ) if $match;
    return undef;
  }
}

sub rule::alternation {
  # XXX - this needs to be able to backtrack 
  # XXX   when it is inside a greedy match, for example
  my @alternates = @_;
  return sub {
    for ( @alternates ) {
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
        return undef unless $matches[0];  
        #print Dumper [ $matches[0], @tail ];
        ($matches[1], @tail) = $concat[1]->(@tail);
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
