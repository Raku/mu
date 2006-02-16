# pX/Common/iterator_engine.pl - fglock
#
# status: the implementation uses fast ARRAY operations, 
# but this makes it difficult to write regex compositions
# such as alternations, so that it
# doesn't scale easily for complex regexes
#
# plan: rewrite using generators instead of ARRAY
# problem: this may be too slow, or difficult to maintain

use strict;

my %rules;  # this is our "global namespace" for rules

# internal composition functions

# XXX - TODO: <rule>+ can be compiled as <rule><rule>*

sub _greedy { 
    my $node_name = shift;
    my @tail = @_;
    my @matches;
    while (1) {
        my ($match, @new_tail) = $rules{ $node_name }(@tail);
        return ( { '_greedy' => [ @matches ] }, @tail ) if ! $match;
        @tail = @new_tail;
        push @matches, $match;
    }
}

sub _non_greedy { 
    my $node_name = shift;
    my ($match, @tail) = $rules{ $node_name }(@_);
    return ( { '_non_greedy' => [ $match ] }, @tail ) if $match;
    return undef;
}

sub _alternation {
    # XXX - this needs to be able to backtrack 
    # XXX   when it is inside a greedy match, for example
    my $alternates = shift;
    for ( @$alternates ) {
        my ($match, @tail) = $rules{ $_ }(@_);
        return ( { '_alternation' =>$match }, @tail) if $match;
    }
    return undef;
}

sub _concat {
    my $concat = shift;
    my @matches;
    my @tail;
    my $match;
    
    ($match, @tail) = $rules{ $concat->[0] }(@_);
    return undef unless $match;  
    #print Dumper [ $match, @tail ];

    # XXX - _greedy / _non_greedy / _alternation / other
    while (1) {
        my $iterations = @{ $match->{'_greedy'} };
        warn "iterations to go: $iterations";
    
        @matches = ();
        push @matches, $match;
        
        my $match2;
        ($match2, @tail) = $rules{ $concat->[1] }(@tail);
        push @matches, $match2 if $match2;
    
        return ( { '_concat'=>[ @matches ] }, @tail) if $match2;
        
        return undef unless @{ $match->{'_greedy'} };
        
        my $last = pop @{ $match->{'_greedy'} };
        unshift @tail, $last;
    }
}

# Prelude - precompiled rules, such as <word>, \x, etc.

%rules = (
    '.' => sub { 
        return ( { '.'=>[ $_[0] ] }, @_[1..$#_] ) if @_;
        return;
    },
    '<slashed_char>' => sub {
        return ( { '<slashed_char>' => [ $_[0], $_[1] ] }, @_[2..$#_] ) if $_[0] eq '\\';
        return;
    },
    '<word_char>' => sub { 
        return ( { '<word_char>'=>[ $_[0] ] }, @_[1..$#_] ) if $_[0] =~ m/[a-zA-Z0-9\_]/;  
        return;
    },
    '<word>' => sub {
        my ($match, @tail) = _greedy( '<word_char>', @_ );
        return { '<word>'=>$match }, @tail if $match;
        return;
    },
  
  # more definitions, just for testing
  
    'a' => sub { 
        return ( { 'a'=>[ $_[0] ] }, @_[1..$#_] ) if $_[0] eq 'a';
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
    'ab|cd' => sub {
        _alternation( [ 'ab', 'cd' ], @_ );
    },
    'a*' => sub { 
        _greedy( 'a', @_ );
    },
    'a*.' => sub { 
        _concat( [ 'a*', '.' ], @_ );
    },
);

use Data::Dumper;
$Data::Dumper::Indent = 1;
my @in = qw( a b b a b c c d );
print Dumper( $rules{'.'}(@in) );
print Dumper( $rules{'abb'}(@in) );
print Dumper( $rules{'ab|cd'}( qw(a b c) ) );
print Dumper( $rules{'a*'}( qw(a a a b c) ) );
print Dumper( $rules{'a*.'}( qw(a a a a) ) );
print Dumper( $rules{'a*.'}( qw(b a a a a) ) );
print Dumper( $rules{'<word>'}( qw(b a a ! !) ) );
