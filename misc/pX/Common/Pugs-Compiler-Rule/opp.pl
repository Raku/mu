package Pugs::Grammar::Category;
use warnings;
use strict;

sub new {
    my ($class, $opt) = @_;
    my $self = { levels => [], %$opt };
    bless $self, $class;
}

sub add {
    # name=>'*', precedence=>'>', other=>'+', rule=>$rule
    my ($self, $opt) = @_;
    print "adding $opt->{name}\n";
    for ( 0 .. $#{$self->{levels}} ) {
        if ( grep { $_->{name} eq $opt->{other} } @{$self->{levels}[$_]} ) {
            if ( $opt->{precedence} eq 'tighter' ) {
                splice( @{$self->{levels}}, $_-1, 0, [ $opt ] );                
            }
            elsif ( $opt->{precedence} eq 'looser' ) {
                splice( @{$self->{levels}}, $_+1, 0, [ $opt ] );                
            }
            elsif ( $opt->{precedence} eq 'equal' ) {
                push @{$self->{levels}[$_]}, $opt;
            }
            else {
                die "invalid precedence: $opt->{precedence}";
            }
            return;
        }
    }
    push @{$self->{levels}}, [ $opt ];
}

sub emit_grammar {
    # item=>'<item>',
    my ($self, $op) = @_;
    print "emitting grammar $self->{name}\n";
    my $s = '';
    for ( 0 .. $#{$self->{levels}} ) {
        my $x = join( ' | ', map {"<$self->{name}:<$_->{name}>>"} @{$self->{levels}[$_]} );
        $x = "[ $x ]" if $#{$self->{levels}[$_]};
        my $prev = $_ - 1;
        if ( $_ == 0 ) {
            $s = $s . "rule r$_ { $op->{item} [ $x <r$_> ]? }\n";
        }
        else {
            $s = $s . "rule r$_ { <r$prev> [ $x <r$_> ]? }\n";
        }
    }
    return $s;
}

use Test::More tests => 1;
use Data::Dumper;

{
    my $cat = Pugs::Grammar::Category->new( {
        name => 'rxinfix',
    } );
    $cat->add( {
        name => '+',
        block => sub {},
        assoc => 'left',
        precedence => 'looser',
        other => '*',
        fixity => 'infix',
    } );
    $cat->add( {
        name => '*',
        block => sub {},
        assoc => 'left',
        precedence => 'tighter',
        other => '+',
        fixity => 'infix',
    } );
    $cat->add( {
        name => '-',
        block => sub {},
        assoc => 'left',
        precedence => 'equal',
        other => '+',
        fixity => 'infix',
    } );
    $cat->add( {
        name => 'or',
        block => sub {},
        assoc => 'left',
        precedence => 'looser',
        other => '+',
        fixity => 'infix',
    } );

    print "cat: ", Dumper($cat);
    print "grammar: \n", $cat->emit_grammar({
        item => '<item>',
    } );
}



__END__

use Test::More tests => 33;
use Data::Dumper;

use_ok( 'Pugs::Compiler::Rule' );

=for pod
  sub rxinfix:<|> 
  { 
    return { alt => [ 
                $_[0]{q1}(), 
                $_[0]{q2}(),
            ] ,} 
    }
  }
  is tighter/looser/equiv
  is assoc left/right/non/chain/list
  (is parsed)
  (will do)
  
  my $cat = Pugs::Grammar::Category->new( 
    name => 'rxinfix',
  );
  $cat->add( 
    name => '|',
    block => sub { ... },
    assoc => 'left',
    precedence => { tighter => '+' },
    fixity => 'infix',
  );

    my $rule = Pugs::Compiler::Rule->compile( '
    (<item>) [ @op (<self>) ]?
    ' );

=cut

{

    my $rule = Pugs::Compiler::Rule->compile( '
    $<q1> := (<previous>) 
    [
        \| 
        $<q2> := (<self>) 

        { return { alt => [ 
                $_[0]{q1}(), 
                $_[0]{q2}(),
            ] ,} 
        }
    
    ]?
            
    { return $_[0]{q1}() } 
    ' );
    my $match = $rule->match( "xyzw" );
    is( "$match", "x", 'stringify 1' );
}


__END__

Operator precedence parser

--- from S05 -----
Syntactic categories
    For writing your own backslash and assertion rules or macros, you may
    use the following syntactic categories:

         rule rxbackslash:<w> { ... }    # define your own \w and \W
         rule rxassertion:<*> { ... }    # define your own <*stuff>
         macro rxmetachar:<,> { ... }    # define a new metacharacter
         macro rxmodinternal:<x> { ... } # define your own /:x() stuff/
         macro rxmodexternal:<x> { ... } # define your own m:x()/stuff/

    As with any such syntactic shenanigans, the declaration must be visible
    in the lexical scope to have any effect. It's possible the
    internal/external distinction is just a trait, and that some of those
    things are subs or methods rather than rules or macros. (The numeric
    rxmods are recognized by fallback macros defined with an empty operator
    name.)
-------------------
