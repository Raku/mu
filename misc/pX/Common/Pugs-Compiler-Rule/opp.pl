# operator precedence parser for Pugs::Compiler::Grammar - fglock

package Pugs::Grammar::Category;
use warnings;
use strict;

# TODO 
#  ok - is tighter/looser/equiv 
#  infix/prefix/circumfix/postcircumfix
#  is assoc left/right/non/chain/list
#  (is parsed) / ternary
#  (will do)

# Operator hash:
# name=>'*', precedence=>'tighter', other=>'+', rule=>$rule (used in is-parsed)
# name2=>')' (used in circumfix/ternary)

sub new {
    my ($class, $opt) = @_;
    my $self = { levels => [], %$opt };
    bless $self, $class;
}

sub exists_op { die "not implemented" };
sub delete_op { die "not implemented" };
sub get_op { die "not implemented" };
sub inherit_category { die "not implemented" };
sub inherit_grammar { die "not implemented" };
sub merge_category { die "not implemented" };

my %relative_precedence = (
    tighter => sub { splice( @{$_[0]->{levels}}, $_[1]-1, 0, [ $_[2] ] ) },
    looser  => sub { splice( @{$_[0]->{levels}}, $_[1]+1, 0, [ $_[2] ] ) },
    equal   => sub {   push( @{$_[0]->{levels}[$_[1]]}, $_[2] ) },
);

sub add_op {
    my ($self, $opt) = @_;
    print "adding $opt->{name}\n";
    $opt->{assoc} = 'left' unless defined $opt->{assoc};
    for ( 0 .. $#{$self->{levels}} ) {
        if ( grep { $_->{name} eq $opt->{other} } @{$self->{levels}[$_]} ) {
            $relative_precedence{$opt->{precedence}}->($self, $_, $opt);
            return;
        }
    }
    push @{$self->{levels}}, [ $opt ];
}

my %rule_templates = (
    prefix =>          '<op> <equal>', 
    circumfix =>       '<op> <parse> <op2>',  
    postfix =>         '<tight> <op>', 
    postcircumfix =>   '<tight> <op> <parse> <op2>', 
    infix_left =>      '<tight> <op> <equal>', 
    infix_right =>     '<equal> <op> <tight>',
    infix_non =>       '<tight> <op> <tight>', 
    infix_chain =>     '<tight> [ <op> <tight> ]+',
    infix_list =>      '<tight> [ <op> <tight> ]+',
    ternary =>         '<tight> <op> <parse> <op2> <equal>',
);
# parsed: - the rule replaces the right side
# note: S06 - 'chain' can't be mixed with other types in the same level

sub _optimize {
    my @rules = reverse sort @_;
    my $i = 0;
    my $j = $i;
    my ($item) = $rules[$i] =~ /^(<.*?>)/;
    #print "item: $item\n";
    for my $i ( $i .. $#rules ) {
        last unless $rules[$i] =~ /^$item/;
        #print "optimize: $rules[$i]\n";
        $j = $i;
    }
    if ( $i != $j ) {
        #print "optimize $item in @rules[$i..$j]\n";
        my $len = length( $item );
        my @r = map { $_ eq $item ? '' : substr($_,$len) } @rules[$i..$j];
        pop @r if $r[-1] eq '';
        #print "optimized: [",join(",",@r),"] $i $j\n";
        splice( @rules, $i, (1+$j-$i), 
            "$item [ " . join( ' | ', @r ) . " ]?"
        );
        #print "optimized: [",join(",",@rules),"]\n";
    }
    return @rules;
}

sub emit_perl6_rule {
    my ($self, $level, $default) = @_;
    my @rules;
    my $tight = $level ? 'r' . ($level - 1) : $default->{item};
    my $equal = "r$level";
    $equal = "parse" if $level == $#{$self->{levels}};
    for my $op ( @{$self->{levels}[$level]} ) {
        my $rule = $op->{fixity};
        $rule .= '_' . $op->{assoc} if $rule eq 'infix';
        my $template = $rule_templates{$rule};
        if ( $op->{rule} ) {
            $template =~ s/<op>.*/<op> $op->{rule}/sg;
        }
        $template =~ s/<equal>/<$equal>/sg;
        $template =~ s/<tight>/<$tight>/sg;
        $template =~ s/<op>/<'$op->{name}'>/sg;
        $template =~ s/<op2>/<'$op->{name2}'>/sg;
        push @rules, $template;
    }
    push @rules, "<$tight>";
    @rules = _optimize( @rules );
    return $rules[0] unless $#rules;
    return "[ " . join( ' | ', @rules ) . " ]";
}

sub emit_grammar_perl6 {
    # item=>'<item>',
    my ($self, $default) = @_;
    print "emitting grammar in perl6\n";
    my $s = "";
    for my $level ( 0 .. $#{$self->{levels}} ) {
        my $equal = "r$level";
        $equal = "parse" if $level == $#{$self->{levels}};
        $s = $s . "    rule $equal { " .
            emit_perl6_rule($self, $level, $default) .
            " }\n";
    }
    return "grammar $self->{name} { \n$s}\n";
}

sub _quotemeta { my $s = shift; $s =~ s!'!\\'!g; $s }

sub emit_grammar_perl5 {
    # item=>'<item>',
    my ($self, $default) = @_;
    print "emitting grammar in perl5\n";
    my $s = "";
    for my $level ( 0 .. $#{$self->{levels}} ) {
        my $equal = "r$level";
        $equal = "parse" if $level == $#{$self->{levels}};
        $s = $s . "    *$equal = Pugs::Compile::Grammar->compile( '" .
            _quotemeta( emit_perl6_rule($self, $level, $default) ) .
            "' )->code;\n";
    }
    return $s;  #"package $self->{name};\n$s\n";
}

use Test::More tests => 1;
use Data::Dumper;

{
    my $cat = Pugs::Grammar::Category->new( {
        name => 'rxinfix',
    } );
    $cat->add_op( {
        name => '+',
        block => sub {},
        assoc => 'left',
        precedence => 'looser',
        other => '*',
        fixity => 'infix',
    } );
    $cat->add_op( {
        name => '*',
        block => sub {},
        assoc => 'left',
        precedence => 'tighter',
        other => '+',
        fixity => 'infix',
    } );
    $cat->add_op( {
        name => '-',
        block => sub {},
        assoc => 'left',
        precedence => 'equal',
        other => '+',
        fixity => 'infix',
    } );
    $cat->add_op( {
        name => 'or',
        block => sub {},
        assoc => 'left',
        precedence => 'looser',
        other => '+',
        fixity => 'infix',
    } );
    $cat->add_op( {
        name => '[',
        name2 => ']',
        block => sub {},
        assoc => 'left',
        precedence => 'looser',
        other => '+',
        fixity => 'postcircumfix',
    } );
    $cat->add_op( {
        name => 'Y',
        block => sub {},
        assoc => 'list',
        precedence => 'looser',
        other => '+',
        fixity => 'infix',
    } );
    $cat->add_op( {
        name => 'custom_op',
        block => sub {},
        assoc => 'left',
        precedence => 'looser',
        other => '+',
        fixity => 'infix',
        rule => '<custom_parsed>',
    } );
    $cat->add_op( {
        name => '??',
        name2 => '!!',
        block => sub {},
        assoc => 'left',
        precedence => 'equal',
        other => 'custom_op',
        fixity => 'ternary',
    } );

    print "cat: ", Dumper($cat);
    print "grammar in perl6: \n", $cat->emit_grammar_perl6({
        item => 'item',
    } );
    print "grammar in perl5: \n", $cat->emit_grammar_perl5({
        item => 'item',
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
  $cat->add_op( 
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
