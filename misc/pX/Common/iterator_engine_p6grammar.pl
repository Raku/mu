# pX/Common/iterator_engine_p6.pl - fglock
#
# experimental implementation of a grammar that could parse p6 
# files, like pge/P6Rule.grammar

use strict;
use warnings;

require 'iterator_engine_p6rule.pl';

# XXX - make grammars inherit from Grammar; make grammars inheritable
# XXX - write an emitter that generates perl5 regexes (or dies)
# XXX - add (API/documentation) to generate unnamed rules, unnamed grammars
# XXX - fix the extra commas in the generated code
# XXX - create error messages for compiling errors

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Pad = '# ';

{
    package grammar1;

    no warnings 'once';
    use vars qw( @statements @terms @ops );

    # bootstrap the 'grammar' syntax
    *grammar = ::compile_rule( <<'__p6__' ); 
        [ <ws>? <@grammar1::statements> ]* <ws>?
__p6__
    *rule_decl = ::compile_rule( <<'__p6__' );
        rule <ws> <ident> <ws>? \{ <rule> \}  
            { return { rule_decl => $<> ,} }
__p6__
    push @grammar1::statements, \&rule_decl;
    # done bootstrapping!
    
    # improve the grammar a little:
    # - 'grammar' declaration
    # - 'push' - add terms into the grammar tables
    Perl6Grammar::compile( <<'__p6__' , {print_ast=>0} )->();
        rule grammar1::grammar_name { 
            grammar <ws> <ident> <ws>? \;
                { return { grammar_name => $<> ,} }
        }
        rule grammar1::_push {
            $op := (push|unshift) <ws> <variable> <ws>? \, <ws>? $code := (.*?) <ws>? \;
                { return { _push => $<> ,} }
        }
__p6__
    push @grammar1::statements, \&grammar_name;
    push @grammar1::statements, \&_push;
    
    # the remaining grammar can be written using itself
    Perl6Grammar::compile( <<'__p6__' , {print_program=>0});
    
        grammar grammar1;
        
        rule term1 {
            <@grammar1::terms>
        }
        
        push @terms, \&variable;
        push @terms, \&literal;
        
        rule pod { 
            \=[pod|head1|kwid] 
            .*? 
            \=cut 
        }
        
        rule list {
            [ <term1> <ws>? \, <ws>? ]* <term1>?
        }
        
        rule _print { 
            $op := (print|say|warn|die) <ws> <list> <ws>? \;
                { return { _print => $<> ,} }
        }
        push @statements, \&_print;
        
        rule _my {
            $op := (my|our|local) <ws> <variable> <ws>? \;
                { return { _my => $<> ,} }
        }
        push @statements, \&_my;
        
        rule _simple_statement {
            $op := (die|\.\.\.) \;
                { return { _simple_statement => $<> ,} }
        }
        push @statements, \&_simple_statement;
        
        rule block {
            \{ <grammar> \}
                { return { block => $<grammar> ,} }
        }
        push @statements, \&block;
        
        rule sub_decl {
            sub <ws> $fix := (infix|prefix|postfix) \: \< $id := (.*?) \> <ws>? <block>
                { return { sub_decl => $<> ,} }
        }
        push @statements, \&sub_decl;
    
        rule term2 {
            $term1 := (<term1>) <ws>? 
            $op    := (<@grammar1::ops>) <ws>? 
            $term2 := (<term1>) 
                { return { sub_application_term => $<> ,} }
        }
        
        rule sub_application {
            $term1 := (<term1>|<term2>) <ws>? 
            $op    := (<@grammar1::ops>) <ws>? 
            $term2 := (<term1>|<term2>) <ws>? \;
                { return { sub_application => $<> ,} }
        }
        push @statements, \&sub_application;
        
        rule eval_perl5 {
            eval <ws>? \( <ws>? 
                <literal> <ws>? \, <ws>? 
                \: lang \< perl5 \> <ws>? 
            \) <ws>? \;
                { return { eval_perl5 => $<literal> } }
        }
        push @statements, \&eval_perl5;

__p6__

=for TODO
    prelude - pre-compile;
    reimplement print(), warn ... using 'sub'
    implement eval_perl6 and eval_block 
    
    operand fixity (infix, prefix...)
    operand precedence (+, *, ln)
    
    class
    
    find out how to change syntax while in the parse-and-generate-ast phase
    (for example, when a new sub is created)
    
    macros
=cut

    Perl6Grammar::compile( q(
        say 'compiling Prelude';
        sub infix:<*> { eval(' $_[0] * $_[1] ', :lang<perl5>); }
        sub infix:<+> { eval(' $_[0] + $_[1] ', :lang<perl5>); }
    ), {print_program=>1, print_ast=>0});

    # my $match = $ops[0]->( 'infix:<+>' );
    # print "match: \n", Dumper( $match );

    Perl6Grammar::compile( q(
        '1' infix:<+> '1';
        '1' infix:<*> '1';
        '1' infix:<+> '1' infix:<*> '1';
        '1' infix:<*> '1' infix:<+> '1';
    ), {print_program=>1, print_ast=>0});
    
    Perl6Grammar::compile( q(
        my $a;
        print 'hello, ';
        say 'world!';
        { say 'in block'; }
        warn 'hi';
        ...;
    ), {print_program=>1, print_ast=>0});
    
=for later
        rule sub_application {
            <@grammar1::terms> <ws>? <@grammar1::ops> <ws>? <@grammar1::terms>
        }
        push @terms, \&sub_application;
    
        # XXX - this doesn't work
        #       say sub { print 'anonymous sub'; } ;
        rule anon_sub {
            sub <block>
                { return { anon_sub => $<block> ,} }
        }
        push @terms, \&anon_sub;


        rule assignment {
            $lvalue := (<variable>) <ws>? \= <ws>? $rvalue := (<variable>) <ws>? \;
                { return { assignment => [ $<$lvalue>, $<$rvalue> ] ,} }
        }
        unshift @terms, \&assignment;
        rule eval_perl5 {
            eval <ws>? \( <ws>? \" <code> \" <ws>? \, <ws>? \:lang\<perl5\> <ws>? \) <ws>? \;
        }
    # sub print ... 
    # sub 'Y' - is assoc 'list'
    # sub infix:<+> { eval( '$_[0]+$_[1]', :lang<perl5> ) }

    #        print '1' + '1';
    #    $a = $b;
    # TODO - $a = $b; - see 'rule assignment' above
    # TODO - rule comment { \# .*? [<newline>|$$] }
=cut

}

# ------ emitter

my $namespace = 'grammar1::';

{
  package Perl6Grammar;
  use Data::Dumper; 

sub header {
    return <<EOT;
#! perl
#
# grammar file
# perl5 code generated by iterator_engine_p6grammar.pl - fglock

use strict;
use warnings;
require 'iterator_engine.pl';
require 'iterator_engine_p6rule_lib.pl';

EOT
}

# compile( $source, {flag=>value} );
#
# flags:
#   print_program=>1 - prints the generated program
#
sub compile {
    #print "matching: \n$_[0]\n";
    my $match = grammar1::grammar->( $_[0] );
    #print "matched.\n";
    my $flags = $_[1];
    die "syntax error in program '$_[0]' at '" . $match->{tail} . "'\n"
        if $match->{tail};
    die "syntax error in program '$_[0]'\n"
        unless $match->{bool};
    print "generated ast:\n", Dumper( $match->{capture} ) if $flags->{print_ast};
    my $program = emit( $match->{capture} );
    print "generated code:\n$program" if $flags->{print_program};
    no strict 'refs';
    my $code = eval($program); die $@ if $@;
    return $code;
}

sub emit 
{
    my $n = $_[0];
    # local $Data::Dumper::Indent = 0;
    # print "emit: ", ref($n)," ",Dumper( $n ), "\n";

    # $n = $n->{match};

    if ( ! defined $n || $n eq '' ) {
        # empty node; maybe a <null> match
        return;
    }
    if ( ref( $n ) eq 'ARRAY' ) {
        my @s;
        for ( @$n ) {
            push @s, emit( $_ );
        }
        return join( '', @s ) ;
    }
    elsif ( ref( $n ) eq 'HASH' ) 
    {
        my ( $k, $v ) = each %$n;
        #print "$k => $v \n";

        if ( $k eq 'pod' ) {
            return;
        }
        if ( $k eq 'ws' ) {
            return;
        }
        if ( $k eq 'grammar_name' ) {
            my $name = match::str( match::get( 
                { capture => $v }, 
                '$<ident>'
            ) );
            return "package $name;\n";
        }
        if ( $k eq 'rule_decl' ) {
            my $name = match::str( match::get( 
                { capture => $v }, 
                '$<ident>'
            ) );
            my $program = main::emit_rule(
                    match::get( 
                        { capture => $v }, 
                        '$<rule>'
                    ), '' );
            return "*$name = \n$program;\n";
        }
        if ( $k eq 'block' ) {
            #print "Code: \n", Dumper($v);
            #my $code = match::get( 
            #    { capture => $v }, 
            #    '$<grammar>'
            #);
            # print "Code: \n", Dumper($v);
            return "    {\n" . emit($v) . "    }\n";
        }
        if ( $k eq 'sub_decl' ) {
            my $fix = match::str( match::get( 
                { capture => $v }, 
                '$<$fix>'
            ) );
            my $id = match::str( match::get( 
                { capture => $v }, 
                '$<$id>'
            ) );
            my $block = match::get( 
                { capture => $v }, 
                '$<block>'
            );
            # XXX - register fixity in grammar
            return 
                # "    { no strict 'refs';\n" .
                "    *{'$fix:<$id>'} = sub\n" . emit($block) . "    ;\n" .
                # "    }\n" .
                "    push \@grammar1::ops, ::compile_rule( '" .
                    quotemeta( $fix . ':<' . $id . '>' ) . "' );\n";
        }
        if ( $k eq 'sub_application' ) {
            #print 'sub_application', Dumper($v); 
            my $term1 = emit( match::get( 
                { capture => $v }, 
                '$<$term1>'
            ) );
            my $op = match::str( match::get( 
                { capture => $v }, 
                '$<$op>'
            ) );
            my $term2 = emit( match::get( 
                { capture => $v }, 
                '$<$term2>'
            ) );
            return 
                "    &{'$op'} ( $term1, $term2 );\n";
        }
        if ( $k eq 'sub_application_term' ) {
            #print 'sub_application', Dumper($v); 
            my $term1 = emit( match::get( 
                { capture => $v }, 
                '$<$term1>'
            ) );
            my $op = match::str( match::get( 
                { capture => $v }, 
                '$<$op>'
            ) );
            my $term2 = emit( match::get( 
                { capture => $v }, 
                '$<$term2>'
            ) );
            return 
                "    &{'$op'} ( $term1, $term2 )\n";
        }
        if ( $k eq '_push' ) {
            my $op = match::str( match::get( 
                { capture => $v }, 
                '$<$op>'
            ) );
            my $name = match::str( match::get( 
                { capture => $v }, 
                '$<variable>'
            ) );
            my $code = match::str( match::get( 
                { capture => $v }, 
                '$<$code>'
            ) );
            return "    $op $name, $code;\n";
        }
        if ( $k eq '_simple_statement' ) {
            my $op = match::str( match::get( 
                { capture => $v }, 
                '$<$op>'
            ) );
            $op = 'die "not implemented"' if $op eq '...';
            return "    $op;\n";
        }
        if ( $k eq '_my' ) {
            my $op = match::str( match::get( 
                { capture => $v }, 
                '$<$op>'
            ) );
            my $name = match::str( match::get( 
                { capture => $v }, 
                '$<variable>'
            ) );
            return "    $op $name;\n";
        }
        if ( $k eq '_print' ) {
            #my $list = match::get( 
            #    { capture => $v }, 
            #    '$<list>'
            #);
            my $op = match::str( match::get( 
                { capture => $v }, 
                '$<$op>'
            ) );
            my $list = match::get( 
                { capture => $v }, 
                '$<list>'
            );
            #print "print: $op ", Dumper($list);
            my $cmd = "    print";
            $cmd = "    warn" if $op eq 'warn';
            my $s;
            for ( @$list ) {
                next unless ref($_) eq 'HASH';
                # print "print: ", Dumper($_);
                # my ($k, $v) = %{ $_->{term1}[0] };
                my $s1 = emit($_);
                $s .= "$cmd $s1;\n"
                    if $s1;
            }
            return $s . "$cmd \"\\n\";\n" if $op eq 'say';
            return $s;
        }
        if ( $k eq 'term1' || $k eq 'term2' ) {
            return emit($v->[0]);
        }
        if ( $k eq 'literal' ) {
            return '"' . quotemeta($v) . '"';
        }
        if ( $k eq 'eval_perl5' ) {
            return eval emit($v);
        }
        if ( $k eq 'variable' ) {
            return $v;
        }
        die "unknown node: ", Dumper( $n );
    }
    die "unknown node: ", Dumper( $n );
}

} # /package

1;
