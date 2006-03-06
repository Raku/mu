# pX/Common/iterator_engine_p6compiler.pl - fglock
#
# experimental implementation of a p6 compiler
# 

use strict;
use warnings;

require 'p6rule.pl';
require 'emit.pl';

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

    my  $immediate_statement_precompiled = ::compile_rule( q(
            <?ws>? <@grammar1::statements> <?ws>?
        ), {print_ast=>0} );
        
    *immediate_statement_rule = sub {
        #print "immediate_statement: parse ###$_[0]###\n";
        my $match = $immediate_statement_precompiled->( @_ );
        #print "immediate_statement: BEGIN AST: \n", Dumper( $match->{capture} );
        return $match;
    };

    *immediate_statement_exec = sub {
        my $match = immediate_statement_rule( @_ );
        # print "immediate_statement_exec: BEGIN AST: \n", Dumper( $match->{capture} );
        return $match unless $match->{bool};
        # print "immediate_statement_exec: BEGIN AST: \n", Dumper( $match->{capture} );
        my $program = Perl6Grammar::emit( $match->{capture} );
        #print "immediate_statement_exec: matching ###$_[0]###\n";
        #print "immediate_statement_exec: eval'ing code:\n###$program###\n";
        no strict 'refs';
        my $code = eval($program);
        print "Error in statement:\n", $program if $@;
        die "error in immediate_statement_exec: " . $@
            if $@;
        # print "immediate_statement_exec: CODE[ $code ]\n";
        return {
            %$match,
            capture => [ { perl5 => $program } ],
        }
    };


    *grammar = ::compile_rule( <<'__p6__' ); 
        <immediate_statement_exec>*
__p6__


    *rule_decl = ::compile_rule( <<'__p6__' );
        rule <ws> <ident> <ws>? \{ <rule> \}  
            { return { rule_decl => $() ,} }
__p6__


    push @grammar1::statements, \&rule_decl;
    # done bootstrapping!
    
    # improve the grammar a little:
    # - 'grammar' declaration
    # - 'push' - add terms into the grammar tables
    Perl6Grammar::compile( <<'__p6__' , {print_ast=>0} );
        rule grammar1::grammar_name { 
            grammar <ws> <ident> <ws>? \;
                { return { grammar_name => $() ,} }
        }
        rule grammar1::_push {
            $op := (push|unshift) <ws> <variable> <ws>? \, <ws>? $code := (.*?) <ws>? \;
                { return { _push => $() ,} }
        }
__p6__
    push @grammar1::statements, \&grammar_name;
    push @grammar1::statements, \&_push;
    
    # the remaining grammar can be written using itself

    # load/precompile Prelude

    my $prelude_file = 'p6prelude';
    my $recompile;
    if ( -f "$prelude_file-cached.pl" ) {
        $recompile = 
            -M "$prelude_file-cached.pl" > 
            -M "$prelude_file.p6";
    }
    else {
        $recompile = 1;
    }
    if ( $recompile ) {
        local $/ = undef; 
        print "* precompiling Prelude: $prelude_file.p6\n";
        open( FILE, "<", "$prelude_file.p6" ) or 
            die "can't open prelude file: $prelude_file.p6 - $!";
        my $prelude = <FILE>;
        # print "Prelude:$prelude\n";
        my $perl5 = Perl6Grammar::compile( $prelude );
        # print "MATCH\n", Dumper($match), "END MATCH\n";
        print "* caching Prelude: $prelude_file-cached.pl\n";
        open( FILE, ">", "$prelude_file-cached.pl" ) or
            die "can't open prelude file: $prelude_file-cached.pl - $!";
        print FILE "# generated file - do not edit!\n" . $perl5;
        close FILE;
    }
    else {
        print "* loading Prelude: $prelude_file-cached.pl\n";
        require "$prelude_file-cached.pl";
    }


    {
        my $filename = shift || die "no filename";
        local $/ = undef; 
        print "* compiling: $filename\n\n";
        open( FILE, "<", $filename ) or 
            die "can't open file: $filename - $!";
        my $source = <FILE>;
        my $perl5 = Perl6Grammar::compile( $source ); 
        # , { print_ast => 1 } );
        # print "MATCH\n", Dumper($match), "END MATCH\n";
    }

    exit;

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
                { return { assignment => [ $<lvalue>, $<rvalue> ] ,} }
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
# perl5 code generated by p6grammar.pl - fglock

use strict;
use warnings;
require 'iterator_engine.pl';
require 'p6rule_lib.pl';

EOT
}

# compile( $source, {flag=>value} );
#
# flags:
#   print_program=>1 - prints the generated program
#
sub compile {
    #print "compile: matching: \n$_[0]\n";
    my $match = grammar1::grammar->( $_[0] );
    #print "compile: matched.\n";
    my $flags = $_[1];
    die "compile: syntax error in program '$_[0]' at '" . $match->{tail} . "'\n"
        if $match->{tail};
    die "compile: syntax error in program '$_[0]'\n"
        unless $match->{bool};
    print "compile: generated ast:\n", Dumper( $match->{capture} ) if $flags->{print_ast};
    my $program = emit( $match->{capture} );
    print "compile: generated code:\n$program" if $flags->{print_program};
    return $program;

}

sub get_data {
    match::get( { capture => $_[0] }, $_[1] ) 
}
sub get_str {
    match::str( get_data( @_ ) )
}

# XXX not used - intended to bind variables in a macro, when it returns an AST
#     instead of string
sub bind_variable {
    my $n = $_[0];
    my ( $var_name, $value ) = @_;
    #print ref($n),"\n";
    if ( ref( $n ) eq 'ARRAY' ) {
        bind_variable( $_, @_[1,2] ) for @$n;
    }
    elsif ( ref( $n ) eq 'HASH' ) 
    {
        #print Dumper($n);
        my ( $k ) = keys %$n;
        my $v = $n->{$k};
        #print "*** $k, $v \n";
        #return unless defined $k;
        return bind_variable( $v, @_[1,2] ) if ref( $v );
        if ( $k eq 'variable' && $v eq $_[1] ) {
            #print "subst $k $v @_[1,2] ",$_[0]->{$k}, "\n";
            $_[0]->{$k} = $_[2];
        }
    }
}

} # /package

1;

