# pX/Common/iterator_engine_p6compiler.pl - fglock
#
# experimental implementation of a p6 compiler
# 

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


    my $immediate_statement_rule;
    *immediate_statement = sub {
        $immediate_statement_rule ||= ::compile_rule( q(
            <?ws>? <@grammar1::statements> <?ws>?
        ), {print_ast=>0} );
        # print "matching #$_[0]#\n";
        my $match = $immediate_statement_rule->( @_ );
        # print "BEGIN AST: \n", Dumper( $match->{capture} );
        return $match unless $match->{bool};
        # print "BEGIN AST: \n", Dumper( $match->{capture} );
        my $program = Perl6Grammar::emit( $match->{capture} );
        # print "eval'ing code:\n$program";
        no strict 'refs';
        my $code = eval($program);
        die "error in statement: " . $@
            if $@;
        # print "CODE[ $code ]\n";
        return {
            %$match,
            capture => [ { perl5 => $program } ],
        }
    };


    *grammar = ::compile_rule( <<'__p6__' ); 
        [ <?ws>? <immediate_statement> ]* <?ws>?
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
    Perl6Grammar::compile( <<'__p6__' , {print_ast=>0} );
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

    # load/precompile Prelude

    my $prelude_file = 'iterator_engine_p6prelude';
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

    return $program;

    # no strict 'refs';
    # my $code = eval($program); die $@ if $@;
    # return $code;
}

sub emit 
{
    my $n = $_[0];
    # local $Data::Dumper::Indent = 0;
    # print "emit: ", ref($n)," ",Dumper( $n ), "\n";

    # $n = $n->{match};

    if ( ! defined $n || ref($n) eq '' ) {
        # empty node; maybe a <null> match
        return '';
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
            return '';
        }
        if ( $k eq 'ws' ) {
            return '';
        }
        if ( $k eq 'grammar_name' ) {
            my $name = match::str( match::get( 
                { capture => $v }, 
                '$<ident>'
            ) );
            return "package $name;\n";
        }
        if ( $k eq 'rule_decl' ) {
            #print "Rule: \n", Dumper($v);
            my $name = match::str( match::get( 
                { capture => $v }, 
                '$<ident>'
            ) );
            # print "rule: name $name\n";
            my $program = main::emit_rule(
                    match::get( 
                        { capture => $v }, 
                        '$<rule>'
                    ), '' );
            # print "rule: program $program\n";
            # my $s = "*{'" . quotemeta($name) ."'} = \n$program;\n";
            my $s = "*{'$name'} = \n$program;\n";
            # print "rule: p5: $s";
            return $s;
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
        if ( $k eq '_return' ) {
            # print "return: ", Dumper($v);
            my $val = emit( match::get( 
                { capture => $v }, 
                '$<$val>'
            ) );
            return "    return $val;\n";
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
            return $s . "$cmd \"\\n\";\n" 
                if $op eq 'say';
            return $s;
        }
        if ( $k eq 'term1' || $k eq 'term2' ) {
            return emit($v->[0]);
        }
        if ( $k eq 'literal' ) {
            # return '"' . quotemeta($v) . '"';
            $v =~ s/(["\$%@])/\\$1/g;
            return '"' . $v . '"';
        }
        if ( $k eq 'eval_perl5' ) {
            # print "eval_perl5: $v\n";
            return eval emit($v);
        }
        if ( $k eq 'variable' ) {
            return $v;
        }
        if ( $k eq 'immediate_statement' ) {
            # print "immediate_statement\n", Dumper($v);
            # return '    print "immediate_statement\n";' . "\n";
            return $v->[0]{perl5};
        }
        if ( $k eq 'macro' ) {
            my ($prefix, $id, $list, $rule, $block) = map { match::get( 
                { capture => $v }, 
                "\$<$_>"
            ) } qw( $prefix $id list rule block );
            $prefix = match::str( $prefix );
            $id = match::str( $id );
            my @args;
            for ( @$list ) {
                next unless ref($_) eq 'HASH';
                push @args, match::str( $_ );  # emit($_);
            }

            print "XXX - macro '$id' parameters are just ignored for now\n" 
                if @args;

            # my $rule_code = main::emit_rule( $rule, '' );
            my $block_code = emit( $block );
            # print "macro: $prefix / $id \n";  #, Dumper($list);
            # print "macro: args = @args\n";
            # print "macro: rule = \n$rule_code\n";
            print "macro: block = \n$block_code\n";

            # XXX TODO: variable substitutions $() in the body AST

            # emit the rule
            local $Data::Dumper::Pad = '    ' x 2;
            local $Data::Dumper::Terse = 1;
            my $res = 

                "*{'$prefix:<$id>'} = sub {\n" .
                "    my \$rule = ruleop::concat( \n" .
                "        ruleop::constant( '$prefix:<$id>' ),\n" .
                "        \\&grammar1::ws_star,\n" .
                main::emit_rule( $rule ) .
                "    );\n" .
                #"    my \$body_ast = \n" .
                #Data::Dumper->Dump( $block ) .
                #"    ;\n" .

                "    my \$match = \$rule->( \@_ );\n" .
                "    return unless \$match;\n" .
                "    my \$code = sub \n" .
                $block_code .
                "    ;\n" .
                "    return {\n" .
                "        \%\$match,\n" .
                "        capture => [ \n" . 
                "             Perl6Grammar::compile( \$code->( \$match ) ) \n" .
                "        ],\n" .
                "    };\n" .
                "};\n";

            # register new syntax in the grammar category

            # example: macro statement_control:<if> ($expr, &ifblock) {...}
            # XXX - this is very rough
            my $category = $prefix;
            $category = 'statements' if $prefix eq 'statement_control';

            $res .= "    push \@grammar1::$category, \\&{'$prefix:<$id>'};\n";

            print "macro: expanded:\n$res";
            return $res;
        }
        die "unknown node: ", Dumper( $n );
    }
    die "unknown node: ", Dumper( $n );
}

} # /package

1;
