use strict;
package Perl6Grammar;
our %nodes;

#{
#  no warnings 'redefine';
#  sub Pugs::Runtime::LrepRule::rule_wrapper { @_ };
#}
my $grammar1 = 'Pugs::Grammar::Rule::';

sub emit_node {
	die "unknown node type: $_[0]" unless $node::{$_[0]};
	no strict 'refs';
	return &{"node::$_[0]"}($_[1]);
}
sub emit {
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
        my ( $k ) = keys %$n;
        my $v = $n->{$k};
        return '' unless defined $k;
	return emit_node($k,$v);
    }
    die "unknown node: ", Dumper( $n );
}
sub node::pod {
	return ''
}
sub node::ws {
	return ''
}
sub node::grammar_name {
    my $ident = get_str( $_[0], '$<ident>' );
    return "package $ident;\n" .
           "use base 'Pugs::Grammar::LrepBase';\n" .
           "use Pugs::Runtime::LrepRule;\n";
}
sub node::rule_decl {
    my $name = get_str( $_[0], '$<ident>' );
    my $program = main::emit_rule( get_data( $_[0], '$<rule>' ), '     ' );
    return "*{'Pugs::Grammar::Rule::$name'} = \n" .
        "sub {\n" . 
        "    my \$grammar = shift;\n" .
        #"    warn 'rule argument is undefined' unless defined \$_[0];\n" .
        "    \$_[0] = '' unless defined \$_[0];\n" .
        #"    print 'GRAMMAR: \$grammar', \"\\n\";\n" .
        "    package Pugs::Runtime::LrepRule;\n" .
        "    rule_wrapper( \$_[0], (\n" . 
        $program . 
        "        )->( \@_ )\n" .
        "    )\n" .
        "};\n" .
        "";
    # XXX - 'hard' replace $grammar references
    # $source =~ s/\$grammar/$grammar/gs;
    # print $source;
}
sub node::perl5_rule_decl {
    # XXX - this sub does only '1' capture
    my $name = get_str( $_[0], '$<ident>' );
    my $regex = get_str( $_[0], '$<perl5_regex>' );
    #print "perl5_rule_decl: $name -- $regex \n";
    my $program =
    "*{'Pugs::Grammar::Rule::$name'} = sub {" . '
    my $grammar = shift;
    #warn "rule argument is undefined" unless defined $_[0];
    $_[0] = "" unless defined $_[0];
    my $bool = $_[0] =~ /'.$regex.'(.*)$/sx;
    return {
        bool  => $bool,
        match => $1,
        tail  => $2,
        #capture => $1,
    }
};
';
    return $program;
}
sub node::block {
    return "    {\n" . emit($_[0]) . "    }\n";
}
sub node::sub_decl {
    my $fix =   get_str( $_[0], '$<fix>' );
    my $id =    get_str( $_[0], '$<id>' );
    my $block = get_data( $_[0], '$<block>' );
    # XXX - register fixity in grammar
    return 
        # "    { no strict 'refs';\n" .
        "    *{'$fix:<$id>'} = sub\n" . emit($block) . "    ;\n" .
        # "    }\n" .
        "    push \@$grammar1::ops, ::compile_rule( '" .
            quotemeta( $fix . ':<' . $id . '>' ) . "' );\n";
}
sub node::sub_application {
    my $term1 = emit( get_data( $_[0], '$<term1>' ) );
    my $op =    get_str( $_[0], '$<op>' );
    my $term2 = emit( get_data( $_[0], '$<term2>' ) );
    return 
        "    &{'$op'} ( $term1, $term2 );\n";
}
sub node::sub_application_term {
    my $term1 = emit( get_data( $_[0], '$<term1>' ) );
    my $op =    get_str( $_[0], '$<op>' );
    my $term2 = emit( get_data( $_[0], '$<term2>' ) );
    return 
        "    &{'$op'} ( $term1, $term2 )\n";
}
sub node::_push {
    my $op =   get_str( $_[0], '$<op>' );
    my $name = get_str( $_[0], '$<variable>' );
    my $code = get_str( $_[0], '$<code>' );
    $name =~ s/^\@/\@$grammar1/;
    $code =~ s/^\\&/\\&$grammar1/;
    return "    $op $name, $code;\n";
}
sub node::_simple_statement {
    my $op = get_str( $_[0], '$<op>' );
    $op = 'warn "not implemented"' if $op eq '...';
    return "    $op;\n";
}
sub node::_my {
    my $op =   get_str( $_[0], '$<op>' );
    my $name = get_str( $_[0], '$<variable>' );
    return "    $op $name;\n";
}
sub node::_return {
    my $val = emit( get_data( $_[0], '$<val>' ) );
    return "    return $val;\n";
}
sub node::_print {
    my $op =   get_str( $_[0], '$<op>' );
    my $list = get_data( $_[0], '$<list>' );
    my $cmd = "    print";
    $cmd =    "    warn" if $op eq 'warn';
    my $s;
    for ( @$list ) {
        next unless ref($_) eq 'HASH';
        my $s1 = emit($_);
        $s .= "$cmd $s1;\n"
            if $s1;
    }
    return $s . "$cmd \"\\n\";\n" 
        if $op eq 'say';
    return $s;
}
sub node::term1 {
    return $_[0] unless ref($_[0]);
    return emit( $_[0]->[0]);
}
sub node::term2 {
    return $_[0] unless ref($_[0]);
    return emit( $_[0]->[0]);
}
sub node::literal {
    # $_[0] =~ s/([\'])/\\$1/g;
    return "'" . $_[0] . "'";
    # return '"' . quotemeta($_[0]) . '"';
    #$_[0] =~ s/(["\$%@])/\\$1/g;
    #return '"' . $_[0] . '"';
}
sub node::eval_perl5 {
    my $res = eval emit($_[0]);
    print "Error in eval_perl5:\n", $_[0] if $@;
    die $@ if $@;
    return $res;
}
sub node::variable {
    return $_[0];
}
sub node::immediate_statement_exec {
    return get_data( $_[0], '$<perl5>' );
}
sub node::macro {
    
            #print Dumper( get_data( $v, "\$()" ) );

            # implementation note: 
            #     $rule and $list are AST
            #     $block is plain text
            
            my ($prefix, $id, $list, $rule, $block) = 
                map { get_data( $_[0], "\$<$_>" ) } 
                qw( prefix id list rule code );
            $prefix = match::str( $prefix );
            $id     = match::str( $id );
            $block  = match::str( $block );
            my @args;
            for ( @$list ) {
                next unless ref($_) eq 'HASH';
                push @args, match::str( $_ );  # emit($_);
            }
            # no parameters? look into the rule for capturable things
            # XXX - more things to look into:
            #     if there is a return-block, then it should be used instead
            unless ( @args ) {
                my %h;
                for ( @$rule ) {
                    next unless ref($_) eq 'HASH';
                    next unless exists $_->{named_capture};
                    
                    for ( @{ $_->{named_capture} } ) {
                        next unless ref($_) eq 'HASH';
                        # XXX - if there are multiple definitions, it is an array
                        my $name = '$' . match::str( $_ );  
                        $h{$name} = 1;
                        last;
                    }
                }
                @args = keys %h;
            } 

            # my $rule_code = main::emit_rule( $rule, '' );
            # print "macro: $prefix / $id \n";  #, Dumper($list);
            # print "macro: args = @args\n";
            # print "macro: rule = \n$rule_code\n";
            # print "macro: block = \n", match::str($block),"\n";
    
            # XXX don't use source filter: 
            #     instead, do variable substitutions '$()' in the body's AST
    
            # XXX macro variables can be AST 
            #     - it all depends on the is-parsed rule (TimToady on #perl6)

            my $binding = '';
            for ( 0 .. $#args ) {
                my $ident = $args[$_];
                $ident =~ s/^.//;  # no sigil
                $binding .= 
                    #"    print \"binding: \\n\" . Dumper( match::get( \$_[0], '\$<$ident>' ) );\n" .
                    "    my $args[$_] = " . 
                    "match::str( match::get( \$_[0], '\$<$ident>' ) );\n" .
                    # "    \$src =~ s/\\\\\\$args[$_]/\\$args[$_]/g; \n" .
                    "    $args[$_] =~ s/([\\'\\\\])/\\\\\$1/g;\n" .
                    #"    print \"bound: \\$args[$_] $args[$_] \$src \"; " .
                    "\n";
            }
            # print "macro: var binding: \n$binding";

            # -- end of macro compile-time processing
    
            # emit the macro expander code

            local $Data::Dumper::Pad = '    ' x 2;
            local $Data::Dumper::Terse = 1;
            my $res = 
    
                "*{'$prefix:<$id>'} = sub {\n" .
                "    my \$rule = concat( \n" .
                "        constant( '$prefix:<$id>' ),\n" .
                "        \\&$grammar1::ws_star,\n" .
                main::emit_rule( $rule ) .
                "    );\n" .
    
                #"    my \$body_ast = \n" .
                #Data::Dumper->Dump( $block ) .
                #"    ;\n" .
    
                "    my \$match = \$rule->( \@_ );\n" .
                "    return unless \$match;\n" .
                "    my \$code = sub { \n" .
                # "    print 'matched: ', Dumper( \$_[0]->{capture} ); \n" .
                "    my \$src = <<\'!EOT!\'; \n" . 
                $block . 
                "\n!EOT!\n" .
                $binding .
                #"    print 'eval: ', \$a, '  ', \$src; \n" .
                "    \$src =~ s/([\\'\"\\\\])/\\\\\$1/g;\n" .
                "    my \$ret = eval( '\"' . \$src . '\"' ); \n" .
                #"    print \"Error in macro eval:\n\", \$src if \$\@; \n" .
                "    die \$@ if \$\@; \n" .
                #"    \$ret =~ s/([\\'\"])/\\\\\$1/g;\n" .
                #"    print \"ret: ###\$ret### \\n\"; \n" .
                "    my \$ast = $grammar1::immediate_statement_rule( \$ret );\n" .
                "    die \"compile: syntax error in macro at '\" . \$ast->{tail} . \"'\\n\"\n" .
                "        if \$ast->{tail};\n" .
                #"    print \"ast: \\n\", Dumper( \$ast->{capture} ); \n" .
                "    my \$perl5 = Perl6Grammar::emit( \$ast->{capture} );\n" .
                #"    print \"perl5: ###\$perl5### \\n\"; \n" .
                "    my \$expanded = eval \$perl5;\n" .
                #"    print \"Error in expanded macro eval:\n\", \$perl5 if \$\@; \n" .
                "    die \$@ if \$\@; \n" .
                #"    print \"expanded: ###\$expanded### \\n\"; \n" .
                "    my \$final_ast = \n" .
                "        ::compile_rule( q( [ <?ws>? <\@$grammar1::statements> ]* <?ws>? ) )\n" .
                "        ->( \$expanded );\n" .
                "    die \"compile: syntax error in macro at '\" . \$final_ast->{tail} . \"'\\n\"\n" .
                "        if \$final_ast->{tail};\n" .
                #"    print \"final ast: \\n\", Dumper(\$final_ast->{capture}); \n" .
                "    return \$final_ast;\n" .
                #"    my \$perl5_final = Perl6Grammar::emit( \$final_ast->{capture} );\n" .
                #"    print \"perl5_final: ###\$perl5_final### \\n\"; \n" .
                #"    return \$perl5_final;\n" .
                "    };\n" .
                "    my \$ast = \$code->( \$match ); \n" .
                #"    print \"tail: \", \$match->{tail}, \"\\n\"; " .
                "    return { \%\$match, capture => [ \$ast->{capture} ] }; \n" .
                "};\n";
    
            # register new syntax in the grammar category
    
            # example: macro statement_control:<if> ($expr, &ifblock) {...}
            # XXX - this is very rough
            my $category = $prefix;
            $category = 'statements' if $prefix eq 'statement_control';
    
            $res .= "    push \@$grammar1::$category, \\&{'$prefix:<$id>'};\n";
    
            #print "macro: expanded:\n$res";
            return $res;
};

1;
