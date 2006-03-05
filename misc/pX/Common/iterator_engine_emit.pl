use strict;
package Perl6Grammar;
our %nodes;
sub node {
	$nodes{$_[0]} = $_[1];
}
sub emit_node {
	die 'unknow node type' unless $nodes{$_[0]};
	return $nodes{$_[0]}->($_[1]);
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
node 'pod',sub {
	return ''
};
node 'ws',sub {
	return ''
};
node 'grammar_name',sub {
    my $ident = get_str( $_[0], '$<ident>' );
    return "package $ident;\n";
};
node 'rule_decl',sub {
    my $name = get_str( $_[0], '$<ident>' );
    my $program = main::emit_rule( get_data( $_[0], '$<rule>' ), '' );
    return "*{'$name'} = \n$program;\n";
};
node 'block',sub {
    return "    {\n" . emit($_[0]) . "    }\n";
};
node 'sub_decl',sub {
    my $fix =   get_str( $_[0], '$<fix>' );
    my $id =    get_str( $_[0], '$<id>' );
    my $block = get_data( $_[0], '$<block>' );
    # XXX - register fixity in grammar
    return 
        # "    { no strict 'refs';\n" .
        "    *{'$fix:<$id>'} = sub\n" . emit($block) . "    ;\n" .
        # "    }\n" .
        "    push \@grammar1::ops, ::compile_rule( '" .
            quotemeta( $fix . ':<' . $id . '>' ) . "' );\n";
};
node 'sub_application',sub {
    my $term1 = emit( get_data( $_[0], '$<term1>' ) );
    my $op =    get_str( $_[0], '$<op>' );
    my $term2 = emit( get_data( $_[0], '$<term2>' ) );
    return 
        "    &{'$op'} ( $term1, $term2 );\n";
};
node 'sub_application_term',sub {
    my $term1 = emit( get_data( $_[0], '$<term1>' ) );
    my $op =    get_str( $_[0], '$<op>' );
    my $term2 = emit( get_data( $_[0], '$<term2>' ) );
    return 
        "    &{'$op'} ( $term1, $term2 )\n";
};
node '_push',sub {
    my $op =   get_str( $_[0], '$<op>' );
    my $name = get_str( $_[0], '$<variable>' );
    my $code = get_str( $_[0], '$<code>' );
    return "    $op $name, $code;\n";
};
node '_simple_statement',sub {
    my $op = get_str( $_[0], '$<op>' );
    $op = 'die "not implemented"' if $op eq '...';
    return "    $op;\n";
};
node '_my',sub {
    my $op =   get_str( $_[0], '$<op>' );
    my $name = get_str( $_[0], '$<variable>' );
    return "    $op $name;\n";
};
node '_return',sub {
    my $val = emit( get_data( $_[0], '$<val>' ) );
    return "    return $val;\n";
};
node '_print',sub {
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
};
node 'term1',sub {
    return $_[0] unless ref($_[0]);
    return emit( $_[0]->[0]);
};
node 'term2',sub {
    return $_[0] unless ref($_[0]);
    return emit( $_[0]->[0]);
};
node 'literal',sub {
    # $_[0] =~ s/([\'])/\\$1/g;
    return "'" . $_[0] . "'";
    # return '"' . quotemeta($_[0]) . '"';
    #$_[0] =~ s/(["\$%@])/\\$1/g;
    #return '"' . $_[0] . '"';
};
node 'eval_perl5',sub {
    return eval emit($_[0]);
};
node 'variable',sub {
    return $_[0];
};
node 'immediate_statement_exec',sub {
    return get_data( $_[0], '$<perl5>' );
};
node 'macro',sub {
    
    #print Dumper( get_data( $_[0], "\$<>" ) );
    
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
    # no parameters? look into the regex
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

    # XXX don't use source filter: variable substitutions $() in the body AST

    # XXX this algorithm depends on variable declaration 
    #     like:  ( $a ) is parsed ( / $a := (.*?) / )

    my $binding = '';
    for ( 0 .. $#args ) {
        my $ident = $args[$_];
        $ident =~ s/^.//;  # no sigil
        $binding .= 
            "    my $args[$_] = " . 
            "match::str( match::get( \$_[0], '\$<$ident>' ) );\n" .
            # "    print \"bound: \\$args[$_] $args[$_] \$src \"; " .
            # "    \$src =~ s/\\\\\\$args[$_]/\\$args[$_]/g; \n" .
            "    $args[$_] =~ s/([\\'])/\\\\\$1/g;\n" .
            "\n";
    }
    # print "macro: var binding: \n$binding";

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
        "    my \$code = sub { \n" .
        # "    print 'matched: ', Dumper( \$_[0]->{capture} ); \n" .
        "    my \$src = <<\'!EOT!\'; \n" . 
        $block . 
        "\n!EOT!\n" .
        $binding .
        #"    print 'eval: ', \$a, '  ', \$src; \n" .
        "    \$src =~ s/([\\'\"])/\\\\\$1/g;\n" .
        "    my \$ret = eval( '\"' . \$src . '\"' ); \n" .
        #"    print \"ret: ###\$ret### \\n\"; \n" .
        "    my \$ast = grammar1::immediate_statement_rule( \$ret )->{capture};\n" .
        "    my \$perl5 = Perl6Grammar::emit( \$ast );\n" .
        #"    print \"perl5: ###\$perl5### \\n\"; \n" .
        "    my \$expanded = eval \$perl5;\n" .
        #"    print \"expanded: ###\$expanded### \\n\"; \n" .
        "    return \$expanded;\n" .
        "    };\n" .
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

    #print "macro: expanded:\n$res";
    return $res;
};
