package Pugs::Emitter::Perl6::Perl5;

# p6-ast to perl5 emitter

use strict;
use warnings;
use Data::Dumper;
$Data::Dumper::Indent = 1;

sub _var_name {
    my $s = shift;
    substr($s,1) =~ s/\$/_DOLLAR_/g;
    $s =~ s/\!/_EXCL_/g;
    
    # globals
    $s = '$::_EXCL_' if $s eq '$_EXCL_';  
    return $s;
}

sub _not_implemented {
    my ( $n, $what, $tab ) = @_;
    return "$tab die q(not implemented $what: " . Dumper( $n ) . ")";
}

sub emit {
    my ($grammar, $ast) = @_;
    # runtime parameters: $grammar, $string, $state, $arg_list
    # rule parameters: see Runtime::Rule.pm
    return _emit( $ast, '' );
        #"do{\n" .
        #_emit( $ast, '    ' ) . "\n" .
        #"}";
}

sub _emit {
    my $n = $_[0];
    my $tab = $_[1];
    #warn "_emit: ", Dumper( $n );
    #warn "fixity: $n->{fixity}\n" if exists $n->{fixity};
    
    # 'undef' example: parameter list, in a sub call without parameters
    return ''
        unless defined $n;
    
    die "unknown node: ", Dumper( $n )
        unless ref( $n ) eq 'HASH';
        
    return $n->{int} 
        if exists $n->{int};
        
    return _var_name( $n->{scalar} )
        if exists $n->{scalar};
        
    return $n->{array} 
        if exists $n->{array};
        
    return $n->{hash} 
        if exists $n->{hash};
        
    return '"' . $n->{double_quoted} . '"' 
        if exists $n->{double_quoted};
            
    return '\'' . $n->{single_quoted} . '\'' 
        if exists $n->{single_quoted};
            
    return 'qw(' . $n->{angle_quoted} . ')' 
        if exists $n->{angle_quoted};
            
    return assoc_list( $n, $tab )
        if exists $n->{assoc}  && $n->{assoc}  eq 'list';
        
    return infix( $n, $tab )
        if exists $n->{fixity} && $n->{fixity} eq 'infix';
    return prefix( $n, $tab )
        if exists $n->{fixity} && $n->{fixity} eq 'prefix';
    return postfix( $n, $tab )
        if exists $n->{fixity} && $n->{fixity} eq 'postfix';
    return circumfix( $n, $tab )
        if exists $n->{fixity} && $n->{fixity} eq 'circumfix';
    return postcircumfix( $n, $tab )
        if exists $n->{fixity} && $n->{fixity} eq 'postcircumfix';

    return statement( $n, $tab )
        if ref $n->{op1} && exists $n->{op1}{stmt};

    return default( $n, $tab );
}

sub assoc_list {
    my $n = $_[0];
    my $tab = $_[1];
    # print "list emit_rule: ", Dumper( $n );

    if ( $n->{op1} eq ';' ||
         $n->{op1} eq ',' ) {
        return join ( $n->{op1} . "\n", 
            map { _emit( $_, $tab ) } @{$n->{list}} 
        );
    }
    
    return _not_implemented( $n->{op1}, "list-op", $tab );
}

sub _emit_parameter_binding {
    my $n = $_[0];
    my $tab = $_[1] || '';
    
    # no parameters
    return ''
        unless defined $n;
        
    # warn "parameter list: ",Dumper $n;
    
    if ( @$n == 1 ) {
        # just one parameter
        my $param = _emit( $n->[0] );
        return "$tab my $param = \$_[0];\n";
    }
    
    return " # XXX - " . (scalar @$n) . " parameters\n";
}

sub default {
    my $n = $_[0];
    my $tab = $_[1] || '';
    #warn "emit: ", Dumper( $n );
    
    if ( exists $n->{END} ) {
        return "$tab END {\n" . _emit( $n->{END}, $tab . '  ' ) . "\n$tab }";
    }
    
    if ( exists $n->{bare_block} ) {
        return  "$tab {\n" . _emit( $n->{bare_block}, $tab . '  ' ) . "\n$tab }\n";
    }

    if ( $n->{op1} eq 'call' ) {
        # warn "call: ",Dumper $n;
        if ( $n->{sub}{bareword} eq 'use' ) {
            # use v6-pugs
            if ( exists $n->{param}{cpan_bareword} &&
                 $n->{param}{cpan_bareword} eq 'v6-pugs' ) {
                return "$tab # use v6-pugs";
            }
            # use module::name 'param'
            return "$tab use " . _emit( $n->{param} );
        }
        return "$tab " . $n->{sub}{bareword} . " '', " . _emit( $n->{param}, '  ' ) 
            if $n->{sub}{bareword} eq 'print' ||
               $n->{sub}{bareword} eq 'warn';
        return "$tab print '', " . _emit( $n->{param}, '  ' ) . ";\n" .
            "$tab print " . '"\n"'
            if $n->{sub}{bareword} eq 'say';
        return $tab . $n->{sub}{bareword} . '(' . _emit( $n->{param}, '  ' ) . ')';
    }
    
    if ( $n->{op1} eq 'method_call' ) {    
        if ( $n->{method}{bareword} eq 'print' ||
             $n->{method}{bareword} eq 'warn' ) {
            return "$tab print '', " . _emit( $n->{self}, '  ' );
        }
        if ( $n->{method}{bareword} eq 'say' ) {
            return "$tab print '', " . _emit( $n->{self}, '  ' ) . ', "\n"';
        }
        return "$tab " . $n->{sub}{bareword} .
            '(' .
            join ( ";\n", 
                map { _emit( $_, $tab ) } @{$n->{param}} 
            ) .
            ')';
    }

    return _not_implemented( $n, "syntax", $tab );
}

sub statement {
    my $n = $_[0];
    my $tab = $_[1] || '';
    #warn "statement: ", Dumper( $n );
    
    if ( $n->{op1}{stmt} eq 'if'     || 
         $n->{op1}{stmt} eq 'unless' ) {
        return  "$tab " . $n->{op1}{stmt} . 
                '(' . _emit( $n->{exp1} ) . ')' .
                " {\n" . _emit( $n->{exp2}, $tab . '  ' ) . "\n$tab }\n" .
                "$tab else" .
                " {\n" . _emit( $n->{exp3}, $tab . '  ' ) . "\n$tab }";
    }

    if ( $n->{op1}{stmt} eq 'sub' ) {
        #warn "sub: ",Dumper $n;
        return  "$tab " . $n->{op1}{stmt} . 
                ' ' . $n->{name}{bareword} . 
                " {\n" . 
                    _emit_parameter_binding( $n->{signature}, $tab . '  ' ) .
                    _emit( $n->{block}, $tab . '  ' ) . 
                "\n$tab }";
    }

    if ( $n->{op1}{stmt} eq 'for' ) {
        #warn "sub: ",Dumper $n->{exp1};
        if ( exists $n->{exp1}{op1} &&
             $n->{exp1}{op1}{op} eq '->' ) {
            return  "$tab " . $n->{op1}{stmt} . 
                    ' my ' . _emit( $n->{exp1}{exp2} ) . '' . 
                    ' (' . _emit( $n->{exp1}{exp1} ) . ')' . 
                    " {\n" . 
                        # _emit_parameter_binding( $n->{signature}, $tab . '  ' ) .
                        _emit( $n->{exp2}, $tab . '  ' ) . 
                    "\n$tab }";
        }
        return  "$tab " . $n->{op1}{stmt} . 
                ' (' . _emit( $n->{exp1} ) . ')' . 
                " {\n" . 
                    # _emit_parameter_binding( $n->{signature}, $tab . '  ' ) .
                    _emit( $n->{exp2}, $tab . '  ' ) . 
                "\n$tab }";
    }

    return _not_implemented( $n, "statement", $tab );
}

sub infix {
    my $n = $_[0];
    my $tab = $_[1];
    # print "infix: ", Dumper( $n );
    
    if ( $n->{op1}{op} eq '~' ) {
        return _emit( $n->{exp1}, $tab ) . ' . ' . _emit( $n->{exp2}, $tab );
    }
    
    if ( $n->{op1}{op} eq ':=' ) {
        #warn "bind: ", Dumper( $n );
        return "$tab tie " . _emit( $n->{exp1}, $tab ) . 
            ", 'Pugs::Runtime::Perl6::Alias::Scalar', " .
            "\\" . _emit( $n->{exp2}, $tab );
    }

    if ( $n->{op1}{op} eq '+'  ||
         $n->{op1}{op} eq '-'  ||
         $n->{op1}{op} eq '==' ||
         $n->{op1}{op} eq '!=' ||
         $n->{op1}{op} eq 'ne' ||
         $n->{op1}{op} eq 'eq' ||
         $n->{op1}{op} eq '&&' ||
         $n->{op1}{op} eq '||' ||
         $n->{op1}{op} eq '=' ) {
        return _emit( $n->{exp1}, $tab ) . ' ' . $n->{op1}{op} . ' ' . _emit( $n->{exp2}, $tab );
    }
    
    return _not_implemented( $n, "infix", $tab );
}

sub circumfix {
    my $n = $_[0];
    my $tab = $_[1]; 
    # print "infix: ", Dumper( $n );
    
    if ( $n->{op1}{op} eq '(' &&
         $n->{op2}{op} eq ')' ) {
        return '()'
            unless defined  $n->{exp1};
        return '(' . _emit( $n->{exp1}, $tab ) . ')';
    }
    
    return _not_implemented( $n, "circumfix", $tab );
}

sub postcircumfix {
    my $n = $_[0];
    my $tab = $_[1]; 
    # print "postcircumfix: ", Dumper( $n );
    
    if ( $n->{op1}{op} eq '[' &&
         $n->{op2}{op} eq ']' ) {
        #return '()'
        #    unless defined  $n->{exp1};
        return _emit( $n->{exp1}, $tab ) . '[' . _emit( $n->{exp2}, $tab ) . ']';
    }
    
    return _not_implemented( $n, "postcircumfix", $tab );
}

sub prefix {
    my $n = $_[0];
    my $tab = $_[1];
    # print "prefix: ", Dumper( $n );
    
    if ( $n->{op1}{op} eq 'my' ||
         $n->{op1}{op} eq 'our' ) {
        return $n->{op1}{op} . ' ' . _emit( $n->{exp1}, $tab );
    }
    if ( $n->{op1}{op} eq 'try' ) {
        return 'eval ' . _emit( $n->{exp1}, $tab ) . "; \$::_EXCL_ = \$@;";
    }
    if ( $n->{op1}{op} eq '++' ||
         $n->{op1}{op} eq '--' ||
         $n->{op1}{op} eq '+'  ) {
        return $n->{op1}{op} . _emit( $n->{exp1}, $tab );
    }
    
    return _not_implemented( $n, "prefix", $tab );
}

sub postfix {
    my $n = $_[0];
    my $tab = $_[1];
    # print "postfix: ", Dumper( $n );

    if ( $n->{op1}{op} eq '++' ||
         $n->{op1}{op} eq '--' ) {
        return _emit( $n->{exp1}, $tab ) . $n->{op1}{op};
    }
    
    return _not_implemented( $n, "postfix", $tab );
}

1;
