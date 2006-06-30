package Pugs::Emitter::Perl6::Perl5;

# p6-ast to perl5 emitter

use strict;
use warnings;
use Data::Dumper;
$Data::Dumper::Indent = 1;

our %env;

sub _mangle_ident {
    my $s = shift;
    $s =~ s/ ([^a-zA-Z0-9_:]) / '_'.ord($1).'_' /xge;
    return $s;
}

sub _mangle_var {
    my $s = $_[0];
    #warn "mangle: $s";
    
    # perl6 => perl5 variables
    return '%::ENV'    if $s eq '%*ENV';  
    return '$^O'       if $s eq '$*OS';  
    
    # special variables
    return '$::_EXCL_' if $s eq '$!';

    substr($s,1) =~ s/ ([^a-zA-Z0-9_:]) / '_'.ord($1).'_' /xge;
    return $s;
}

sub _var_get {
    my $n = $_[0];
    
    if ( ! exists $n->{scalar} ) {
        if ( exists $n->{bare_block} ) {
            # TODO - check if it is a comma-delimited list
            return ' sub ' . _emit( $n );
        }
        return _emit( $n );
    }

    my $s = $n->{scalar};

    return $env{$s}{get}
        if exists $env{$s} &&
           exists $env{$s}{get};
    
    # default
    return "\$self->{'" . substr($s,2) . "'}"
        if substr($s,1,1) eq '.';
    return _mangle_var( $s );
}

sub _var_set {
    my $s = $_[0];
    
    #warn "emit: set $s - ", Dumper %env;
    
    return $env{$s}{set}
        if exists $env{$s}{set};
    
    # default
    return sub { _mangle_var( $s ) . " = " . $_[0] };
}

sub _not_implemented {
    my ( $n, $what ) = @_;
    return "die q(not implemented $what: " . Dumper( $n ) . ")";
}

sub emit {
    
    # <audreyt> %Namespace:: = ();  # clear stash
    local %env;
    
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
    #die "_emit: ", Dumper( $n ); 
    #warn "fixity: $n->{fixity}\n" if exists $n->{fixity};
    
    # 'undef' example: parameter list, in a sub call without parameters
    return ''
        unless defined $n;
    
    die "unknown node: ", Dumper( $n )
        unless ref( $n ) eq 'HASH';
        
    return _mangle_ident( $n->{bareword} )
        if exists $n->{bareword};
        
    return $n->{code} 
        if exists $n->{code};
        
    return $n->{int} 
        if exists $n->{int};
        
    return $n->{num} 
        if exists $n->{num};
        
    return _var_get( $n )
        if exists $n->{scalar};
        
    return _mangle_var( $n->{array} )
        if exists $n->{array};
        
    return _mangle_var( $n->{hash} )
        if exists $n->{hash};
        
    return '"' . $n->{double_quoted} . '"' 
        if exists $n->{double_quoted};
            
    return '\'' . $n->{single_quoted} . '\'' 
        if exists $n->{single_quoted};
            
    return 'qw(' . $n->{angle_quoted} . ')' 
        if exists $n->{angle_quoted};
            
    return assoc_list( $n )
        if exists $n->{assoc}  && $n->{assoc}  eq 'list';
        
    if ( exists $n->{fixity} ) {
        return infix( $n )
            if $n->{fixity} eq 'infix';
        return prefix( $n )
            if $n->{fixity} eq 'prefix';
        return postfix( $n )
            if $n->{fixity} eq 'postfix';
        return circumfix( $n )
            if $n->{fixity} eq 'circumfix';
        return postcircumfix( $n )
            if $n->{fixity} eq 'postcircumfix';
        return ternary( $n )
            if $n->{fixity} eq 'ternary';
    }
    
    return statement( $n )
        if ref $n->{op1} && exists $n->{op1}{stmt};

    return default( $n );
}

sub assoc_list {
    my $n = $_[0];
    # print "list emit_rule: ", Dumper( $n );

    if ( $n->{op1} eq ';' ||
         $n->{op1} eq ',' ) {
        return join ( $n->{op1} . "\n", 
            map { _emit( $_ ) } @{$n->{list}} 
        );
    }
    
    return _not_implemented( $n->{op1}, "list-op" );
}

sub _emit_parameter_binding {
    my $n = $_[0];

    # no parameters
    return ''
        if  ! defined $n ||
            @$n == 0;
    
    #warn "parameter list: ",Dumper $n;

    #    'scalar' => '$desc',    $v
    #    'optional' => 1,        $v?
    #    'named_only' => 1,      :$v
    #    'type' => {             Str $v
    #      'bareword' => 'Str',
    #    }
    #    'splat' => 1,           *$v
    #    'attribute' => \@attr   $v is rw

    # XXX: This is ugly, but nessary to support lexical subs passed it.
    # This will go away along with Data::Bind integration, hopefully.
    my $bind_code = '';
    for (@$n) {
	if ( exists $_->{type} && $_->{type}{bareword} eq 'Code' ) {
	    my $name = _emit($_);
	    $name =~ s/^&//;
	    $bind_code .= "   local *$name = shift;\n";
	}
	else {
	    $bind_code .= "   my "._emit( $_ )." = shift;\n";
	}
    }
    return $bind_code;
}

sub default {
    my $n = $_[0];
    #warn "emit: ", Dumper( $n );
    
    if ( exists $n->{pointy_block} ) {
	# XXX: no signature yet
        return  "sub {\n" . _emit( $n->{pointy_block} ) . "\n }\n";
    }

    if ( exists $n->{bare_block} ) {
        if ( exists $n->{trait} ) {
            # BEGIN/END
            return $n->{trait} . " {\n" . _emit( $n->{bare_block} ) . "\n }";
        }
        return  "{\n" . _emit( $n->{bare_block} ) . "\n }\n";
    }

    if ( $n->{op1} eq 'call' ) {
        # warn "call: ",Dumper $n;

        if ( $n->{sub}{bareword} eq 'class'  ||
             $n->{sub}{bareword} eq 'module' ) {
            # Moose: package xxx; use Moose;
            # class Point;
            #warn "class: ",Dumper $n;
            local %env;
            my $id;
            $id = exists $n->{param}{cpan_bareword} 
                  ? _mangle_ident( $n->{param}{cpan_bareword} )
                  : _emit( $n->{param}{sub} );
            my @a = split "-", $id;
            my $version = ( @a > 1 && $a[-1] =~ /^[0-9]/ ? $a[-1] : '' );
            return 'package ' . $a[0] .
                ( $version ? ";\$$a[0]::VERSION = '$version'" : '' ) .
                ( $n->{sub}{bareword} eq 'class' ? ';use Moose' : '' ) .
                ";use Exporter 'import';our \@EXPORT";
        }

        if ( $n->{sub}{bareword} eq 'is' ) {
            # is Point;
            #warn "inheritance: ",Dumper $n;
            my $id;
            $id = exists $n->{param}{cpan_bareword} 
                  ? _mangle_ident( $n->{param}{cpan_bareword} )
                  : _emit( $n->{param}{sub} );
            my @a = split "-", $id;
            my $version = ( @a > 1 && $a[-1] =~ /^[0-9]/ ? $a[-1] : '' );
            return "extends '" . $a[0] . "'";
        }
        
        if ( $n->{sub}{bareword} eq 'call' ) {
            # call;
            #warn "super call: ",Dumper $n;
            return "super";  # param list?
        }
        
        if ( $n->{sub}{bareword} eq 'use' ) {
            # use v6-pugs
            if ( exists $n->{param}{cpan_bareword} ) {
                if ( $n->{param}{cpan_bareword} =~ /^v6-/ ) {
                    return " # use v6-pugs\n";
                }
            }
            #warn "call: ",Dumper $n;
            if ( $n->{param}{sub}{bareword} =~ /^v5/ ) {
                return "warn 'use v5 - not implemented'";
            }
            if ( $n->{param}{sub}{bareword} eq 'v6' ) {
                return " # use v6\n";
            }
            # use module::name 'param'
            return "use " . _emit( $n->{param} );
        }

        return " " . $n->{sub}{bareword} . " '', " . _emit( $n->{param} ) 
            if $n->{sub}{bareword} eq 'print' ||
               $n->{sub}{bareword} eq 'warn';
        return " print '', " . _emit( $n->{param} ) . ";\n" .
            " print " . '"\n"'
            if $n->{sub}{bareword} eq 'say';
            
        # ???
        $n->{sub}{bareword} = 'die'
            if $n->{sub}{bareword} eq 'fail';
            
        return ' ' . _mangle_ident( $n->{sub}{bareword} ) . '(' . _emit( $n->{param} ) . ')';
    }
    
    if ( $n->{op1} eq 'method_call' ) {    
        #warn "method_call: ", Dumper( $n );
        if ( $n->{method}{bareword} eq 'print' ||
             $n->{method}{bareword} eq 'warn' ) {
            my $s = _emit( $n->{self} );
            if ( $s eq _mangle_var('$*ERR') ) {  
                return " print STDERR '', " . _emit( $n->{param} );
            }
            return " print '', $s";
        }
        if ( $n->{method}{bareword} eq 'say' ) {
            my $s = _emit( $n->{self} );
            if ( $s eq _mangle_var('$*ERR') ) { 
                return " print STDERR '', " . _emit( $n->{param} ) . ', "\n"';
            }
            return " print '', $s" . ', "\n"';
        }
        if ( $n->{method}{bareword} eq 'perl' ) {
            return 'Pugs::Runtime::Perl6::perl(' . _emit( $n->{self} ) . ")\n";
        }
        #warn "method_call: ", Dumper( $n );
        
        # "autobox"
        
        if ( exists $n->{self}{code} ) {
            # &code.goto;
            return 
                " \@_ = (" . _emit( $n->{param}, '  ' ) . ");\n" .
                " " . _emit( $n->{method}, '  ' ) . " " .
                    _emit( $n->{self}, '  ' );
        }
        
        if ( exists $n->{self}{scalar} ) {
            # $.scalar.method(@param)
            return " " . _emit( $n->{self} ) . '->' .
                _emit( $n->{method} ) .
                '(' . _emit( $n->{param} ) . ')'
                if $n->{self}{scalar} =~ /^\$\./;
            
            # $scalar.++;
            return 
                " Pugs::Runtime::Perl6::Scalar::" . _emit( $n->{method}, '  ' ) . 
                "(" . _emit( $n->{self}, '  ' ) .
                ", " . _emit( $n->{param}, '  ' ) . ")" ;
        }
        
        if ( exists $n->{self}{op1} ) {
            # %var<item>.++;
            #warn "method: ", Dumper( $n );
            return " " . _emit( $n->{method} ) .
                '(' .
                join ( ",\n", 
                    map { _emit( $_ ) } ( $n->{self}, $n->{param} )
                ) .
                ')';
        }
            
        # normal methods or subs
        
        return " " . _mangle_ident( $n->{sub}{bareword} ) .
            '(' .
            join ( ";\n",   # XXX
                map { _emit( $_ ) } @{$n->{param}} 
            ) .
            ')';
    }

    return _not_implemented( $n, "syntax" );
}

sub statement {
    my $n = $_[0];
    #warn "statement: ", Dumper( $n );
    
    if ( $n->{op1}{stmt} eq 'if'     || 
         $n->{op1}{stmt} eq 'unless' ) {
        return  " " . $n->{op1}{stmt} . 
                '(' . _emit( $n->{exp1} ) . ')' .
                " {\n" . _emit( $n->{exp2} ) . "\n }\n" .
                " else" .
                " {\n" . _emit( $n->{exp3} ) . "\n }";
    }

    if ( $n->{op1}{stmt} eq 'sub' ) {
        #warn "sub: ",Dumper $n;

        my $name = _mangle_ident( $n->{name}{bareword} );

        my $export = '';
        for my $attr ( @{$n->{attribute}} ) {
            if ( $attr->[0]{bareword} eq 'is' &&
                 $attr->[1]{bareword} eq 'export' ) {
                $export = "push \@EXPORT, '$name';";
            }
        }

        return  $export .
                " " . $n->{op1}{stmt} . 
                ' ' . $name . 
                " {\n" . 
                    _emit_parameter_binding( $n->{signature} ) .
                    _emit( $n->{block} ) . 
                "\n }";
    }

    if ( $n->{op1}{stmt} eq 'method' ) {
        # Moose: sub clear { my $self = shift;';
        # method clear {
        #warn "method: ",Dumper $n;
        return "sub " . _emit( $n->{name} ) .
            " { my \$self = shift; " . 
                _emit_parameter_binding( $n->{signature} ) .
                _emit( $n->{block} ) . 
            "\n }";
    }


    if ( $n->{op1}{stmt} eq 'for' ) {
        #warn "sub: ",Dumper $n;
        if ( exists $n->{exp2}{pointy_block} ) {
            return  " " . $n->{op1}{stmt} . 
                    ' my ' . _emit( $n->{exp2}{signature} ) . '' . 
                    ' (' . _emit( $n->{exp1} ) . ')' . 
                    " {\n" . 
                        # _emit_parameter_binding( $n->{signature} ) .
                        _emit( $n->{exp2}{pointy_block} ) . 
                    "\n }";
        }
        return  " " . $n->{op1}{stmt} . 
                ' (' . _emit( $n->{exp1} ) . ')' . 
                " {\n" . 
                    # _emit_parameter_binding( $n->{signature} ) .
                    _emit( $n->{exp2} ) . 
                "\n }";
    }

    return _not_implemented( $n, "statement" );
}

sub infix {
    my $n = $_[0];
    #print "infix: ", Dumper( $n );
    
    # XXX - parser bug - this should be a statement
    if ( exists $n->{op1}{stmt} && (
           $n->{op1}{stmt} eq 'if'     ||
           $n->{op1}{stmt} eq 'unless' ) ) {
        #warn "IF: ", Dumper( $n );
        return _emit( $n->{exp1} ) . 
            " $n->{op1}{stmt} " .
            _emit( $n->{exp2} );
    }

    if ( $n->{op1}{op} eq '~' ) {
        return _emit( $n->{exp1} ) . ' . ' . _emit( $n->{exp2} );
    }
    if ( $n->{op1}{op} eq '~=' ) {
        return _emit( $n->{exp1} ) . ' .= ' . _emit( $n->{exp2} );
    }
    if ( $n->{op1}{op} eq '//'  ||
         $n->{op1}{op} eq 'err' ) {
        return ' do { my $_tmp_ = ' . _emit( $n->{exp1} ) . 
            '; defined $_tmp_ ? $_tmp_ : ' . _emit( $n->{exp2} ) . '}';
    }
    
    if ( $n->{op1}{op} eq ':=' ) {
        #warn "bind: ", Dumper( $n );
        return " tie " . _emit( $n->{exp1} ) . 
            ", 'Pugs::Runtime::Perl6::Scalar::Alias', " .
            "\\" . _emit( $n->{exp2} );
    }

    if ( $n->{op1}{op} eq '=' ) {
        #warn "{'='}: ", Dumper( $n );
        if ( exists $n->{exp1}{scalar} ) {
            #warn "set $n->{exp1}{scalar}";
            return _var_set( $n->{exp1}{scalar} )->( _var_get( $n->{exp2} ) );
        }
        if ( exists $n->{exp1}{op1}  &&
             $n->{exp1}{op1}{op} eq 'has' ) {
            #warn "{'='}: ", Dumper( $n );
            # XXX - changes the AST
            push @{ $n->{exp1}{attribute} },
                 [  { bareword => 'default' }, 
                    $n->{exp2} 
                 ]; 
            #warn "{'='}: ", Dumper( $n );
            return _emit( $n->{exp1} );
        }
        return _emit( $n->{exp1} ) . 
            " = " . _var_get( $n->{exp2} );
    }

    if ( $n->{op1}{op} eq '+=' ) {
        #warn "{'='}: ", Dumper( $n );
        if ( exists $n->{exp1}{scalar} ) {
            #warn "set $n->{exp1}{scalar}";
            return _var_set( $n->{exp1}{scalar} )->( 
                _emit(
                  {
                    fixity => 'infix',
                    op1 => { op => '+' },
                    exp1 => $n->{exp1},
                    exp2 => $n->{exp2},
                  }
                )
            );
        }
        return _emit( $n->{exp1} ) . 
            " = " . _emit( $n->{exp2} );
    }

    if ( exists $n->{exp2}{bare_block} ) {
        # $a = { 42 } 
        return " " . _emit( $n->{exp1} ) . ' ' . 
            $n->{op1}{op} . ' ' . "sub " . _emit( $n->{exp2} );
    }

    return _emit( $n->{exp1} ) . ' ' . 
        $n->{op1}{op} . ' ' . _emit( $n->{exp2} );
}

sub circumfix {
    my $n = $_[0];
    # print "infix: ", Dumper( $n );
    
    if ( $n->{op1}{op} eq '(' &&
         $n->{op2}{op} eq ')' ) {
        return '()'
            unless defined  $n->{exp1};
        return '(' . _emit( $n->{exp1} ) . ')';
    }
    
    return _not_implemented( $n, "circumfix" );
}

sub postcircumfix {
    my $n = $_[0];
    #warn "postcircumfix: ", Dumper( $n );
    
    if ( $n->{op1}{op} eq '(' &&
         $n->{op2}{op} eq ')' ) {
        # warn "postcircumfix:<( )> ", Dumper( $n );
        # $.scalar(@param)
        return " " . _emit( $n->{exp1} ) . 
            '->(' . _emit( $n->{exp2} ) . ')'
            if exists $n->{exp1}{scalar} &&
               $n->{exp1}{scalar} =~ /^\$\./;
    }
            
    if ( $n->{op1}{op} eq '[' &&
         $n->{op2}{op} eq ']' ) {

        if ( ! exists $n->{exp2} ) {
            # $array[]
            return '@{ ' . _emit( $n->{exp1} ) . ' }';
        }
                
        # avoid p5 warning - "@a[1] better written as $a[1]"
        if (   (  exists $n->{exp2}{int} 
               || exists $n->{exp2}{scalar} 
               ) 
               && exists $n->{exp1}{array} ) {
            my $name = _emit( $n->{exp1} );
            $name =~ s/^\@/\$/;
            return $name . '[' . _emit( $n->{exp2} ) . ']';
        }
        
        return _emit( $n->{exp1} ) . '[' . _emit( $n->{exp2} ) . ']';
    }
    
    return _not_implemented( $n, "postcircumfix" );
}

sub prefix {
    my $n = $_[0];
    # print "prefix: ", Dumper( $n );
    
    if ( $n->{op1}{op} eq ':' ) {
        return _emit( $n->{exp1} ) . "  # XXX :\$var not implemented\n";
    }
    
    if ( $n->{op1}{op} eq 'my' ||
         $n->{op1}{op} eq 'our' ) {
        die "not implemented 'attribute'",Dumper $n
            if @{$n->{attribute}};
        return $n->{op1}{op} . ' ' . _emit( $n->{exp1} );
    }

    if ( $n->{op1}{op} eq 'has' ) {
            # Moose: has 'xxx';
            # has $x;
            #warn "has: ",Dumper $n;
            
            my $name = _emit( $n->{exp1} );
            #my $name = _emit( $n->{exp1} );
            $name =~ s/^\$//;  # remove sigil
            
            my $raw_name;
            $raw_name = $n->{exp1}{scalar} if exists $n->{exp1}{scalar};
            $env{$raw_name}{set} = sub { 
                "\$self->" . substr($raw_name,2) . "(" . $_[0] . ")" 
            };
            # is rw?
            #warn Dumper @{$n->{attribute}};
            my $is_rw = grep { $_->[0]{bareword} eq 'is' &&
                               $_->[1]{bareword} eq 'rw' } @{$n->{attribute}};
            $env{$raw_name}{set} = sub { 
                "\$self->{'" . substr($raw_name,2) . "'} = " . $_[0] 
            }
                if $is_rw;
            
            my $attr = join( ', ', 
                map { 
                    join( ' => ', map { "'" . _emit($_) . "'" } @$_ )
                } @{$n->{attribute}}
            );

            return $n->{op1}{op} . " '" . substr($raw_name,2) . "' => ( $attr )";
    }

    if ( $n->{op1}{op} eq 'try' ) {
        #warn "try: ", Dumper( $n );
        #if ( exists $n->{trait} ) {
        #    # CATCH/CONTROL
        #    return $n->{trait} . " {\n" . _emit( $n->{bare_block} ) . "\n }";
        #}
        return 'eval ' . _emit( $n->{exp1} ) . "; " . 
            _mangle_var( '$!' ) . " = \$@;";
    }
    if ( $n->{op1}{op} eq 'eval' ) {
        return 
            'do { ' . 
            'use Pugs::Compiler::Perl6; ' . # XXX - load at start
            'local $@; ' .
            # call Perl::Tidy here? - see v6.pm ???
            'my $p6 = Pugs::Compiler::Perl6->compile( ' . _emit( $n->{exp1} ) . ' ); ' .
            'my @result = eval $p6->{perl5}; ' .     # XXX - test want()
            _mangle_var( '$!' ) . ' = $@; ' .
            '@result }';  # /do
    }
    if ( $n->{op1}{op} eq '~' ) {
        return ' "" . ' . _emit( $n->{exp1} );
    }
    if ( $n->{op1}{op} eq '!' ) {
        return _emit( $n->{exp1} ) . ' ? 0 : 1 ';
    }
    if ( $n->{op1}{op} eq '++' ||
         $n->{op1}{op} eq '--' ||
         $n->{op1}{op} eq '+'  ) {
        return $n->{op1}{op} . _emit( $n->{exp1} );
    }
    
    return _not_implemented( $n, "prefix" );
}

sub postfix {
    my $n = $_[0];
    # print "postfix: ", Dumper( $n );

    if ( $n->{op1}{op} eq '++' ||
         $n->{op1}{op} eq '--' ) {
        return _emit( $n->{exp1} ) . $n->{op1}{op};
    }

    if ( $n->{op1}{op} eq 'ANGLE' ) {
        my $name = _emit( $n->{exp1} );
        $name =~ s/^\%/\$/;
        return $name . '{ \'' . $n->{op1}{angle_quoted} . '\' }';
    }

    return _not_implemented( $n, "postfix" );
}

sub ternary {
    my $n = $_[0];
    # print "ternary: ", Dumper( $n );

    if ( $n->{op1}{op} eq '??' ||
         $n->{op2}{op} eq '!!' ) {
        return _emit( $n->{exp1} ) . 
            ' ? ' . _emit( $n->{exp2} ) .
            ' : ' . _emit( $n->{exp3} ) ;
    }

    return _not_implemented( $n, "ternary" );
}
1;
