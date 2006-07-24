package Pugs::Emitter::Perl6::Perl5;

# p6-ast to perl5 emitter

use strict;
use warnings;
use Data::Dumper;
$Data::Dumper::Indent = 1;

use Pugs::Emitter::Rule::Perl5::Ratchet;
use Pugs::Runtime::Common;
use Digest::MD5 'md5_hex';

our %env;

sub _var_get {
    my $n = $_[0];
    
    if ( ! exists $n->{scalar} ) {
        if ( exists $n->{bare_block} ) {
            my $block = _emit( $n );
            # TODO - check if it is a comma-delimited list
            # print "block: [$block]\n";
            return $block if $block =~ /# hash\n$/s;
            return ' sub ' . $block;
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
    return Pugs::Runtime::Common::mangle_var( $s );
}

sub _var_set {
    my $s = $_[0];
    
    #warn "emit: set $s - ", Dumper %env;
    
    return $env{$s}{set}
        if exists $env{$s}{set};
    
    # default
    return sub { Pugs::Runtime::Common::mangle_var( $s ) . " = " . $_[0] };
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
    warn Pugs::Runtime::Perl6::perl( $ast )
        if $ENV{V6DUMPAST}; 
    return _emit( $ast );
        #"do{\n" .
        #_emit( $ast, '    ' ) . "\n" .
        #"}";
}

sub _emit_code {
    my $code = $_[0];
    if (substr($code, 1,1) eq '?') {
        my $caller_level = 0;
        while ($code =~ s/^&\?CALLER::/&?/) {
            ++$caller_level;
        }
        my $name = substr($code, 2);
        # special!
        if ($name eq 'ROUTINE') {
            return "Pugs::Runtime::Perl6::Routine->new(Devel::Caller::caller_cv($caller_level))";
        }
        elsif ($name eq 'POSITION') {
	    if ($caller_level == 0) {
		return "join(' line ', __FILE__, __LINE__)";
	    }
            return "join(' line ', (caller(".($caller_level-1)."))[1,2])";
        }
        die 'unhandled magic variable';
    }

    return "Pugs::Runtime::Perl6::Routine->new(\\$code)";
}

sub _emit_double_quoted {
    my $n = $_[0];
    # special case for $POSITION
    my @strings = map { $_ =~ s/\$(\?.*POSITION)/&$1/; $_ } (split /([&\$][*?][:\w.]+)/, $n);
    return '""' unless @strings;
    return join('.', map { /^\$\*/ ? Pugs::Runtime::Common::mangle_var($_)
                         : /^.\?/ ? 'do { '.Pugs::Compiler::Perl6->compile($_)->{perl5}.' }'
                         : '"'.$_.'"' }
                     grep { length $_ } @strings);
}

sub _emit {
    my $n = $_[0];
    #die "_emit: ", Dumper( $n ); 
    #warn "_emit: ", Dumper( $n ); 
    
    # 'undef' example: parameter list, in a sub call without parameters
    return ''
        unless defined $n;
    
    die "unknown node: ", Dumper( $n )
        unless ref( $n ) eq 'HASH';


    if (exists $n->{statements}) {
        my $statements = join ( ";\n", 
            map { defined $_ ? _emit( $_ ) : "" } @{$n->{statements}}, undef 
        );
        return length $statements ? $statements : " # empty block\n";
    }

    return Pugs::Runtime::Common::mangle_ident( $n->{bareword} )
        if exists $n->{bareword};

    return Pugs::Runtime::Common::mangle_ident( $n->{dot_bareword} )
        if exists $n->{dot_bareword};
        
    return _emit_code($n->{code})
        if exists $n->{code};
        
    return $n->{int} 
        if exists $n->{int};
        
    return $n->{num} 
        if exists $n->{num};
        
    return '{' . _emit( $n->{pair}{key} ) . '=>' . _emit( $n->{pair}{value} ) . '}'
        if exists $n->{pair};
        
    return _var_get( $n )
        if exists $n->{scalar};
        
    return Pugs::Runtime::Common::mangle_var( $n->{array} )
        if exists $n->{array};
        
    return Pugs::Runtime::Common::mangle_var( $n->{hash} )
        if exists $n->{hash};
        
    return _emit_double_quoted($n->{double_quoted})
        if exists $n->{double_quoted};
            
    return '\'' . $n->{single_quoted} . '\'' 
        if exists $n->{single_quoted};
            
    return 'qw(' . $n->{angle_quoted} . ')' 
        if exists $n->{angle_quoted};
            
    return $n->{perl5source}  
        if exists $n->{perl5source};
        
    return assoc_list( $n )
        if exists $n->{assoc}  && $n->{assoc}  eq 'list';
        
    return assoc_chain( $n )
        if exists $n->{assoc}  && $n->{assoc}  eq 'chain';
        
    return reduce( $n )
        if exists $n->{reduce};

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
        if exists $n->{statement};

    return default( $n );
}

sub reduce {
    my $n = $_[0];
    # print "list emit_rule: ", Dumper( $n );

    return 
        "do { use List::Util 'reduce'; reduce { \$a " .
        $n->{op}{op} . " \$b } " . _emit( $n->{param} ) .
        " } ";
}

sub assoc_list {
    my $n = $_[0];
    # print "list emit_rule: ", Dumper( $n );

    if ( $n->{op1} eq ';' ||
         $n->{op1} eq ',' ) {
        return join ( $n->{op1} . "\n", 
            map { exists $_->{null}
                ? ()
                : _emit( $_ ) 
            } @{$n->{list}} 
        );
    }
    
    return _not_implemented( $n->{op1}, "list-op" );
}

sub assoc_chain {
    my $n = $_[0];
    my @chain = @{$n->{chain}};
    #print "chain emit_rule: ", Dumper( @chain );

    if ( @chain == 3 ) {
        my $exp1 = _emit( $chain[0] );
        my $op   = $chain[1];
        my $exp2 = _emit( $chain[2] );
        return "$exp1 $op $exp2"
    }
    my @e;
    for ( my $i = 0; $i < @chain; $i += 2 ) {
        push @e,  "(" . _emit( $chain[$i] ) . ")";
    }
    my $s = "do { my \@_tmp = (" . join(",", @e) . "); ";
    @e = ();
    for ( my $i = 1; $i < @chain; $i += 2 ) {
        push @e, "\$_tmp[" . int($i/2) . "] $chain[$i] \$_tmp[" . (int($i/2)+1) . "]";
    }
    return $s . join(" && ", @e) . " }";
}

sub _emit_parameter_signature {
    my $n = $_[0] or return '';
    return '' unless @$n;
#     { var => '$self', invocant => 1 },
#     { var => '$title' },
#     { var => '$subtitle', optional => 1 },
#     { var => '$case', named_only => 1 },
#     { var => '$justify', named_only => 1, required => 1});

    return join(",\n         ", map { _emit_data_bind_param_spec($_) } @$n );


}

sub _emit_data_bind_param_spec {
    my %param = %{$_[0]};
    # XXX: translate other attributes
    $param{var} = delete $param{name};
    $param{var} = delete $param{code} if $param{code};
    $param{optional} = 1 if delete $param{default};
    my $dumped = Dumper(\%param);
    $dumped =~ s/^\$VAR1 = //g;
    $dumped =~ s/;$//;
    $dumped =~ s/\n//mg;
    return $dumped;
}

sub _emit_parameter_binding {
    my $n = $_[0];
    # no parameters
    return ''
        unless defined $n;
    #warn "parameter list: ",Dumper $n;

    #    'name' => '$desc',      $v
    #    'optional' => 1,        $v?
    #    'named_only' => 1,      :$v
    #    'type' => 'Str'         Str $v
    #    'is_slurpy' => 1,           *$v
    #    'attribute' => \@attr   $v is rw

    my @params = @$n or return '';
    my $defaults = '';
    my $param = join( ',' , 
        map { _emit( {%$_, scalar => $_->{name}} ) } grep { substr($_->{name}, 0, 1) ne '&' } @params
    );
    for (grep { $_->{default} } @params) {
        my $var = $_->{default}{code} ? '\\'. $_->{default}{code} : _emit( $_->{default} );
        if ( substr($_->{name}, 0, 1) eq '&' ) {
            my $name = substr($_->{name}, 1);
            my $var = $_->{default}{code} ? '\\'. $_->{default}{code} : _emit( $_->{default} );
            $defaults .= "local *$name = $var unless *$name;\n"; # XXX: WRONG
        }
        else {
            my $name = _emit( {%$_, scalar => $_->{name}} );
            $defaults .= "$name = $var unless defined $name;\n";
        }
    }
    return((length($param) ? "  my ($param);\n" : '').
           "  Data::Bind->arg_bind(\\\@_);\n  $defaults;\n");
}

sub _emit_parameter_capture {
    my $n = $_[0];
    return '' unless $n;

    # XXX: gah i am lazy
    if ( exists $n->{fixity} && $n->{fixity} eq 'circumfix') {
        $n = $n->{exp1} or return '';
    }
    $n = { list => [$n] }
        if !($n->{assoc} && $n->{assoc} eq 'list');

    my ($positional, @named) = ("\\(");
    for (@{$n->{list}}) {
        if (my $pair = $_->{pair}) {
            push @named, $pair->{key}{single_quoted}.' => \\('._emit($pair->{value}).')';
        }
	elsif ($_->{fixity} && $_->{fixity} eq 'infix' && $_->{op1}{op} eq '=>') {
            push @named, _emit($_->{exp1}).' => \\('._emit($_->{exp2}).')';
	}
        else {
            # \($scalar, 123, ), \@array, \($orz)
            if (exists $_->{array} || exists $_->{hash}) {
                $positional .= "), \\"._emit($_).", \\(";
            }
            else {
                $positional .= (exists $_->{bare_block} ? 'sub '._emit($_) : _emit($_)).',';
            }
        }
    }
    $positional .= ')';

    return "[$positional], {".join(',', @named).'}';
}

sub _emit_closure {
    my ($signature, $block) = @_;
    return " Data::Bind->sub_signature( sub {" .
        _emit_parameter_binding( $signature ) .
        _emit( $block ) .
    "\n }, "._emit_parameter_signature( $signature ).")\n";
}

sub default {
    my $n = $_[0];
    #warn "emit: ", Dumper( $n );
    
    if ( exists $n->{die} ) {
        return  "do { die '" . $n->{die} . "' }";
    }

    if ( exists $n->{pointy_block} ) {
        # XXX: no signature yet
        return  "sub {\n" . _emit( $n->{pointy_block} ) . "\n }\n";
    }

    if ( exists $n->{bare_block} ) {
        if ( exists $n->{trait} ) {
            # BEGIN/END
            return $n->{trait} . " {\n" . _emit( $n->{bare_block} ) . "\n }";
        }
        #<audreyt> If the closure
        #<audreyt> appears to delimit nothing but a comma-separated list starting with
        #<audreyt> a pair (counting a single pair as a list of one element), the closure
        #<audreyt> will be immediately executed as a hash composer.
        #<audreyt> also, {} is a hash
        #warn "block: ",Dumper $n;
        if ( exists $n->{bare_block}{statements} ) {
            if ( @{$n->{bare_block}{statements}} == 0 ) {
                return "{}  # hash\n";
            }
            if (
                @{$n->{bare_block}{statements}} == 1        &&
                exists $n->{bare_block}{statements}[0]{op1} &&
                $n->{bare_block}{statements}[0]{op1} eq ','
                # TODO -   && is it a pair?
            ) {
                return  "{\n" . _emit( $n->{bare_block}{statements}[0] ) . "\n }  # hash\n";
            }
        }
        return  "{\n" . _emit( $n->{bare_block} ) . "\n }\n";
    }

    if ( $n->{op1} eq 'call' ) {
        # warn "call: ",Dumper $n;

        if ($n->{sub}{scalar} || $n->{sub}{statement}) {
            return _emit($n->{sub}). '->(' . _emit_parameter_capture( $n->{param} ) . ')';
        }

        if ( $n->{sub}{bareword} eq 'grammar'  ||
             $n->{sub}{bareword} eq 'class'    ||
             $n->{sub}{bareword} eq 'package'  ||
             $n->{sub}{bareword} eq 'module'   ) {
            # Moose: package xxx; use Moose;
            # class Point;
            #warn "class: ",Dumper $n;
            local %env;
            my $id;
            $id = exists $n->{param}{cpan_bareword} 
                  # ? Pugs::Runtime::Common::mangle_ident( $n->{param}{cpan_bareword} )
                  ? $n->{param}{cpan_bareword} 
                  : _emit( $n->{param}{sub} );
            my @a = split "-", $id;
            my $version = ( @a > 1 && $a[-1] =~ /^[0-9]/ ? $a[-1] : '' );
            my $decl = 'package ' . $a[0].';' .
                ( $version ? ";\$$a[0]::VERSION = '$version'" : '' ) .
                ( $n->{sub}{bareword} eq 'grammar' 
                    ? ';use Pugs::Compiler::Rule' .
                      ';use base \'Pugs::Grammar::Base\'' 
                    : '' ) .
                ( $n->{sub}{bareword} eq 'class' 
                    ? ';use Moose; Pugs::Runtime::Perl6->setup_class' 
                    : '' ) .
                "; use Exporter 'import'; 
                push our \@ISA, 'Exporter';
                our \@EXPORT; ";

            return exists $n->{param}{param}{bare_block}
                ? "{ $decl; ".(@{$n->{param}{param}{bare_block}{statements}}
                               ? _emit($n->{param}{param}) : '')."}"
                : $decl;
        }

        if ( 0 && $n->{sub}{bareword} eq 'is' ) { # XXX: this is wrong, consider is() from Test.pm
            # is Point;
            #warn "inheritance: ",Dumper $n;
            my $id;
            $id = exists $n->{param}{cpan_bareword} 
                  ? Pugs::Runtime::Common::mangle_ident( $n->{param}{cpan_bareword} )
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
        
        if ( $n->{sub}{bareword} eq 'undef' ) {
            return ' undef ';
        }

        if ( $n->{sub}{bareword} eq 'use' ) {
            # use v6-alpha
            if ( exists $n->{param}{cpan_bareword} ) {
                if ( $n->{param}{cpan_bareword} =~ /^v6-/ ) {
                    return " # use v6-alpha\n";
                }
            }
            #warn "call: ",Dumper $n;
            if ( $n->{param}{sub}{bareword} =~ /^v5/ ) {
                return "warn 'use v5 - not implemented'";
            }
            if ( $n->{param}{sub}{bareword} eq 'v6' ) {
                return " # use v6\n";
            }
            # use perl5:module::name 'param'
            if ( $n->{param}{sub}{lang} &&
                 $n->{param}{sub}{lang} eq 'perl5' ) {
                return 
                    "{ " .
                    # restore PERL5LIB - see 'perldoc lib'
                    'local @INC = @lib::ORIG_INC; ' . 
                    "use " . _emit( $n->{param}{sub} ) . ' ' . 
                        (exists $n->{param}{param} ? _emit($n->{param}{param}) : '' ) .
                    "; } ";
            }
            # use module::name 'param'
            return "use " . _emit( $n->{param}{sub} ) . ' ' . 
                   (exists $n->{param}{param} ? _emit($n->{param}{param}) : '' );
        }

        if ( $n->{sub}{bareword} eq 'enum' ) {
            # enum name list;
            if ( exists $n->{param}{sub} ) {
                my $name = _emit( $n->{param}{sub} );
                my @param = eval _emit( $n->{param}{param} );
                return 
                    "do { " .
                    "{ package ${name}; require Exporter; " .
                    " our \@ISA = qw(Exporter);" .
                    " our \@EXPORT = (" . ( join ",", map {
                        "'$_'"
                    } @param ) . "); " .
                    ( join "\n", map {
                        " sub $param[$_] { $_ } "; 
                    } 0 .. $#param ) .
                    "}" .
                    " ${name}->import(); " .
                    "1 } "; # /do -- t/oo/enums.t depends on enum returning true
            }
        }

        if ($n->{sub}{bareword} eq 'sub') {
            # defining anonymous sub.  XXX: this shouldn't be here. fix the parser.
            return _emit_closure($n->{signature}, $n->{param}{bare_block});
        }

        return " " . $n->{sub}{bareword} . " '', " . _emit( $n->{param} ) 
            if $n->{sub}{bareword} eq 'print' ||
               $n->{sub}{bareword} eq 'warn';
        return " do { print '', " . _emit( $n->{param} ) . ";\n" .
            " print " . '"\n" } '
            if $n->{sub}{bareword} eq 'say';

            
        # XXX: handle args
        return "Pugs::Runtime::Perl6::Routine->new(Devel::Caller::caller_cv(1))"
            if $n->{sub}{bareword} eq 'caller';

        # ???
        $n->{sub}{bareword} = 'die'
            if $n->{sub}{bareword} eq 'fail';
            

        # TODO - other builtins
        my $subname = $n->{sub}{bareword};
        if ($subname eq 'defined') {
            my $param = _emit( $n->{param} );
            # when testing defined-ness of $!, it is testing the emptiness of $@ in perl5.
            return " length(\$@) " if $param eq '$::_V6_ERR_';
            return " (defined $param )";
        }

        if ($subname eq 'substr' || $subname eq 'split' || $subname eq 'die' || $subname eq 'return' || $subname eq 'push' || $subname eq 'shift' || $subname eq 'join' || $subname eq 'index' || $subname eq 'undef' || $subname eq 'rand' || $subname eq 'int' || $subname eq 'splice' || $subname eq 'keys' || $subname eq 'values' || $subname eq 'sort') {
            return $subname . '(' . _emit( $n->{param} ) . ')';
        }

        # XXX: !(0) is not correctly parsed. workaround here.
        if ($subname eq '!' || $subname eq 'not') {
            return $subname.' '._emit($n->{param});
        }
        # runtime thunked builtins
        if ($subname eq 'eval') {
            return 'Pugs::Runtime::Perl6::eval('. _emit_parameter_capture( $n->{param} ) . ')';
        }

        my $sub_name = Pugs::Runtime::Common::mangle_ident( $n->{sub}{bareword} );
        $sub_name = "\&{'$sub_name'}"
            if $sub_name =~ /^v6::/;  # avoid perl5 syntax error
        return ' ' . $sub_name .
            (exists $n->{param} ? '(' . _emit_parameter_capture( $n->{param} ) . ')' : '');
    }
    
    if ( $n->{op1} eq 'method_call' ) {    
        #warn "method_call: ", Dumper( $n );
        if ( $n->{method}{dot_bareword} eq 'print' ||
             $n->{method}{dot_bareword} eq 'warn' ) {
            my $s = _emit( $n->{self} );
            if ( $s eq Pugs::Runtime::Common::mangle_var('$*ERR') ) {  
                return " print STDERR '', " . _emit( $n->{param} );
            }
            return " print '', $s";
        }
        if ( $n->{method}{dot_bareword} eq 'say' ) {
            my $s = _emit( $n->{self} );
            if ( $s eq Pugs::Runtime::Common::mangle_var('$*ERR') ) { 
                return " print STDERR '', " . _emit( $n->{param} ) . ', "\n"';
            }
            return " print '', $s" . ', "\n"';
        }
        if ( $n->{method}{dot_bareword} eq 'perl' ) {
            return 'Pugs::Runtime::Perl6::perl(' . _emit( $n->{self} ) . ")\n";
        }
        # TODO: other builtins
        if ( $n->{method}{dot_bareword} eq 'defined' ) {
            return '(defined ' . _emit( $n->{self} ) . ")\n";
        }
        
        #warn "method_call: ", Dumper( $n );

        # constructor
        if ( exists $n->{self}{bareword} ) {
            # Str.new;
            return 
                " " . _emit( $n->{self} ) . "->" . _emit( $n->{method} ) . 
                "(" . _emit( $n->{param} ) . ") ";
        }
    
        # "autobox"
        
        if ( exists $n->{self}{code} && $n->{method}{dot_bareword} eq 'goto') {
            # &code.goto;
            return 
                " \@_ = (" . _emit_parameter_capture( $n->{param} ) . ");\n" .
                " " . _emit( $n->{method} ) . "( " .
                    _emit( $n->{self} ) . "->code )";
        }
        if ( exists $n->{self}{code} ) {
            # &?ROUTINE.name;
            return 
                _emit( $n->{self} ) . "->" .
                _emit( $n->{method} ) . "(" . _emit( $n->{param} ) . ")"
        }
        
        #warn "method: ", Dumper( $n );
        if ( exists $n->{self}{scalar} ) {
            # $.scalar.method(@param)
            return " " . _emit( $n->{self} ) . '->' .
                _emit( $n->{method} ) .
                '(' . _emit( $n->{param} ) . ')'
                if $n->{self}{scalar} =~ /^\$\./;
            
            # $scalar.++;
            # runtime decision - method or lib call
	    return _emit( $n->{method} ).'('._emit( $n->{self} ).')'
		if $n->{method}{dot_bareword} eq 'ref';
            return 
                "( Scalar::Util::blessed " . _emit( $n->{self} ) . " ? " .
            
                    _emit( $n->{self} ) . "->" . 
                    _emit( $n->{method} ) . "(" . _emit( $n->{param} ) . ")" .
                
                " : " .
                
                    " Pugs::Runtime::Perl6::Scalar::" . _emit( $n->{method}, '  ' ) . 
                    "(" . _emit( $n->{self} ) .
                    ", " . _emit( $n->{param} ) . ")" .
                
                " )";
        }
        
        if ( exists $n->{self}{hash} ) {
            # %hash.keys
            return " (" . 
                _emit( $n->{method} ) . ' ' .
                _emit( $n->{self}   ) . ')';
        }
        
        if (exists $n->{self}{array} ||
            (exists $n->{self}{exp1}{assoc} && $n->{self}{exp1}{assoc} eq 'list')) {
            if ($n->{method}{dot_bareword} eq 'map') {
                my $param = $n->{param}{fixity} eq 'circumfix' ? $n->{param}{exp1} : undef;
                my $code = $param->{bare_block} ? 'sub { '._emit($param).' }' : _emit($param);
                return 'Pugs::Runtime::Perl6::Array::map([\('.$code.', '. _emit($n->{self}).')], {})';
            }
            elsif ($n->{method}{dot_bareword} eq 'delete') {
                my $self = _emit($n->{self});
                $self =~ s{\@}{\$};
                return _emit( $n->{method} ).' '.$self.'['._emit($n->{param}).']';
            }
            else {
                return _emit( $n->{method} ).' '.(join(',', grep { length $_} map { _emit($_) } $n->{self}, $n->{params}));
            }
        }

        if ( exists $n->{self}{op1} ) {
            # %var<item>.++;
            return
                _emit( $n->{self} ) . "->" . 
                _emit( $n->{method} ) . "(" . _emit( $n->{param} ) . ")";
        }
    
        # normal methods or subs
        
        return " " . Pugs::Runtime::Common::mangle_ident( $n->{sub}{bareword} ) .
            '(' .
            join ( ";\n",   # XXX
                map { _emit( $_ ) } @{$n->{param}} 
            ) .
            ')';
    }

    if ( exists $n->{substitution}) {
        return 'XXXX';
    }

    if ( exists $n->{rx} ) {
	return 'qr{'.$n->{rx}{rx}.'}' if $n->{rx}{options}{perl5};
    }

    return _not_implemented( $n, "syntax" );
}

sub statement {
    my $n = $_[0];
    #warn "statement: ", Dumper( $n );
    
    if ( $n->{statement} eq 'if'     || 
         $n->{statement} eq 'unless' ) {
        $n->{exp2} = { bare_block => $n->{exp2} } if $n->{exp2} && !$n->{exp2}{bare_block};
        $n->{exp3} = { bare_block => $n->{exp3} } if $n->{exp3} && !$n->{exp3}{bare_block};
        return  " " . $n->{statement} . 
                '(' . _emit( $n->{exp1} ) . ')' .
                _emit( $n->{exp2} ) . "\n" .
                ( $n->{exp3} ? " else" . _emit( $n->{exp3} ) : '' );
    }

    if ( $n->{statement} eq 'sub'       ||
         $n->{statement} eq 'submethod' ||
         $n->{statement} eq 'method'     ) {
        #warn "sub: ",Dumper $n;

        my $name = Pugs::Runtime::Common::mangle_ident( $n->{name} );

        my $export = '';
        for my $attr ( @{$n->{attribute}} ) {
            if ( $attr->[0]{bareword} eq 'is' &&
                 $attr->[1]{bareword} eq 'export' ) {
                $export = "push \@EXPORT, '$name';";
            }
        }

        if (length $name) {
            my $wrapper_name = $name;
            my $multi_sub = '';
            my $sigs = _emit_parameter_signature ( $n->{signature} ) ;
            if ($n->{multi}) {
                $name .= '_'.md5_hex($sigs);
                $multi_sub = "BEGIN { Sub::Multi->add_multi('$wrapper_name', \\&$name) }\n";
            }
            # XXX: check incompatible attributes
            return "local *$name = "._emit_closure($n->{signature}, $n->{block}) if $n->{my};

            return  $export . " sub " . $name . 
                " {\n" .
                    (
                        $n->{statement} =~ /method/
                        ? " my \$self = shift; "   # default invocant 
                        : ""
                    ) .
                    _emit_parameter_binding( $n->{signature} ) .
                    _emit( $n->{block} ) . 
                "\n };\n" . # ; required when assigning to local
                "## Signature for $name\n" .
                " Data::Bind->sub_signature\n".
                " (\\&$name, $sigs);\n$multi_sub";
        }
        else {
            return _emit_closure($n->{signature}, $n->{block});
        }
    }

    if ( $n->{statement} eq 'for'   ||
         $n->{statement} eq 'while' ||
         $n->{statement} eq 'until' ) {
        #warn "for: ",Dumper $n;
        if ( exists $n->{exp2}{pointy_block} ) {
            my $sig = $n->{exp2}{signature} ? ' my ' . _emit( $n->{exp2}{signature} ) : '';
            my $head = $n->{statement} eq 'for'
                ?  $n->{statement} . 
                    $sig . 
                    ' ( ' . _emit( $n->{exp1} ) . ' )'
                :   $n->{statement} . ' ( '.
                    ( $sig ? $sig . ' = ' : ''
                    ) . _emit( $n->{exp1} ) . ' )';

            return  $head . 
                    " { " . _emit( $n->{exp2}{pointy_block} ) . " }";
        }
        die 'for/while/until should contain a block' unless $n->{exp2}{bare_block};
        return  " " . $n->{statement} . 
                ' ( ' . _emit( $n->{exp1} ) . ' )' . 
                _emit( $n->{exp2} );
    }

    if ( $n->{statement} eq 'loop' ) {
        if ($n->{postfix}) {
            # YES, remember the do {{ }} thingy?
            return " do {"._emit($n->{content})."} while ("._emit($n->{exp2}).")";
        }
        return  " for( ". join(';', map { $_->{null} ? ' ' : _emit($_) } @{$n}{qw/exp1 exp2 exp3/}).
            ")\n"._emit($n->{content});
    }

    if ( $n->{statement} eq 'rule'  ||
         $n->{statement} eq 'token' ||
         $n->{statement} eq 'regex' ) {
        #warn "rule: ",Dumper $n;

        my $name = Pugs::Runtime::Common::mangle_ident( $n->{name} );

        my $export = '';
        for my $attr ( @{$n->{attribute}} ) {
            if ( $attr->[0]{bareword} eq 'is' &&
                 $attr->[1]{bareword} eq 'export' ) {
                $export = "push \@EXPORT, '$name';";
            }
        }

        my $perl5 = Pugs::Emitter::Rule::Perl5::Ratchet::emit( 
            'Pugs::Grammar::Base', 
            $n->{block}, 
            {},   # options
        );
        $perl5 =~ s/^sub/sub $name/ if $name;
        # TODO - _emit_parameter_binding( $n->{signature} ) .
        return  $export .
                $perl5 .
                "## Signature for $name\n" .
                " Data::Bind->sub_signature\n".
                " (\\&$name, ". _emit_parameter_signature ( $n->{signature} ) . ");\n";
    }

    return _not_implemented( $n, "statement" );
}

sub infix {
    my $n = $_[0];
    #print "infix: ", Dumper( $n );

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

    if ( $n->{op1}{op} eq '=:=' ) {
	# XXX: Data::Bind needs to provide an API.  we are now
	# actually with different address using the magic proxying in D::B.
	return 'Scalar::Util::refaddr(\\'._emit($n->{exp1}).
          ') == Scalar::Util::refaddr(\\'._emit($n->{exp2}).')';
    }

    if ( $n->{op1}{op} eq ':=' ) {
        #warn "bind: ", Dumper( $n );
        # The hassle here is that we can't use \(@x) or \(my @x)
        my $_emit_value = sub { exists $_[0]->{array} ||
                                (exists $_[0]->{fixity} && $_[0]->{fixity} eq 'prefix' &&
                                 exists $_[0]->{op1}{op} &&
                                 $_[0]->{op1}{op} eq 'my' && exists $_[0]->{exp1}{array})
                                ? '\\'._emit($_[0]) : '\\('._emit($_[0]).')'};
        return " Data::Bind::bind_op2( " . $_emit_value->( $n->{exp1} ) . ','
            . $_emit_value->( $n->{exp2} ). ' )';
    }
    if ( $n->{op1}{op} eq '~~' ) {
        if ( my $subs = $n->{exp2}{substitution} ) {
            # XXX: use Pugs::Compiler::RegexPerl5
            # XXX: escape
            return _emit( $n->{exp1} ) . ' =~ s{' . $subs->{substitution}[0]. '}{'. $subs->{substitution}->[1] .'}' .
                ( $subs->{options}{g} ? 'g' : '' )
                if $subs->{options}{p5};
            return _not_implemented( $n, "rule" );
        }
	if ( my $rx = $n->{exp2}{rx} ) {
	    if ( !$rx->{options}{perl5} ) {
		return '$::_V6_MATCH_ = Pugs::Compiler::Regex->compile( q{'.$rx->{rx}.'} )->match('._emit($n->{exp1}).')';
	    }
	}
        return _emit( $n->{exp1} ) . ' =~ (ref(' . _emit( $n->{exp2} ).') eq "Regexp" ? '._emit($n->{exp2}).' : quotemeta('._emit($n->{exp2}).'))';
    }

    if ( $n->{op1}{op} eq '=' ) {
        # warn "{'='}: ", Dumper( $n );
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

    return '(' . _emit( $n->{exp1} ) . ' ' . 
        $n->{op1}{op} . ' ' . _emit( $n->{exp2} ) . ')';
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
    
    if ( $n->{op1}{op} eq '[' &&
         $n->{op2}{op} eq ']' ) {
        return '[]'
            unless defined  $n->{exp1};
        return '[' . _emit( $n->{exp1} ) . ']';
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
        
        return _emit( $n->{exp1} ) . '->[' . _emit( $n->{exp2} ) . ']';
    }
    
    if ( $n->{op1}{op} eq '<' &&
         $n->{op2}{op} eq '>' ) {
        my $name = _emit( $n->{exp1} );
        $name =~ s/^\%/\$/;
        return $name . '{ \'' . $n->{exp2}{angle_quoted} . '\' }';
    }

    if ( $n->{op1}{op} eq '{' &&
         $n->{op2}{op} eq '}' ) {
        my $name = _emit( $n->{exp1} );
        die unless $name =~ m/^\%/;
        $name =~ s/^\%/\$/;
        return $name . 
            '{ ' . 
            join('}{', 
                map { 
                    _emit($_) 
                } @{$n->{exp2}{statements}} ) . 
            ' }';
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
        #die "not implemented 'attribute'",Dumper $n
        #    if @{$n->{attribute}};
        return $n->{op1}{op} . ' ' . _emit( $n->{exp1} );
    }

    if ( $n->{op1}{op} eq 'do' ) {
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
        return 'do { my @__ret = eval ' . _emit( $n->{exp1} ) . "; " . 
            Pugs::Runtime::Common::mangle_var( '$!' ) . " = \$@; \@__ret }";
    }
    if ( $n->{op1}{op} eq '~' ) {
        return ' "'._emit( $n->{exp1}).'"' if $n->{exp1}{array};
        return ' "" . ' . _emit( $n->{exp1} );
    }
    if ( $n->{op1}{op} eq '!' ) {
        return _emit( $n->{exp1} ) . ' ? 0 : 1 ';
    }
    if ($n->{op1}{op} eq '+' && exists $n->{exp1}{array}) { # num context
        return 'scalar '._emit( $n->{exp1} );
    }
    if ( $n->{op1}{op} eq '++' ||
         $n->{op1}{op} eq '--' ||
         $n->{op1}{op} eq '+'  ||
         $n->{op1}{op} eq '-'  ) {
        return $n->{op1}{op} . _emit( $n->{exp1} );
    }

    if ($n->{op1}{op} eq '?') { # bool
        return '('._emit($n->{exp1}).' ? 1 : 0 )';
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
