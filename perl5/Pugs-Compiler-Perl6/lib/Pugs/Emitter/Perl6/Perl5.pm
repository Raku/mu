package Pugs::Emitter::Perl6::Perl5;

# p6-ast to perl5 emitter

#use Smart::Comments;
use strict;
use warnings;
use Data::Dumper;
$Data::Dumper::Indent = 1;

use Pugs::Emitter::Rule::Perl5::Ratchet;
use Pugs::Runtime::Common;
use Digest::MD5 'md5_hex';
use Pugs::Runtime::Perl6;

# TODO - finish localizing %_V6_ENV at each block
our %_V6_ENV;
our $id = int( 1000 + rand(9000) );
our $_V6_SELF = '$_V6_SELF';

sub _var_get {
    my $n = $_[0];
    my $s;

    for ( qw( scalar array hash ) ) {
        $s = $n->{$_} if exists $n->{$_};
    }

    #print "get: $s\n";

    if  (  defined $s
        && $s =~ /\$\? .* POSITION $/x
        ) {
        # $?CALLER::CALLER::CALLER::POSITION
        my $code = $s;
        $code =~ s/\$/\&/;
        return _emit_code( $code );
    }

    if ( ! $s ) {
        if ( exists $n->{bare_block} ) {
            my $block = _emit( $n );
            # TODO - check if it is a comma-delimited list
            #print "block: [$block]\n";
            #return $block
            #    if $block =~ / \# \s* hash \s* \n \s* }? \s* $/xs;
            #print "sub: [$block]\n";
            return ' sub ' . $block;
        }
        return _emit( $n );
    }

    return $_V6_ENV{$s}{get}
        if exists $_V6_ENV{$s} &&
           exists $_V6_ENV{$s}{get};

    if ( ref $s eq 'HASH' ) {
        my $v = $s->{match_variable};
        return Pugs::Runtime::Common::mangle_var( '$/' ) . '->{' . $v . '}';
    }

    # default
    return "\$_V6_SELF->{'" . substr($s,2) . "'}"
        if substr($s,1,1) eq '.';
    return Pugs::Runtime::Common::mangle_var( $s );
}

sub _var_set {
    my $s = $_[0];

    #warn "emit: set $s - ", Dumper %_V6_ENV;

    return $_V6_ENV{$s}{set}
        if exists $_V6_ENV{$s}{set};

    # default
    return sub { Pugs::Runtime::Common::mangle_var( $s ) . " = " . $_[0] };
}

sub _not_implemented {
    my ( $n, $what ) = @_;
    return "die q(not implemented $what: " . Dumper( $n ) . ")";
}

# modified from http://www.stonehenge.com/merlyn/UnixReview/col30.html
sub deep_copy {
    my $this = shift;
    if (ref $this eq '') {
        $this;
    } elsif (ref $this eq "ARRAY") {
        [map deep_copy($_), @$this];
    } elsif (ref $this eq "HASH") {
        +{map { $_ => deep_copy($this->{$_}) } keys %$this};
    } else {
        #print "deep_copy: ", ref($this), "\n";
        $this;
    }
}

sub emit {

    # <audreyt> %Namespace:: = ();  # clear stash
    my %old_env = %{ deep_copy( \%_V6_ENV ) };
    local %_V6_ENV = %old_env;

    my ($grammar, $ast) = @_;
    # runtime parameters: $grammar, $string, $state, $arg_list
    # rule parameters: see Runtime::Rule.pm
    warn Dumper( $ast )
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


sub _emit_str {
    my $n = $_[0];
    return '"@{[ ' . _emit( $n ) . ' ]}"'
        if exists $n->{array};
    return _emit( $n );
}

# XXX - dead code?
sub _emit_double_quoted {
    my $n = $_[0];
    # special case for $POSITION
    my @strings = map { $_ =~ s/\$(\?.*POSITION)/&$1/; $_ } (split /([&\$][*?][:\w.]+\w)/, $n);
    return '""' unless @strings;
    return join('.', map { /^\$\*/ ? Pugs::Runtime::Common::mangle_var($_)
                         : /^.\?/ ? 'do { '.Pugs::Compiler::Perl6->compile($_)->{perl5}.' }'
                         : '"'.$_.'"' }
                     grep { length $_ } @strings);
}

sub _emit_angle_quoted {
    my $n = $_[0];
    return "qw($n)" unless $n =~ /[()]/;
    return "qw!$n!" unless $n =~ /[!]/;
    return "qw^$n^" unless $n =~ /[\^]/;
    die "can't quote string [$n]";
}

sub _emit_reference {
    my $n = $_[0];

    if ( exists $n->{fixity}
       && $n->{fixity} eq 'circumfix'
       && $n->{op1} eq '('
       ) {
        $n = $n->{exp1}
    }

    #print "_emit_reference: ", Dumper( $n );

    if ( exists $n->{array} ) {
        return ( 'bless \\' . $n->{array} . ", 'Pugs::Runtime::Perl5Container::Array' " );
    }
    if ( exists $n->{hash} ) {
        return ( 'bless \\' . $n->{hash} . ", 'Pugs::Runtime::Perl5Container::Hash' " );
    }
    if ( exists $n->{pair} ) {
        return ( 'bless ' . _emit($n) . ", 'Pugs::Runtime::Perl5Container::Pair' " );
    }
    if ( exists $n->{fixity}
       && $n->{fixity} eq 'infix'
       && $n->{op1} eq '=>'
       ) {
        return ( 'bless ' . _emit_pair( { key => $n->{exp1}, value => $n->{exp2} } ) . ", 'Pugs::Runtime::Perl5Container::Pair' " );
    }
    if ( exists $n->{list}
       && $n->{assoc} eq 'list'
       && $n->{op1} eq ','
       ) {
        return ( 'bless [' . _emit($n) . "], 'Pugs::Runtime::Perl5Container::Array' " );
    }

    if ( exists $n->{fixity}
       && $n->{fixity} eq 'circumfix'
       && $n->{op1} eq '['
       ) {
        return ( 'bless ' . _emit($n) . ", 'Pugs::Runtime::Perl5Container::Array' " );
    }

    if ( exists $n->{anon_hash} ) {
        return ( 'bless ' . _emit($n) . ", 'Pugs::Runtime::Perl5Container::Hash' " );
    }

    if ( exists $n->{scalar} ) {
        # this is a ref already
        return _emit($n);
    }

    return undef;  # '\\( ' . _emit( $_[0] ) . ' )';
}

sub _emit_pair {
    my $n = $_[0];
    my $value = _emit_reference( $n->{value} );
    $value = _emit( $n->{value} )
        unless defined $value;
    return '{' . _emit( $n->{key} ) . '=>' . $value . '}'
}

sub _emit {
    my $n = $_[0];
    #die "_emit: ", Dumper( $n );
    #warn "_emit: ", Dumper( $n );


    # special-case '@_' to use v6.pm calling convention
    if (exists $n->{array}
        && $n->{array} eq '@_'
        )
    {
        #return '$_[0]';
    }

    # 'undef' example: parameter list, in a sub call without parameters
    return ''
        unless defined $n;

    die "unknown node: ", Dumper( $n )
        unless ref( $n ) eq 'HASH';


    if (exists $n->{statements}) {
        my $statements = join ( ";\n",
            map { defined $_ ? _emit( $_ ) : "" }
            @{$n->{statements}}, undef
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

    return _emit_pair( $n->{pair} )
        if exists $n->{pair};

    return emit_anon_hash( $n->{anon_hash} )
        if exists $n->{anon_hash};

    return _var_get( $n )
        if exists $n->{scalar};

    return _var_get( $n )
        if exists $n->{array};

    return _var_get( $n )
        if exists $n->{hash};

    return _emit_double_quoted( $n->{double_quoted} )
        if exists $n->{double_quoted};

    return '\'' . $n->{single_quoted} . '\''
        if exists $n->{single_quoted};

    return _emit_angle_quoted( $n->{angle_quoted} )
        if exists $n->{angle_quoted};

    return $n->{perl5source}
        if exists $n->{perl5source};

    return assoc_list( $n )
        if exists $n->{assoc}  && $n->{assoc}  eq 'list';

    return assoc_chain( $n )
        if exists $n->{assoc}  && $n->{assoc}  eq 'chain';

    return reduce( $n )
        if exists $n->{reduce};

    return emit_block( $n )
        if exists $n->{bare_block};

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

    return variable_declarator( $n )
        if exists $n->{variable_declarator};

    return term( $n )
        if exists $n->{term};

    return default( $n );
}

sub reduce {
    my $n = $_[0];
    # print "list emit_rule: ", Dumper( $n );

    return
        "( List::Util::reduce { \$a " .
        $n->{op}{op} . " \$b } " . _emit( $n->{param} ) .
        " ) ";
}

sub assoc_list {
    my $n = $_[0];
    # print "list emit_rule: ", Dumper( $n );

    if ( $n->{op1} eq ';' ||
         $n->{op1} eq ',' ) {
        return join ( ",\n",
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
        push @e,  emit_parenthesis( $chain[$i] );
    }
    my $id1 = $id++;
    my $s = 'do { $_V6_PAD{'.$id1.'} = [' . join(",", @e) . "]; ";
    @e = ();
    for ( my $i = 1; $i < @chain; $i += 2 ) {
        push @e,
            '$_V6_PAD{'.$id1.'}[' . int($i/2) . "] $chain[$i] " .
            '$_V6_PAD{'.$id1.'}[' . (int($i/2)+1) . "]";
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

sub _emit_parameter_binding_2 {
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
sub _emit_parameter_binding {
    return
        _emit_parameter_binding_2(@_)
        # construct '@_'
        . ' @_ = map { ref $_ eq "SCALAR" ? $$_ : $_ } @{$_[0]}; '
        #. ' use Data::Dumper; print Dumper( \@_ ); '
        ;
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
            #print "EMIT PARAM ",Dumper($pair->{value});
            push @named, $pair->{key}{single_quoted}.' => \\'.emit_parenthesis($pair->{value});
        }
        elsif ($_->{fixity} && $_->{fixity} eq 'infix' && $_->{op1} eq '=>') {
            push @named, autoquote($_->{exp1}).' => \\'.emit_parenthesis($_->{exp2});
        }
        else {
            # \($scalar, 123, ), \@array, \($orz)
            if (exists $_->{array} || exists $_->{hash}) {
                $positional .= "), \\"._emit($_).", \\(";
            }
            else {
                $positional .= (exists $_->{bare_block} ? 'sub ' : '')._emit($_).', ';
            }
        }
    }
    $positional .= ')';

    return "[$positional], {".join(',', @named).'}';
}

sub runtime_method {
    my $n = $_[0];
    # runtime decision - method or lib call
    my $self = _emit( $n->{self} );
    if ( $self eq $_V6_SELF ) {
        # '$_V6_SELF' is known to be an object
        return $self . '->' . _emit( $n->{method} ) . emit_parenthesis( $n->{param} );
    }
    return
        'do { my @_V6_TMP = ' . $self . "; " .
        '( @_V6_TMP == 1 && Scalar::Util::blessed $_V6_TMP[0] ' .
        " ? " .
          '$_V6_TMP[0]->' .
          _emit( $n->{method} ) . emit_parenthesis( $n->{param} ) .
        " : " .
          " Pugs::Runtime::Perl6::Scalar::" . _emit( $n->{method}, '  ' ) .
          '( @_V6_TMP, ' . emit_parenthesis( $n->{param} ) . ")" .
        " ) }";
}

sub emit_parenthesis {
    my $n = $_[0];
    #print "paren: ", Dumper($n);
    return emit_parenthesis( $n->{exp1} )
        if  ref( $n )
            && exists $n->{fixity}
            && $n->{fixity} eq 'circumfix'
            && $n->{op1} eq '('
            && $n->{op2} eq ')';
    return '(' . ( defined $n ? _emit($n) : '' ) . ')';
}

sub emit_block_nobraces {
    my $n = $_[0];
    $n = { bare_block => $n }
        if $n && !$n->{bare_block};
    return  _emit( $n->{bare_block} );
}

sub emit_block {
    my $n = $_[0];
    $n = { bare_block => $n }
        if $n && !$n->{bare_block};
    my $s = emit_block_nobraces( $n );
    if ( exists $n->{trait} ) {
        # BEGIN/END
        return $n->{trait} . " { $s } ";
    }
    return " { $s } ";
}

sub emit_anon_hash {
    my $n = $_[0];
    return '{}' if exists $n->{null};
    return
        '{' .
        join ( ", ",
            map {
                exists $_->{null}
                ? ()
                : exists $_->{pair}
                ? _emit( $_->{pair}{key} ) . '=>' . _emit( $_->{pair}{value} )
                : _emit( $_ )
            } @{$n->{list}}
        ) .
        '}';
}

sub _emit_closure {
    my ($signature, $block) = @_;
    return " Data::Bind->sub_signature( sub {" .
        "   my %_V6_PAD;\n" .
        _emit_parameter_binding( $signature ) .
        emit_block_nobraces( $block ) .
    "\n }, "._emit_parameter_signature( $signature ).")\n";
}

sub default {
    my $n = $_[0];
    #warn "emit: ", Dumper( $n );

    if ( exists $n->{pointy_block} ) {
        # XXX: no signature yet
        return _emit_closure($n->{signature}, $n->{pointy_block});

        return  "sub {\n" . _emit( $n->{pointy_block} ) . "\n }\n";
    }

    if ( exists $n->{op1} && $n->{op1} eq 'call' ) {
        #warn "call: ",Dumper $n;

        if ($n->{sub}{scalar} || $n->{sub}{exp1} || $n->{sub}{statement}) {
            return _emit($n->{sub}). '->(' .
                _emit_parameter_capture( $n->{param} ) . ')';
        }

        if (my $type = $n->{sub}{type}) {
            $type =~ s/^:://;
            return
                " bless({" . emit_parenthesis( $n->{param} ) . "}, '$type')";
        }
    }
    if ( exists $n->{op1}
        && $n->{op1} eq 'call'
        && exists $n->{sub}{bareword}
        )
    {
        if ( $n->{sub}{bareword} eq 'call' ) {
            # call;
            #warn "super call: ",Dumper $n;
            return "super";  # param list?
        }

        if ( $n->{sub}{bareword} eq 'hash' ) {
            return ' %{{ ' . _emit( $n->{param} ) . ' }} ';
        }

        if (  $n->{sub}{bareword} eq 'use'
           || $n->{sub}{bareword} eq 'require'
           ) {
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
                   (exists $n->{param}{param}
                    ? _emit($n->{param}{param})
                    : '' );
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

        return " " . $n->{sub}{bareword} . " '', " . _emit_str( $n->{param} )
            if $n->{sub}{bareword} eq 'print' ||
               $n->{sub}{bareword} eq 'warn';
        return " ( print '', " . emit_parenthesis( $n->{param} ) . "," . '"\n" ) '
            if $n->{sub}{bareword} eq 'say';


        # XXX: handle args
        return "Pugs::Runtime::Perl6::Routine->new(Devel::Caller::caller_cv(1))"
            if $n->{sub}{bareword} eq 'caller';

        # ???
        $n->{sub}{bareword} = 'die'
            if $n->{sub}{bareword} eq 'fail';

        if ( $n->{sub}{bareword} eq 'each' ) {
            # special case: each ( @a; @b );
            # works like 'Y'
            if ( exists $n->{param}{exp1}
                && exists $n->{param}{exp1}{list}
                ) {
                my @param = map { _emit( $_ ) }
                    @{ $n->{param}{exp1}{list} };
                my @param2 = map { '$' . substr($_,1) }
                    @param;
                return
                    "do { " .
                    " my \$n = $param[0] > $param[1] ? $param[0] : $param[1]; " .
                    " map { ( ".$param2[0]."[\$_], ".$param2[1]."[\$_] ) } 0..\$n-1" .
                    "}"
            }
        }

        # TODO - other builtins
        my $subname = $n->{sub}{bareword};
        if ( $subname ) {
            if ($subname eq 'defined') {
                my $param = _emit( $n->{param} );
                # when testing defined-ness of $!, it is testing the emptiness of $@ in perl5.
                return " length(\$@) " if $param eq '$::_V6_ERR_';
                return " (defined $param )";
            }

            if ($subname eq 'substr' || $subname eq 'split' || $subname eq 'die' || $subname eq 'return' || $subname eq 'push' || $subname eq 'shift' || $subname eq 'join' || $subname eq 'index' || $subname eq 'undef' || $subname eq 'rand' || $subname eq 'int' || $subname eq 'splice' || $subname eq 'keys' || $subname eq 'values' || $subname eq 'sort' || $subname eq 'chomp' || $subname eq 'lc') {
                return $subname . emit_parenthesis( $n->{param} );
            }

            # XXX: !(0) is not correctly parsed. workaround here.
            if ($subname eq '!' || $subname eq 'not') {
                return $subname.' '._emit($n->{param});
            }
            if ($subname eq 'WHAT') {
                # WHAT  was .ref
                # WHICH was .id was .SKID
                # HOW (class) was .META
                return 'Pugs::Runtime::Perl6::Scalar::ref( \\'. _emit( $n->{param} ) . ')';
            }
            # runtime thunked builtins
            if ($subname eq 'eval') {

                return
            'sub {
                my $_eval_string = Pugs::Runtime::Perl6::eval_preprocess('. _emit_parameter_capture( $n->{param} ) . ');
                local $@;
                no warnings;
                my @result;
                if (wantarray) {
                    @result = eval $_eval_string;
                }
                else {
                    $result[0] = eval $_eval_string;
                }
                $::_V6_ERR_ = $@;
                #warn $::_V6_ERR_ if $::_V6_ERR_;
                wantarray ? @result : $result[0];' .
            "\n}->()";
            }
            if ($subname eq 'open') {
                return 'Perl6::Internals::open('. _emit_parameter_capture( $n->{param} ) . ')';
            }

            my $sub_name = Pugs::Runtime::Common::mangle_ident( $n->{sub}{bareword} );
            $sub_name = "\&{'$sub_name'}"
                if $sub_name =~ /^v6::/;  # avoid perl5 syntax error
            return ' ' . $sub_name .
                (exists $n->{param} ? '(' . _emit_parameter_capture( $n->{param} ) . ')' : '()');
        }
    }

    if ( exists $n->{op1} && $n->{op1} eq 'method_call_hyper' ) {
        my $inner_call = _emit({
            %$n,
            op1     => 'method_call',
            self    => { scalar => '$_' },
        });
        return '(map { ' . $inner_call . '} @{' . _emit($n->{self}) . '})';
    }

    if ( exists $n->{op1} && $n->{op1} eq 'method_call' ) {
        no warnings 'uninitialized';
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
        if ( $n->{method}{dot_bareword} =~ /^perl$|^yaml$/) {
            return "Pugs::Runtime::Perl6::$n->{method}{dot_bareword}" . emit_parenthesis( $n->{self} );
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
                " '" . _emit( $n->{self} ) . "'->" . _emit( $n->{method} ) .
                emit_parenthesis( $n->{param} );
        }

        # "autobox"

        if ( exists $n->{self}{code} ) {
            if ( $n->{method}{dot_bareword} eq 'goto' ) {
                # &code.goto;
                return
                    " \@_ = (" . _emit_parameter_capture( $n->{param} ) . ");\n" .
                    " " . _emit( $n->{method} ) . "( " .
                    _emit( $n->{self} ) . "->code )";
            }
            # &?ROUTINE.name;
            return
                _emit( $n->{self} ) . "->" .
                _emit( $n->{method} ) . emit_parenthesis( $n->{param} )
        }

        #warn "method: ", Dumper( $n );
        if ( exists $n->{self}{scalar} ) {
            # $.scalar.method(@param)
            return " " . _emit( $n->{self} ) . '->' .
                _emit( $n->{method} ) .
                emit_parenthesis( $n->{param} )
                if $n->{self}{scalar} =~ /^\$\./;

            # $scalar.++;
            return 'ref' . emit_parenthesis( $n->{self} )
                if $n->{method}{dot_bareword} eq 'WHAT';  # "ref"
            # runtime decision - method or lib call
            return runtime_method( $n );
        }

        if ( exists $n->{self}{hash} ) {
            # %hash.keys
            if ($n->{method}{dot_bareword} eq 'kv') {
                return _emit( $n->{self}); # just use it as array
            }
            if ($n->{method}{dot_bareword} eq 'WHAT') {
                return 'Pugs::Runtime::Perl6::Scalar::ref( \\'. _emit( $n->{self} ) . ')';
            }
            if ($n->{method}{dot_bareword} eq 'isa') {
                return 'Pugs::Runtime::Perl6::Scalar::isa( \\'. _emit( $n->{self} ) . ', ' . _emit( $n->{param} ) . ')';
            }
            if ($n->{method}{dot_bareword} eq 'elems') {
                return "( scalar keys "._emit( $n->{self} )." )";
            }
            return " (" .
                _emit( $n->{method} ) . ' ' .
                _emit( $n->{self}   ) . ')';
        }

        if (  exists $n->{self}{array}
           || (  exists $n->{self}{exp1}{assoc}
              && $n->{self}{exp1}{assoc} eq 'list'
              )
           ) {
            if ($n->{method}{dot_bareword} eq 'map') {
                my $param = $n->{param}{fixity} eq 'circumfix' ? $n->{param}{exp1} : undef;
                my $code = $param->{bare_block} ? 'sub { '._emit($param).' }' : _emit($param);
                return 'Pugs::Runtime::Perl6::Array::map([\('.$code.', '. _emit( $n->{self} ).')], {})';
            }
            if (  $n->{method}{dot_bareword} eq 'delete'
               || $n->{method}{dot_bareword} eq 'exists'
               ) {
                my $self = _emit($n->{self});
                $self =~ s{\@}{\$};
                return _emit( $n->{method} ).' '.$self.'['._emit($n->{param}).']';
            }
            if ($n->{method}{dot_bareword} eq 'kv') {
                my $array = emit_parenthesis( $n->{self} );
                return "( map { ( \$_, ".$array."[\$_] ) } 0..".$array."-1 )";
            }
            if ($n->{method}{dot_bareword} eq 'keys') {
                my $array = emit_parenthesis( $n->{self} );
                return "( 0..".$array."-1 )";
            }
            if ($n->{method}{dot_bareword} eq 'values') {
                return emit_parenthesis( $n->{self} );
            }
            if ($n->{method}{dot_bareword} eq 'WHAT') {
                return 'Pugs::Runtime::Perl6::Scalar::ref( \\'. _emit( $n->{self} ) . ')';
            }
            if ($n->{method}{dot_bareword} eq 'isa') {
                return 'Pugs::Runtime::Perl6::Scalar::isa( \\'. _emit( $n->{self} ) . ', ' . _emit( $n->{param} ) . ')';
            }
            if ($n->{method}{dot_bareword} eq 'elems') {
                return "( scalar "._emit( $n->{self} )." )";
            }
            return _emit( $n->{method} ).' '.
                ( join( ',',
                    grep { length $_ }
                    map { _emit($_) }
                    ( $n->{self}, $n->{param} )
                ) );
        }

        if (  exists $n->{self}{op1}
           || exists $n->{self}{term}
           ) {
            # %var<item>.++;
            return runtime_method( $n );
        }

        # normal methods or subs

        if ( exists $n->{sub} && exists $n->{sub}{bareword} ) {
            return " " . Pugs::Runtime::Common::mangle_ident( $n->{sub}{bareword} ) .
            '(' .
            join ( ";\n",   # XXX
                map { _emit( $_ ) } @{$n->{param}}
            ) .
            ')';
        }
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

    if ( # XXX: obsoleted, fix unless to use new structure
         $n->{statement} eq 'unless' ) {
        return  " " . $n->{statement} .
                emit_parenthesis( $n->{exp1} ) .
                emit_block( $n->{exp2} ) . "\n" .
                ( $n->{exp3} ? " else" . emit_block( $n->{exp3} ) : '' );
    }
    if ( $n->{statement} eq 'if' ) {
        my $ret = $n->{statement} .
                emit_parenthesis( $n->{exp1} ) .
                emit_block( $n->{exp2} ) . "\n";
	for (@{$n->{exp3} || []}) {
	    if (ref($_) eq 'ARRAY') {
		$ret .= 'elsif '.emit_parenthesis( $_->[0] ) .
		    emit_block( $_->[1] ) . "\n";
	    }
	    else {
		$ret .= 'else '. emit_block( $_ ) . "\n";
	    }
	}
	return $ret;
    }

    if ( $n->{statement} eq 'do' ) {
        return 'do ' . emit_block( $n->{exp1} );
    }
    if ( $n->{statement} eq 'given' ) {
        return  'for (1) { local $_ = ' . _emit( $n->{exp1} ) . '; ' .
            emit_block_nobraces( $n->{exp2} ) . '  } ';
    }
    if ( $n->{statement} eq 'when' ) {
        return
            'if (' .
            _emit(
                {
                    exp1 => { scalar => '$_' },
                    exp2 => $n->{exp1},
                    op1   => { op => '~~' },
                    fixity => 'infix',
                }
            ) . ') {' . emit_block_nobraces( $n->{exp2} ) . '; last; V6_CONTINUE: ; } ';
    }
    if ( $n->{statement} eq 'default' ) {
        return
            '{' . emit_block_nobraces( $n->{exp1} ) . '; last; V6_CONTINUE: ; } ';
    }
    if ( $n->{statement} eq 'continue' ) {
        return
            'goto V6_CONTINUE';
    }
    if ( $n->{statement} eq 'break' ) {
        return
            'next';
    }
    if ( $n->{statement} eq 'for'   ||
         $n->{statement} eq 'while' ||
         $n->{statement} eq 'until' ) {
        #warn "for: ",Dumper $n;
        if ( exists $n->{exp2}{pointy_block} ) {
            if ($n->{statement} eq 'for'
                && $n->{exp2}{signature}
                && @{$n->{exp2}{signature}} > 1) {
                return 'Pugs::Runtime::Perl6::Array::map([\\'._emit($n->{exp2}).', ['._emit($n->{exp1}).']], {})';
            }
            my @sigs = map { { scalar => $_->{name} } } @{$n->{exp2}{signature}};
            my $sig = $n->{exp2}{signature} ? ' my ' . _emit( @sigs ) : '';
            my $head = $n->{statement} eq 'for'
                ?  $n->{statement} .
                    $sig .
                    emit_parenthesis( $n->{exp1} )
                :   $n->{statement} . ' ( '.
                    ( $sig ? $sig . ' = ' : ''
                    ) . _emit( $n->{exp1} ) . ' )';

            return  $head .
                    " { " . _emit( $n->{exp2}{pointy_block} ) . " }";
        }
        #die 'for/while/until should contain a block' unless $n->{exp2}{bare_block};
        return  " " . $n->{statement} .
                emit_parenthesis( $n->{exp1} ) .
                emit_block( $n->{exp2} );
    }
    if ( $n->{statement} eq 'loop' ) {
        if ( ! exists $n->{exp1} ) {
            return " while (1) " . emit_block( $n->{content} );
        }
        return  " for ( ".
            join(';', map { $_->{null} ? ' ' : _emit($_) } @{$n}{qw/exp1 exp2 exp3/}).
            ")\n" . emit_block( $n->{content} );
    }

    return _not_implemented( $n, "statement" );
}

sub autoquote {
    my $n = $_[0];
    #print "autoquote: ", Dumper( $n );
    if ( exists $n->{'op1'} &&
         $n->{'op1'} eq 'call' &&
         ! exists $n->{'param'} &&
         exists $n->{'sub'}{'bareword'}
       )
    {
        return "'" . $n->{'sub'}{'bareword'} . "'";
    }
    return _emit( $n );
}

sub emit_sub_name {
    my $n = $_[0];
    #print "sub name: ", Dumper( $n );
    my $name = Pugs::Runtime::Common::mangle_ident( $n->{name} );
    return $name
        unless $n->{category};
    return _emit( $n->{name} );
}

sub term {
    my $n = $_[0];
    #print "term: ", Dumper( $n );

    if ( $n->{term} eq 'self' ) {
        return $_V6_SELF;
    }
    if ( $n->{term} eq 'yada' ) {
        return  '( die "not implemented" )';
    }
    if ( $n->{term} eq 'undef' ) {
        return ' undef ';
    }
    if ( $n->{term} eq 'grammar'  ||
         $n->{term} eq 'class'    ||
         $n->{term} eq 'package'  ||
         $n->{term} eq 'module'   ||
         $n->{term} eq 'role'     ) {
        # Moose: package xxx; use Moose;
        # class Point;
        # print Dumper($n);
        my %old_env = %{ deep_copy( \%_V6_ENV ) };
        local %_V6_ENV = %old_env;

        my $id;
        # TODO - anonymous class
        # TODO - attributes
        $id = ref( $n->{name} )
            ? $n->{name}{cpan_bareword}
            : $n->{name};
        my @a = split "-", $id;
        my $version = ( @a > 1 && $a[-1] =~ /^[0-9]/ ? $a[-1] : '' );
        my $namespace = $a[0]
                ? Pugs::Runtime::Common::mangle_ident( $a[0] )
                : '';

        my $attributes = '';
        for my $attr ( @{$n->{attribute}} ) {
            if ( $attr->[0]{bareword} eq 'is' &&
                 $attr->[1]{bareword} ne 'export' ) {
                $attributes .= "push \@ISA, '" . Pugs::Runtime::Common::mangle_ident( $attr->[1]{bareword} ) . "';";
            }
            if ( $attr->[0]{bareword} eq 'does' ) {
                # TODO
                # $attributes .= "with '" . Pugs::Runtime::Common::mangle_ident( $attr->[1]{bareword} ) . "';";
                $attributes .= "use base '" . Pugs::Runtime::Common::mangle_ident( $attr->[1]{bareword} ) . "';";
            }
        }

        my $decl = "package $namespace" .
                ( $version
                    ? ";
                        \$".$namespace."::VERSION = '$version'"
                    : "" ) .
                ( $n->{term} eq 'grammar'
                    ? ";
                        use Pugs::Compiler::Rule;
                        use Moose;
                        use base 'Pugs::Grammar::Base';
                        no strict 'refs'"
                    : "" ) .
                ( $n->{term} eq 'class'
                    ? ";
                        use Moose;
                        Pugs::Runtime::Perl6->setup_class;
                        no strict 'refs'"
                    : "" ) .
                ( $n->{term} eq 'role'
                    ? ";
                        # use Moose::Role;  XXX - need '\$object does role'
                        use Moose;
                        Pugs::Runtime::Perl6->setup_class;
                        no strict 'refs'"
                    : "" ) .
                ";
                use Exporter 'import';
                push our \@ISA, 'Exporter';
                our \@EXPORT;
                bool->import();  # True, False
                $attributes ";

        return ref( $n->{block} ) && exists $n->{block}{bare_block}
                ? "{ $decl; ".(@{$n->{block}{bare_block}{statements}}
                               ? _emit($n->{block}) : '')."}"
                : $decl;
    }

    if ( $n->{term} eq 'sub'       ||
         $n->{term} eq 'submethod' ||
         $n->{term} eq 'method'     ) {
        #warn "sub: ",Dumper $n;
        my %old_env = %{ deep_copy( \%_V6_ENV ) };
        local %_V6_ENV = %old_env;

        my $name = emit_sub_name( $n );

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

            return "$name = "._emit_closure($n->{signature}, $n->{block})
                if $n->{category};

            return "local *$name = "._emit_closure($n->{signature}, $n->{block})
                if $n->{my};

            return  $export . " sub " . $name .
                " {\n" .
                "   my %_V6_PAD;\n" .
                    (
                        $n->{term} =~ /method/
                        ? " my \$_V6_SELF = shift; "   # default invocant
                        : ""
                    ) .
                    _emit_parameter_binding( $n->{signature} ) .
                    emit_block_nobraces( $n->{block} ) .
                "\n };\n" . # ; required when assigning to local
                "## Signature for $name\n" .
                " Data::Bind->sub_signature\n".
                " (\\&$name, $sigs);\n$multi_sub";
        }
        else {
            return _emit_closure($n->{signature}, $n->{block});
        }
    }
    if ( $n->{term} eq 'rule'  ||
         $n->{term} eq 'token' ||
         $n->{term} eq 'regex' ) {
        #warn "rule: ",Dumper $n;

        my $name = emit_sub_name( $n );

        my $export = '';
        for my $attr ( @{$n->{attribute}} ) {
            if ( $attr->[0]{bareword} eq 'is' &&
                 $attr->[1]{bareword} eq 'export' ) {
                $export = "push \@EXPORT, '$name';";
            }
        }

        my $perl5;

        for my $attr ( @{$n->{attribute}} ) {
            if ( $attr->[0]{bareword} eq ':P5' ) {
                die "TODO: regex :P5 {...}";
            }
        }

        if ( $n->{term} eq 'regex' ) {
            $perl5 = Pugs::Emitter::Rule::Perl5::emit(
                'Pugs::Grammar::Base',
                $n->{block},
                {},   # options
            );
        }
        elsif ( $n->{term} eq 'rule' ) {
            ### rule found: $n->{name}
            $perl5 = Pugs::Emitter::Rule::Perl5::Ratchet::emit(
                'Pugs::Grammar::Base',
                $n->{block},
                { sigspace => 1 },   # options
            );
        }
        else {
            $perl5 = Pugs::Emitter::Rule::Perl5::Ratchet::emit(
                'Pugs::Grammar::Base',
                $n->{block},
                {},   # options
            );
        }

        if ( $n->{category} ) {
            # XXX - signature, exports are currently disabled, need more work
            $perl5 =~ s/
              my \s+ \$grammar \s+ = .*? ; \s+
              my \s+ \$s       \s+ = .*? ;
            /
              my \$s       = \$_[0] || '';
              my \$grammar = \$_[1] || __PACKAGE__;
            /sx;
            return "$name = $perl5";
        }
        elsif ( $name ) {
            $perl5 =~ s/
              (my \s+ \$grammar)
            /
              \$_[3] = \$_[2];
              eval{ \$_[2] = undef };
              $1
            /sx;
            $perl5 = "*$name = $perl5";
        }
        else {
            $perl5 =~ s/
              my \s+ \$grammar \s+ = .*? ; \s+
              my \s+ \$s       \s+ = .*? ;
            /
              my \$s       = \$_[0] || '';
              my \$grammar = \$_[1] || __PACKAGE__;
              \$_[3] = \$_[2];
              \$_[2] = undef;
            /sx;
            return $perl5;
        }

        # TODO - _emit_parameter_binding( $n->{signature} ) .
        return  $export .
                $perl5 . ";" .
                "## Signature for $name\n" .
                " Data::Bind->sub_signature\n".
                " (\\&$name, ". _emit_parameter_signature ( $n->{signature} ) . ");\n";
    }
}

sub infix {
    my $n = $_[0];
    #print "infix: ", Dumper( $n );

    if ( $n->{op1} eq 'xx' ) {
        return
            'do { my @_V6_TMP1 = ' . _emit( $n->{exp1} ) . '; ' .
            ' my @_V6_TMP2; push @_V6_TMP2, @_V6_TMP1 for 1..' .
            _emit( $n->{exp2} ) . '; @_V6_TMP2 } ';
    }
    if ( $n->{op1} eq 'xx=' ) {
        return
            '(' .
            _emit( $n->{exp1} ) . ' = ' .
            'do { my @_V6_TMP1 = ' . _emit( $n->{exp1} ) . '; ' .
            ' my @_V6_TMP2; push @_V6_TMP2, @_V6_TMP1 for 1..' .
            _emit( $n->{exp2} ) . '; @_V6_TMP2 } ' .
            ')';
    }
    if ( $n->{op1} eq '~' ) {
        return _emit_str( $n->{exp1} ) . ' . ' . _emit_str( $n->{exp2} );
    }
    if ( $n->{op1} eq '=>' ) {
        #print "autoquote: ", Dumper( $n->{exp1} );
        if ( exists $n->{exp2}{array} ) {
            return autoquote( $n->{exp1} ) . ' => ' . ( 'bless \\' . $n->{exp2}{array} . ", 'Pugs::Runtime::Perl5Container::Array' " );
        }
        if ( exists $n->{exp2}{hash} ) {
            return autoquote( $n->{exp1} ) . ' => ' . ( 'bless \\' . $n->{exp2}{hash} . ", 'Pugs::Runtime::Perl5Container::Hash' " );
        }
        return autoquote( $n->{exp1} ) . ' => ' . _emit( $n->{exp2} );
    }
    if ( $n->{op1} eq '~=' ) {
        return _emit( $n->{exp1} ) . ' .= ' . _emit_str( $n->{exp2} );
    }
    if ( $n->{op1} eq '//'  ||
         $n->{op1} eq 'err' ) {

        # ( !defined ($::TMP=( my $x = $v )) ? $y : $::TMP )
        my $id1 = $id++;
        return
            ' ( !defined ( $_V6_PAD{'.$id1.'} = ( ' . _emit( $n->{exp1} ) . ' )) ' .
            ' ? ( ' . _emit( $n->{exp2} ) . ' ) ' .
            ' : $_V6_PAD{'.$id1.'} ) ';
    }
    if ( $n->{op1} eq 'does' ) {
        # XXX - fix this when Moose implements '$object does'
        #print Dumper( $n->{exp2} );
        return "'" . $n->{exp2}{sub}{bareword} . "'" .
             '->new( ' . _emit( $n->{exp1} ) . ' )'
    }

    if ( $n->{op1} eq '=:=' ) {
        # XXX: Data::Bind needs to provide an API.  we are now
        # actually with different address using the magic proxying in D::B.
        return 'Scalar::Util::refaddr(\\'._emit($n->{exp1}).
          ') == Scalar::Util::refaddr(\\'._emit($n->{exp2}).')';
    }

    if ( $n->{op1} eq ':=' ) {
        #warn "bind: ", Dumper( $n );
        # The hassle here is that we can't use \(@x) or \(my @x)
        my $_emit_value = sub { exists $_[0]->{array} ||
                                (exists $_[0]->{fixity} && $_[0]->{fixity} eq 'prefix' &&
                                 exists $_[0]->{op1} &&
                                 $_[0]->{op1} eq 'my' && exists $_[0]->{exp1}{array})
                                ? '\\' . _emit($_[0])
                                : '\\'.  emit_parenthesis($_[0])
                            };
        return " Data::Bind::bind_op2( " . $_emit_value->( $n->{exp1} ) . ','
            . 'scalar '.$_emit_value->( $n->{exp2} ). ' )';
    }
    if ( $n->{op1} eq '~~' ) {
        if ( my $subs = $n->{exp2}{substitution} ) {
            # XXX: use Pugs::Compiler::RegexPerl5
            # XXX: escape
            my $p5options = join('', map { $subs->{options}{$_} ? $_ : '' } qw(s m g e));
            return _emit( $n->{exp1} ) . ' =~ s{' . $subs->{substitution}[0]. '}{'. $subs->{substitution}->[1] .'}' . $p5options
                if $subs->{options}{p5};
            return _not_implemented( $n, "rule" );
        }
        if ( my $rx = $n->{exp2}{rx} ) {
            if ( !$rx->{options}{perl5} ) {
                my $regex = $rx->{rx};
                # XXX: hack for /$pattern/
                $regex = 'q{'.$regex.'}' unless $regex =~ m/^\$[\w\d]+/;
                return '$::_V6_MATCH_ = Pugs::Compiler::Regex->compile( '.$regex.' )->match('._emit($n->{exp1}).')';
            }
        }
        return _emit( $n->{exp1} ) . ' =~ (ref' . emit_parenthesis( $n->{exp2} ).' eq "Regexp" '.
            ' ? '._emit($n->{exp2}).
            ' : quotemeta'.emit_parenthesis($n->{exp2}).
            ')';
    }

    if ( $n->{op1} eq '=' ) {
        #print "infix:<=> ", Dumper( $n );
        if ( exists $n->{exp1}{scalar} ) {
            my $rvalue = _emit_reference( $n->{exp2} );
            if ( defined $rvalue ) {
                return _var_set( $n->{exp1}{scalar} )->( $rvalue );
            }
            return _var_set( $n->{exp1}{scalar} )->( _var_get( $n->{exp2} ) );
        }

        if ( exists $n->{exp1}{op1}  && ref $n->{exp1}{op1} &&
             $n->{exp1}{op1} eq 'has' ) {
            #print "{'='}: ", Dumper( $n );
            # XXX - changes the AST
            push @{ $n->{exp1}{attribute} },
                 [  { bareword => 'default' },
                    $n->{exp2}
                 ];
            #print "{'='}: ", Dumper( $n );
            return _emit( $n->{exp1} );
        }

        # XXX - declarator hack
        my $exp1 = _emit( $n->{exp1} );
        if ( exists $n->{exp1}{variable_declarator} ) {
            $n->{exp1} = $n->{exp1}{exp1};
        }

        if ( exists $n->{exp1}{hash} ) {
            my $exp2 = $n->{exp2};
            $exp2 = $exp2->{exp1}
                if     exists $exp2->{fixity}
                    && $exp2->{fixity} eq 'circumfix'
                    && $exp2->{op1} eq '(';

            # %h = ();
            return "$exp1 = ()"
                unless defined $exp2;

            #print "{'='}: set hash ",Dumper($exp2);
            # Note - the AST is changed in-place here

            # %hash = { pair, pair }
            if ( exists $exp2->{'anon_hash'} ) {
                $exp2 = $exp2->{'anon_hash'};
            }

            # %hash = ( pair, pair )
            if ( exists $exp2->{'list'} ) {
                $exp2->{'list'} = [
                    map {
                        exists ( $_->{pair} )
                        ?   ( $_->{pair}{key},
                              $_->{pair}{value}
                            )
                        : $_
                    }
                    @{ $exp2->{'list'} }
                ];
            }

            return "$exp1 = " . emit_parenthesis( $exp2 );
        }
        if ( exists $n->{exp1}{array} ) {
            my $exp2 = $n->{exp2};
            $exp2 = $exp2->{exp1}
                if     exists $exp2->{fixity}
                    && $exp2->{fixity} eq 'circumfix'
                    && $exp2->{op1} eq '(';
            return "$exp1 = " . emit_parenthesis( $exp2 );
        }

        my $rvalue;
        $rvalue = _emit_reference( $n->{exp2} )
            unless $exp1 =~ / \[ | \{ /x;   # XXX - hack, detects lvalue slicing
        my $exp2 = _var_get( $n->{exp2} );
        $exp2 = $rvalue
            if defined $rvalue;
        return "$exp1 = ( $exp2 )";
    }

    if ( $n->{op1} eq '+=' ) {
        #print "{'='}: ", Dumper( $n );
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
            $n->{op1} . ' ' . "sub " . _emit( $n->{exp2} );
    }

    return '(' . _emit( $n->{exp1} ) . ' ' .
        $n->{op1} . ' ' . _emit( $n->{exp2} ) . ')';
}

sub circumfix {
    my $n = $_[0];
    #print "circumfix: ", Dumper( $n );

    if ( $n->{op1} eq '(' &&
         $n->{op2} eq ')' ) {
        return emit_parenthesis( $n->{exp1} );
    }

    if ( $n->{op1} eq '[' &&
         $n->{op2} eq ']' ) {
        return '[]'
            unless defined  $n->{exp1};
        return '[' . _emit( $n->{exp1} ) . ']';
    }

    return _not_implemented( $n, "circumfix" );
}

sub postcircumfix {
    my $n = $_[0];
    #warn "postcircumfix: ", Dumper( $n );

    if ( $n->{op1} eq '(' &&
         $n->{op2} eq ')' ) {
        # warn "postcircumfix:<( )> ", Dumper( $n );
        # $.scalar(@param)
        return " " . _emit( $n->{exp1} ) .
            '->' . emit_parenthesis( $n->{exp2} )
            if exists $n->{exp1}{scalar} &&
               $n->{exp1}{scalar} =~ /^\$\./;
    }

    if ( $n->{op1} eq '[' &&
         $n->{op2} eq ']' ) {

        if ( ! exists $n->{exp2} ) {
            # $array[]
            return '@{ ' . _emit( $n->{exp1} ) . ' }';
        }

        if ( ! defined $n->{exp2}
            && exists $n->{exp1}{array}
            )
        {
            # @array[]
            return _emit( $n->{exp1} );
        }

        # avoid p5 warning - "@a[1] better written as $a[1]"
        if (   (  exists $n->{exp2}{int}
               || exists $n->{exp2}{scalar}
               || exists $n->{exp2}{array}
               || exists $n->{exp2}{op1}
               )
            && ( exists $n->{exp1}{array}
                || (  exists $n->{exp1}{op1}
                   && $n->{exp1}{fixity} eq 'circumfix'
                   && $n->{exp1}{op1} eq '('
                   && exists $n->{exp1}{exp1}{list}
                   )
                )
            ) {
            my $name = _emit( $n->{exp1} );
            $name =~ s/^\@/\$/
                unless exists $n->{exp2}{list}
                    || exists $n->{exp2}{array}
                    || (  exists $n->{exp2}{op1}
                       && $n->{exp2}{fixity} eq 'circumfix'
                       && $n->{exp2}{op1} eq '('
                       && exists $n->{exp2}{exp1}{list}
                       )
                    || (  exists $n->{exp2}{fixity}
                       && $n->{exp2}{fixity} eq 'infix'
                       && $n->{exp2}{op1} eq '..'
                       )
                       ;
            return $name . '[' . _emit( $n->{exp2} ) . ']';
        }

        return _emit( $n->{exp1} ) . '->[' . _emit( $n->{exp2} ) . ']';
    }

    if ( $n->{op1} eq '<' &&
         $n->{op2} eq '>' ) {
        my $name = _emit( $n->{exp1} );
        #$name =~ s/^\%/\$/;

        # $/<x>
        return " " . _emit( $n->{exp1} ) .
            '->{ ' . _emit_angle_quoted( $n->{exp2}{angle_quoted} ) . ' }'
            if exists $n->{exp1}{scalar};

        # looks like a hash slice
        $name =~ s/^(?: \% | \$ ) / \@ /x;

        return $name . '{ ' . _emit_angle_quoted( $n->{exp2}{angle_quoted} ) . ' }';
    }

    if ( $n->{op1} eq '{' &&
         $n->{op2} eq '}' ) {
        my $name = _emit( $n->{exp1} );

        if ( exists $n->{exp2}{statements} ) {
        # $/{'x'}
        return " " . _emit( $n->{exp1} ) .
            '->{' . _emit( $n->{exp2}{statements}[0] ) . '}'
            if exists $n->{exp1}{scalar};

        # die "trying to emit ${name}{exp}" unless $name =~ m/^\%/;
        #print "postcircumfix{} ",Dumper( $n->{exp2}{statements} );
        if (  exists $n->{exp2}{statements}[0]{list}
           )
        {
            # looks like a hash slice
            $name =~ s/^(?: \% | \$ ) / \@ /x;
        }
        else {
            $name =~ s/^\%/\$/;
        }
        return $name .
            '{ ' .
            join('}{',
                map {
                    _emit($_)
                } @{$n->{exp2}{statements}} ) .
            ' }';
        }

        # $/{'x'}
        return " " . _emit( $n->{exp1} ) .
            '->{' . _emit( $n->{exp2} ) . '}'
            if exists $n->{exp1}{scalar};

        # die "trying to emit ${name}{exp}" unless $name =~ m/^\%/;
        #print "postcircumfix{} ",Dumper( $n->{exp2}{statements} );
        if (  exists $n->{exp2}{list}
           )
        {
            # looks like a hash slice
            $name =~ s/^(?: \% | \$ ) / \@ /x;
        }
        else {
            $name =~ s/^\%/\$/;
        }
        return $name .
            '{ ' . _emit( $n->{exp2} ) . ' }';
    }

    return _not_implemented( $n, "postcircumfix" );
}

sub prefix {
    my $n = $_[0];
    # print "prefix: ", Dumper( $n );

    if ( $n->{op1} eq '\\' ) {
        # see t/var/autoref.t
        #print "prefix:<\\> ", Dumper( $n );
        my $rvalue = _emit_reference( $n->{exp1} );
        return $rvalue
            if defined $rvalue;
        return '\\( ' . _emit( $n->{exp1} ) . ' )';
    }

    if ( $n->{op1} eq ':' ) {
        return _emit( $n->{exp1} ) . "  # XXX :\$var not implemented\n";
    }

    if (  $n->{op1} eq 'scalar'
       || $n->{op1} eq '$'
       ) {
        return '${' . _emit( $n->{exp1} ) . '}';
    }

    if (  $n->{op1} eq 'array'
       || $n->{op1} eq '@'
       ) {
        return '@{' . _emit( $n->{exp1} ) . '}';
    }

    if ( $n->{op1} eq 'hash'
       || $n->{op1} eq '%'
       ) {
        return '%{' . _emit( $n->{exp1} ) . '}';
    }

    if ( $n->{op1} eq 'try' ) {
        #warn "try: ", Dumper( $n );
        #if ( exists $n->{trait} ) {
        #    # CATCH/CONTROL
        #    return $n->{trait} . " {\n" . _emit( $n->{bare_block} ) . "\n }";
        #}
        my $id1 = $id++;
        return 'do { $_V6_PAD{'.$id1.'} = [ eval ' . _emit( $n->{exp1} ) . " ]; " .
            Pugs::Runtime::Common::mangle_var( '$!' ) . ' = $@; @{$_V6_PAD{'.$id1.'}} }';
    }
    if ( $n->{op1} eq '~' ) {
        # TODO - use _emit_str() instead
        return ' Pugs::Runtime::Perl6::Hash::str( \\' . _emit( $n->{exp1} ) . ' ) '
            if exists $n->{exp1}{hash};
        return ' "' . _emit( $n->{exp1} ) . '"'
            if exists $n->{exp1}{array}
                || ( exists $n->{exp1}{fixity}
                   && $n->{exp1}{fixity} eq 'postcircumfix'
                   && $n->{exp1}{op1} eq '['
                   )
            ;
        #print "prefix:<~> ", Dumper $n;
        return ' "" . ' . _emit( $n->{exp1} );
    }
    if ( $n->{op1} eq '!' ) {
        return _emit( $n->{exp1} ) . ' ? 0 : 1 ';
    }
    if ($n->{op1} eq '+' ) {
        #if ( exists $n->{exp1}{array} ) { # num context
        #    return 'scalar '._emit( $n->{exp1} );
        #}
        if ( exists $n->{exp1}{hash} ) {
            return ( '(0 + keys ' . $n->{exp1}{hash} . ")" );
        }
        return '0 + '._emit( $n->{exp1} );
    }
    if ( $n->{op1} eq '++' ||
         $n->{op1} eq '--' ||
         $n->{op1} eq '+'  ||
         $n->{op1} eq '-'  ) {
        return $n->{op1} . _emit( $n->{exp1} );
    }

    if ($n->{op1} eq '?') { # bool
        return '('._emit($n->{exp1}).' ? 1 : 0 )';
    }

    if ($n->{op1} eq '=') { # iterate

        if (exists $n->{exp1}{single_quoted}) {
            # fish operator =<>
            return '( my $tmp = <STDIN> )';
        }

        return _emit($n->{exp1}).'->getline';
    }

    return _not_implemented( $n, "prefix" );
}

sub postfix {
    my $n = $_[0];
    # print "postfix: ", Dumper( $n );

    if ( $n->{op1} eq '++' ||
         $n->{op1} eq '--' ) {
        return _emit( $n->{exp1} ) . $n->{op1};
    }

    return _not_implemented( $n, "postfix" );
}

sub ternary {
    my $n = $_[0];
    # print "ternary: ", Dumper( $n );

    if ( $n->{op1} eq '??' ||
         $n->{op2} eq '!!' ) {
        return _emit( $n->{exp1} ) .
            ' ? ' . _emit( $n->{exp2} ) .
            ' : ' . _emit( $n->{exp3} ) ;
    }

    return _not_implemented( $n, "ternary" );
}

sub variable_declarator {
    my $n = $_[0];

    # constant %hash is not supported
    if ( $n->{'variable_declarator'} eq 'constant' ) {
        for (qw( hash array )) {
            $n->{'variable_declarator'} = 'our' if exists $n->{exp1}{$_}
        }
    }

    if ( $n->{'variable_declarator'} eq 'my' ||
         $n->{'variable_declarator'} eq 'our' ) {
        #die "not implemented 'attribute'",Dumper $n
        #    if @{$n->{attribute}};
        if  (  ref $n->{exp1}
            && exists $n->{exp1}{term}
            ) {
            $n->{exp1}{my} = $n->{'variable_declarator'};
            return _emit( $n->{exp1} );
        }
        return $n->{'variable_declarator'} . ' ' . _emit( $n->{exp1} );
    }

    if ( $n->{'variable_declarator'} eq 'constant' ) {
        my $name;
        for (qw( scalar hash array )) {
            $name = $n->{exp1}{$_} if exists $n->{exp1}{$_}
        }
        $name = _emit( $n->{exp1} ) unless $name;
        my $no_sigil = substr( $name, 1 );
        $_V6_ENV{$name}{get} = $_V6_ENV{$name}{set} = $no_sigil;
        return "use constant $no_sigil ";  # TODO - set initial value
    }

    if ( $n->{'variable_declarator'} eq 'state' ) {
        $id++;
        #print "State: $id $name ", Dumper( $n->{exp1} );
        my $name;
        for (qw( scalar hash array )) {
            $name = $n->{exp1}{$_} if exists $n->{exp1}{$_}
        }
        $name = _emit( $n->{exp1} ) unless $name;
        my $sigil = substr( $name, 0, 1 );
        $_V6_ENV{$name}{get} = $_V6_ENV{$name}{set} =
            $sigil . '{$_V6_STATE{'.$id.'}}';
        return _emit( $n->{exp1} );
    }
    if ( $n->{'variable_declarator'} eq 'has' ) {
            # Moose: has 'xxx';
            # has $x;
            #warn "has: ",Dumper $n;

            my $name = _emit( $n->{exp1} );
            #my $name = _emit( $n->{exp1} );
            $name =~ s/^\$//;  # remove sigil

            my $raw_name;
            $raw_name = $n->{exp1}{scalar} if exists $n->{exp1}{scalar};
            $_V6_ENV{$raw_name}{set} = sub {
                "\$_V6_SELF->" . substr($raw_name,2) . "(" . $_[0] . ")"
            };
            # is rw?
            #warn Dumper @{$n->{attribute}};
            my $is_rw = grep { $_->[0]{bareword} eq 'is' &&
                               $_->[1]{bareword} eq 'rw' } @{$n->{attribute}};

            unless ( $is_rw ) {
                # ... then it 'is ro'
                my $is_ro = grep { $_->[0]{bareword} eq 'is' &&
                               $_->[1]{bareword} eq 'ro' } @{$n->{attribute}};
                unless ( $is_ro ) {
                    push @{$n->{attribute}}, [ { bareword => 'is' }, { bareword => 'ro' } ];
                }
            }

            if ( $is_rw ) {
                $_V6_ENV{$raw_name}{set} = sub {
                    "\$_V6_SELF->{'" . substr($raw_name,2) . "'} = " . $_[0]
                }
            }

            my $attr = join( ', ',
                map {
                    join( ' => ', map { "'" . _emit($_) . "'" } @$_ )
                } @{$n->{attribute}}
            );

            return $n->{'variable_declarator'} . " '" . substr($raw_name,2) . "' => ( $attr )";
    }
}

1;
