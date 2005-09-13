use strict;
use warnings;

no warnings 'recursion';

use Digest::SHA1 ();
use Scalar::Util ();
use List::Util ();
use Data::Dumper ();
use String::Escape ();

{
    package Map;
    sub new {
        my $class = shift;
        bless {@_}, $class;
    }

    sub AUTOLOAD {
        our $AUTOLOAD =~ /([^:]+)$/;
        $_[0]->get($1);
    }

    sub get {
        $_[0]{$_[1]}
    }

    sub fmap {
        my $self = shift;
        my $f = shift;
        my $new = (ref $self)->new(%$self);

        $_ = &$f($_) for values %$new;

        $new;
    }

    sub values { values %{ $_[0] } }

    sub str {
        map { "$_ => $_[0]{$_}" } keys %{ $_[0] };
    }

    package Node;
    sub new {
        my $class = shift;
        bless [@_], $class;
    }

    sub str { @{ $_[0] } }

    sub fmap {
        my $self = shift;
        my $f = shift;
        (ref $self)->new(map { &$f($_) } @$self);
    }

    sub values { @{ $_[0] } }
    
    package Unit;
    sub new {
        my $class = shift;
        my $value = shift;
        bless \$value, $class;
    }

    sub val { ${ $_[0] } }
    sub str { $_[0]->val }

    sub fmap {
        my $self = shift;
        my $f = shift;
        (ref $self)->new(&$f($self->val));
    }

    package Thunk;
    use base 'Unit';

    sub digest {
        my $self = shift;
        Digest->compute($self);
    }

    package Env;
    use base 'Map';

    package MEnv;
    use base 'Env';
    sub set { $_[0]{$_[1]} = $_[2] }

    package AST;
    use base 'Unit';

    package Sym;
    use base 'Unit';

    package Param;
    use base 'Unit';

    package App;
    use base 'Node';

    package Val;
    use base 'Unit';

    package Placeholder;
    use base 'Val';
    sub set { ${ $_[0] } = $_[1]; bless $_[0], "Val" }

    package Seq;
    use base 'Node';

    package Prim;
    use base qw/Map/;

    package Stub;
    use base qw/Unit/;
    
    package Pad;
    use base qw/Map/;
}

sub stub { map { $_, Thunk->new(Stub->new($_)) } @_ } # subs that do nothing but have distinct digests

my $env = Env->new(
    stub (
        '&ternary:<?? !!>',
        '&print',
        '&infix:<==>',
        '&infix:<->',
        '&infix:<+>',
    ),
    '&infix:<*>' => Thunk->new(
        Seq->new(
            Param->new('$x'),
            Param->new('$y'),
            App->new(
                Sym->new('&multiply_recursion_helper'),
                Sym->new('$x'),
                Sym->new('$x'),
                Sym->new('$y'),
            ),
        ),
    ),
    '&multiply_recursion_helper' => Thunk->new(
        Seq->new(
            Param->new('$accum'),
            Param->new('$x'),
            Param->new('$y'),
            App->new(
                Sym->new('&control_structure:<if>'),
                App->new(
                    Sym->new('&infix:<==>'),
                    Sym->new('$y'),
                    Val->new(1),
                ),
                Val->new(
                    Thunk->new(
                        Sym->new('$accum'),
                    ),
                ),
                Val->new(
                    Thunk->new(
                        App->new(
                            Sym->new('&multiply_recursion_helper'),
                            App->new(
                                Sym->new('&infix:<+>'),
                                Sym->new('$accum'),
                                Sym->new('$x'),
                            ),
                            Sym->new('$x'),
                            App->new(
                                Sym->new('&infix:<->'),
                                Sym->new('$y'),
                                Val->new(1),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    ),
    
    '&control_structure:<if>' => Thunk->new(
        Seq->new(
            Param->new('$cond'),
            Param->new('$left_thunk'),
            Param->new('$right_thunk'),
            App->new(
                App->new(
                    Sym->new('&ternary:<?? !!>'),
                    Sym->new('$cond'),
                    Sym->new('$left_thunk'),
                    Sym->new('$right_thunk'),
                ),  
            ),
        ),
    ),
    '&say' => Thunk->new(
        Seq->new(
            Param->new('$string'),
            App->new(
                Sym->new('&print'),
                Sym->new('$*OUT'),
                Sym->new('$string'),
                Val->new("\n"),
            ),
        ),
    ),

    '$*OUT' => Val->new(\*STDOUT),


    # user definitions
    '&postfix:<!>' => Thunk->new(
        Seq->new(
            Param->new('$n'),
            App->new(
                App->new(
                    Sym->new('&control_structure:<if>'),
                    App->new(
                        Sym->new('&infix:<==>'),
                        Sym->new('$n'),
                        Val->new(0),
                    ),
                    Val->new(
                        Thunk->new(
                            Val->new(1),
                        ),
                    ),
                    Val->new(
                        Thunk->new(
                            App->new(
                                Sym->new('&infix:<*>'),
                                Sym->new('$n'),
                                App->new(
                                    Sym->new('&postfix:<!>'),
                                    App->new(
                                        Sym->new('&infix:<->'),
                                        Sym->new('$n'),
                                        Val->new(1),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    ),
);

sub native {
    my $params = @_ == 1 ? shift : { @_ };

    my $stub = $params->{env}->get($params->{name});
    my $digest = $stub->digest;

    Prim->new(
        equals => $digest,
        body => $params->{body},
        arity => $params->{arity},
        name => $params->{name},
    )
}



my $prog = AST->new(
    App->new(
        Sym->new('&say'),
        App->new(
            Sym->new('&postfix:<!>'),
            Val->new(6),
        ),
    ),
);

#warn "$_ => " .  $prog->get($_)->digest for grep { $prog->get($_)->isa("Thunk") } keys %$prog;

my $r = Runtime->new( map { native(env => $env, %$_) }
    {
        arity => 3,
        name => '&ternary:<?? !!>',
        body => sub {
            $_[1] ? $_[2] : $_[3]
        }
    },
    {
        arity => 3,
        name => '&print',
        body => sub {
            my $self = shift;
            my $fh = shift->val;
            print $fh @_;
        }
    },
    map {{ arity => 2, name => "&infix:<$_>", body => eval 'sub { $_[1] '.$_.' $_[2] }' }} qw/+ - ==/, do {
        my $f = rand(1) > 0.5;
        warn "Runtime " . ($f ? "with" : "without") . " builtin &infix:<*>";
        ($f ? "*" : ());
    }
);

print "Resulting AST: " . Dumper->reduce($r->run($env, $prog)), "\n";


package Reducer;

sub reduce {
    my $self = shift;
    my $node = shift;

    my $type = "reduce_" . lc(Scalar::Util::blessed($node));
    
    $self->can($type)
        ? $self->$type($node)
        : $self->generic_reduce($node);
}

sub generic_reduce {
    my $self = shift;
    my $node = shift;
    
    Scalar::Util::blessed($node) && $node->can("fmap")
        ? $node->fmap(sub { $self->reduce($_[0]) })
        : $node
}

package DynamicScopes;

sub find_dyn_sym {
    my $self = shift;
    my $symbol = shift;

    foreach my $pad ($self->pads){
        return $pad if $pad->name eq $symbol;
    }

    die "symbol $symbol could not be resolved by $self";
}

sub new_pad {
    my $self = shift;
    
    my $name = shift;
    my $val = shift;

    push @{ $self->{scopes}[-1] }, Pad->new(name => $name, val => $val);
}

sub pads {
    my $self = shift;
    map { @$_ } reverse @{ $self->{scopes} };
}

sub enter_scope {
    my $self = shift;
    push @{ $self->{scopes} }, [];
}

sub leave_scope {
    my $self = shift;
    pop @{ $self->{scopes} };
}

package Runtime;

sub new {
    my $class = shift;

    bless {
        map { $_->equals => $_  } @_,
    }, $class;
}

sub compile {
    my $self = shift;
    
    my $env = shift;
    my $ast = shift;

    Compiler->new->compile($self, $env, $ast);
}

sub execute {
    my $self = shift;
    my $ast = shift;
    Interpreter->new->reduce($ast);
}

sub provides {
    my $self = shift;
    my $hash = shift;

    $self->{$hash};
}

sub run {
    my $self = shift;
    $self->execute($self->compile(@_))->val;
}

package Interpreter;
use base qw/Reducer DynamicScopes/;

sub new {
    bless { params => undef, param_stack => [] }, shift;
}

sub reduce_val { $_[1]->val }

sub reduce_stub {
    my $self = shift;
    die "trying to reduce a stub at runtime";
}

sub reduce_app {
    my $self = shift;
    my $app = shift;

    my ($thunk, @params) = map { $self->reduce($_) } $app->values;

    $self->prepare_scope(@params);

    my $v = $self->reduce($thunk);

    #warn "applying @params to " . Dumper->reduce($thunk) . " yielded $v";

    $v;
}

sub reduce_thunk {
    my $self = shift;
    my $thunk = shift;

    my $body = $thunk->val;

    $self->enter_scope;
    
    my $v = $self->reduce($body);

    $self->leave_scope;

    return $v;
}

sub reduce_param {
    my $self = shift;
    my $param = shift;

    $self->new_pad($param->val => $self->shift_param);
}

sub reduce_prim {
    my $self = shift;
    my $prim = shift;

    # the body is a code ref
    $self->enter_scope;
    my @params = $self->params;

    my $v = $prim->body->($self, map { $self->shift_param } 1 .. $prim->arity);

    $self->leave_scope;

    return $v;
}

sub reduce_sym {
    my $self = shift;
    my $symbol = shift->val;

    $self->find_dyn_sym($symbol)->val;
}

sub reduce_seq {
    my $self = shift;
    my $seq = shift;
    (map { $self->reduce($_) } $seq->values)[-1];
}

sub leave_scope {
    my $self = shift;

    $self->SUPER::leave_scope(@_);
    
    warn "unbound params @{ $self->{params} }" if $self->{params} and @{ $self->{params} };
    $self->{params} = pop @{ $self->{param_stack} };
}

sub prepare_scope {
    my $self = shift;
    my @params = @_;
    push @{ $self->{param_stack} }, $self->{params};
    $self->{params} = \@params;
}

sub shift_param {
    my $self = shift;
    shift @{ $self->{params} };
}

sub params {
    my $self = shift;
    @{ $self->{params} };
}

package Compiler;
use base qw/Reducer Map DynamicScopes/;

sub compile {
    my $self = shift;

    $self->{cache} = {};
    $self->{runtime} = shift;
    my $env = $self->{env} = shift;
    $self->{digests} = { map { Digest->compute($_) => $_ } $env->values };
    my $ast = shift;

    $self->reduce($ast);
}

sub reduce_param {
    my $self = shift;
    my $param = shift;

    $self->new_pad($param->val => undef);

    return $param;
}

sub reduce_thunk {
    my $self = shift;
    my $thunk = shift;

    $self->enter_scope;
    my $node = Thunk->new($self->reduce($thunk->val));
    $self->leave_scope;

    return $node;
}

sub reduce_sym {
    my $self = shift;
    my $node = shift;
    my $symbol = $node->val;

    # if the symbol is a lexical then it can't be prebound
    eval { $self->find_dyn_sym($symbol) };
    return $node if not $@;
    
    $self->compile_time_resolution($symbol);
}

sub compile_time_resolution {
    my $self = shift;
    my $symbol = shift;

    return $self->{cache}{$symbol} if exists $self->{cache}{$symbol};
    my $val = $self->{cache}{$symbol} = Placeholder->new(undef);
    
    # if the symbol is not a predefined global it's an error. If it exists, it can be reduced to it's value
    my $builtin = $self->env->get($symbol) || die $@;

    my $r = $self->runtime;

    if ($builtin->can("digest") and my $native = $r->provides($builtin->digest)){
        $val->set($native); # it's a Prim
    } else {
        if ($builtin->isa("Thunk")) {
            $builtin = $self->reduce($builtin);
        }
        $val->set($builtin);
    }

    return $val;
}

sub reduce_val {
    my $self = shift;
    my $node = shift;

    return Val->new($self->reduce($node->val));
}

package Dumper;
use base qw/Reducer/;

sub layout {
    our $indent;

    if (@_ > 1 or "@_" =~ /\n/){
        return join(
            "\n" . ("\t" x $indent),
            "(", (map { "$_," } @_)
        ) . "\n" . ("\t" x ($indent-1)) . ")";
    } else {
        return "( @_ )";
    }   
}

sub generic_reduce {
    my $self = shift;
    my $node = shift;

    our $indent ||= 0;
    local $indent = $indent + 1;
    
    our $seen;
    local $seen = $seen || {};

    if ($seen->{$node}++){
        return "... recursive=$node ...";
    }

    if (Scalar::Util::blessed($node)){
        if ($node->can("fmap")){
            my @gut =  $node->fmap(sub { $self->generic_reduce($_[0]) })->str;

            return (ref $node) . "->new<=$node>" . layout(@gut);
        } else {
            warn "weird - $node doesn't fmap but is an object";
            return "$node";
        }
    } else {
        return defined $node ? String::Escape::qprintable($node) : "undef";
    }
}


package Digest;

sub compute {
    my $class = shift;
    my $thunk = shift;

    use Data::Structure::Util qw/signature/;;
    return  signature($thunk);
    Digest::SHA1::sha1_hex($class->reduce($thunk->val));
}

# vim:set expandtab:
