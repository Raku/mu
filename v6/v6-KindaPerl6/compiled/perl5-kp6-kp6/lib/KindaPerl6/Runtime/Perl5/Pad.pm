
package Pad;
use strict;
use Carp;


use KindaPerl6::Visitor::Emit::AstPerl;
use KindaPerl6::Visitor::Emit::Perl5;
use KindaPerl6::Visitor::Emit::Perl6;
use KindaPerl6::Visitor::MetaClass;
use KindaPerl6::Visitor::Token;
use KindaPerl6::Visitor::Global;

my $visitor_dump_ast    = ::DISPATCH( $::KindaPerl6::Visitor::Emit::AstPerl, 'new' );
my $visitor_emit_perl5  = ::DISPATCH( $::KindaPerl6::Visitor::Emit::Perl5,   'new' );
my $visitor_emit_perl6  = ::DISPATCH( $::KindaPerl6::Visitor::Emit::Perl6,   'new' );
my $visitor_metamodel   = ::DISPATCH( $::KindaPerl6::Visitor::MetaClass,     'new' );
my $visitor_token       = ::DISPATCH( $::KindaPerl6::Visitor::Token,         'new' );
my $visitor_global      = ::DISPATCH( $::KindaPerl6::Visitor::Global,        'new' );

    # This is the Perl 5 <--> Perl 6 bridge code
    my $perl6_dispatcher =  sub {
                my ($self, $method, @param) = @_;

                #print "# Calling Pad.$method \n";
                #return $self->$method( @param );

                # TODO

                if ( $method eq 'true' ) {
                    return ::DISPATCH( $::Bit, 'new', $_[0]->{bool} )
                }
                if ( $method eq 'LOOKUP' ) {
                    my $what = ::DISPATCH( $param[0], 'Str' )->{_value};
                    return $_[0]->{hash}{$what};
                }
                if ( $method eq 'does' ) {
                    return ::DISPATCH( $::Bit, 'new', 0 )
                        if $param[0] eq $::List   # XXX
                        || $param[0] eq $::NamedArgument
                        ;
                    my $what = ::DISPATCH( $param[0], 'Str' )->{_value};
                    return ::DISPATCH( $::Bit, 'new', 0 )
                        if $what eq 'Junction';
                    #print "Pad.does $what ???\n";
                    return ::DISPATCH( $::Bit, 'new', 1 );  # it probably does
                }
                if ( $method eq 'isa' ) {
                    return ::DISPATCH( $::Bit, 'new', 0 )
                        if $param[0] eq $::List   # XXX
                        || $param[0] eq $::NamedArgument
                        ;
                    my $what = ::DISPATCH( $param[0], 'Str' )->{_value};
                    return ::DISPATCH( $::Bit, 'new', 0 )
                        if $what eq 'Junction';
                    return ::DISPATCH( $::Bit, 'new', 1 )
                        if $what eq 'Pad';
                    #print "Pad.isa $what ???\n";
                    return ::DISPATCH( $::Bit, 'new', 0 );  # it probably isn't
                }
                if ( $method eq 'scalar' ) {
                    return $_[0]->result;
                }
                if ( $method eq 'perl' ) {
                    return ::DISPATCH( $::Str, 'new', 'Pad.new( ... )' )
                }
                if ( $method eq 'defined' ) {
                    return ::DISPATCH( $::Bit, 'new', 1 )
                }

                if ( $method eq 'declaration' ) {
                    return $self->declaration( @param );
                }

                die "Method Pad.$method() not implemented";
                # $self->$method( @param );
        };


sub new {
    #print __PACKAGE__,"->new [",Dump(\@_),"]\n";
    my $class = shift;
    my %data  = @_;  # $.outer, @.lexicals, $.namespace
        # :add_lexicals -- when called from add_lexicals()
    my $parent =
            $data{outer}
        ||  bless {
                evaluator      => sub {
                    package Main;
                    eval $_[0]
                        or do{ Carp::carp( $@ ) if $@ };
                },
                variable_names => [ ],
                namespace      => 'Main',
                parent         => undef,
            }, $class;
    my $namespace =
            ( ref( $data{namespace} )
              ?  $data{namespace}{_value}
              :  $data{namespace}
            )
        ||  $parent->namespace;

    my @declarations = map {
        #print "Pad declaration: ", ::DISPATCH( $_, 'perl' )->{_value}, "\n";
        #print "Pad declaration perl5: ", ::DISPATCH( $_, 'emit_perl5' )->{_value}, "\n";
        ::DISPATCH( $_, 'emit_perl5' )->{_value};
    } @{$data{lexicals}};
    my @names = map {
        #print "Pad var: ", ::DISPATCH( ::DISPATCH( $_, 'var' ), 'perl' )->{_value}, "\n";
        #print "Pad var perl5: ", ::DISPATCH( ::DISPATCH( $_, 'var' ), 'emit_perl5' )->{_value}, "\n";
        ::DISPATCH( ::DISPATCH( $_, 'var' ), 'emit_perl5' )->{_value};
    } @{$data{lexicals}};

    #print Dump( @names );
    my $cmd = 'package ' . $namespace . '; '
            . ($data{add_lexicals} ? '' : 'my $_MODIFIED = {}; ')
            . (scalar @names ? join( '; ', @declarations, '' ) : '')
            . 'sub { '
            .     (join '; ', '$_MODIFIED', @names, '' )        # make sure it's compiled as a closure
            .     'eval $_[0] or do{ Carp::carp( $@ ) if $@ }; '
            . '} ';
    #print "Pad.new $cmd\n";
    bless {
        evaluator      => $parent->eval( $cmd ),
        variable_names => $data{lexicals},
        namespace      => $namespace,
        parent         => $parent,
        _dispatch      => $perl6_dispatcher,
    }, $class;
}

# create a new pad, and copy the lexicals to it.
# optionally, move to another outer pad.
sub clone {
    my $self = shift;
    my %data = @_;
    my $clone = (ref $self)->new(
        outer     => $data{outer} || $self->outer,
        lexicals  => $self->lexicals,
        namespace => $self->namespace,
    );
    # copy the lexical values
    # the pads still share the OUTER pads
    my @names = map {
        $_->var->emit_perl5
    } @{$self->{variable_names}};
    #print Dump( @names );
    local $@;
    for my $name ( @names ) {
        local $Pad::Temp = $self->eval( $name );
        eval { $Pad::Temp = $Pad::Temp->clone };
        $clone->eval( $name . ' = $Pad::Temp ' );
    }
    $clone;
}

sub eval {
    #print "Pad.eval $_[1]\n";
    $_[0]{evaluator}( $_[1] )
}

sub variable_names { $_[0]{variable_names} } # XXX  - remove
sub lexicals { $_[0]{variable_names} }

sub namespace { $_[0]{namespace} }

sub outer { $_[0]{parent} }

sub emit {
    # XXX in 'Visitor::Emit::AstPerl.pm'
    my $self  = shift;
    my $visitor  = shift;
    my $s = '::Pad( ';

    my @d;
    for my $decl ( @{$self->lexicals} ) {
        #print 'decl: ', $decl,"\n";
        push @d, $decl->emit( $visitor );
    }
    $s = $s . 'lexicals => [' . join( ', ', @d ) . '], ';
    $s = $s . 'namespace => "' . $self->namespace . '", ';
    $s = $s . 'parent => ::Pad(...), ';
    $s = $s . 'evaluator => ::Sub(...),';
    return $s . ')';
}

sub add_lexicals {  # [ Decl, Decl, ... ]
    my $self  = shift;

    #print "add_lexicals: ", ::DISPATCH( $_[0], 'perl' )->{_value}, "\n";
    my @lexicals = @{ ::DISPATCH( $_[0], 'INDEX' )->{_value}{_array} };

    # look for new lexicals only
    my @new_lexicals;
    VARS: for my $new ( @lexicals ) {
        for my $old ( @{$self->{variable_names}} ) {
            if ( _var_eq( $new->var, $old->var ) )
            {
                # redeclaration
                next VARS;
            }
        }
        push @new_lexicals, $new;
    }
    #print "add_lexicals: @new_lexicals\n";

    my $inner = Pad->new(
        outer    => $self,
        lexicals => \@new_lexicals,
        # namespace ,
        add_lexicals => 1,
    );
    $self->{evaluator} = $inner->{evaluator};
    $self->{variable_names} = [
        @{$self->{variable_names}},
        @new_lexicals,
    ];
    $self;
}

# look up for a variable's declaration
sub declaration { # Var
    my ( $self, $var ) = @_;

    # Perl 5: if ( @{ $var->namespace } ) {
    # Perl 6: if ($var.namespace).elems {
    if ( ::DISPATCH( ::DISPATCH( ::DISPATCH( ::DISPATCH( $var, 'namespace', ), 'elems', ), "true" ), "p5landish" ) ) {    
        # global variable, doesn't require predeclaration
        return $var;   # XXX
    }

    for my $decl ( @{$self->{variable_names}} ) {
        return $decl
            if ( _var_eq( $decl->var, $var ) );
    }
    if ( $self->{parent} ) {
        return $self->{parent}->declaration( $var );
    }
    else {
        return $::Undef
    }
}

sub _var_eq {
    my ( $new, $old ) = @_;    
    ::DISPATCH( $new, 'perl' )->{_value} eq ::DISPATCH( $old, 'perl' )->{_value}
}

# returns a hashref with names of variables that were modified with .STORE or .BIND
# XXX - modified since when?
sub side_effects {
    keys %{ $_[0]->eval( '$_MODIFIED' ) };
}

sub eval_ast {
    my $self     = shift;
    my $code     = shift;
    for ( $visitor_token, $visitor_metamodel, $visitor_global, $visitor_emit_perl5, ) {
        $code = $code->emit( $_ );
    }
    return $self->eval( $code );
}

# This is a subroutine, not a method
# TODO - replace COMPILER::Pad with a scalar, and then traverse the Pads using .outer instead
sub begin_block {
    # this routine is called by begin-blocks at compile time, in order to execute the code
    # Input: '::Lit::Code' AST node

    my $ast = shift;

    # execute the code inside the current pad
    unshift @COMPILER::PAD, Pad->new(
        outer     => $COMPILER::PAD[0],
        lexicals  => [ ],
        namespace => $_[0],  # optional
    );
    my $data = $COMPILER::PAD[0]->eval_ast( $ast );  # XXX - want() context
    shift @COMPILER::PAD;
    die "At BEGIN: " . $@ if $@;
    #print "RETURN DATA: ", Dumper($data);

    # check for side-effects
    my @begin_stmts;

    #print "=pod\n";
    #print "# BEGIN ENV: ", Dumper( $COMPILER::PAD[0]->lexicals ), "\n";

    for my $pad ( @COMPILER::PAD ) {
        #print "# Lexicals here: ", Dumper( $pad->lexicals ), "\n";
        for my $name ( $pad->side_effects ) {
            my $value = $COMPILER::PAD[0]->eval( "$name" );

            # TODO - create AST, instead of source code
            my $src = '';
            if (   ref( $value->{_value} eq 'HASH' )
                && $name ne $value->{_value}{name}
            ) {
                # it seems to be a bound variable
                if ( $value->{_value}{name} ) {
                    # the binded thing has a name
                    $src = $src . "$name := " . $value->{_value}{name} . '; ';
                    # optimize repeated assignments
                    $src = $src . "$name = " . $value->{_dispatch}( $value, 'perl' )->{_value};
                }
                else {
                    # no name; bind to the value
                    $src = $src . "$name := " . $value->{_dispatch}( $value, 'perl' )->{_value} . '; ';
                }
            }
            else {
                # plain assignment
                $src = $src . "$name = " . $value->{_dispatch}( $value, 'perl' )->{_value};
            }

            # TODO - convert directly DATA->AST, instead of DATA->PERL->AST
            #print "# BEGIN SIDE-EFFECT: $src \n\n";
            my $p = KindaPerl6::Grammar->exp_stmts( $src, 0);
            my $pos = $p->to;
            #print "# parsed to $pos - length = ",length($src)," [$src]\n";
            if ( $pos != length( $src ) ) {
                die "Syntax error serializing BEGIN block, after position $pos in: $src\n";
            }
            #print "AST: ", Dumper($$p);
            # TODO - check for shared data (BIND)
            push @begin_stmts, @$$p;
        }
    }
    unshift @COMPILER::PAD, Pad->new(
        outer     => $COMPILER::PAD[0],
        lexicals  => [ ],
        namespace => $_[0],  # optional
    );
    my $begin_ast = BEGIN->new(
        block => Lit::Code->new(
            sig   => Sig->new(
                                 'invocant' => undef,
                                 'positional' => []
                             ),
            body  => \@begin_stmts,
            pad   => $COMPILER::PAD[0],
            state => {},
        ),
    );
    shift @COMPILER::PAD;
    #print "BEGIN AST: ",Dumper($begin_ast);
    #print "BEGIN native: ", $begin_ast->emit( $visitor_emit_perl5  ) );
    #print "data: ", Dumper( $data );

    # - convert the 'result' data to ast
    my $source = $data->{_dispatch}( $data, 'perl' )->{_value};
    #print "# begin - result data: $source\n";
    my $p = KindaPerl6::Grammar->exp($source, 0);
    #say( Main::perl( $$p ) );
    unshift @COMPILER::PAD, Pad->new(
        outer     => $COMPILER::PAD[0],
        lexicals  => [ ],
        namespace => $_[0],  # optional
    );
    my $final_ast = Do->new(
            'block' => Lit::Code->new(
                pad   => $COMPILER::PAD[0],
                state => { },
                sig   => Sig->new( 'invocant' => undef, 'positional' => [ ] ),
                body  => [ $begin_ast, $$p ],
            ),
    );
    shift @COMPILER::PAD;
    #print "FINAL AST: ",Dumper($final_ast);
    #print "FINAL native: ", $final_ast->emit( $visitor_emit_perl5  );

    # create the runtime initializer
    # @COMPILER::BEGIN_RUNTIME
    ## push @COMPILER::BEGIN_RUNTIME, $initializer_name;
    #print "/begin_block\n";
    #print "\n=cut\n";

    return $final_ast;
}


1;

__END__

package main;

my $env1 = Pad->new( undef, ['$x'] );

$env1->eval( '$x = 3' );
$env1->eval( ' print "x=$x\n" ' );

my $env2 = Pad->new( $env1, ['$y'] );

$env2->eval( '$y = 42' );
$env2->eval( ' print "y=$y\n" ' );

my $env3 = Pad->new( $env2, ['$z'] );

$env3->eval( ' $y++ ' );
$env3->eval( ' print "y=$y\n" ' );
$env3->eval( ' $z = $y ' );

my $env4 = Pad->new( $env3, ['$y'] );

$env4->eval( '$y = 42' );
$env4->eval( ' print "y=$y\n" ' );

$env3->eval( ' print "y=$y\n" ' );
print "variables: @{[ $env3->variable_names ]} \n";

$env3->eval( ' my $j = 123 ' );
$env3->eval( ' print "j=$j\n" ' );
print "end\n";


=begin

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
