
package Pad;
use strict;
use Carp;
#use Data::Dump::Streamer;

sub new {
    #print __PACKAGE__,"->new [",Dump(\@_),"]\n";
    my $class = shift;
    my %data  = @_;  # $.outer, @.lexicals, $.namespace
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
            $data{namespace} 
        ||  $parent->namespace;
           
    my @declarations = map {
        $_->emit_perl5
    } @{$data{lexicals}};
    my @names = map {
        $_->var->emit_perl5
    } @{$data{lexicals}};
           
    #print Dump( @names );
    my $cmd = 'package ' . $namespace . '; '
            . (scalar @names ? join( '; ', @declarations ) . '; ' : '')
            . 'sub { ' . (join ',', @names ) . '; eval $_[0] or do{ Carp::carp( $@ ) if $@ }; } ';
    #print "Pad.new $cmd\n";
    bless {
        evaluator      => $parent->eval( $cmd ),
        variable_names => $data{lexicals},
        namespace      => $namespace,
        parent         => $parent,
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
    print "Pad.eval $_[1]\n";
    $_[0]{evaluator}( $_[1] ) 
}

sub variable_names { $_[0]{variable_names} } # XXX  - remove
sub lexicals { $_[0]{variable_names} }

sub namespace { $_[0]{namespace} }

sub outer { $_[0]{parent} }

sub emit { return '...' }  # XXX in 'Visitor::Perl.pm'

sub add_lexicals {  # [ Decl, Decl, ... ]
    my $self  = shift;
    
    # look for new lexicals only
    my @new_lexicals;
    VARS: for my $new ( @{$_[0]} ) {
        for my $old ( @{$self->{variable_names}} ) {
            if ( _var_eq( $new->var, $old->var ) )
            {
                # redeclaration
                next VARS;
            }
        }
        push @new_lexicals, $new;
    }
    
    my $inner = Pad->new( 
        outer    => $self, 
        lexicals => \@new_lexicals,
        # namespace 
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
    for my $decl ( @{$self->{variable_names}} ) {
        return $decl 
            if ( _var_eq( $decl->var, $var ) );
    }
    if ( $self->{parent} ) {
        return $self->{parent}->declaration( $var );
    }
    else {
        return undef
    }
}

sub _var_eq {
    my ( $new, $old ) = @_;
    (  $new->name   eq $old->name
    && $new->twigil eq $old->twigil
    && $new->sigil  eq $old->sigil
    )
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