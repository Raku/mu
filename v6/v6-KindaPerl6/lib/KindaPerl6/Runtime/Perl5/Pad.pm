
package Pad;
use strict;
use Carp;


use KindaPerl6::Visitor::Perl;
use KindaPerl6::Visitor::EmitPerl5;
use KindaPerl6::Visitor::EmitPerl6;
use KindaPerl6::Visitor::MetaClass;
use KindaPerl6::Visitor::Token;
use KindaPerl6::Visitor::Global;

my $visitor_dump_ast    = KindaPerl6::Visitor::Perl->new();
my $visitor_emit_perl5  = KindaPerl6::Visitor::EmitPerl5->new();
my $visitor_emit_perl6  = KindaPerl6::Visitor::EmitPerl6->new();
my $visitor_metamodel   = KindaPerl6::Visitor::MetaClass->new();
my $visitor_token       = KindaPerl6::Visitor::Token->new();
my $visitor_global      = KindaPerl6::Visitor::Global->new();


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
    # XXX in 'Visitor::Perl.pm'
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
    
    if ( @{$var->namespace} ) {
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
        return undef
    }
}

sub _var_eq {
    my ( $new, $old ) = @_;
    (  $new->name   eq $old->name
    && $new->twigil eq $old->twigil
    && $new->sigil  eq $old->sigil
    
    && join( '::', @{$new->namespace} ) eq join( '::', @{$old->namespace} ) 
    
    )
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
