
package Code;

use strict;
#use Carp;
#use Data::Dump::Streamer;

my @Blocks = qw( enter leave undo keep start pre post inner );

sub new {
    my $class = shift;
    # pad - contains the lexicals; see Pad.pm
    #        evaluator      => $pad->eval( $cmd ),
    #        variable_names => lexicals,
    #        namespace      => namespace,
    #        outer          => parent,
    # native_source - ' sub { ... } '
    # ast - contains the ast - see kp6-perl5.pl
    #        my @ast = @{$_[0]};
    #        @ast = map { $_->emit( $visitor_metamodel )       } @ast;
    #        my $native = join( ";\n", (map { $_->emit( $visitor_emit_perl5  ) } @ast ));
    # leave - LEAVE blocks
    # enter - ENTER blocks
    # undo, keep, start, pre, post - UNDO, KEEP, START, PRE, POST blocks
    # inner - all other code blocks: for, do, while, try ...
    my %data  = @_; 
    my $code = $data{pad}->eval( $data{native_source} );  
    bless {
        %data,
        code  => $code,    # closure entry point
        leave => $data{leave} || [ ],
        enter => $data{enter} || [ ],
        undo  => $data{undo}  || [ ],
        keep  => $data{keep}  || [ ],
        start => $data{start} || [ ],
        pre   => $data{pre}   || [ ],
        post  => $data{post}  || [ ],
        inner => $data{inner} || [ ],
        started => 0,
    }, $class;
}

=pod

Compiled code example:

    sub { for 1,2,3 -> $x { say $x } }
    
is compiled to:
    
    $sub = Code.new(
        pad => Pad.new( lexicals => [ $x ] ),
        code => { $GLOBAL::for->( List.new(1,2,3), $sub.inner.[0] ) }
        inner => [
            Code.new(
                sig => Sig.new( positionals => [ $x ] ),
                code => { say $x }
            )
        ]
     )

=cut

# note: closure "clone" has 2 different meanings:
#
# functional clone - state blocks and state variables are reinitialized
#   my $y; $x = sub { $y ...}
# functional clone is implemented with:
#   $x = Code.new( ... )
#
# OO clone - state variables are cloned, and keep the current value
#   $x2 = $x.clone

print "CODE:CLONE\n";

# create a new pad, and copy the lexicals to it.
# optionally, move to another outer pad. 
sub clone {
    my $self = shift;
    my %data = @_;
    my $outer = $data{outer} || $self->{pad}->outer;
    # TODO - clone all lexical variables that the code uses, even if they are in outer pads
    my $clone = (ref $self)->new( 
        %{$self},
        pad => $self->{pad}->clone( outer => $outer ), 
    );
    # clone all inner closures recursively
    for my $block ( @Blocks ) {
        for my $code ( 0 .. $#{$clone->{$block}} ) {
            $clone->{$block}[$code] = $clone->{$block}[$code]->clone( outer => $clone->{pad} );
        }
    }
    $clone;
}

sub apply {
    my $self = shift;
    # TODO 
    # - signature,
    # - parameters
    # - want() context
    # - initializers
    # - state
    # - multi
    # - methods ???
    # - inlining (S04 - "START executes inline" ???)
    # - apply junction
    # my $param = ...

    # TODO - init, end

    for ( @{$self->{pre}} ) {
        die "Invalid condition in PRE block. " . ( defined $@ ? $@ : '' )
            unless $_->apply() 
    }

    unless ( $self->{started} ) {
        $_->apply() for @{$self->{start}};
        $self->{started} = 1;
    }
    $_->apply() for @{$self->{enter}};
    my $result = eval { $self->{code}() };   
    my $error = $@;
    
    if ( $error ) {
        # TODO - finish catch, control
        # TODO - see 'use fatal'
        warn $@ unless @{$self->{catch}};
        
        $_->apply() for @{$self->{catch}};
        $_->apply() for @{$self->{control}};
    }
    
    $_->apply() for @{$self->{leave}};
    if ( $error ) {
        $_->apply() for @{$self->{undo}};
    }
    else {
        $_->apply() for @{$self->{keep}};
    }
    
    for ( @{$self->{post}} ) {
        die "Invalid condition in POST block. " . ( defined $@ ? $@ : '' )
            unless $_->apply() 
    }
    
    $result;
}

# TODO - coro
# TODO - multi sub
sub add_multi { }

# TODO - accessors
sub add_init_block { }
sub add_end_block { }
sub add_start_block { }
sub add_enter_block { }
sub add_leave_block { }
sub add_undo_block { }
sub add_pre_block { }
sub add_post_block { }
sub add_catch_block { }
sub add_control_block { }

1;
__END__

package Main;
use Data::Dump::Streamer;

my $s1 = Code->new(
    pad => Pad->new(
        # my $x = 3;  # XXX - no initializers yet
        lexicals => [
            ( bless {
                decl => 'my',
                type => undef,
                var  =>                
                    ( bless {
                        sigil   => '$',
                        twigil  => '',
                        name    => 'x',
                      }, 'Var' 
                    ),            
              }, 'Decl'
            ),
        ],
    ),
    native_source => ' sub { $x++ } ',
    ast => undef,  # XXX
);

$s1->{pad}->eval( ' $x = 3 ' );   # initialize
#print Dump( $s1 );

print "s1: ",$s1->apply(), "\n";

print "clone\n";
my $s2 = $s1->clone;

print "s1: ",$s1->apply(), "\n";
print "s2: ",$s2->apply(), "\n";
print "s1: ",$s1->apply(), "\n";
print "s2: ",$s2->apply(), "\n";

