# - lexical grammar changes, such as
#  my multi infix:<+> ...
#  - the p6-parser is executed in the lexical context under compilation

use strict;
our @parsed = (
    '{',
    'my $x',
    '}',
);
our $env;
our @vars;

sub enter_pad {
    $env = sub {
        push @vars, {};
        { print "enter\n" }
    };
}
sub create_var {
    $env = sub {
        my $x = 42;  # the var is OUTER to the parser
        # problem - tied variables (FETCH and REF may have side-effects)
        $vars[-1]{'$x'} = \$x;
        { print "create\n" }
    };
}
sub exit_pad {
    $env = sub {
        { print "exit\n" }
        pop @vars;
    };
}

sub do_something {
    my $s = shift;
    return enter_pad()  if $s eq '{';
    return create_var() if $s eq 'my $x';
    return exit_pad()   if $s eq '}';
}

# main parser sub
$env = sub { 
    print "init\n";
};
for ( @parsed ) {
    do_something( $_ );
    $env->();
}

__END__

use strict;

    INIT { 
        Main::_begin_001_(); 
    }
    
    package Main;
    use Data::Dump::Streamer;
    my $y;  
    my $z; 
    sub _begin_001_ {
        my $x;  
        $y = sub { $x };   
        $z = sub { $x } 
    }
    print Dump( $y );
    print Dump( $z );

__END__
=pod

    module Main;
    my $y;  
    my $z; 
    BEGIN { 
        my $x;  
        $y = { $x };   
        $z = { $x } 
     }

=cut

package Main;

    Compiler::set_scope();

package Compiler;
    
    use PadWalker qw(peek_my peek_our peek_sub closed_over);

    our $main_scope;
    sub set_scope {
        $main_scope = peek_my(1);
    }

__END__

    # how to declare my vars in another module?
    our $Main::y;
    our $Main::z;
    # how to scope the sub into another module?
    my $_begin = sub {
        my $x;
        $y = { $x };   
        $z = { $x } 
    };
    # list side-effects?
    