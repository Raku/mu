
use v5;
use strict;
no strict 'vars';
use KindaPerl6::Perl5::Runtime;

    my $_EXCEPTION;
    my  $x = bless \( do{ my $v = $GLOBAL::undef } ), 'Type_Scalar'; 
    ($GLOBAL::Code_import)->(); 

    $x->STORE( 42 );
    print "x is ", $x->FETCH, "\n";

    {
        # user's my variables
        my $v = 99;
        # try block
        local $@;
        # let $x;
        my @_let = ( [ $x, $x->FETCH ] );
        my @_catch = ( sub { print "catched; v was $v; exception is $_EXCEPTION\n" } );
        my $_result = eval {
            $x->STORE( 123 );
            print "x changed to ", $x->FETCH, "\n";
            print "dieing\n";
            die "now dieing";
        };
        if ( $_EXCEPTION = $@ || $_EXCEPTION ) {
            for ( @_let ) {
                $_->[0]->STORE( $_->[1] );  # TODO - hash, array
            }
            for ( @_catch ) {
                $_->();
            }
        }
        $_result;
    }

    print "x is ", $x->FETCH, "\n";
