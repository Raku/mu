#!/usr/bin/perl6

use v6;

=pod

=head1 Default values

You want to provide default values for false or undefined variables.

    my ($true,$false,$string,$defined,$last);

    # Provide a default for undefined values

    $false //= 0; 

    # The default for $string will be 'string', 
    # unless the left-hand side of || evaluates 
    # as True.
    # Similar to Perl 5, use with caution. 

    $string = $false || 'string'; # 0 is (normally) Bool::False: 
                                  # thus default is 'string' 
    
    # Default to the first defined value in a list
    # use the reduce operator

    $defined = [//] ($true,$false,$string,$defined,$last); # 0 
    
    # Default to the first true value in a list 
    # use the reduce operator

    $true = [||] ($true,$false,$string,$defined,$last); # 'string' 

    # To provide the last defined value in a list, as default,
    # define your own right-hand side definedness test
    # use the reduce operator

    sub infix:<\\>($lhs,$rhs){ $rhs // $lhs };
    $last = [\\] ($true,$false,$string,$defined,$last); # 0  
    
    # force strict boolean context on a variable 

    my $y is 0 but true;
    $y ||= 4;
    say $y; # 0

    my $z is 'Ovid' but false;
    $z ||= 4;
    say $z; # 4
    
=cut

my ($true,$false,$string,$defined,$last);

$false //= 0; 
say "Default for undefined: $false"; 

$string = $false || 'string'; # 0 is (normally) False: 
say "Default if False: $string"; # string 

$defined = [//] ($true,$false,$string,$defined,$last); # 0 
say "Default is first defined value of list: $defined";

$true = [||] ($true,$false,$string,$defined,$last); # 'string' 
say "Default is first True value of list: $true";

sub infix:<\\>($lhs,$rhs){ $rhs // $lhs ;}
$last = [\\] ($true,$false,$string,$defined,$last); # 0  
say "Default is last defined value of List: $last";

#my $y is 0 but true;
# TODO Error

#my $z is 'Ovid' but false;
# TODO Error

