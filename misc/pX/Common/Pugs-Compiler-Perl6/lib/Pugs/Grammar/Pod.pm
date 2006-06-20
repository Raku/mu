package Pugs::Grammar::Pod;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);

# placeholder for the Pod grammar

# XXX Pod can only start at a line start

BEGIN {
    __PACKAGE__->add_rule(
        "=begin" => q(
                #{ print "pod - $_[0]\n" }
                .* \n \= end .*? \n
            
            {
                return { pod_block => $() ,}
            }
    ) );
    __PACKAGE__->recompile;
}


1;
