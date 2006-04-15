package Pugs::Grammar::Perl6;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);
use Pugs::Grammar::StatementControl;
use Pugs::Grammar::Expression;
use Pugs::Grammar::Pod;

# TODO - redefine <ws> to test Pod.pm after each \n

*parse
    = Pugs::Compiler::Rule->compile( q(
    
            #{ print "trying Grammar::StatementControl::parse $_[0] \n"; }
            <?ws>?
            <Pugs::Grammar::StatementControl.parse> 
            #{ print "end of statement\n"; }
            <?ws>?
            <parse>?
            {
                #print "statement... $/ \n";
                my $match = $/{'parse'}[0];
                #print "match ", ref $match, "\n";
                #print "match: ",Dumper $match->();
                #my @a = @{ $match->() };
                return [
                    $/{'Pugs::Grammar::StatementControl.parse'}->(),
                    @{ $match->() },
                ];
            }
        |
            #{ print "trying Grammar::Expression::parse $_[0] \n"; }
            <Pugs::Grammar::Expression.parse> \;?
            #\;
            <parse>?
            {
                #print "tail ", ${$_[0]}->{tail}, "\n";
                #print "expression... $/ \n";
                my $match = $/{'parse'}[0];
                #print "match ", ref $match, "\n";
                #print "match: ",Dumper $match->();
                return [
                    $/{'Pugs::Grammar::Expression.parse'}->(),
                    @{ $match->() },
                ];
            }
        |
            { 
                #print "no match... $/ \n";
                return [];
            }
            
    ) )->code;

=for extending
BEGIN {
    __PACKAGE__->add_rule(
        xxx => q(...)
    );
    __PACKAGE__->recompile;
}
=cut

1;
