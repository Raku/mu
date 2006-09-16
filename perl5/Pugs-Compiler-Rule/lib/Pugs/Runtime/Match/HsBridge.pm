package Pugs::Runtime::Match::HsBridge;

use strict;
use warnings;
use Pugs::Grammar::Base;
use Pugs::Compiler::Regex;
use base 'Pugs::Grammar::Base';

sub __CMD__ {
    local $| = 1;

    # Command line shell interface - compatible with run_pge.pir
    binmode STDIN, ':utf8';
    binmode STDOUT, ':utf8';

    my @subrules;
    while (<STDIN>) {
        #/^(\w+) (\d+) (\d+)/ or die "Unrecognized command: $_";
        /^(\w+) \d+ \d+/ or die "Unrecognized command: $_";
        my $cmd = $1; # my ($cmd, $sz1, $sz2) = ($1, $2, $3);

        my $line1 = <STDIN>; chomp($line1); $line1 =~ s{\\(?:(\\)|(n))}{$1 ? $1 : "\n"}eg;
        my $line2 = <STDIN>; chomp($line2); $line2 =~ s{\\(?:(\\)|(n))}{$1 ? $1 : "\n"}eg;

        if ($cmd eq 'add_rule') {
            push @subrules, $line1, $line2;
        }
        elsif ($cmd eq 'match') {
            if (my $rv = eval { __PACKAGE__->__RUN__($line1, $line2, @subrules) }) {
                my $len = bytes::length($rv)+1;
                print "OK $len\n";
                print "$rv\n\n";
            }
            else {
                my $err = $@;
                $err =~ s/([\\\n])/\\$1/g;
                print "$err\n";
            }
            @subrules = ();
        }
        else {
            die "Unrecognized command: $cmd";
        }
    }
}

sub __RUN__ {
    my $self        = shift;
    my $match_text  = shift;
    my $rule_text   = shift;
    my %subrules    = @_;

    while (my ($name, $body) = each %subrules) {
        my %opts = (grammar => __PACKAGE__);
        ($1 and $opts{$1} = 1) while $body =~ s/^:(\w*)\(1?\)\[(.*)\]\z/$2/s;
        Pugs::Compiler::Regex->install( $name => $body, \%opts );
    }

    my %opts = (grammar => __PACKAGE__);
    ($1 and $opts{$1} = 1) while $rule_text =~ s/^:(\w*)\(1?\)\[(.*)\]\z/$2/s;

    my $rule    = Pugs::Compiler::Regex->compile( $rule_text, \%opts );
    my $match   = $rule->match( $match_text );

    # Uninstall the previously registered subrules. 
    no strict 'refs';
    #delete @{__PACKAGE__.'::'}{keys %subrules};

    return $match->dump_hs;
}

1;
