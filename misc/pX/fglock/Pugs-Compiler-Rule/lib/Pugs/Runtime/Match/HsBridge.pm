package Pugs::Runtime::Match::HsBridge;

use utf8;
use strict;
use warnings;
use Pugs::Grammar::Base ();
use Pugs::Compiler::Regex ();
use base 'Pugs::Grammar::Base';

BEGIN { local $@; eval { require 'utf8_heavy.pl' } }

use ops ($ENV{PUGS_SAFEMODE} ? (':default', 'binmode', 'entereval') : ());

sub __RUN__ {
    my $self        = shift;
    my $match_text  = shift;
    my $rule_text   = shift;
    my %subrules    = @_;

    while (my ($name, $body) = each %subrules) {
        my %sub_opts = (grammar => __PACKAGE__);
        ($1 and $sub_opts{$1} = 1) while $body =~ s/^:(\w*)\(1?\)\[(.*)\]\z/$2/s;
        Pugs::Compiler::Regex->reinstall( $name => $body, \%sub_opts );
    }

    my %opts = (grammar => __PACKAGE__);
    ($1 and $opts{$1} = 1) while $rule_text =~ s/^:(\w*)\(1?\)\[(.*)\]\z/$2/s;

    # L<S05/Modifiers/"The C<:ratchet> modifier also implies that the anchoring">
    if ( $opts{ratchet} ) {
        $rule_text  = "^$rule_text" unless $opts{p} or $opts{pos};
        $rule_text .= '$'           unless $opts{c} or $opts{continue};
    }

    local $SIG{__WARN__} = sub { 1 };

    my $rule    = Pugs::Compiler::Regex->compile( $rule_text, \%opts );
    my $match   = $rule->match( $match_text );

    return $match->dump_hs;
}

sub __CMD__ {
    local $| = 1;

    # Command line shell interface - compatible with run_pge.pir
    if ($] >= 5.007) {
        binmode STDIN, ':utf8';
        binmode STDOUT, ':utf8';
    }

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

1;
