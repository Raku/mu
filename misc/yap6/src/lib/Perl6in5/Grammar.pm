package Perl6in5::Grammar;

use strict;
use warnings;
no warnings qw{ reserved closure recursion };

use base 'Exporter';
our @EXPORT = qw ( run_program );

use Perl6in5::Compiler::Parser ':all';
use Filter::Simple;
use Data::Dumper;
use File::Slurp;

FILTER {
    my @rules = m/^rule\s+([A-Za-z_]\w*)\s+\{/mg;
    s/^rule\s+([A-Za-z_]\w*)\s+\{/rule '$1' => sub {/mg;
    s/'(.)'/hit('$1')/mg;
    s/\}\{/}, sub {/mg;
    s/\+\+/ + nothing/mg;
    s/--/ - nothing/mg;
    $_ = join('',map {"sub $_();\n"} @rules).$_;
};

{
    my @order;
    my %rules;
    sub rule {
        my ($name,$code) = @_;
        my $stub = parser { $rules{$name}->(@_) };
        $N{$stub} = ucfirst($name);
        {
            $Perl6in5::Grammar::{$name} = sub() {$stub};
        }
        push(@order,[$name,$code]);
    }
    sub finish_rule_creation {
        for (@order) {
            my ($name,$code) = @$_;
            $rules{$name} = $code->();
        }
    }
}

END {
    my ($input,$name) = $ARGV[0]?
        (scalar(read_file($ARGV[0])),$ARGV[0]):
        (join('',(<>)),'STDIN');
    
    $input = { inp => $input, 'pos' => 0, line => 1, name => $name, col => 1, mut => 0 , success => -1, fated => 0 , backed => 0, ast=>[] };
    #  success 0 means the branch failed
    #  success 1 means the branch succeeded (to EOI)
    #  success -1 means outcome not yet determined
    finish_rule_creation;
    
    my $r = (program()->($input));
    unless ($r->{success}) {
        my $msg;
        if ($r->{expected} eq 'EOI') {
            $msg = "syntax error near the end of input";
        } else {
            $msg = "syntax error at line "
            .($r->{line})." col ".$r->{col}." near ".
            (sprintf '%.20s', Dumper(left($r))).
            ($r->{expected}?"\nExpected: ".Dumper($r->{expected}).".":'');
        }
        print STDERR $msg."\n".Dumper($r->{ast});
        exit 255;
    } else {
        print "parsed: ".Dumper($r->{ast})."\n";
        exit 0;
    }
}

1;
