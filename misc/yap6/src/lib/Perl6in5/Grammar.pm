package Perl6in5::Grammar;

use strict;
use warnings;
no warnings qw{ reserved closure recursion };

use Scalar::Util qw( weaken );

use Perl6in5::Compiler::Parser ':all';

use base 'Exporter';
our @EXPORT = qw ( run_program head tail );

use Filter::Simple;
use Data::Dumper;
use File::Slurp;

sub head { &Perl6in5::Compiler::Parser::head(@_) }
sub tail { &Perl6in5::Compiler::Parser::tail(@_) }

FILTER {
    my @rules = m/^rule\s+([A-Za-z_]\w*)\s+\{/mg;
    my @lrules = m/^lrule\s+([A-Za-z_]\w*)\s+\{/mg; # I suck
    s/^rule\s+([A-Za-z_]\w*)\s+\{/rule '$1' => sub {/mg;
    s/^lrule\s+([A-Za-z_]\w*)\s+\{/lrule '$1' => sub {/mg;
    s/'(.)'/lit('$1')/mg;
    $_ = join('',map {"sub $_();"} @rules).$_;
    $_ = join('',map {"sub $_;"} @lrules).$_;
};

my (@order,%rules,@lorder);

sub rule {
    my ($name,$code) = @_;
    my $stub;
    my $tmp = $stub = parser {
        $rules{$name}->(@_);
    };
    weaken($stub);
    $N{$stub} = ucfirst($name);
    {
        $Perl6in5::Grammar::{$name} = sub() {$stub};
    }
    push(@order,[$name,$code]);
}

sub lrule {
    my ($name,$code) = @_;
    # generate a function that generates parser generators that curry eachself.
    my $stub = sub {
        my $s = shift;
        my $p;
        $p = $code->($p,$s);
        $N{$p} = $name.'( '.$N{$s}.' )';
        $p;
    };
    {
        $Perl6in5::Grammar::{$name} = sub { $stub->($_[0]) };
    }
}

END {
    $rules{$_->[0]} = $_->[1]->() for (@order);
    my ($input,$name) = $ARGV[0]?
        (scalar(read_file($ARGV[0])),$ARGV[0]):
        (join('',(<>)),'STDIN');
    
    $input = { inp => $input, 'pos' => 0, line => 1, name => $name, col => 1, mut => 0 , success => -1, fated => 0 , backed => 0, ast=>[[],[]] };
    
    my $r = (program()->($input));
    
    $Data::Dumper::Indent = 1;
    $Data::Dumper::Terse = 1;
    unless ($r->{success}) {
        my $msg;
        if (defined $r->{expected} && $r->{expected} eq 'EOI') {
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
