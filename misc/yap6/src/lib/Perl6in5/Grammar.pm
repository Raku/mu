package Perl6in5::Grammar;

use strict;
use warnings;
no warnings qw{ reserved closure recursion prototype };

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
    my @patterns = m/^pattern\s+([A-Za-z_]\w*)\s+\{/mg;
    s/^pattern\s+([A-Za-z_]\w*)\s+\{/pattern '$1' => sub {/mg;
    s/'(.)'/lit('$1')/mg;
    $_ = join('',map {"sub $_(@);"} @patterns).$_;
};

sub pattern {
    my ($name,$code) = @_;
    my ($continuation, $stub);
    $continuation = sub {
        my @r = @_;
        my $p;
        $p = parser { $code->($p,@r)->(@_) };
        $N{$p} = (scalar @r)?($name.'( '.join( "','",map($N{$_},@r)).' )'):$name;
        $p;
    };
    {
        $Perl6in5::Grammar::{$name} = $continuation;
    }
}

END {
    my ($input,$name) = $ARGV[0]?
        (scalar(read_file($ARGV[0])),$ARGV[0]):
        (join('',(<>)),'STDIN');
    
    $input = { inp => $input, 'pos' => 0, line => 1, name => $name, col => 1, mut => 0 , success => -1, fated => 0 , backed => 0, ast=>[[],[]] };
    
    my $r = (program()->($input));
    
    $Data::Dumper::Indent = 0;
    $Data::Dumper::Terse = 1;
    unless ($r->{success}) {
        my $msg;
        if (defined $r->{expected} && $r->{expected} eq 'EOI') {
            $msg = "statement not terminated properly near the end of input";
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
        print "stats: ".Dumper(\%stat) if keys %stat;
        exit 0;
    }
}

1;
