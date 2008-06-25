package Perl6in5::Grammar;

use strict;
use warnings;
no warnings qw{ reserved closure recursion prototype };

use Perl6in5::Compiler::Trace;
use Perl6in5::Compiler::Parser ':all';
use Filter::Simple;
use Data::Dumper;
use File::Slurp;
use Memoize;

FILTER {
    
    # in order to enable adaptive grammars, take the entire grammar input here,
    #       serialize it using Data::Dumper (or something), and append (or
    #       prepend) it to the grammar file as a string assigned to a global
    #       scalar.
    
    my @patterns = m/^pattern\s+([A-Za-z_]\w*)\s+\{/mg;
    s/^pattern\s+([A-Za-z_]\w*)\s+\{/pattern '$1' => sub {/mg;
    s/'(.)'/lit('$1')/mg;
    $_ = join('',map {"sub $_(@);"} @patterns).$_;
    
};

{
    my ( %MP );
    sub pattern {
        my ($name,$code) = @_;
        my $a;
        $a = sub {
            my @r = @_;
            return $MP{$name.$code} if exists $MP{$name.$code};
            my $p;
            $p = parser( sub { $code->($p,@r)->(@_) }, $name );
            $N{$p} = (scalar @r)?($name.'( '.join( "','",map($N{$_},@r)).' )'):$name; # trace
            $MP{$name.$code} = $p;
        };
        {
            $Perl6in5::Grammar::{$name} = $a;
        }
    }
}

END {
    my ($input,$name) = $ARGV[0]?
        (scalar(read_file($ARGV[0])),$ARGV[0]):
        (join('',(<>)),'STDIN');
    
    $input = { inp => $input, 'pos' => 0, line => 1, name => $name, col => 1, mut => 0 , success => -1, fated => 0 , backed => 0, ast=>[], hits=>[], both=>0 };
    
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
        print STDERR $msg."\n".Dumper($r->{hits});
        print "stats: ".Dumper(\%stat)."\n" if keys %stat;
        print "names: ".scalar(keys(%N))."\n" if keys %N;
        exit 255;
    } else {
        print "tree: ".Dumper($r->{hits})."\n";
        print "stats: ".Dumper(\%stat)."\n" if keys %stat;
        print "names: ".scalar(keys(%N))."\n" if keys %N;
        exit 0;
    }
}

1;
