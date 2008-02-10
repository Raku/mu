#!/usr/bin/perl
use warnings;
use strict;
use Cwd qw/cwd chdir abs_path/;
use File::Basename qw/dirname/;

use lib abs_path(dirname($0)) . '/lib5' ;
use Perlhints::Parse;
use Perlhints::Lookup;
use Data::Dumper;

chdir dirname($0) ;

my $query = shift @ARGV || die "Usage: $0 <character>\n";
print "Looking up '$query'...\n";
my $parser = Perlhints::Parse->new({filename => '../../src/perl6/STD.pm'});
#print Dumper $parser;
my $l = Perlhints::Lookup->new($parser->records);

my @out = $l->lookup($query);

#print Dumper($l);

my $delim = "=" x 20;
my %legend = (
        id      => 'Identifier',
        syn     => 'Syntax',
        desc    => 'Description',
        name    => 'Name',
        ex      => 'Example(s)',
        con     => 'Context',
        );
my @order = qw(syn name con desc ex);

for my $r (@out){
    print $delim, $/;
    OUT:
    for my $o (@order){
        next OUT unless ($r->{$o});
        print $legend{$o}, ":\n";
        my $str = $r->{$o};
        $str =~ s/\n/\n\t/g;
        $str = "\t" . $str;
        print $str, "\n";
    }
}

# vim: ts=4 sw=4 expandtab
