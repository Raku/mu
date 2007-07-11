#!/usr/bin/perl
use warnings;
use strict;
BEGIN {
    if (-d 'lib5'){
        use lib 'lib5';
    }
}
use Perlhint::Parse;
use Perlhint::Lookup;
use Data::Dumper;

my $query = shift @ARGV || die "Usage: $0 <character>\n";
print "Looking up '$query'...\n";
my $parser = Perlhint::Parse->new({filename => 'data/perl-6.en.dat'});
my $l = Perlhint::Lookup->new($parser->records);

my @out = $l->lookup($query);

#print Dumper($l);

my $delim = "=" x 20;
my %legend = (
        syn     => 'Synopsis',
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
