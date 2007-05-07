# preprocessor 
# from: Perl-6.0.0-STD.pm 
# to:   v6.pm

# TODO
# - preprocess {*} #=identifiers

use strict;
local $/;
my $fh;
my $src = 'Perl-6.0.0-STD.pm';
open $fh, '<', "../../src/perl6/$src";
my $in = <$fh>;
print "read ",length($in)," chars from $src\n";
die "can't read STD" unless length($in);

my $out = 'STD.pm';
print "output file is $out\n";
open $fh, '>', $out;
print $fh <<EOT;
# do not edit - generated from $src

use v6-alpha;

grammar Perl;

EOT

my @tok = (
{
    name => 'token \s+ unsp',
    ast => [ '', '', '' ]
},
{
    name => 'token \s+ unv',
    ast => [ '', '', '', '', '' ]
},
{
    name => 'token \s+ pod_comment',
    ast => [ '', '', '' ]
},
{
    name => 'constant \s+ %open2close',
    ast => [ ]
},
{
    name => 'regex \s+ bracketed',
    ast => [ '' ]
},
);
for my $tok ( @tok ) {
my ($code) = $in =~ m/(
    $tok->{name} [^{]+? 
    \{ 
        (?: \{  [^}]*?  \} | [^}] )+? 
    \}
    ;?
    \s*\n
    )
/xs;
#print "code: $code\n";
for ( @{$tok->{ast}} ) {
    unless ( $code =~ s/\{\*\}/$_/ ) {
        warn "missing {*} in ",$tok->{name};
    };
}
warn "extra {*} in ",$tok->{name}
    if $code =~ /\{\*\}/;
#print "final code: $code\n";
print $fh $code;
}

