#!/usr/bin/perl

# --------------------------------------------------
# This script converts p5 source to p6. Currently,
# only regexp matching and substitution are used, and
# this brute-force, simplistic, and extremely foolish
# script, ignoring deeper context, needs lots of
# improvement. Due to regexp's limits, it is only
# expected to serve as a tool to somewhat speed up
# the rewrite of existing p5 programs instead of doing
# full conversions. :-/
#
# c.f. Mini HowTo: How to port Perl 5 modules to Perl 6:
#      http://perlmonks.org/index.pl?node_id=442402
#
# --------------------------------------------------
# Author: Yung-chung Lin (xern@cpan.org)
# --------------------------------------------------

use strict;
use IO::All;


# --------------------------------------------------
# Regexps
# --------------------------------------------------


our $patt_sigil = '[\$\@\%]';
our $patt_symbol = '\w[\w\d]*';
our $patt_varname = $patt_sigil.$patt_symbol;
our $patt_comma = '(?:,|=>)';
our $patt_scopeop = '(?:my|our)';

my $conv = <<'CONVERSION_SUBS';
conv_array {
    s/\$(${patt_symbol}\[.+])/\@$1/g;
    s/\$#(${patt_symbol})/\@$1.last/g;  # last_index
}

conv_hash {
    s/\$(${patt_symbol}\{.+?\})/%$1/g;
}

conv_reference {
    s/\$(${patt_symbol})->(\[.+?])/\$$1$2/g;
    s/\$(${patt_symbol})->(\{.+?\})/\$$1$2/g;
    s/\$(${patt_symbol})->(\(.+?\))/\$$1$2/g;
    s/(${patt_varname})\s*=\s*\\(${patt_varname})/$1 = $2/g;
}

conv_self {
    s/\$self->{(${patt_symbol})}/\$.$1/g;
}

conv_foreach {
    my $foreach_kw = 'for(?:each)?';
    s/${foreach_kw}\s*${patt_scopeop}?\s+(${patt_varname})\s*\(((?:\@${patt_symbol}\s*,\s*)*\@${patt_symbol})\)\s*\{/for $2 -> $1 {/g;

    s/${foreach_kw}\s*\(((?:\@${patt_symbol}\s*,\s*)*\@${patt_symbol})\)\s*\{/for $1 {/g;
}

conv_package {
    s/package\s+((?:${patt_symbol}::)*${patt_symbol})\s*;/class $1;/g;
}

CONVERSION_SUBS

$conv =~ s/(conv_.+?)\s*\{(.*?)?\}\n/sub $1 \{ \$_ = shift; $2; \$_\}/sg;

eval $conv;
die $@ if $@;


my $src;
my $srcfile;
my $DEBUG;


if(!@ARGV || $ARGV[0] eq '-h'){ 
    print( <<'HELP') and exit;

<< USAGE >>

 % p526.pl        # Convert from p5 code to p6 code

    -c            # Show conversion rules
    -e            # Convert one-liner
    -s            # List supported features
    -d            # Dump debugging information
    p5_source.pl  # Convert the file. The script appends
                  # '.p6' to the original name

HELP
}

while(my $arg = shift @ARGV){
    if($arg eq '-e'){
	$src = shift(@ARGV) or die "Enter a one-liner";
    }
    elsif($arg eq '-d'){
	$DEBUG = 1;
    }
    elsif ($arg eq '-c'){
	no strict;
	print "---- Regexps ----\n";
	print map{ "$_ => ${$_}\n" } sort grep { /patt/ } keys %main::;
	print "\n---- Conversion subs ----\n";
	print $conv;
    }
    elsif  ($arg eq '-s'){
	print <<'SUPPORTED_FEATURES' and exit;

<< SUPPORTED FEATURES >>

    $v[0] --> @v[0];

    $v{0} --> %v{0};

    $#v   --> @v.last_index;

    $r = \@v --> $r = @v;
    $r = \%v --> $r = %v;
    
    $r->[0]  --> $r[0];
    $r->{0}  --> $r{0};
    $r->(0)  --> $r(0);


    foreach (@a)        -->  for @a
    foreach my $r (@a)  -->  for @a -> $r

    package MY::Class;  -->  class MY::Class;

SUPPORTED_FEATURES
}
    else {
	$srcfile = $arg;
        local $/;
	open my $f, $srcfile or die "Could'nt open file $srcfile";
	$src = <$f>;
    }
}

for my $c (qw(
    conv_self
    conv_array
    conv_hash
    conv_reference
    conv_foreach
    conv_package
	      )){
    no strict;
    $src = &{$c}($src);
    print "---- ( $c ) ----\n$src\n" if $DEBUG;
}

if($srcfile){
    open my $f, '>', "${srcfile}.p6"
	or "Couldn't open file ${srcfile}.p6 for writing";
    print {$f} $src;
}
else {
    print "$src\n";
}

__END__
