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
use Regexp::Common;
use Data::Dumper;

# --------------------------------------------------
# Regexps
# --------------------------------------------------



our $re_sigil = '[\$\@\%]';
our $re_symbol = '\w[\w\d]*';
our $re_condition = '(?:if|elsif|else|unless|while|unless)';
our $re_reserved = "(?:$re_condition)";
our $re_varname = $re_sigil.$re_symbol;
our $re_comma = '(?:,|=>)';
our $re_scopeop = '(?:my|our)';

# conv_foreach is moved out of CONVERSION_SUBS section
# temporarily for the use of $RE{balanced}{-parens=>'{}'};
sub conv_foreach {
    $_ = shift;
    my $foreach_kw = 'for(?:each)?';
    s/${foreach_kw}\s*${re_scopeop}?\s+(${re_varname})\s*\(((?:\@${re_symbol}\s*,\s*)*\@${re_symbol})\)\s*\{/for $2 -> $1 {/g;
    s/${foreach_kw}\s*\(((?:\@${re_symbol}\s*,\s*)*\@${re_symbol})\)\s*\{/for $1 {/g;
    $_
}


my $conv = <<'CONVERSION_SUBS';
conv_array {
    s/\$(${re_symbol}\[.+])/\@$1/g;
    s/\$#(${re_symbol})/\@$1.last/g;  # last_index
}

conv_hash {
    s/\$(${re_symbol}\{.+?\})/%$1/g;
}

conv_reference {
    s/\$(${re_symbol})->(\[.+?])/\$$1$2/g;
    s/\$(${re_symbol})->(\{.+?\})/\$$1$2/g;
    s/\$(${re_symbol})->(\(.+?\))/\$$1$2/g;
    s/(${re_varname})\s*=\s*\\(${re_varname})/$1 = $2/g;
}

conv_self {
    my $sub_context;
    # Check if it's now in 'sub' context
    if(/\bsub\s*(${re_symbol}\s*)?($RE{balanced}{-parens=>'{}'})/){
       my ($pre, $post) = ($`, $');
       my ($n, $c) = ($1, $2);
       my (@s, @e) = (@-, @+);

#       print Dumper \@s, \@e;
#       foreach my $i (0..$#s){
#          print "$i >> ", substr($_, $n[$i], $e[$i] - $s[$i]),$/;
#       }

       if($n !~ /${re_reserved}/){
           $c =~ s/\$self->{(${re_symbol})}/\$.$1/g;
           $c =~ s/\$self->/\$./g;
       }
       $_ = $` . 'sub ' . $n . $c . $';
    }
}




conv_package {
    s/package\s+((?:${re_symbol}::)*${re_symbol})\s*;/class $1;/g;
}

conv_open {
    s/open (${re_scopeop}?\s*${re_varname})\s*${re_comma}\s*($RE{quoted})/$1 = open $2/g;
#    s/open (\${re_varname})\s*${re_comma}\s*($RE{quoted})/$1 = open $2/g;
}

conv_condition {
    if(/(${re_condition})\s*($RE{balanced}{-parens=>'()'})\s*($RE{balanced}{-parens=>'{}'})/){
        my ($c, $t, $p) = ($1, $2, $3);
        $t =~ s/^\((.+)\)$/$1/;
        $_ = "$c $t $p";
    }
}

CONVERSION_SUBS

my $newconv;
$conv =~ s/^\s*#.+$//g;
while($conv =~ /(conv_\w+)\s*($RE{balanced}{-parens=>'{}'})\n/sg){
    my ($n, $c) = ($1, $2);
    $c =~ s/^\{[\s\n]*(.+)\}/$1/s;
    $newconv .= <<".";
sub $n {
  \$_ = shift;
  $c;
  \$_;
}

.
}
$conv = $newconv;
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

    open $f, 'file';      -->  $f = open 'file';
    open my $f, 'file';   -->  my $f = open 'file';
    open our $f, 'file';  -->  our $f = open 'file';

    sub { $self->blah }         --> sub { $.blah };
    sub my_sub { $self->blah }  --> sub my_sub { $.blah };

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

              conv_self conv_array conv_hash conv_reference
              conv_foreach conv_package conv_open conv_condition
          
              )){
    no strict;
if(main->can($c)){
    $src = &{$c}($src);
    print "---- ( $c ) ----\n$src\n" if $DEBUG;
}
else {
    print "Skipping conversion rule: $c\n";
}
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
