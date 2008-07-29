#!/usr/bin/perl

use 5.008006;
use strict;
use warnings;

my @inc;
BEGIN {
    @inc = (-e '../../inc' and -e '../../VICTUALS')
        ? (prefix => '../../inc')
        : (-e '../../../../inc' and -e '../../../../VICTUALS')
        ? (prefix => '../../../../inc')
        : ()
}
use lib '../..', (@inc ? $inc[-1] : ());
use inc::Module::Install (@inc, '.');
use File::Spec;

    name('v6');
all_from('lib/v6.pm');

requires('Pugs::Compiler::Rule' => '0.37');
requires('Module::Compile'      => '0.20');
requires('Scalar::Util'         => '0');
requires('Quantum::Superpositions' => '0');
requires('Devel::Caller'        => '0.10');
requires('Data::Bind'           => '0.28');
requires('Sub::Multi'           => '0.003');
requires('Moose'                => '0.10');
#requires('Pugs::Emitter::Perl6::Perl5' => '0');
#requires('Lexical::Alias'       => '0');
requires('YAML::Syck'           => '0');

recommends('Perl::Tidy'         => '0');

# Parse MANIFEST and copy the tests
my $rewrite_test;
$rewrite_test = sub {
    my ($out, $text) = @_;
    
    # split big tests in smaller files
    if ( $out =~ /\.t$/ && length($text) > 30000 ) {
        $out =~ s/-?(\d*)\.t$/ "-" . sprintf("%02d", (($1||0)+0)) . ".t" /e;
        my ($part, $text) = $text =~ /^(.{20000,}?\n)(.*)/s;
        $rewrite_test->($out, $part);
        $out =~ s/-?(\d*)\.t$/ "-" . sprintf("%02d", (($1||0)+1)) . ".t" /e;
        $rewrite_test->($out, $text);
        return;
    }

    # fix problems caused by splitting; update the "todo" lists
    
    if ( $out eq 't/regex/from_perl6_rules/named_chars-00.t' ) {
        my $cpan_tester_errors = "148, 149, 158, 160";
        $text =~ s/^\s*plan \d+;/plan 200;/m;
        $text =~ s/^\s*force_todo.*?\n/force_todo($cpan_tester_errors)\n/m;
        $text .= "}\n";
    }
    if ( $out eq 't/regex/from_perl6_rules/named_chars-01.t' ) {
        $text = 
              "plan 219;\n"
            . "{\n"
            . $text;
    }
    if ( $out =~ m{t/regex/from_perl6_rules/propcharset-(\d+).t} ) {
        # I can't reproduce these errors - fglock
        my %cpan_tester_errors = (
            0 => "112, 113, 139, 140, 147..149, 156..158, 166..170, 177",
            1 => "1, 2, 10..14, 21..23, 31..35, 42",
            2 => "108, 109, 141, 142",
            3 => "1..2, 118, 119, 126, 145..146",
            4 => "39, 40, 47, 96, 97, 99, 100, 105, 106, 113, 135..136, 143..145, 152..154, 161, 168..169, 207",
            5 => "1, 8..10, 17, 69..70, 81..82, 93..94, 108..109, 116, 126..127, 132..133, 137, 168..169, 176..178, 185, 201, 202",
            6 => "4, 5, 52..53, 60, 133..134, 141..143, 150..152, 159, 169..170, 177",
            7 => "11..12, 50..51, 92..93, 101..103, 110..111, 155..156",
            8 => "163..164, 171..173",
            9 => "7",
            10 => "48, 49",
            );
        if ( $1 eq '00' ) {
            $text =~ s/^\s*plan \d+;//m;
            $text =~ s/^\s*force_todo.*?\n//m;
            $text .= "}\n";
        }
        $text = "{\n" . $text   if $1 eq '12';
        $text = 
              ( $1 eq '00' ? "plan 177;\nforce_todo(111, $cpan_tester_errors{0});\n" 
              : $1 eq '01' ? "plan 190;\nforce_todo($cpan_tester_errors{1});\n" 
              : $1 eq '02' ? "plan 185;\nforce_todo($cpan_tester_errors{2});\n" 
              : $1 eq '03' ? "plan 205;\nforce_todo(120, 122, $cpan_tester_errors{3});\n" 
              : $1 eq '04' ? "plan 207;\nforce_todo(137, 139, 167, $cpan_tester_errors{4});\n" 
              : $1 eq '05' ? "plan 206;\nforce_todo($cpan_tester_errors{5});\n" 
              : $1 eq '06' ? "plan 182;\nforce_todo(171, 173, $cpan_tester_errors{6});\n" 
              : $1 eq '07' ? "plan 172;\nforce_todo(49, 94, 96, 109, 154, $cpan_tester_errors{7});\n" 
              : $1 eq '08' ? "plan 173;\nforce_todo($cpan_tester_errors{8});\n" 
              : $1 eq '09' ? "plan 173;\nforce_todo($cpan_tester_errors{9});\n" 
              : $1 eq '10' ? "plan 173;\nforce_todo($cpan_tester_errors{10});\n" 
              : $1 eq '11' ? "plan 173;\n" 
              : $1 eq '12' ? "plan 133;\n" 
              : "" )
            . $text;
    }
    if ( $out =~ m{t/regex/from_perl6_rules/properties-(\d+).t} ) {
        if ( $1 eq '00' ) {
            $text =~ s/^\s*plan \d+;//m;
            $text =~ s/^\s*force_todo.*?\n//m;
            $text .= "}\n";
        }
        $text = "{\n" . $text   if $1 eq '09';
        $text = 
              ( $1 eq '00' ? "plan 189;\nforce_todo(4, 11, 99, 5..6, 12..13, 100..101, 134..135, 151..152, 168..169);\n" 
              : $1 eq '01' ? "plan 199;\n" 
              : $1 eq '02' ? "plan 216;\nforce_todo(157, 158, 22..23, 59..60, 156, 162, 204..205);\n\n" 
              : $1 eq '03' ? "plan 219;\n" 
              : $1 eq '04' ? "plan 213;\nforce_todo(185, 10..11, 20..21, 30..31, 161, 167, 186..187);\n" 
              : $1 eq '05' ? "plan 181;\nforce_todo(42, 43, 89, 125, 126, 144, 167, 168, 41, 47, 90..91, 124, 131..133, 145..146, 159..161, 165..166, 170..171, 175);\n\n" 
              : $1 eq '06' ? "plan 178;\n" 
              : $1 eq '07' ? "plan 188;\nforce_todo(999);\n" 
              : $1 eq '08' ? "plan 179;\nforce_todo(80..81, 118..119, 166..167);" 
              : $1 eq '09' ? "plan 119;\n" 
              : "" )
            . $text;
    }    
    if ( $out eq 't/regex/p6regex/p6regex.t' ) {
        my $cpan_tester_errors = ', 31..32, 36, 114..115, 117, 121..122, 124, 129..130,
                143..144, 157..158, 171..172, 185..186, 191..192,
                194, 214, 219, 221, 256, 309, 311..312, 315..318,
                442, 444..445, 447, 449..451, 462..463, 466..469,
                472, 474..475, 477..478, 480, 483, 500..501,
                524..525, 529..533';
        $text =~ s/^\s*plan \d+;/plan 535;\nforce_todo(18..29,49..54,62,64,66,68,77,79,81..92,94..96,101..113,247,254..255,257,259..263,266,269,274,284,286,301,303..304,314,332..372,374..388,393..394,398,400..402,404..433,484..499,502,504,507,516..522,526..528$cpan_tester_errors);/m;
    }
    if ( $out eq 't/regex/perl5_9.t' ) {
        $text =~ s/^\s*plan \d+;/plan 84;/m;
        $text =~ s/^\s*force_todo.*?\n/force_todo(18,34,40,51,57,66,67);\n/m;
    }
    
    # add "use Test" to most files
    if ( $out ne 't/01-sanity/05-sub.t' ) {
        $text = "use Test;\n" . $text;
    }
    
    # add "use v6-alpha" everywhere
    # $text =~ s/^use v6;/use v6-alpha;/m;
    $text = "use v6-alpha;\n" . $text;

    open( my $fh, ">", $out ) || 
        die "can't create $out $!" ;
    print $fh $text;
};

if (@inc) {
    use File::Path;
    use File::Basename;

    open MANIFEST, '< MANIFEST' or die "Cannot open MANIFEST: $!";
    my %seen;
    while (<MANIFEST>) {
        chomp;
        /^t\// or next;
        s/-\d+\.t$/.t/;
        next if $seen{$_};
        $seen{$_} = 1;
        my $out = $_;
        mkpath(dirname($out));
        my $src = "$inc[-1]/../$out";
        $src = "$inc[-1]/../ext/Test/lib/Test.pm" if $_ eq 't/Test.pm';
        next unless -f $src;
        my $text = do { local( @ARGV, $/ ) = $src ; <> } ;
        $rewrite_test->( $out, $text );
    }
}

makemaker_args  (
    test => {
        TESTS => join ' ',
            map {glob File::Spec->catfile('t', ('*') x $_, '*.t')} 0..3
    },
);

clean_files(map {glob File::Spec->catfile('t', ('*') x $_, '*.{t,pm}c')} 0..3);

&WriteAll;

# Evil hack to add PERL6LIB path to Makefile
system($^X, '-p', '-i.old', '-e', 's+(?=test_harness)+%ENV=(%ENV,qw( PERL6LIB ./t) );+g', 'Makefile');

# clean the precompiled rules cache
eval {
    require Cache::FileCache;
    my $cache = new Cache::FileCache( { 'namespace' => 'v6-rules' } );
    $cache->Clear;
};

# see Module::Compile
    # precompile Perl6Prelude.pm and Test.pm
    pmc_support();

