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

requires('Pugs::Compiler::Rule' => '0.33');
requires('Module::Compile'      => '0.20');
requires('Scalar::Util'         => '0');
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
        $text =~ s/^\s*plan \d+;/plan 200;/m;
        $text =~ s/^\s*force_todo.*?\n//m;
        $text .= "}\n";
    }
    if ( $out eq 't/regex/from_perl6_rules/named_chars-01.t' ) {
        $text = 
              "plan 219;\n"
            . "{\n"
            . $text;
    }
    if ( $out =~ m{t/regex/from_perl6_rules/propcharset-(\d+).t} ) {
        if ( $1 eq '00' ) {
            $text =~ s/^\s*plan \d+;//m;
            $text =~ s/^\s*force_todo.*?\n//m;
            $text .= "}\n";
        }
        $text = "{\n" . $text   if $1 eq '12';
        $text = 
              ( $1 eq '00' ? "plan 177;\nforce_todo(111);\n" 
              : $1 eq '01' ? "plan 190;\n" 
              : $1 eq '02' ? "plan 185;\n" 
              : $1 eq '03' ? "plan 205;\nforce_todo(120, 122);\n" 
              : $1 eq '04' ? "plan 207;\nforce_todo(137, 139, 167);\n" 
              : $1 eq '05' ? "plan 206;\n" 
              : $1 eq '06' ? "plan 182;\nforce_todo(171, 173);\n" 
              : $1 eq '07' ? "plan 172;\nforce_todo(49, 94, 96, 109, 154);\n" 
              : $1 eq '08' ? "plan 173;\n" 
              : $1 eq '09' ? "plan 173;\n" 
              : $1 eq '10' ? "plan 173;\n" 
              : $1 eq '11' ? "plan 173;\n" 
              : $1 eq '12' ? "plan 133;\n" 
              : "" )
            . $text;
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

