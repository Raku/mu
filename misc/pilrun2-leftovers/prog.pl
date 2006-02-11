#!/usr/bin/perl -w

use FindBin '$Bin';
use lib ("$Bin/lib",
         "$Bin/../Perl6-Value/lib",
         "$Bin/../Perl6-Container/lib",
         "$Bin/../Perl6-MetaModel2.0", # XXX
         "$Bin/../Perl6-MetaModel2.0/lib");
use Getopt::Long qw(:config no_auto_version);

my (@eval,$repl,@inc_dirs,$compile,$as_yaml);my($debug);
use vars qw($Perl6::Run::OnPerl5::X1::BB::debug
	    $Perl6::Run::OnPerl5::X1::BB::pugs
	    $Perl6::Run::OnPerl5::X1::BB::pugs_args);
BEGIN{
GetOptions(
    'version'   => sub{ print "--version is not implemented.\n"; exit; },
    'V'         => sub{ print "$0 has no version itself.\n";
                        system("pugs","-V");
                        exit;},
    'repl'      => \$repl,
    'debug'     => \$debug,
    'I=s'       => \@inc_dirs,
    'pugs=s'    => \$Perl6::Run::OnPerl5::X1::BB::pugs,
    'w'         => \my $ignore1,
    'B=s'       => \my $ignore2,
    # kludge:
    'yaml'       => \$as_yaml,
    'e|eval=s'  => \@eval,
    'c=s'       => \$compile,
    );

$Perl6::Run::OnPerl5::X1::BB::debug = $debug;
$Perl6::Run::OnPerl5::X1::BB::pugs_args = join(" ",map{"-I$_"}@inc_dirs);
}

BEGIN {
    require Perl6::Run::OnPerl5::X1::Compile;
    if($as_yaml) {
	my $code = join(" ",@eval);
	$code .= join("\n",map{`cat $_`}@ARGV) if !@eval;
	my $cc = Perl6::Run::OnPerl5::X1::CodeCompile->new('p6'=>$code)->compile;
	print $cc->as_pil_tree_yaml;
	exit(0);
    }
}

use Perl6::Run::OnPerl5::X1;
use Perl6::Run::OnPerl5::X1::Compile;
use Perl6::Run::OnPerl5::X1::Api;
use Perl6::Run::OnPerl5::X1::Repl;
use strict;


for (@inc_dirs) {p6_eval("push(\@INC,'$_');");}

for my $e (@eval) {
    p6_eval($e);
}
for my $fn (@ARGV) {
    p6_eval_file($fn);
}
if($compile) {
    open(F,"<$compile") or die "open $compile: $!\n";
    my $f = do{local $/=undef; <F>}; close F;
    my $type = $compile =~ /\.pil$/ ? 'pil_code' : 'p6';
    print Perl6::Run::OnPerl5::X1::CodeCompile->new($type=>$f)->as_p5();
}
if ($repl || (!@eval && !@ARGV && !$compile)) {
    Perl6::Run::OnPerl5::X1::Repl::run_repl();
}
exit(0);

__END__
