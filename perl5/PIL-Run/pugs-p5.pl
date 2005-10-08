#!/usr/bin/perl -w

use FindBin '$Bin';
use lib ("$Bin/lib",
         "$Bin/..",
         "$Bin/../Perl6-Value/lib",
         "$Bin/../Perl6-Container/lib",
         "$Bin/../Perl6-MetaModel", # XXX
         "$Bin/../Perl6-MetaModel/lib");
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Terse = 1;
require YAML;
use Scriptalicious 1.05;
use Getopt::Long qw(:config no_auto_version);

my (@eval, $repl,$warn,$timeout,@inc_dirs);my($debug);my($ignore);
use vars qw($PIL::Run::EvalX::pugs);
GetOptions(
    'version'   => sub{ print "--version is not implemented.\n"; exit; },
    'V'         => sub{ print "$0 has no version itself.\n";
                        system("pugs","-V");
                        exit;},
    'e|eval=s'  => \@eval,
    'repl'      => \$repl,
    'w'         => \$warn,
    'timeout=i' => \$timeout,
    'debug'     => \$debug,
    'I=s'       => \@inc_dirs,
    'pugs=s'    => \$PIL::Run::EvalX::pugs,
    'B=s'       => \$ignore,
    );

$timeout = defined $timeout ? $timeout : $ENV{PUGS_HACK_TIMEOUT};
$timeout = 1*60 if !defined($timeout) && @ARGV && !$repl;
local $SIG{ALRM} = sub { die "timeout\n" } if $timeout;
alarm $timeout if $timeout;

# XXX - create a blackboard namespace
use vars qw($main::global_debug $main::pugs_args);
local $main::global_debug = $debug; 
local $main::pugs_args = join(" ",map{"-I$_"}@inc_dirs);

eval q{
use PIL::Run::MainX;
use PIL::Run::EvalX;
use PIL::Run::ApiX; # for p6_to_s() p6_main()
}; die $@ if $@;

for (@inc_dirs) {p6_eval("push(\@INC,'$_');");}

for my $e (@eval) {
    p6_eval($e);
}
for my $fn (@ARGV) {
    p6_eval_file($fn);
}
if ($repl || (!@eval && !@ARGV)) {
    p6_repl();
}
exit(0);


sub p6_repl_simple {
    my $verbose = 0;
    while (1) {
        my $line = prompt_string("p5ugs> ");
        my @res = p6_eval($line);
        print "\n",@res,"\n";
    }
}

sub p6_repl_print_help {
    print ":h             show this help\n";
    print ":q             quit\n";
    print ":v             toggles verbose output\n";
    print ":5 <p5code>    run perl5 code\n";
    print " <p6code>      run perl6 code\n";
    print ":l <filename>  run perl6 file\n";
}

sub p6_repl {
    my $verbose = 0;
    my $eval_p6 = sub {
        my($p6)=@_;
        my $pil = pil_from_p6($p6);
        print $pil,"\n" if $verbose;
        my $pilc = pilc_from_pil($pil);
        print Dumper($pilc),"\n\n" if $verbose;
        print YAML::Dump($pilc),"\n\n" if $verbose;
        my $p5r = p5r_from_pilc($pilc);
        print $p5r,"\n" if $verbose;
        print "----\n";
        my @res = run_p5r($p5r);
        print "\n",(map {p6_to_s($_)} @res),"\n";
    };
    p6_repl_print_help();
    while (1) {
        my $line = prompt_string("p5ugs> ");
        last if !defined $line;
        if ($line =~ /\A:h\s*\Z/) { p6_repl_print_help(); next;}
        if ($line =~ /\A:q\s*\Z/) { exit(0);}
        if ($line =~ /\A:v\s*\Z/) { $verbose = !$verbose; next;}
        if ($line =~ /\A:5\s+(.+)/) {
            print eval("package ".&p6_main()."; ".$1),"\n";
            warn $@ if $@;
            next;
        }
        if ($line =~ /\A:l\s+(\S+)/) {
            my $filename = $1;
            open IN, $filename or do{ warn $!; next; };
            my $code = do { local $/; <IN> }; close IN;
            $eval_p6->($code);
            next;
        }
        $eval_p6->($line);
    }
}


__END__
