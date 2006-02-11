package Perl6::Run::OnPerl5::X1::Repl;
use Perl6::Run::OnPerl5::X1;
use Perl6::Run::OnPerl5::X1::Api;
use Perl6::Run::OnPerl5::X1::Compile;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Terse = 1;
require YAML;
use Scriptalicious 1.05;
use strict;

sub print_repl_help {
    print ":h             show this help\n";
    print ":q             quit\n";
    print ":v             toggles verbose output\n";
    print ":5 <p5code>    run perl5 code\n";
    print " <p6code>      run perl6 code\n";
    print ":l <filename>  run perl6 file\n";
}

sub run_repl {
    my $verbose = $Perl6::Run::OnPerl5::X1::BB::debug;
    my $eval_p6 = sub {
        my($p6)=@_;
	$p6 = "{;$p6}" if $p6 =~ /^\s*[\"\']|^\S+$/s; # so "3" works
	my $cc = Perl6::Run::OnPerl5::X1::CodeCompile->new(p6=>$p6)->compile;
	my $p5 = p6_wrap_code_with_package($cc->as_p5,'main');
	if($verbose) {
	    print $cc->as_pil_tree_yaml;
	    print "# p5\n",number_the_lines($p5),"\n";
	}
	print $cc->warnings;
        print "----\n";
	my @res = eval($p5);
	print $@ if $@;
        print "\n",(map {p6_to_perl($_)} @res),"\n";
    };
    print_repl_help();
    while (1) {
        my $line = prompt_string("p5ugs> ");
        last if !defined $line;
        if ($line =~ /\A:h\s*\Z/) { print_repl_help(); next;}
        if ($line =~ /\A:q\s*\Z/) { exit(0);}
        if ($line =~ /\A:v\s*\Z/) { $verbose = !$verbose; next;}
        if ($line =~ /\A:5\s+(.+)/) {
	    my $code = p6_wrap_code_with_package($1,'main');
            my @result = eval($code);
            warn $@ if $@;
	    print "",(map{defined $_ ? $_ : 'undef'}@result),"\n";
            next;
        }
        if ($line =~ /\A:l\s+(\S+)/) {
            my $filename = $1;
	    my $cc = Perl6::Run::OnPerl5::X1::CodeCompile->new(p6_file=>$filename)->get_p6_file;
	    $eval_p6->($cc->as_p6);
            next;
        }
        $eval_p6->($line);
    }
}

sub number_the_lines {
    my($s)=@_;
    my $cnt = 1;
    $s =~ s/^/$cnt++."\t"/mge;
    $s;
}

1;
__END__

sub original_repl { # not used
    my $verbose = 0;
    while (1) {
        my $line = prompt_string("p5ugs> ");
        my @res = p6_eval($line);
        print "\n",@res,"\n";
    }
}
