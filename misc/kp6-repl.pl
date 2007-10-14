package RenameMe; #XXX

BEGIN {
    chomp(my $lib = `kp6 -lib`);
    eval("use lib '$lib';");
}
use KindaPerl6::Runtime::Perl5::Runtime;
use File::Temp 'tmpnam';
use Scriptalicious 1.05;
use strict;

sub call_kp6 {
    my($input,$args)=@_;
    my $tmpfn = tmpnam();
    open(F,"|kp6 $args > $tmpfn") or die;
    print F $input;
    close(F);
    my $output = `cat $tmpfn`;
    unlink($tmpfn);
    return $output;
}

sub print_repl_help {
    print ":h             show this help\n";
    print ":q             quit\n";
    print ":v             toggles verbose output\n";
    print ":5 <p5code>    run perl5 code\n";
    print " <p6code>      run perl6 code\n";
    print ":l <filename>  run perl6 file\n";
}

# Design issue [Getting-Expression-Value]
# kp6 currently generates p5 code which always evaluates to 1.
# So how should we print the value of the user's expression?
# One can wrap simple expressions in "say".  "say 3;".
# But kp6 doesn't currently understand "{;3}" as a way of
# wrapping expressions without altering scope.
# And wrapping a function and calling it seems likely to
# cause trouble (eg, "sub wrap(){ 3 } say wrap();").
# For now, fudge.

# Design issue [Perserving-Variables]
# kp6 currently uses our() variables, so they are lost at the
# end of the eval(), and unavailable for subsequent user input.
# For now, accumulate all non-error causing p6, and eval the
# entire accumulation.
# We can't accumulate p5 instead of p6, because compilation
# requires knowing previous declarations.
# So the accumulated p6 keeps getting bigger. :(  XXX

my $accumulated_p6; # See [Perserving-Variables].
sub run_repl {
    my $verbose = 0;
    my $eval_p5 = sub {
	my($p5)=@_;
	my @result = eval($p5);
	print $@ if $@;
	# @result is always [1].  See [Getting-Expression-Value].
	#print "",(map{defined $_ ? $_ : 'undef'}@result),"\n";
    };
    my $eval_p6 = sub {
        my($p6)=@_;
	my $code = $p6;

	# XXX Wrap one-liners in say().  See [Getting-Expression-Value].
	$code = "say $code;" if $code =~ tr/\n/\n/ <= 1;

	# XXX See [Perserving-Variables].
	$code = $accumulated_p6 ."\n". $code;
	my $new_accumulation = $accumulated_p6 ."\n". $p6;

	my $p5 = call_kp6($code,'');
	if($verbose) {
	    print call_kp6($p6,'-ast | perltidy');
	    print "# p5\n",number_the_lines($p5),"\n";
	}
        print "----\n";
	$eval_p5->($p5);

	$accumulated_p6 = $new_accumulation if !$@;
    };
    print_repl_help();
    while (1) {
        my $line = prompt_string("p5ugs> ");
        last if !defined $line;
        if ($line =~ /\A:h\s*\Z/) { print_repl_help(); next;}
        if ($line =~ /\A:q\s*\Z/) { exit(0);}
        if ($line =~ /\A:v\s*\Z/) { $verbose = !$verbose; next;}
        if ($line =~ /\A:5\s+(.+)/) {
	    $eval_p5->($1);
	    next;
        }
        if ($line =~ /\A:l\s+(\S+)/) {
            my $filename = $1;
	    my $p6 = `cat $filename`;
	    $eval_p6->($p6);
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

#XXX remove me
run_repl();

1;
__END__
