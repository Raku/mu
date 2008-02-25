use strict;
use warnings;

print "1..5\n";

sub sixrx {
    my(@args)=@_;
    my $ret = system("./sixrx @args > deleteme_output 2> /dev/null");
    $ret = ($? >> 8) if $ret;
    my $output = `cat deleteme_output`;
    [$ret,$output];
}
sub sixrx_ok {
    my($args,$ret,$out_file)=@_;
    my $out = undef;
    if($out_file){
	open(F,"<$out_file") or die "$out_file: $!";
	$out = join("",<F>); close(F);
    }
    my $res = sixrx(@$args);
    if($ret != $res->[0]) {
	print "# sixrx exit code was not $ret: $res->[0]\n";
	print "not ok\n";
    }
    elsif($out and $out ne $res->[1]) {
	my $msg = "# sixrx output was not\n$out\n# but instead\n$res->[1]\n";
	$msg =~ s/^/#/mg;
	print $msg;
	print "not ok\n";
    }
    else {
	print "ok\n";
    }
}

sixrx_ok(['--help'],0);
sixrx_ok(['--blah'],2);
sixrx_ok([qw{ pcr t/t01.g MyC def t/t01.01.i }],0,'t/t01.01.o');
sixrx_ok([qw{ --yaml pcr t/t01.g MyC def t/t01.01.i }],0,'t/t01.01.oy');
sixrx_ok([qw{ pcr t/t01.g MyC def t/t01.02.i }],1);
