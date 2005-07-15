
package PIL::Run::EvalX;
use strict;
use vars qw($VERSION @ISA @EXPORT);
require Exporter;
$VERSION = '0.01';
@ISA = qw(Exporter);
@EXPORT =
    qw(
       pil_from_p6
       p5r_from_p6
       pilc_from_pil
       p5r_from_pilc
       run_p5r
       p6_eval
       );
use PIL::Run::ApiX;

my %handlers =
    (
     PIL_Environment => sub{my($hr)=@_; $hr->{'pilMain'}},
     PStmts => sub{my($h,$t)=@_; $h.$t},
     VStr => sub{my($s)=@_;  "'$s'"},
     VInt => sub{my($n)=@_;  "$n"},
     Val => sub{my($v)=@_; $v},
     Var => sub{my($name)=@_; p6_mangle($name)},
     App => sub{my($f,$x,$args,@y)=@_;
		$f."(".join(",",@$args).")"},
     PPos => sub{my($pos,$v)=@_; $v},
     Pos => sub{my($pos,$v)=@_; $v},
     );
my %warned_about;

sub c {
    my($cmd,@args)=@_;
    my $h = $handlers{$cmd};
    if (!$h) {
	$warned_about{$cmd}++
	    or warn "Punting on $cmd.\n";
	return "";
    }
    $h->(@args);
}

sub expand {
    my($pilc)=@_;
    my $s = eval($pilc);
    warn "".(caller(0))[3].": $@" if $@;
    $s;
}

#======================================================================
use PIL::ReadToStr1;

sub pil_from_p6 {
    my($p6)=@_;
    my $fn = "deleteme.p6";
    open(F,">$fn") or die "Couldn't open \"$fn\" for writing: $!\n"; # XXX - kluge
    print F $p6; close F or die "Couldn't close \"$fn\": $!\n";
    my $pil = `pugs -Cpil $fn`; #die if $!;
    unlink $fn or die "Couldn't remove \"$fn\": $!\n";
    $pil;
}

sub p5r_from_p6 {
    my($p6)=@_;
    my $pil = pil_from_p6($p6);
    my $pilc = pilc_from_pil($pil);
    my $p5r = p5r_from_pilc($pilc);
    $p5r;
}

sub pilc_from_pil {
    my($pil)=@_;
    my $pilc = PIL::ReadToStr1::read($pil);
    $pilc;
}

sub p5r_from_pilc {
    my($pilc)=@_;
    my $p5r = PIL::Run::EvalX::expand($pilc);
    $p5r;
}

sub run_p5r {
    my($p5r)=@_;
    my @res = eval($p5r);
    warn "".(caller(0))[3].": $@" if $@;
    @res;
}

sub p6_eval {
    my($p6)=@_;
    my $p5r = p5r_from_p6($p6);
    run_p5r($p5r);
}

1;
__END__
