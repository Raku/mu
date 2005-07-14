#!/usr/bin/perl -w
# run me...

package ReadPIL;

my $str;
my $pos;
my @M;
sub match {
    if (substr($str,$pos) =~ $_[0]) {
	$pos += length($1);
	@M = ('',$1,$2,$3);
	return 1;
    } else {
	return 0;
    }
}
sub parsefail {
    my($msg)=@_;
    my $frm = $pos-300; $frm = 0 if $frm < 0;
    die ("parsefail: $msg\npos $pos\n"
	 .substr($str,$frm,$pos-$frm)
	 ."\n----HERE----\n"
	 .substr($str,$pos));
}
sub read {
    my($s)=@_;
    $str = $s;
    $pos = 0;
    read_an_expr();
}
my %arity =
    (
     App => 3,
     CxtVoid => 0,
     False => 0,
     MkPos => 5,
     Noop => 0,
     Nothing => 0,
     PApp => 3,
     PAssign => 2,
     PCode => 3,
     PExp => 1,
     PIL_Environment => 1,
     PLit => 1,
     PNil => 0,
     PNoop => 0,
     Pos => 2,
     PPos => 3,
     PStmt => 1,
     PStmts => 2,
     PSub => 2,
     PVal => 1,
     PVar => 1,
     SMy => 2,#?
     SubPointy => 0,
     Stmts => 2,
     SubBlock => 0,
     SubPrim => 2,
     Syn => 2,
     TCxtVoid => 0,
     True => 0,
     Val => 1,
     Var => 1,
     VInt => 1,
     VStr => 1,
     VUndef => 0,
     );
sub read_an_expr {
    match(qr/\A(\s+)/);
    if (0) {
    } elsif (match(qr/\A(Pugs.AST.Internals.Exp -> \(\) Pugs.AST.Internals.Val)/)) {
	"bug('b1')";
    } elsif (match(qr/\A([a-zA-Z]\w+)/)) {
	my $cmd = $M[1];
	if (!exists $arity{$cmd}) {
	    $arity{$cmd} = 1;
	    warn "Guessing $cmd has arity 1.\n";
	}
	my $arity = $arity{$cmd};
	my $args = read_n_exprs($arity);
	@$args[0] =~ s/\A\n// if defined @$args[0];
	parsefail("for $cmd, found only ".(0+@$args)." of $arity arguments:\n"
		  .join(" --- ",@$args))
	    if @$args != $arity;
	"\n\&c(".join(",","'$cmd'",@$args).")";
    } elsif (match(qr/\A(\d+)/)) {
	"$M[1]";
    } elsif (match(qr/\A(\"([^\"]*)\")/)) { # XXX - kludge
	my $s = $M[2];
	$s =~ s/\'/\\\'/g;
	$s =~ s/\\/\\\\/g;
	"'$s'";
    } elsif (match(qr/\A(\()/)) {
	my @es;
	while (1) {
	    match(qr/\A(,)/);
	    my $e = read_an_expr();
	    last if $e eq "";
	    push(@es,$e);
	}
	match(qr/\A(\s*\))/) or parsefail("expected \)");
	"".join(", ",@es)."";
    } elsif (match(qr/\A(\[)/)) {
	my @es;
	while (1) {
	    match(qr/\A(,)/);
	    my $e = read_an_expr();
	    last if $e eq "";
	    push(@es,$e);
	}
	match(qr/\A(\s*\])/) or parsefail("expected \]");
	$es[0] =~ s/\A\n/ / if defined $es[0];
	"[".join(", ",@es)."]";
    } elsif (match(qr/\A(\{)/)) {
	my @pairs;
	while (match(qr/\A(\s*,?\s*([a-z]\w+) = )/)) {
	    my $k = $M[2];
	    my $v = read_an_expr();
	    $v =~ s/\A\n//;
	    push(@pairs,"\n$k => $v");
	}
	match(qr/\A(\s*\})/) or parsefail("expected \}");
	"{".join(",",@pairs)."}";
    } elsif (match(qr/\A(<ref>)/)) {
	"bug('ref')";
    } else {
	"";
    }
}
sub read_n_exprs {
    my($n)=@_;
    my $cnt = 0;
    my @es;
    while ($cnt++ != $n) {
	my $e = read_an_expr();
	last if $e eq "";
	push(@es,$e);
    }
    \@es;
}

package ExpandPilc;

sub mangle {
    my($n)=@_;
    $n =~ s/\A([\$])/${1}perl6__/;
    $n =~ s/\A([\@\%\&])(.+)/(\\${1}perl6__$2)/;
    $n;
}

my %handlers =
    (
     PIL_Environment => sub{my($hr)=@_; $hr->{'pilMain'}},
     PStmts => sub{my($h,$t)=@_; $h.$t},
     VStr => sub{my($s)=@_;  "'$s'"},
     VInt => sub{my($n)=@_;  "$n"},
     Val => sub{my($v)=@_; $v},
     Var => sub{my($name)=@_; mangle($name)},
     App => sub{my($f,$x,$args,@y)=@_;
		$f."->(".join(",",@$args).")"},
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
    my $s = eval($pilc); warn "".(caller(0))[3].": $@" if $@;
    $s;
}

package main;

sub perl6__say {my(@args)=@_; print "",@args,"\n"; 'mumble::true'};


sub pil_from_p6 {
    my($p6)=@_;
    my $fn = "deleteme.p6";
    open(F,">$fn") or die "Couldn't open \"$fn\" for writing: $!\n"; # XXX - kluge
    print F $p6; close F or die "Couldn't close \"$fn\": $!\n";
    my $pil = `pugs -Cpil $fn`; #die if $!;
    unlink $fn or die "Couldn't remove \"$fn\": $!\n";
    $pil;
}

sub p6_to_p5r {
    my($p6)=@_;
    my $pil = pil_from_p6($p6);
    my $pilc = pilc_from_pil($pil);
    my $p5r = p5r_from_pilc($pilc);
    $p5r;
}

sub pilc_from_pil {
    my($pil)=@_;
    my $pilc = ReadPIL::read($pil);
    $pilc;
}

sub p5r_from_pilc {
    my($pilc)=@_;
    my $p5r = ExpandPilc::expand($pilc);
    $p5r;
}

sub run_p5r {
    my($p5r)=@_;
    my @res = eval($p5r);  warn "".(caller(0))[3].": $@" if $@;
    @res;
}

sub p6_eval {
    my($p6)=@_;
    my $p5r = p6_to_p5r($p6);
    run_p5r($p5r);
}

sub p6_repl_simple {
    my $verbose = 0;
    while (1) {
	print "> ";
	my $line = readline STDIN;
	last if !defined $line;
	my @res = p6_eval($line);
	print "\n",@res,"\n";
    }
}

sub p6_repl {
    my $verbose = 0;
    print ":v  toggles verbose output\n";
    print "say 'hi' and say 3 are about all that works.\n";
    while (1) {
	print "> ";
	my $line = readline STDIN;
	last if !defined $line;
	if ($line =~ /\A\s*:v\s*\Z/) {
	    $verbose = !$verbose;
	    next;
	}
	my $p6 = $line;
	my $pil = pil_from_p6($p6);
	print $pil,"\n" if $verbose;
	my $pilc = pilc_from_pil($pil);
	print $pilc,"\n\n" if $verbose;
	my $p5r = p5r_from_pilc($pilc);
	print $p5r,"\n" if $verbose;
	print "----\n";
	my @res = run_p5r($p5r);
	print "\n",@res,"\n";
    }
}

p6_repl();

__END__
Next steps:
 Create ./perl5/PIL-Run/lib/PIL/{Object,String,Sub}.pm
 Get "say 'hi'" running on them.
   
