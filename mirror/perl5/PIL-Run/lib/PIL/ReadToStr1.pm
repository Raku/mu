
package PIL::ReadToStr1;

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

1;
__END__
