use Perl6::Run::OnPerl5::X1::PilUtil;

package Perl6::Run::OnPerl5::X1::Pugs;
use strict;
use FindBin;
use File::Spec;

sub path_from_me { File::Spec->catfile($FindBin::Bin, @_) }
my $src_root = path_from_me();

sub compile_p6 {
    my($p6)=@_;

    my $pugs = $Perl6::Run::OnPerl5::X1::BB::pugs;
    $pugs = $ENV{PUGS_EXECUTABLE} if !defined $pugs;
    $pugs = "pugs" if !defined $pugs;

    my $frob = sub{ my $n=$_[0];($n =~ /^perl5/ ? "" : "require $n;")."use_avoiding_pugs('$n');"};
    $p6 =~ s/^use\s+([^;]+);/$frob->($1)/emg;

    my $fn = "deleteme$$.p6";
    open(F,">$fn") or die "Couldn't open \"$fn\" for writing: $!\n"; # XXX - kludge
    print F $p6; close F or die "Couldn't close \"$fn\": $!\n";
    my $dir = "-I$src_root/lib6";
    if ( $^O =~ /win/i ) {
        # fixes dir-name-with-spaces in Windows
        $dir = '"' . $dir . '"';
    }
    my $extra_args = $Perl6::Run::OnPerl5::X1::BB::pugs_args || "";
    my $pilfn = "deleteme_$$";
    my $cmd = "$pugs $extra_args $dir -CPerl5 $fn 1> $pilfn 2> $pilfn.err";
    my $err = "";
    if(system($cmd)) {
	#$err = $!."\n";
	$err = `cat $pilfn.err`.$err;
    }
    my $pil_code = `cat $pilfn`;
    unlink "$pilfn.err" or die "Couldn't remove \"$pilfn.err\": $!\n";
    unlink $pilfn or die "Couldn't remove \"$pilfn\": $!\n";
    unlink $fn or die "Couldn't remove \"$fn\": $!\n";

    $pil_code = "use Math::BigInt;\n".$pil_code; # part of spec.  eew.

    ($pil_code,$err);
}


package Perl6::Run::OnPerl5::X1::Result;
use strict;
sub new {
    my($cls,$value,$warnings,$has_failed)=@_;
    bless {
	value => $value,
	warnings => $warnings,
	has_failed => $has_failed,
    }, $cls;
}
sub value { $_[0]{'value'} }
sub warnings { $_[0]{'warnings'} }
sub has_failed { $_[0]{'has_failed'} }

package Perl6::Run::OnPerl5::X1::CodeCompile;
use strict;
use Carp;

sub new {
    my($cls,$datatype,$data)=@_;
    die "bug" if $datatype !~ /\A(p6_file|p6|pil_code|pil_tree|p5)\z/;
    bless {
	stages => [qw(p6_file p6 pil_code pil_tree p5)],
	$datatype => Perl6::Run::OnPerl5::X1::Result->new($data,"",0),
    }, $cls;
}
sub _stages {@{$_[0]{'stages'}}}

sub has_failed {
    my($self)=@_;
    for my $stage ($self->_stages) {
	my $result = $self->{$stage};
	return 1 if defined $result && $result->has_failed();
    }
    return 0;
}
sub warnings {
    my($self)=@_;
    my $ret="";
    for my $stage ($self->_stages) {
	my $result = $self->{$stage};
	next if !defined $result;
	my $warn = $result->warnings();
	$ret .= $warn if defined $warn;
    }
    $ret;
}
sub compile {
    my($self)=@_;
    my @todo;
    for my $stage (reverse $self->_stages) {
	last if defined $self->{$stage};
	push(@todo,$stage);
    }
    #print STDERR "==",join(" ",@todo),"==\n";
    for my $stage (reverse @todo) {
	#print STDERR "--$stage--\n";
	if($stage eq 'p5') {
	    my $pil_tree = $self->as_pil_tree;
	    my($p5,$warn) = Perl6::Run::OnPerl5::X1::PilToPerl5::emit($pil_tree);
	    my $failed = !defined $p5;
	    $warn = "Pil to p5 compile failed: $warn\n" if $warn;
	    $self->{'p5'} = Perl6::Run::OnPerl5::X1::Result->new($p5,$warn,$failed);
	} elsif($stage eq 'pil_tree') {
	    my $pil_code = $self->as_pil_code;
	    #print STDERR ">>$pil_code<<<";
	    my $pil_tree = eval($pil_code);
	    #print STDERR $pil_tree;
	    my $failed = $@ || !defined $pil_tree;
	    my $warn = $@ ? "Eval of -CPerl5 code failed: $@\n$pil_code" : "";
	    $self->{'pil_tree'} = Perl6::Run::OnPerl5::X1::Result->new($pil_tree,$warn,$failed);
	} elsif($stage eq 'pil_code') {
	    my $p6 = $self->as_p6;
	    my($pil_code,$warn) = Perl6::Run::OnPerl5::X1::Pugs::compile_p6($p6);
	    my $failed = !defined $pil_code;
	    $warn = "pugs -CPerl5 failed: $warn\n" if $warn;
	    $self->{'pil_code'} = Perl6::Run::OnPerl5::X1::Result->new($pil_code,$warn,$failed);
	} elsif($stage eq 'p6_file') {
	    $self->get_p6_file;
	} else { die "bug" }
    }
    $self;
}
sub get_p6_file {
    my($self)=@_;
    my $p6_file = $self->as_p6_file;
    my $p6;
    my $warn = sub{
	open IN, $p6_file or return "open: $p6_file: $!\n";
	$p6 = do { local $/; <IN> }; close IN;
	"";
    }->();
    my $failed = $warn?1:0;
    $self->{'p6'} = Perl6::Run::OnPerl5::X1::Result->new($p6,$warn,$failed);
    $self;
}
sub as_p5 { $_[0]{'p5'}->value }
sub as_pil_tree { $_[0]{'pil_tree'}->value }
sub as_pil_code { $_[0]{'pil_code'}->value }
sub as_p6 { my $r = $_[0]{'p6'}; $r ? $r->value : undef; }
sub as_p6_file { $_[0]{'p6_file'}->value }

sub as_pil_tree_yaml {
    my $pil_tree = $_[0]->as_pil_tree;
    eval { require YAML; }; die $@ if $@;
    my $dump = YAML::Dump($pil_tree)."\n\n";
    $dump =~ s/Perl6::Run::OnPerl5::X1::PilToPerl5:://g;
    $dump =~ s/!perl\//!/g;
    $dump;
}


package Perl6::Run::OnPerl5::X1::PilToPerl5;
BEGIN{
  eval(PIL::PIL1::NodeSet0::gen_code(__PACKAGE__));
  PIL::PIL1CPerl5::Util::FilterNodeDefs::import(__PACKAGE__,'emit');
}
use strict;
use Perl6::Run::OnPerl5::X1::Api;

sub emit {
    my($pil_tree)=@_;
    return (undef,"emit: pil_tree was undefined") if !defined $pil_tree;
    use Data::Dumper;
    PIL::PIL1CPerl5::Util::rebless_with_prefix_and_cleanup($pil_tree,__PACKAGE__.'::');
    #print STDERR Dumper($pil_tree);
    my $p5 = $pil_tree->emit();
    "use Perl6::Run::OnPerl5::X1::Api; use utf8; use Error qw(:try); $p5";
}

NODE PIL_Environment ($pilGlob, $pilMain) {
    join("\n",map{DOWN($_)} @$pilGlob,$pilMain);
}

NODE PNil () {
    "";
}
NODE PStmts ($pStmt, $pStmts) {
    DOWN($pStmt).DOWN($pStmts);
}
NODE PPad ($pScope, $pSyms, $pStmts) {
    my(@vars);
    for (@$pSyms) {
	my($var)=@$_;
        push(@vars,$var);
    }

    my @varsm = map{p6_mangle($_)} @vars;# XXX - Api abstraction violation
    my $varlist = join(",",map{'$'.$_}@varsm); 
    my $init = '=('.join(",",map{p6_container_for_var_CODE($_)}@vars).')';
    my $decl = "";

    my $sn = $pScope;
    $sn = lc $sn; $sn =~ s/^s//;
    if($sn eq 'state') {
	warn "state PPad not implemented\n"; # use MM?
    }
    elsif($sn eq 'my') {
	$decl = "my($varlist)$init;";
    }
    elsif($sn eq 'our') {
	warn "our PPad not implemented\n"; # use MM?
    }
    elsif($sn eq 'let') {
	warn "let PPad not implemented\n";
    }
    elsif($sn eq 'temp') {
	my $vl2 = join(",",map{'${__PACKAGE__."::'.$_.'"}'}@varsm);
	my $vl3 = join(",",map{'*{__PACKAGE__."::'.$_.'"}'}@varsm);
	my $vl4 = join(",",map{'\\$'.$_}@varsm);
	$decl = ("no strict 'refs'; my($varlist);local($vl2);($vl3)=($vl4);"
		 ."($varlist)$init; use strict;");
    }
    elsif($sn eq 'global') {
	warn "global PPad may not be doing the right thing\n";
	$decl = "no strict; ($varlist)$init; use strict;";
    }
    else { die "bug $sn" }
    my $body = DOWN($pStmts);
    "(do{$decl\n$body})";
}

NODE PNoop () {
    ";";
}
NODE PStmt ($pExpr) {
    DOWN($pExpr).";\n";
}
NODE PPos ($pPos, $pExp, $pNode) {
    (defined($pExp) ? DOWN($pExp) : "")."".DOWN($pNode); # XXX - Exp?
}

NODE PRawName ($pRawName) { "$pRawName" }
NODE PExp ($pLV) { DOWN($pLV); }
NODE PLit ($pLit) { DOWN($pLit); }
NODE PThunk ($pThunk) {
    "(do{ ".DOWN($pThunk)."})";
}
NODE PCode ($pType, $pParams, $pLValue, $pIsMulti, $pBody) {
    code_helper("", $pType, $pParams, $pLValue, $pIsMulti, $pBody);
}
NODE PSub ($pSubName, $pSubType, $pSubParams, $pSubLValue, $pSubIsMulti, $pSubBody) {
    code_helper($pSubName, $pSubType, $pSubParams, $pSubLValue, $pSubIsMulti, $pSubBody);
}
sub code_helper {
    my($name,$pType, $pParams, $pLValue, $pIsMulti, $pBody)=@_;
    my $type = $pType;
    my @pams = map{DOWN($_)} @$pParams;
    my $lval = $pLValue; # XXX - ?
    my $body = DOWN($pBody);
    p6_code_mk_CODE($name,$type,\@pams,$lval,$body);
}

NODE PVal ($pVal) { DOWN($pVal); }

NODE PVar ($pVarName) {
    return "is macrop5 $pVarName" if p6_macrop5($pVarName);
    p6_var_CODE($pVarName);
}
NODE PApp ($pCxt, $pFun, $pInv, $pArgs) {
    my $has_inv = defined($pInv) ? 1 : 0;
    my @inv = defined($pInv) ? (DOWN($pInv)) : ();
    my @args = (@inv,map{DOWN($_)}@$pArgs);
    my $argl = join(",",@args);
    my $fun = DOWN($pFun);
    if($fun =~ /^is macrop5 (.+)$/) {
	return p6_macrop5($1)->(@args);
    }
    if($has_inv) {
	my $fun_name = do{
	    my $n=$self;
	    for my $k qw(pFun pLV pVarName){
		if(exists $n->{$k}){$n=$n->{$k}}else{$n=undef;last}}
	    $n =~ s/^&// if $n;
	    $n};
	return "p6_applym('$fun_name',$argl)" if $fun_name;
	return "p6_applyi($fun,$argl)";
    }
    return "p6_apply($fun,$argl)";
}

NODE PAssign ($pLHS, $pRHS) {
    die "bug @$pLHS" if @$pLHS > 1;
    my($lhs)=@$pLHS;
    $lhs = DOWN($lhs);
    my $rhs = DOWN($pRHS);
    "p6_assign($lhs,$rhs)"
}
NODE PBind ($pLHS, $pRHS) {
    die "bug @$pLHS" if @$pLHS > 1;
    my($lhs)=@$pLHS;
    my $rhs = $pRHS;
    "p6_bind($lhs,$rhs)"
}

NODE MkTParam ($tpParam, $tpDefault) {
    #my $type = defined $tpDefault ? DOWN($tpDefault)." " : "";
    #$type." ".DOWN($tpParam);
    DOWN($tpParam);
}

NODE TCxtVoid () { "should not appear" }
NODE TCxtLValue ($type) { "should not appear" }
NODE TCxtItem   ($type) { "should not appear" }
NODE TCxtSlurpy ($type) { "should not appear" }
NODE TTailCall  ($tcxt) { "should not appear" }

NODE MkTEnv ($tLexDepth, $tTokDepth, $tCxt, $tReg, $tLabel) {
    ""; # XXX - ???
}

NODE VUndef () { "p6_undef()" }
NODE VBool ($value) { "p6_Bool($value)" }
NODE VInt ($value) { "p6_Int($value)" }
NODE VRat ($value) { "p6_Rat($value->[0],$value->[1])" }
NODE VNum ($value) {
    return "p6_Num($value)" if !ref($value);
    # Math::BigInt-> bnan binf binf('-') is part of -CPerl5 spec.
    my %unpack = ('inf' => '100**100**100',
		  '-inf' => '-100**100**100',
		  'NaN' => '100**100**100/100**100**100');
    my $v = $unpack{$value->bstr};
    return "p6_Num($v)";
}
NODE VStr ($value) { my $s = $value; $s =~ s/\\/\\\\/g; $s =~ s/\'/\\\'/g; "p6_Str('$s')" }
NODE VList ($value) { "p6_List(".join(",",map{DOWN($_)}@$value).")" } # XXX
NODE VType ($value) { die "VType $value" } # XXX

NODE MkType ($typename) {
    p6_type_mk($typename);
}
NODE TypeOr ($lhs, $rhs) {
    #DOWN($lhs)."|".DOWN($rhs);
    p6_type_or(DOWN($lhs),DOWN($rhs));
}
NODE TypeAnd ($lhs, $rhs) {
    #DOWN($lhs)."&".DOWN($rhs);
    p6_type_and(DOWN($lhs),DOWN($rhs));
}

NODE MkParam ($isInvocant, $isOptional, $isNamed, $isLValue, $isWritable, $isLazy, $paramName, $paramContext, $paramDefault) {
    my $default; $default = DOWN($paramDefault) if defined($paramDefault);
    return { isInvocant=>$isInvocant, isOptional=>$isOptional, isNamed=>$isNamed, isLValue=>$isLValue, isWritable=>$isWritable, isLazy=>$isLazy, paramName=>$paramName, paramContext=>$paramContext, paramDefault=>$default };
}

NODE MkPos ($posName, $posBeginLine, $posBeginColumn, $posEndLine, $posEndColumn) {
    "";
}


1;
__END__
