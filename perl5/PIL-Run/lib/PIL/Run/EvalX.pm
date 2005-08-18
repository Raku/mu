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
use UNIVERSAL;

sub subnodes_of {
    my($n)=@_;
    if (UNIVERSAL::isa($n,'ARRAY')) {
	@$n;
    } elsif (UNIVERSAL::isa($n,'HASH')) {
	values(%$n);
    } else {
	();
    }
}

no strict;
package EvalX::BaseClass;
use Scalar::Util qw(blessed);
sub expand {
    my($self)=@_;
    my @subnodes = PIL::Run::EvalX::subnodes_of($self);
    my $code = "";
    while (@subnodes) {
	my $n = shift(@subnodes);
	if(blessed($n)) {
	    $code .= $n->expand();
	} elsif(ref($n)) {
	    unshift(@subnodes,PIL::Run::EvalX::subnodes_of($n));
	}
    }
    $code;
}
package VInt; @ISA = qw(EvalX::BaseClass); sub expand {
    "p6_new('Int','$_[0][0]')";
}
package VStr; @ISA = qw(EvalX::BaseClass); sub expand {
    my $s = $_[0][0];
    $s =~ s/\\/\\\\/; $s =~ s/\'/\\\'/;
    "p6_new('Str','$s')";
}
package PVar; @ISA = qw(EvalX::BaseClass); sub expand {
    PIL::Run::ApiX::p6_mangle($_[0]{'pVarName'});
}
package PApp; @ISA = qw(EvalX::BaseClass); sub expand {
    my($self)=@_;
    my $f = $self->{'pFun'}->expand();
    my @args = map{$_->expand()} @{$self->{'pArgs'}};
    "p6_apply(".join(",",$f,@args).")";
}
package PStmt; @ISA = qw(EvalX::BaseClass); sub expand {
    $_[0]->SUPER::expand().";\n";
}


package PIL::Run::EvalX; # continued.
use strict;

sub get_classes {
    my($node)=@_;
    my @nodes; my %classes;
    push(@nodes,$node);
    while (@nodes) {
	my $n = shift(@nodes);
	my $cls = ref($n) or next;
	$classes{$cls} = 1;
	push(@nodes,subnodes_of($n));
    }
    keys(%classes);
}

my %have_classes = map {($_,1)} qw();
sub define_classes_for {
    my($pilc)=@_;
    my @classes = get_classes($pilc);
    my @classes_needed = grep {!$have_classes{$_}} @classes;
    return if !@classes_needed;
    my $code;
    foreach (@classes_needed) {
	$have_classes{$_} = 1;
	$code .= "package $_; \@ISA = qw(EvalX::BaseClass);\n";
    }
    $code = "no strict;\n".$code;
    #print $code;
    eval($code); die "Eval of class definitions failed. $@" if $@;
}


sub expand {
    my($pilc)=@_;
    define_classes_for($pilc);
    $pilc->expand();
}

#======================================================================

sub pil_from_p6 {
    my($p6)=@_;
    my $fn = "deleteme.p6";
    open(F,">$fn") or die "Couldn't open \"$fn\" for writing: $!\n"; # XXX - kluge
    print F $p6; close F or die "Couldn't close \"$fn\": $!\n";
    my $pil = `pugs -CPerl5 $fn`; #die if $!;
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
    my $pilc = eval($pil);
    die "Eval of -CPerl5 code failed. $@" if $@;
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
