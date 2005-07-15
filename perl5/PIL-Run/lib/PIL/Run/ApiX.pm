
package PIL::Run::ApiX;
use strict;
use vars qw($VERSION @ISA @EXPORT);
require Exporter;
$VERSION = '0.01';
@ISA = qw(Exporter);
@EXPORT =
    qw(
       p6_to_n
       p6_to_s
       p6_to_a
       p6_from_n
       p6_from_s
       p6_from_a
       p6_die
       p6_root
       p6_mangle
       p6_unmangle
       p6_new
       );

sub p6_to_n {my(@n_objs)=@_; map{
    $_->internal_numify()
    } @n_objs}
sub p6_to_s {my(@s_objs)=@_; map{
    $_->internal_stringify()
    } @s_objs}
sub p6_to_a {my($a_obj)=@_; [@$a_obj]}
sub p6_from_n {my($n)=@_; p6_new(int($n) == $n ? 'Int' : 'Rat', 0+$n)}
sub p6_from_s {my($s)=@_; p6_new('Str',"$s")}
sub p6_from_a {my($a)=@_; [@$a]}
sub p6_die {my(@args)=@_; die @args;}
sub def_prim { # used by Prim
    my($flavor, $name, $f)=@_;
    my $n = $name;
    if ($n =~ /\>$/) {
	$n =~ s/:\<(.+?)\>$/:$1/;
    } elsif ($n =~ /\]$/) {
	$n =~ s/:\[(.+?)\]$/:$1/;
    }
    my $mn = p6_mangle($n);
    my $gn = globify_mangled($mn);
    eval("$gn = \$f");  die "bug: $@" if $@;
}
sub p6_root {"PIL::Run::Root"}
sub sigil_and_rest {
    my($n)=@_;
    my $sigil = "";
    my $bare = $n;
    if ($n =~ /^([\$\@\%\&]?)(.+)/) {
	$sigil = $1;
	$bare = $2;
    }
    return ($sigil,$bare);
}
# XXX - change mangling scheme - ${'foo'}::{'bar'}
sub p6_mangle {
    my($n)=@_;
    my($sigil,$mn) = sigil_and_rest($n);
    $mn =~ s/^(::)?(\*)?(::)?//;
    $mn =~ s/_/__/g;
    $mn =~ s/::/\cA/g;
    $mn =~ s/([^a-z0-9_\cA])/"_".ord($1)."x"/ieg;
    $mn =~ s/\cA/::/g;
    $mn = $sigil."PIL::Run::Root::".$mn;
    $mn;
}
sub p6_unmangle {
    my($mn)=@_;
    my($sigil,$n) = sigil_and_rest($mn);
    $n =~ s/^PIL::Run::Root::/::*::/;
    $n =~ s/__/\cA/g;
    $n =~ s/_(\d+)x/chr($1)/eg;
    $n =~ s/\cA/_/g;
    $n = $sigil.$n;
    $n;
}
sub globify_mangled {
    my($mn)=@_;
    my($sigil,$bare) = sigil_and_rest($mn);
    '*'.$bare;
}

sub p6_new {
    my($type,@args)=@_;
    "PIL::Run::Type::$type"->new(@args);
}


1;
__END__

print &p6_mangle("&say"),"\n";
def_prim("","&say",sub {print "hi\n"});
eval(&p6_mangle("&say"));
