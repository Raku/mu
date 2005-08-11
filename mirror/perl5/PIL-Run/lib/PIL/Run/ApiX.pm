
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
       p6_apply
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
    #my $gn = globify_mangled($mn);
    my $subobj = p6_new('Sub',$f);
    eval("$mn = \$subobj");  die "bug: $@" if $@;
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
my %space_from_sigil = (
	     '$' => 'scalar',
	     '@' => 'array',
	     '%' => 'hash',
	     '&' => 'code'
	     );
my %sigil_from_space = map {$space_from_sigil{$_},$_} keys(%space_from_sigil);

# XXX - change mangling scheme - ${'foo'}::{'bar'}
# XXX - this is getting crufty

# Hey, there is no reason why ::code_say couldnt be ::say_code,
# avoiding the need to split on ::.  Though if we switch to
# ${'foo'}::{'bar'}, we'll likely split anyway.

sub p6_mangle {
    my($n)=@_;
    my($sigil,$mn) = sigil_and_rest($n);
    my $space = $space_from_sigil{$sigil};
    $mn =~ s/^(::)?(\*)?(::)?//;
    $mn =~ s/_/__/g;
    $mn =~ s/::/\cA/g;
    $mn =~ s/([^a-z0-9_\cA])/"_".ord($1)."x"/ieg;
    $mn =~ s/\cA/::/g;
    my @parts = split('::',$mn);
    $parts[-1] = $space."_".$parts[-1];
    $mn = join('::',@parts);
    $mn = '$PIL::Run::Root::'.$mn;
    $mn;
}
sub p6_unmangle {
    my($mn)=@_;
    my($always_dollar,$n) = sigil_and_rest($mn);
    $always_dollar eq '$' or die "bug";
    my @parts = split('::',$n);
    $parts[-1] =~ /\A([^_]+)_(.+)/ or die "bug";
    $parts[-1] = $2;
    $n = join('::',@parts);
    my $space = $1;
    my $sigil = $sigil_from_space{$space};
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
sub p6_apply {
    my($f,@args)=@_;
    $f->apply(@args);
}


1;
__END__

print &p6_mangle("&say"),"\n";
def_prim("","&say",sub {print "hi\n"});
eval(&p6_mangle("&say"));
