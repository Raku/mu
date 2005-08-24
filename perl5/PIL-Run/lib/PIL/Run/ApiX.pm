
package PIL::Run::ApiX;
use strict;
use vars qw($VERSION @ISA @EXPORT);
require Exporter;
$VERSION = '0.01';
@ISA = qw(Exporter);
@EXPORT =
    qw(
       p6_to_b
       p6_to_n
       p6_to_s
       p6_to_a
       p6_to_l
       p6_from_b
       p6_from_n
       p6_from_s
       p6_from_a
       p6_from_l
       p6_die
       p6_set
       p6_var
       p6_root
       p6_mangle
       p6_unmangle
       p6_new
       p6_apply
       );

sub p6_to_b {my($b)=@_; $b->bit()->unboxed ? 1 : 0}
sub p6_to_n {my($n)=@_; $n->num()->unboxed;}
sub p6_to_s {my($n)=@_; $n->str()->unboxed;}
sub p6_to_a {my($a_obj)=@_; my @list; while ( $a_obj->elems ) { push @list, $a_obj->shift }; @list }
sub p6_to_l {my($a_obj)=@_; my @list; while ( $a_obj->elems ) { push @list, $a_obj->shift }; @list }
sub p6_from_b {my($b)=@_; p6_new('Bit',$b ? 1 : 0)}
sub p6_from_n {my($n)=@_; p6_new(int($n) == $n ? 'Int' : 'Num', 0+$n)}
sub p6_from_s {my($s)=@_; p6_new('Str',"$s")}
sub p6_from_a {my(@a)=@_; p6_new('Array', @a)}
sub p6_from_l {my(@a)=@_; p6_new('List', @a)}; 
sub p6_die {my(@args)=@_; die @args;}
sub p6_set {my($o,$v)=@_; $o->store($v)}
sub p6_var : lvalue {
    my($name)=@_;
    my $m = p6_mangle($name);
    no strict;
    my $v = defined $$m ? $$m : do {$$m = Scalar->new()};
    $v;
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

sub def_prim { # used by Prim
    my($flavor, $name, $argl, $f)=@_;
    my $n = $name;
    if ($n =~ /\>$/) {
        $n =~ s/:\<(.+?)\>$/:$1/;
    } elsif ($n =~ /\]$/) {
        $n =~ s/:\[(.+?)\]$/:$1/;
    }
    $n = '&'.$n;
    my $mn = p6_mangle($n);
    #my $gn = globify_mangled($mn);
    my $cls = 'Sub';
    $cls = 'Macro' if $flavor =~ /macro/i;
    my $subobj = p6_new($cls,$name,$argl,$f);
    no strict;
    $$mn = $subobj;
}
sub p6_new {
    my($type,@arg)=@_;
    return Bit->new('$.unboxed' => @arg) if $type eq 'Bit';
    return Int->new('$.unboxed' => @arg) if $type eq 'Int';
    return Num->new('$.unboxed' => @arg) if $type eq 'Num';
    return Str->new('$.unboxed' => @arg) if $type eq 'Str';
    if ($type eq 'Sub') {
        my($name,$argl,$f)=@arg;
        my @args = map{s/\s+/ /; s/\A\s+//; s/\s+\Z//; $_} @$argl;
        my @args_nonslurpy = grep /^[^\*]/, @args;
        my @args_slurpy    = grep /^\*/, @args;
        @args_slurpy = map{s/^\*//;$_} @args_slurpy;
        my @params = map{Perl6::Param->new('name' => $_)} @args;
        my $wrapper = sub {
            my $sub = shift;
            my %param = $sub->bind_params( @_ );
            my(@a) = (@param{@args_nonslurpy},
                      map{@$_} @param{@args_slurpy});
            $f->(@a);
        };
        return Sub->new('$.name' => $name,
                        '$.params' => \@params,
                        '$.body' => $wrapper);
    }
    if ($type eq 'Macro') {
        my($name,$argl,$f)=@arg;
        return "PIL::Run::Type::$type"->new($f);
    }
    if ($type eq 'List') {
        return List->new( '$.unboxed' => Perl6::Value::List->from_single(@arg) );
    }
    if ($type eq 'Array') {
        my $ary = Array->new;
        $ary->push( @arg );
        return $ary;
    }
    return "PIL::Run::Type::$type"->new(@arg);
}
sub p6_apply {
    my($f,@args)=@_;
    $f->do(@args);
}


1;
__END__

print &p6_mangle("&say"),"\n";
def_prim("","&say",sub {print "hi\n"});
eval(&p6_mangle("&say"));
