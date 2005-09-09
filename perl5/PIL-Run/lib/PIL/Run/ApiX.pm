
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
       p6_to_x
       p6_from_b
       p6_from_n
       p6_from_s
       p6_from_a
       p6_from_l
       p6_from_x
       p6_undef
       p6_defined
       p6_die
       p6_set
       p6_bind
       p6_var_macro
       p6_var
       p6_root
       p6_main
       p6_mangle
       p6_unmangle
       p6_new
       p6_new_sub_from_pil_macro
       p6_declare_class
       p6_create_instance
       p6_apply
       p6_package_init
       );

sub p6_to_b {my($b)=@_; return 0 unless defined $b; $b->bit()->unboxed ? 1 : 0}
sub p6_to_n { Perl6::Value::numify( @_ ) }
sub p6_to_s {
    if (UNIVERSAL::isa($_[0],"Perl6::Class")) {
#	$_[0]->meta->name();
	die "Help!  How do I stringify Class into a class name?";
    } else {
        Perl6::Value::stringify( @_ );
    }
}
sub p6_to_a {my($a_obj)=@_; [ $a_obj->unboxed ] }
sub p6_to_l {my($a_obj)=@_;   $a_obj->unboxed }
sub p6_to_x {my($o)=@_; die "XXX - Unimplemented"}
sub p6_from_b {my($b)=@_; p6_new('Bit',$b ? 1 : 0)}
sub p6_from_n {my($n)=@_; p6_new(int($n) == $n ? 'Int' : 'Num', 0+$n)}
sub p6_from_s {my($s)=@_; p6_new('Str',"$s")}
sub p6_from_a {my(@a)=@_; p6_new('Array', @a)}
sub p6_from_l {my(@a)=@_; p6_new('List', @a)}; 
sub p6_from_x {
    return (map{p6_from_x($_)} @_) if @_ > 1;
    my($x)=@_;
    if(my $cls = ref($x)) {
	return p6_from_a(@$x) if $cls eq "ARRAY";
	return p6_from_h(@$x) if $cls eq "HASH";
	Carp::confess "Dont know what to do with $x";
    }
    return p6_undef() if !defined $x;
    return p6_from_n($x) if (~$x & $x) eq 0;
    return p6_from_s($x);
}
sub p6_undef {Scalar->new()}
sub p6_defined {my($x)=@_; defined $x;}# XXX - p6_defined(p6_undef) is not 1!
sub p6_die {my(@args)=@_; die @args;}
sub p6_set {my($o,$v)=@_; $o->store($v);}
sub p6_bind {my($o,$v)=@_; $o->bind($v);}
sub p6_var_macro {
    my($name,$defined1_autovivify2)=@_;
    $name =~ s/\[ \]/\[\]/; # XXX - pugsbug workaround.
    $name = "&".$name if $name =~ /^__init_/; # XXX - pugsbug? workaround.
    $name =~ /([\$\@\%\&\:])(.+)/ or die "bug $name";
    my $sigil = $1;
    my $barename = $2;
    my $m = p6_mangle($name);
    my $mn = $m; $mn =~ s/\\/\\\\/g; $mn =~ s/\'/\\\'/g;
    my $dontdie = $defined1_autovivify2 ? ',1' : '';
    my %vivifiers = ('$' => 'Scalar->new()',
                     '&' => 'Scalar->new()',
                     '@' => 'Array->new()',
                     '%' => 'Hash->new()',
                     ':' => "p6_declare_class('$barename')",
                     );
    my $vivifier = $vivifiers{$sigil} || die "bug";
    my $vivify = ($defined1_autovivify2 && $defined1_autovivify2 == 2
                  ? "||do{\$$m = $vivifier}" : '');
    "do{no strict;(defined(\$$m)?\$$m:p6_lookup('$mn'$dontdie)$vivify)}";
}
sub p6_var {
    my($name,$defined1_autovivify2)=@_;
    my $code = ("package ".p6_main()."; "
		.&p6_var_macro($name,$defined1_autovivify2));
    my $v = eval $code;
    Carp::confess "p6_var: $@\n$code\n" if $@;
    $v;
}
sub p6_root {"PIL::Run::Root"}
sub p6_main {"PIL::Run::Root::main"}
sub sigil_and_rest {
    my($n)=@_;
    my $sigil = "";
    my $bare = $n;
    if ($n =~ /^([\$\@\%\&\:]?)(.+)/) {
        $sigil = $1;
        $bare = $2;
    }
    return ($sigil,$bare);
}
my %space_from_sigil = (
             '$' => 'scalar',
             '@' => 'array',
             '%' => 'hash',
             '&' => 'code',
             ':' => 'type' # XXX - PIL uses it.  but is it spec?
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
    my $is_absolute_name = $mn =~ /::/;
    $mn =~ s/^(::)?(\*)?(::)?//;
    $mn =~ s/_/__/g;
    $mn =~ s/::/\cA/g;
    $mn =~ s/([^a-z0-9_\cA])/"_".ord($1)."x"/ieg;
    $mn =~ s/\cA/::/g;
    my @parts = split('::',$mn);
    $parts[-1] = $space."_".$parts[-1];
    $mn = join('::',@parts);
    $mn = 'PIL::Run::Root::'.$mn if $is_absolute_name;
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
    package PIL::Run::Root;
    $$mn = $subobj;
}
sub p6_new {
    my($type,@arg)=@_;
    return Bit->new('$.unboxed' => @arg) if $type eq 'Bit';
    return Int->new('$.unboxed' => @arg) if $type eq 'Int';
    return Num->new('$.unboxed' => @arg) if $type eq 'Num';
    return Str->new('$.unboxed' => @arg) if $type eq 'Str';
    return Ref->new('$.referred' => @arg) if $type eq 'Ref';
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
    if ($type eq 'Code') {
###....
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
        @arg = map { 
            UNIVERSAL::isa( $_, 'Array' ) ? $_->items : $_ } @arg;
        $ary->push( @arg );
        return $ary;
    }
    Carp::confess "unknown class";
}
sub p6_new_sub_from_pil_macro {
    my($name,$pil_params,$body,$want_macro,$SubOrCode)=@_;
    my $listify = sub {
        join(",",map{
            s/\\/\\\\/g; s/\'/\\\'/g;
            "'$_'";
        } @_);
    };
    my(@names6arg,@names6param,@names5);
    for my $p (@{$pil_params}) {
        my $n6p = $p->{'tpParam'}{'paramName'};
        my $n5  = '$'.p6_mangle($n6p);
        my $n6a = $n6p;
        my $is_slurpy = ref($p->{'tpParam'}{'paramContext'}) =~ /Slurpy/;
        my $is_optional = ref($p->{'tpParam'}{'isOptional'});
        $n6a = '?'.$n6a if $is_optional;
        $n6a = '*'.$n6a if $is_slurpy;
        push(@names6param,$n6p);
        push(@names6arg,$n6a);
        push(@names5,$n5);
    }
    my $my_args = "";
    if (@names5) {
        my $sig = @names6param > 1 ? '@' : '$';
        $my_args = ('my('
                    .join(",",@names5)
                    .")=${sig}_param{"
                    .$listify->(@names6param)
                    .'};');
    }
    my $subdef = ('sub{'
                  .'my $_sub = shift; my %_param = $_sub->bind_params(@_); '
                  .$my_args
                  .$body
                  .'}');
    if($want_macro) {
        my $params = ('[map{Perl6::Param->new(\'name\' => $_)} '
                      .'('.$listify->(@names6arg).')]');
        $SubOrCode."->new(".("'\$.name' => '$name',".
                     "'\$.params' => $params,".
                     "'\$.body' => $subdef)");
    } else {
        my $params = [map{Perl6::Param->new('name' => $_)} @names6arg];
        my $subdef = eval($subdef);  die "bug $@" if $@;
        $SubOrCode->new('$.name' => $name,
                        '$.params' => $params,
                        '$.body' => $subdef);
    }
}
sub p6_declare_class {
    my($name)=@_;
    use Perl6::MetaModel;
    use Perl6::Object;
    my $cls = class $name => {
        is => [ 'Perl6::Object' ]
        };
    $cls;
}
sub p6_create_instance {
    my($name,@args)=@_;
    use Perl6::MetaModel;
    use Perl6::Object;
    $name->create(@args);
}

sub p6_apply {
    my($f,@args)=@_;
    #print STDERR "\n<$f,",@args,">\n";
    return p6_from_b(0) if $f eq 'bit' && ! defined $args[0];  # test for definedness - undef.bit()
    if(!ref($f)) { # XXX - see PApp in EvalX.
        return $args[0]->$f(splice(@args,1));
    }
    if(!p6_to_b($f->defined())) {
        Carp::confess "Error: Application of undef.\n";
    }
    $f->do(@args);
}

sub p6_package_init {
    my($pkg)=@_;

    my @path = split(/::/,$pkg);
    my $tmp = "";
    my @classes = map{my $c = $tmp.$_;$tmp=$c.'::';$c} @path;
    @classes = reverse map{$_='PIL::Run::Root::'.$_} @classes;
    shift(@classes); # dont want current class;
    push(@classes,'PIL::Run::Root');
    my $code = "";
    $code .= "package PIL::Run::Root".($pkg eq "" ? "" : "::$pkg").";\n";
    $code .= "use PIL::Run::ApiX;\n";
    $code .= "sub p6_lookup { ";
    for my $cls (@classes) {
        my $symtab = '$'.$cls.'::{$_[0]}';
        $code .= "exists $symtab ? \${$symtab} : ";
    }
    $code .= '$_[1] ? 0 : Carp::croak("Undefined variable $_[0]") ';
    $code .= "}\n";
    #print $code;
    eval($code); die if $@;
}

p6_package_init('');
p6_package_init('main');

$PIL::Run::Root::main::hash_ENV = Hash->new();
# $PIL::Run::Root::main::hash_ENV->store( $_, $ENV{$_} ) for keys %ENV;
my $h = Perl6::Container::Hash::Native->new( hashref => \%ENV );
#use Data::Dumper;
#print Dumper( $h );
$PIL::Run::Root::main::hash_ENV->{'instance_data'}{'$:cell'}{tieable} = 1;
$PIL::Run::Root::main::hash_ENV->tie( $h );

END { p6_apply(p6_var('&*END')); }

1;
__END__

