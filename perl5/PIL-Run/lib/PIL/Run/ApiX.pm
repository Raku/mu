
package PIL::Run::ApiX;
use strict;
use vars qw($VERSION @ISA @EXPORT);
require Exporter;
$VERSION = '0.01';
@ISA = qw(Exporter);
@EXPORT =
    qw(
       p6_isa
       p6_to_b
       p6_to_n
       p6_to_s
       p6_to_a
       p6_to_l
       p6_to_x
       p6_to_perl
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
       p6_return_macro
       p6_loop_macro
       p6_last_macro
       p6_next_macro
       p6_redo_macro
       );

sub p6_isa {
    my($o,$cls)=@_;
    my $ref = ref($o);
    return 1 if $ref eq 'Dispatchable' && $o->isa($cls);
    return 1 if lc($ref) eq lc($cls);
    return 0;
}

sub p6_to_b {my($b)=@_; return 0 unless defined $b; $b->bit()->unboxed ? 1 : 0}
sub p6_to_n { Perl6::Value::numify( @_ ) }
sub p6_to_s {
    if ("$_[0]" =~ /^\#<Class=/) { # XXX - kludge
	$_[0]->name; # {identifier} is long, {name} is short
    } else {
        Perl6::Value::stringify( @_ );
    }
}
sub p6_to_a {
    my($ary)=@_;
    my $is_infinite = 0;
    #eval { $is_infinite = Perl6::Value::numify( $ary->is_infinite ) };
    warn "Trying to instantiate an infinite Array"
        if $ary->can('is_infinite') && $ary->is_infinite;
    my @a;
    for(my $i=Perl6::Value::numify( $ary->elems ) - 1; $i >=0; $i--) {
	$a[$i] = $ary->fetch($i)->fetch;
    }
    \@a;
}
sub p6_to_l {my($a_obj)=@_;   $a_obj->unboxed }
sub p6_to_x {my($o)=@_; die "XXX - Unimplemented"}
sub p6_to_perl {my($o)=@_; eval {p6_to_s($o->perl)} || p6_to_s($o) }
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
sub p6_die  {my(@args)=@_; die @args;}
sub p6_set  {my($o,$v)=@_; $o->store($v);}
sub p6_bind {my($o,$v)=@_; $o->bind($v);}
sub p6_var_macro {
    my($name,$defined1_autovivify2)=@_;
    $name =~ s/\[ \]/\[\]/; # XXX - pugsbug workaround.
    # see EvalX.pm re __export_ etal.
    $name = "&".$name if $name =~ /^__init_/; # XXX - pugsbug? workaround.
    $name = "&".$name if $name =~ /^__export_/; # XXX - pugsbug? workaround.
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
                  ? "||do{warn \"autovivifying $m\\n\" if \$main::global_debug;\$$m = $vivifier}" : '');
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
    return "${mn}::MM" if $sigil eq ':';
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
                      map{@{p6_to_a($_)}} @param{@args_slurpy});
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
    if ($type eq 'Junction') {
        my $type = shift @arg;
        my $j = Junction->new;
        $j->type( $type );    # unboxed str
        my @val;
        for my $arg ( @arg ) {
            if ( $arg->isa( "Array" ) ) {
                push @val, $arg->fetch($_)->fetch for 0 .. $arg->elems->unboxed - 1;
            }
            else {
                push @val, $arg->fetch;
            }
        }
        $j->things( \@val );  # unboxed array of objects
        $j->junction_normalize;
        # warn "JUNCTION: ", $j->type," of @{$j->values}";
        return $j;
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
	my @info;
	my $default = $p->{'tpDefault'};
	if(defined $default) {
	    my $code = $default->expand(); # XXX - abstraction violation
	    $code = "sub{ $code }";
	    push(@info,['default',$code]);
	}
        my $n6p = $p->{'tpParam'}{'paramName'};
        my $n5  = '$'.p6_mangle($n6p);
        my $n6a = $n6p;
        my $is_slurpy = ref($p->{'tpParam'}{'paramContext'}) =~ /Slurpy/;
        my $is_invocant = $p->{'tpParam'}{'isInvocant'};
        my $is_lvalue   = $p->{'tpParam'}{'isLValue'};
        my $is_lazy     = $p->{'tpParam'}{'isLazy'};
        my $is_named    = $p->{'tpParam'}{'isNamed'};
        my $is_optional = $p->{'tpParam'}{'isOptional'};
        my $is_writable = $p->{'tpParam'}{'isWritable'};
        $n6a = '?'.$n6a if $is_optional;
        $n6a = '*'.$n6a if $is_slurpy;
	push(@info,['invocant',1]) if $is_invocant;
	push(@info,['lvalue',1])   if $is_lvalue;
	push(@info,['lazy',1])     if $is_lazy;
	push(@info,['named',1])    if $is_named;
	push(@info,['writable',1]) if $is_writable;
        push(@names6param,$n6p);
        push(@names6arg,[['name',$listify->($n6a)],@info]);
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
    my $body_wrapped = $body;
    $body_wrapped = ("try {"
		     ."\n".$body_wrapped
		     ."\n} catch PIL::Run::ApiX::Exception::Return with {"
		     ." return \@{\$_[0]{'-value'}}; }\n"
		     ) if $SubOrCode eq 'Sub';
    my $subdef = ('sub{'
                  .'my $_sub = shift; my %_param = $_sub->bind_params(@_); '
                  .$my_args
                  .$body_wrapped
                  .'}');
    if($want_macro) {
	my $args = join(",",map{
	    "[".join(",",map{join("=>",@$_)}@$_)."]"
	    } @names6arg);
        my $params = ('[map{Perl6::Param->new(@$_)} '
                      .'('.$args.')]');
        $SubOrCode."->new(".("'\$.name' => '$name',".
                     "'\$.params' => $params,".
                     "'\$.body' => $subdef)");
    } else {
        my $params = [map{Perl6::Param->new(map{@$_}@$_)} @names6arg];
        my $subdef = eval($subdef);  die "bug $@" if $@;
        $SubOrCode->new('$.name' => $name,
                        '$.params' => $params,
                        '$.body' => $subdef);
    }
}
sub p6_declare_class {
    my($name)=@_;
    use Perl6::Value;
    my $cls = class1($name => {
        is => [ $::Object ],
        });
    $cls;
}
sub p6_create_instance {
    my($name,@args)=@_;
    $name->new(@args);
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
    $code .= "use Error qw(:try);\n";
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

no strict 'vars';
package PIL::Run::ApiX::Exception;
@ISA=qw(Error);
package PIL::Run::ApiX::Exception::Return;
@ISA=qw(PIL::Run::ApiX::Exception);
package PIL::Run::ApiX::Exception::LoopControl;
@ISA=qw(PIL::Run::ApiX::Exception);
package PIL::Run::ApiX;

sub p6_return_macro {
    my(@args)=@_;
    my $argl = join(",",@args);
    #return "return ($argl)";
    $EvalX::PStmt::protection_unacceptable = 1;
    "(throw PIL::Run::ApiX::Exception::Return(-text =>q{$argl},-value => [$argl]))";
}
sub p6_loop_macro {
    my($body)=@_;
    $EvalX::PStmt::protection_unacceptable = 1;
    'my $__flag;
     try { '.$body.' }
     catch PIL::Run::ApiX::Exception::LoopControl with { $__flag = $_[0]{"-text"} };
     if($__flag) {
        last if $__flag eq "last";
        next if $__flag eq "next";
        redo if $__flag eq "redo";
        die "p6_loop_macro: bug";
     }'."\n";
}
sub p6_last_macro {
    $EvalX::PStmt::protection_unacceptable = 1;
    "(throw PIL::Run::ApiX::Exception::LoopControl('-text' => 'last'))";
}
sub p6_next_macro {
    $EvalX::PStmt::protection_unacceptable = 1;
    "(throw PIL::Run::ApiX::Exception::LoopControl('-text' => 'next'))";
}
sub p6_redo_macro {
    $EvalX::PStmt::protection_unacceptable = 1;
    "(throw PIL::Run::ApiX::Exception::LoopControl('-text' => 'redo'))";
}


sub p6_initialize {
    # Boot runtime.  # XXX - needs a cleanup pass
    p6_package_init('');
    p6_package_init('main');

    if(0){
	require Perl6::Container::Hash;
	require Perl6::Container::Array;
	$PIL::Run::Root::main::hash_ENV = Hash->new();
	my $h = Perl6::Container::Hash::Native->new( hashref => \%ENV );
	$PIL::Run::Root::main::hash_ENV->{'instance_data'}{'$:cell'}{tieable} = 1;
	$PIL::Run::Root::main::hash_ENV->tie( $h );
	
	$PIL::Run::Root::main::Perl5::array_INC = Array->new();
	my $a = Perl6::Container::Array::Native->new( arrayref => \@INC );
	$PIL::Run::Root::main::Perl5::array_INC->{'instance_data'}{'$:cell'}{tieable} = 1;
	$PIL::Run::Root::main::Perl5::array_INC->tie( $a );
    }
    
    END { p6_apply(p6_var('&*END')); }
}
p6_initialize();

sub p6_initialize_late {
    { local $SIG{__WARN__} = sub {};
      require PIL::Run::PrimP5; }

    require PIL::Run::EvalX;
    PIL::Run::EvalX::p6_eval('require P5Runtime::PrimP6;');
}
p6_initialize_late();



1;
__END__

