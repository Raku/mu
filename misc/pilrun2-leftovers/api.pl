
package Perl6::Run::OnPerl5::X1::Api;
use strict;
use vars qw($VERSION @ISA @EXPORT);
require Exporter;
$VERSION = '0.01';
@ISA = qw(Exporter);
@EXPORT =
    qw(
       p6_is_object
       p6_is_perl6_object
       p6_is_native
       p6_ref
       p6_isa
       p6_undef
       p6_bool
       p6_bit
       p6_int
       p6_num
       p6_str
       p6_Undef
       p6_Bool
       p6_Bit
       p6_Int
       p6_Num
       p6_Rat
       p6_Str
       p6_Pair
       p6_Array
       p6_Hash
       p6_Ref
       p6_List
       p6_Scalar
       p6_new
       p6_new_code_CODE
       p6_code_mk_CODE
       p6_as_bool
       p6_as_num
       p6_as_str
       p6_as_array
       p6_as_hash
       p6_as_perl5_whatever
       p6_perl6_objects_from_whatever
       p6_meta
       p6_meta_CODE
       p6_initialize_package
       p6_wrap_code_with_package
       p6_eval
       p6_eval_file
       p6_macrop5
       p6_def_macrop5
       p6_container_for_var_CODE
       p6_var_CODE
       p6_var
       p6_assign
       p6_setq
       p6_bind
       p6_mangle
       p6_apply
       p6_applym
       p6_return_CODE
       p6_catch_return_CODE
       p6_loop_CODE
       p6_last_CODE
       p6_next_CODE
       p6_redo_CODE
       p6_die
       p6_to_perl
       p6_defined
       p6_declare_package
       );
#cat other/api06.pl |perl -wne '/^sub (p6_\S+)/ and print(" " x 7,$1,"\n");'
our @CARP_NOT = (__PACKAGE__);
my $ROOT = "Perl6::Run::OnPerl5::X1::Root";

=pod
https://svn.perl.org/perl6/doc/trunk/design/syn/S06.pod
    bit         single native bit
    int         native integer
    str         native string (sequence of integers, no Unicode)
    num         native floating point
    ref         native pointer 
    bool        native boolean
    Bit         Perl single bit (allows traits, aliasing, etc.)
    Int         Perl integer (allows traits, aliasing, etc.)
    Str         Perl string (Unicode semantics)
    Num         Perl number
    Complex     Perl complex number
    Ref         Perl reference
    Bool        Perl boolean
    Array       Perl array
    Hash        Perl hash
    IO          Perl filehandle
    Code        Base class for all executable objects
    Routine     Base class for all nameable executable objects
    Sub         Perl subroutine
    Method      Perl method
    Submethod   Perl subroutine acting like a method
    Macro       Perl compile-time subroutine
    Rule        Perl pattern
    Block       Base class for all unnameable executable objects
    Bare        Basic Perl block
    Parametric  Basic Perl block with placeholder parameters
    Package     Perl 5 compatible namespace
    Module      Perl 6 standard namespace
    Class       Perl 6 standard class namespace
    Object      Perl 6 object
    Grammar     Perl 6 pattern matching namespace
    List        Perl list
    Lazy        Lazily evaluated Perl list
    Eager       Non-lazily evaluated Perl list
=cut

sub p6_is_object {!p6_is_native(@_)}
sub p6_is_perl6_object {
    my $ref = ref($_[0]);
    $ref eq 'Dispatchable';
}
sub p6_is_native {
    my $ref = ref($_[0]);
    !$ref || $ref ne 'Dispatchable';
}
sub ref_flavor {
    my $x = shift;
    return 'undef' if !defined $x;
    my $ref = ref($x);
    if(!$ref) {
	return 'non_numeric' if !((~$x & $x) eq 0);
	return 'numeric';
    }
    if($ref eq 'SCALAR') {
	return 'scalar_non_numeric' if !((~$$x & $$x) eq 0);
	return 'scalar_numeric';
    }
    return lc($ref) if $ref =~ /\A(?:ARRAY|HASH|CODE|REF|GLOB|LVALUE)\z/;
    return 'perl5_object' if $ref ne 'Dispatchable';
    return 'perl6_object';
}
sub p6_ref {
    my $x = shift;
    return 'undef' if !defined $x;
    my $ref = ref($x);
    if(!$ref) {
	return 'str' if !((~$x & $x) eq 0);
	return 'int' if int($x) == $x;
	return 'num';
    }
    return lc($ref) if $ref =~ /\A(?:SCALAR|ARRAY|HASH|CODE|REF|GLOB|LVALUE)\z/;
    return 'perl5:$ref' if $ref ne 'Dispatchable';
    return $ref->ref();
}
sub p6_isa {
    my($x,$cls)=@_;
    my $ref = ref($x);
    if($ref) {
	return $x->isa($cls) if $ref eq 'Dispatchable';
	$cls =~ s/^perl5:(?=\w)//s;
	return if $ref eq $cls;
	return if lc($ref) eq $cls;
	return UNIVERSAL::isa($x,$cls);
    }
    return ($cls eq 'undef') if !defined $x;
    return ($cls eq 'str') if !((~$x & $x) eq 0);
    return ($cls eq 'num' || $cls eq 'int') if int($x) == $x;
    return ($cls eq 'num');
}

sub p6_undef {undef}
sub p6_bool {$_[0]?1:undef}
sub p6_bit {$_[0]?1:0}
sub p6_int {int($_[0])}
sub p6_num {0+$_[0]}
sub p6_str {"".$_[0]}
#sub p6_ref {ref($_[0])?$_[0]:die "bug"} # XXX - name conflict. sigh.

# some abbreviations for p6_new('Foo')
sub p6_Undef {p6_new('Undef',@_)}
sub p6_Bool {p6_new('Bool',@_)}
sub p6_Bit {p6_new('Bit',@_)}
sub p6_Int {p6_new('Int',@_)}
sub p6_Num {p6_new('Num',@_)}
sub p6_Rat {p6_new('Rat',@_)}
sub p6_Str {p6_new('Str',@_)}
sub p6_Ref {p6_new('Ref',@_)}
sub p6_Pair {p6_new('Pair',@_)}
sub p6_Array {p6_new('Array',@_)}
sub p6_Hash {p6_new('Hash',@_)}
sub p6_List {p6_new('List',@_)}
sub p6_Scalar {p6_new('Scalar',@_)}

# XXX - Using field names here is a major abstraction violation.
# Need to figure out positional constructors.  At least it's isolated here. :(
sub new_box {get_meta($_[0])->new('$.unboxed'=>$_[1])}
sub p6_new {
    my $cls = shift;
    return new_box('Undef',@_) if $cls eq 'Undef';
    return new_box('Bool',(shift()?1:undef),@_) if $cls eq 'Bool';
    return new_box('Bit',(shift()?1:0),@_) if $cls eq 'Bit';
    return new_box('Int',@_) if $cls eq 'Int';
    return new_box('Num',@_) if $cls eq 'Num';
    return get_meta('Rat')->new('$.a' => $_[0], '$.b' => $_[1]) if $cls eq 'Rat';
    return get_meta('Complex')->new('$.real'=>$_[0],'$.imag'=>$_[1]) if $cls eq 'Complex';
    return new_box('Str',@_) if $cls eq 'Str';
    return get_meta('Ref')->new('$.referred',@_) if $cls eq 'Ref';
    return get_meta('Pair')->new('$.key' => $_[0], '$.value' => $_[1]) if $cls eq 'Pair';
    if($cls eq 'Scalar') {
	my $o = get_meta('Scalar')->new();
	$o->ASSIGN($_[0]);
	return $o;
    }
    if($cls eq 'Array') {
        my $a = get_meta('Array')->new;
	$a->push( @_ );
        return $a;
    }
    if($cls eq 'Hash') {
	die "unimplemented";
    }
    if($cls eq 'IO') {
	die "unimplemented";
    }
    if($cls =~ /\A(?:Code|Routine|Sub|Method|Submethod|Macro|Rule|Block|Bare|Parametric)\z/) {
	my($name,$argl,$f,$lval)=@_;
	my $body = $f;
	$body = '$f->(@_)' if ref($f);
	my $code = code_for_code_CODE(name=>$name,kind=>$cls,params=>$argl,
				      body=>$body,lvalue=>$lval);
	my $ret = eval($code);
	Carp::confess("p6_new($cls,$name,...):\n$@\n") if $@;
	return $ret;
    }
    if($cls =~ /\A(?:Package|Module|Class|Object|Grammar)\z/) {
	my @args;
	push(@args,'$.name',$_[0]) if @_;
	return get_meta($cls)->new(@args);
    }
    if($cls =~ /\A(?:List|Lazy|Eager)\z/) {
	return get_meta($cls)->new_from_single(@_);
    }
    my $meta = get_meta($cls);
    return $meta->new(@_) if defined $meta;
    Carp::confess("p6_new: unknown class $cls\n");
}

sub p6_code_mk_CODE {
    my($name,$kind,$pams,$lval,$body)=@_;
    code_for_code_CODE(name=>$name, kind=>$kind, params=>$pams,
		       lvalue=>$lval, body=>$body);
}
sub code_for_code_CODE {
    my(%c)=@_;
    my($name,$kind,$params,$body,$lvalue,$class) =
	@c{qw(name kind params body lvalue class)};
    # XXX - lvalue is ignored - use proxy?

    my $hybrid_kludge;
    if(!ref($params)) { # a string
	$params =~ s/\A\s*\(?\s*//;
	$params =~ s/\s*\)?\s*\z//;
	my @pn = split(/\s*[,:]\s*|(?<=:)\s*/,$params);
	$params = \@pn;
	$hybrid_kludge = 1;
    }
    if(@$params && !ref($params->[0])) { # an array of strings
	my @p = map{
	    my %h;
	    s/\s+/ /; s/\A\s+//; s/\s+\Z//;
	    $h{'isOptional'}=1 if s/\?//;
	    $h{'slurpy'}=1 if s/\*//;
	    $h{'isNamed'}=1 if s/\+//;
	    $h{'isInvocant'}=1 if s/\s*:\s*$//;
	    $h{'isLValue'}=1 if s/\s+is lvalue//;
	    $h{'isLazy'}=1 if s/\s+is lazy//;
	    $h{'isWritable'}=1 if s/\s+is rw//;
	    $h{'type'}=$1 if s/\A(\S+)\s+(\S+)\z/$2/;
	    $h{'paramName'}=$_;
	    \%h;
	} @$params;
	$params = \@p;
	$hybrid_kludge = 1;
    }

    my $stringify = sub {
	my($s)=@_;
	$s =~ s/\\/\\\\/g; $s =~ s/\'/\\\'/g;
	$s = "'$s'";
    };

    return "" if $kind eq 'SubPrim' && $name =~ /END/;#XXX

    $kind = 'Sub' if $kind eq 'SubRoutine';
    $kind = 'Sub' if $kind eq 'SubPrim';
    $kind =~ s/^Sub(\w+)$/$1/;

    my $code_body = $body;

    if($kind eq 'Macro') {
	warn "Macro is not implemented.  And is unexpected.\n";
	return "";
    }
    if($kind eq 'Coroutine') {
	warn "Coroutine not implemented.\n";
	return "";
    }
    if($kind eq 'Method') {
	$code_body = p6_catch_return_CODE($code_body);
	if(!defined $class) {
	    $class = $params->[0]{'type'};
	}
    }
    if($kind eq 'Sub') {
	$code_body = p6_catch_return_CODE($code_body);
    }
    if($kind eq 'Pointy') {
    }
    if($kind eq 'Block') {
    }

    my(@names5,@names6,@param_info);
    for my $p (@$params) {
	my %op = %$p;
	my @args;

	my $default = $op{'paramDefault'};
	push(@args,"default=>sub{$default}") if defined $default;

	my $slurpy = ref($op{'paramContext'}) =~ /Slurpy/ #XXX
	    || $op{'slurpy'};

	my $pname = $op{'paramName'};
	push(@names6,$pname);
	if($hybrid_kludge) {
	    my $pn = $pname;
	    $pn =~ s/[\@\%]/\$/;
	    push(@names5,$pn);
	} else {
	    push(@names5,'$'.p6_mangle($pname));
	}
	my $aname = $pname;
	$aname = '?'.$aname if $op{'isOptional'};
	$aname = '*'.$aname if $slurpy;
	$aname = '+'.$aname if $op{'isNamed'};
	push(@args,"name=>".$stringify->($aname));

	push(@args,"invocant=>1") if $op{'isInvocant'};
	push(@args,"lvalue=>1") if $op{'isLValue'};
	push(@args,"lazy=>1") if $op{'isLazy'};
	push(@args,"named=>1") if $op{'isNamed'};
	push(@args,"writable=>1") if $op{'isWritable'};

	if(!$class && $op{'isInvocant'}) {
	    $class = $op{'type'}; # for calls via Api.
	}
	if(!$class && $op{'isInvocant'}) {
	    my @parts = split(/::/,$name); pop(@parts);
	    $class = join("::",@parts) if @parts;
	}

	my $args = "[".join(",",@args)."]";
	push(@param_info,$args);
    }
    my $code_param_info = join(",",@param_info);
    my $my_args = "";
    if(@$params) {
        my $sig = @names6 > 1 ? '@' : '$';
	$my_args = ('my('
		    .join(",",@names5)
		    .")=(${sig}_param{"
		    .join(",",map{$stringify->($_)}@names6)
		    .'});');
    }

    $code_body = ('sub{ my $_sub = shift; my %_param = $_sub->bind_params(@_); '
		  .$my_args.$code_body.'}');
    my $code_params = '[map{Perl6::Param->new(@$_)}('.$code_param_info.')]';
    my $namestr = $stringify->($name);
    my $code_args = "";
    $code_args .= "'\$.name'=>$namestr," if $name;
    $code_args .= "'\$.params'=>$code_params,";
    $code_args .= "'\$.body'=>$code_body";
    my $code_val = "p6_meta('$kind')->new($code_args)";
    my $code = $code_val;
    if($kind eq 'Method') {
	my $k = lc $kind;
	my $meth = "";
	Carp::confess("class undefined") if !defined $class;
	$code = ("(p6_meta('$class')".
		 "->add_method($namestr".
		 "=> ::make_${k}(do{my \$__f".
		 "= $code_val; sub{\$__f->(\@_)}})));");
    } elsif($name) {
	$code = "(p6_setq(__PACKAGE__,$namestr,$code_val));";
    }
    $code;
}

my %as_bool_handlers =
    ( undef => sub { undef },
      numeric     => sub { $_[0] ? 1 : undef },
      non_numeric => sub { $_[0] ? 1 : undef },
      scalar_numeric     => sub { ${$_[0]} ? 1 : undef },
      scalar_non_numeric => sub { ${$_[0]} ? 1 : undef },
      array => sub { @{$_[0]} ? 1 : undef },
      hash  => sub { %{$_[0]} ? 1 : undef },
      code  => sub { 1 },
      ref   => sub { ${$_[0]} ? 1 : undef }, # p6_as_bool(${$_[0]}) ?
      glob  => sub { ${$_[0]} ? 1 : undef },
      lvalue => sub { p6_as_bool(${$_[0]}) },
      perl5_object => sub { $_[0] ? 1 : undef },
      perl6_object => sub { $_[0]->as_perl5_bool },
     );
sub p6_as_bool {
    my($x)=@_;
    my $reftyp = ref_flavor($x);
    my $handler = $as_bool_handlers{$reftyp};
    die "bug $reftyp" if !defined $handler;
    $handler->($x);
}

my $nan = 100**100**100 / 100**100**100;
my %as_num_handlers =
    ( undef => sub { Carp::carp("p6_as_num called on undef"); $nan },
      numeric     => sub { $_[0] },
      non_numeric => sub { 0+ $_[0] },
      scalar_numeric     => sub { ${$_[0]} },
      scalar_non_numeric => sub { 0+ ${$_[0]} },
      array => sub { 0+ @{$_[0]} },
      hash  => sub { 0+ %{$_[0]} },
      code  => sub { Carp::carp("p6_as_num called on code"); $nan },
      ref   => sub { 0+ ${$_[0]} },
      glob  => sub { 0+ ${$_[0]} },
      lvalue => sub { p6_as_num(${$_[0]}) },
      perl5_object => sub { 0+ $_[0] },
      perl6_object => sub { $_[0]->as_perl5_num },
     );
sub p6_as_num {
    my($x)=@_;
    my $reftyp = ref_flavor($x);
    my $handler = $as_num_handlers{$reftyp};
    die "bug $reftyp" if !defined $handler;
    $handler->($x);
}

my %as_str_handlers =
    ( undef => sub { Carp::carp("p6_as_str called on undef"); "" },
      numeric     => sub { "". $_[0] },
      non_numeric => sub { "". $_[0] },
      scalar_numeric     => sub { "". ${$_[0]} },
      scalar_non_numeric => sub { "". ${$_[0]} },
      array => sub { "". @{$_[0]} },
      hash  => sub { "". %{$_[0]} },
      code  => sub { "". $_[0] },
      ref   => sub { "". $_[0] },
      glob  => sub { "". ${$_[0]} },
      lvalue => sub { p6_as_str(${$_[0]}) },
      perl5_object => sub { "". $_[0] },
      perl6_object => sub { $_[0]->as_perl5_str },
     );
sub p6_as_str {
    my($x)=@_;
    my $reftyp = ref_flavor($x);
    my $handler = $as_str_handlers{$reftyp};
    die "bug $reftyp" if !defined $handler;
    $handler->($x);
}

my $as_array_complain = sub { Carp::carp("p6_as_array called on $_[0]"); undef };
my %as_array_handlers =
    ( undef => sub { $as_array_complain->("undef"); [] },
      numeric     => sub { $as_array_complain->("numeric") },
      non_numeric => sub { $as_array_complain->("non_numeric") },
      scalar_numeric     => sub { $as_array_complain->("scalar_numeric") },
      scalar_non_numeric => sub { $as_array_complain->("scalar_non_numeric") },
      array => sub { $_[0] },
      hash  => sub { [%{$_[0]}] },
      code  => sub { $as_array_complain->("code") },
      ref   => sub { $as_array_complain->("ref") },
      glob  => sub { $as_array_complain->("glob") },
      lvalue => sub { p6_as_array(${$_[0]}) },
      perl5_object => sub { [@{$_[0]}] },
      perl6_object => sub { $_[0]->as_perl5_array },
     );
sub p6_as_array {
    my($x)=@_;
    my $reftyp = ref_flavor($x);
    my $handler = $as_array_handlers{$reftyp};
    die "bug $reftyp" if !defined $handler;
    $handler->($x);
}

my $as_hash_complain = sub { Carp::carp("p6_as_hash called on $_[0]"); undef };
my %as_hash_handlers =
    ( undef => sub { $as_hash_complain->("undef"); [] },
      numeric     => sub { $as_hash_complain->("numeric") },
      non_numeric => sub { $as_hash_complain->("non_numeric") },
      scalar_numeric     => sub { $as_hash_complain->("scalar_numeric") },
      scalar_non_numeric => sub { $as_hash_complain->("scalar_non_numeric") },
      array => sub { {@{$_[0]}} },
      hash  => sub { $_[0] },
      code  => sub { $as_hash_complain->("code") },
      ref   => sub { $as_hash_complain->("ref") },
      glob  => sub { $as_hash_complain->("glob") },
      lvalue => sub { p6_as_hash(${$_[0]}) },
      perl5_object => sub { \%{$_[0]} },
      perl6_object => sub { $_[0]->as_perl5_hash },
     );
sub p6_as_hash {
    my($x)=@_;
    my $reftyp = ref_flavor($x);
    my $handler = $as_hash_handlers{$reftyp};
    die "bug $reftyp" if !defined $handler;
    $handler->($x);
}

sub p6_as_perl5_whatever {
    my($x)=@_;
    return $x if !p6_is_perl6_object($x);
    $x->as_perl5_whatever();
}

my $as_p6obj_complain = sub { Carp::carp("p6_perl6_objects_from_whatever called on $_[0]"); undef };
my %as_p6obj_handlers =
    ( undef => sub { p6_Undef() },
      numeric     => sub { p6_Num($_[0]) },
      non_numeric => sub { p6_Str($_[0]) },
      scalar_numeric     => sub { p6_Num(${$_[0]}) },
      scalar_non_numeric => sub { p6_Str(${$_[0]}) },
      array => sub { p6_Array(@{$_[0]}) },
      hash  => sub { p6_Hash(%{$_[0]}) },
      code  => sub { $as_p6obj_complain->("code") }, # XXX - unimplemented.
      ref   => sub { $as_p6obj_complain->("ref") },
      glob  => sub { $as_p6obj_complain->("glob") },
      lvalue => sub { p6_perl6_objects_from_whatever(${$_[0]}) },
      perl5_object => sub { $as_p6obj_complain->("perl5_object"); $_[0] }, # XXX - what's right?
      perl6_object => sub { $_[0] },
     );
sub p6_perl6_objects_from_whatever {
    return (map{p6_perl6_objects_from_whatever($_)}@_) if @_ > 1;
    return () if @_ < 1;
    my($x)=@_;
    my $reftyp = ref_flavor($x);
    my $handler = $as_p6obj_handlers{$reftyp};
    die "bug $reftyp" if !defined $handler;
    $handler->($x);
}


sub p6_meta_CODE {
    my($pkg)=@_;
    "\$${ROOT}::${pkg}::META";
}
sub p6_meta {
    my $m = get_meta($_[0]);
    die("p6_meta: undefined package '$_[0]'.\n") if !$m;
    $m;
}
sub get_meta {
    my($pkg)=@_;
    my $meta = "${ROOT}::${pkg}::META";
    no strict;
    $$meta;
}
sub set_meta { # will hopefully remain unused.
    my($pkg,$meta_object)=@_;
    my $meta = "${ROOT}::${pkg}::META";
    $$meta = $meta_object;
}
sub p6_initialize_package {
    my($pkg,$kind)=@_;
    $kind = "" if !defined $kind;
    my $Kind = ucfirst $kind;
    my @path = split(/::/,$pkg);

    my $tmp = "";
    my @classes = map{my $c = $tmp.$_;$tmp=$c.'::';$c} @path;
    @classes = reverse map{$_=$ROOT.$_} @classes;
    shift(@classes); # dont want current class;
    push(@classes,$ROOT);

    my $code = "";
    $code .= "package $ROOT".($pkg eq "" ? "" : "::$pkg").";\n";
    $code .= "use utf8;\n";
    $code .= "use Perl6::MetaModel;\n";
    $code .= "our \$META;\n";
    $code .= "\$META = \$::${Kind}->new('\$:name' => '$pkg');\n" if $kind;
    $code .= "use Perl6::Run::OnPerl5::X1::Api;\n";
    $code .= "use Error qw(:try);\n";

    my $hlp = sub{my $cls=$_[0];
		  my $symtab = '$'.$cls.'::{$_[0]}';
		  "  exists $symtab\n     ? \${$symtab} : \n";};
    $code .= "sub p6__lookup {\n";
    $code .= $hlp->(shift @classes);
    $code .= '  exists($META->FETCH("{}")->{$_[1]}) ? $META->FETCH($_[1]) :'."\n";
    for my $cls (@classes) {
	$code .= $hlp->($cls);
    }
    my $where = $pkg;  $where = "root namespace" if $where eq "";
    my $tell = 0 ? "confess" : "croak";
    $code .= '  $_[2] ? 0 : Carp::'.$tell.'("Undefined variable $_[1] in '.$where.'\n") '.";\n";
    $code .= "}\n";

    eval_p5_code($code);
}
sub p6_wrap_code_with_package {
    my($code,$pkg)=@_;
    "package ${ROOT}::$pkg; $code";
}


sub eval_wa {
    my($code,$wa)=@_;
    my(@a,$s);
    if($wa) {
	@a = eval($code);
    } elsif(defined $wa) {
	$s = eval($code);
    } else {
	eval($code);
    }
    $wa ? @a : $s;
}

BEGIN{unlink("deleteme_code.pl") if -e "deleteme_code.pl";}
my $eval_log_file;
sub eval_p5_log_code {
    my($code)=@_;
    if($Perl6::Run::OnPerl5::X1::BB::debug && !$eval_log_file) {
	open($eval_log_file,">>deleteme_code.pl") or die $!;
    }
    print $eval_log_file $code,"\n" if $eval_log_file;
    print $code if 0;
}
sub eval_p5_log_error {
    my($code,$err)=@_;
    Carp::confess("$@\n".number_the_lines($code)."\n");
}
sub eval_p5_code {
    my($code)=@_;
    eval_p5_log_code($code);    
    my @res = eval_wa($code,wantarray);
    eval_p5_log_error($code,$@) if $@;
    return(@res);
}
sub number_the_lines {
    my($s)=@_;
    my $cnt = 1;
    $s =~ s/^/$cnt++."\t"/mge;
    $s;
}


sub p6_eval {
    my($p6,$pkg)=@_;
    $pkg = (caller())[0] if !defined $pkg;
    $pkg = 'main' if $pkg !~ /\A$ROOT/;
    $pkg = "${ROOT}::main" if $pkg eq 'main';
    $pkg = "${ROOT}" if $pkg eq '';
    my $cc = Perl6::Run::OnPerl5::X1::CodeCompile->new(p6=>$p6)->compile;
    print STDERR $cc->warnings;
    my $p5 = "package $pkg;".$cc->as_p5;
    eval_p5_code($p5);
}
sub p6_eval_file {
    my($fn)=@_;
    my $cc = Perl6::Run::OnPerl5::X1::CodeCompile->new(p6_file=>$fn)->get_p6_file;
    p6_eval($cc->as_p6);
}


my %macros;
sub p6_macrop5 {
    my($name)=@_;
    $macros{$name};
}
sub p6_def_macrop5 {
    my($kind,$name,$param,$fun)=@_;
    my $mfun = $fun;
    if($param =~ /\*/) {
	my $modifyargs ="";
	my $argl = $param;
	while($argl =~ s/\*([\@\%])(\w+)/$1$2/) {
	    $modifyargs .= "my \$$2 = \\$1$2;";
	}
	my $argl2 = $argl;
	$argl2 =~ s/[\@\%]/\$/g;
	my $code = "# macrop5 $name\n sub{my$argl=\@_; $modifyargs \$fun->$argl2}\n";
	eval_p5_log_code($code);
	$mfun = eval($code);
	eval_p5_log_error($code,$@) if $@;
    }
    $macros{$name}=$mfun;
}

sub p6_container_for_var_CODE {
    my($name)=@_;
    return "\$${ROOT}::Scalar::META->new()" if $name =~ /\$|\&/;
    return "\$${ROOT}::Array::META->new()" if $name =~ /\@/;
    return "\$${ROOT}::Hash::META->new()" if $name =~ /\%/;
    Carp::confess "bug >$name<";
}
sub p6_var_CODE {
    my($name)=@_;
    my $mn = p6_mangle($name);
    "(do{no strict;defined(\$$mn)?\$$mn:p6__lookup('$mn','$name')})";
}
sub p6_var { # XXX - not quite the right thing
    my($name)=@_;
    my $mn = p6_mangle($name);
    my $pkg = (caller)[0];
    #my $ret = eval "package $pkg;".<<'    END';
    #  no strict;defined($$mn)?$$mn:p6__lookup('$mn','$name')
    #END
    #die "p6_var: bug: $@" if $@;
    #$ret;
    my $look = "${pkg}::p6__lookup";
    no strict 'refs';
    &$look($mn,$name);
}
sub p6_setq  {
    my($pkg,$n,$v)=@_;
    my $mn = p6_mangle($n,$pkg);
#    print STDERR $mn,"\n";
    no strict 'refs';
    $$mn = p6_meta('Scalar')->new();
    $$mn->ASSIGN($v);
}
sub p6_assign {my($o,$v)=@_; $o->ASSIGN($v);}
sub p6_bind {my($o,$v)=@_; $o->BIND($v);}


my %space_from_sigil;
my %sigil_from_space;
BEGIN{
%space_from_sigil = ( '$' => 'scalar', '@' => 'array', '%' => 'hash',
			 '&' => 'code', ':' => 'type');
%sigil_from_space = map {$space_from_sigil{$_},$_} keys(%space_from_sigil);
}
sub p6_mangle {
    my($n,$pkg)=@_;
    my $sigil = substr($n,0,1);
    my $mn    = substr($n,1);
    my $is_absolute_name = $mn =~ /::|^\*/;
    $mn =~ s/^(::)?(\*)?(::)?//;
    $mn =~ s/_/__/g;
    $mn =~ s/([^a-z0-9_])/"_".ord($1)."x"/ieg;    
    $mn =~ s/_58x_58x/::/g;    
    if($sigil eq ':') {
	$mn .= "::META";
    } else {
	my $space = $space_from_sigil{$sigil};
	Carp::confess "bogus name?: '$n' with sigil '$sigil'" if !$space;
	my @parts = split('::',$mn);
	$parts[-1] = $space."_".$parts[-1];
	$mn = join('::',@parts);
    }
    $mn = $ROOT."::".$mn if $is_absolute_name;
    $mn = $pkg."::".$mn if !$is_absolute_name && $pkg;
    $mn;
}    

sub p6_apply {
    my($f,@args)=@_;
    #print STDERR "\n<$f,",@args,">\n";
    return p6_from_b(0) if $f eq 'bit' && ! defined $args[0];  # undef.bit() # XXX eep
    if(!ref($f)) { # XXX - see PApp in EvalX.
        return $args[0]->$f(splice(@args,1));
    }
    if(!p6_as_bool($f->defined())) {
        Carp::confess "Error: Application of undef.\n";
    }
    $f->do(@args);
}
sub p6_applym {
    my($m,$o,@args)=@_;
    $o->$m(@args);
}


no strict 'vars';
package Perl6::Run::OnPerl5::X1::Api::Exception;
@ISA=qw(Error);
package Perl6::Run::OnPerl5::X1::Api::Exception::Return;
@ISA=qw(Perl6::Run::OnPerl5::X1::Api::Exception);
package Perl6::Run::OnPerl5::X1::Api::Exception::LoopControl;
@ISA=qw(Perl6::Run::OnPerl5::X1::Api::Exception);
package Perl6::Run::OnPerl5::X1::Api;
use strict;
sub p6_return_CODE {
    my(@args)=@_;
    my $argl = join(",",@args);
    "(throw Perl6::Run::OnPerl5::X1::Api::Exception::Return(-text =>q{$argl},-value => [$argl]))";
}
sub p6_catch_return_CODE {
    my($code)=@_;
    '(try { '.$code.' } catch Perl6::Run::OnPerl5::X1::Api::Exception::Return with { @{$_[0]{"-value"}} })'
}
sub p6_loop_CODE {
    my($body)=@_;
    # the caller is responsible for providing any value capture needed, in $body.
    '{ my $__flag;
       try { '.$body.' }
       catch Perl6::Run::OnPerl5::X1::Api::Exception::LoopControl with { $__flag = $_[0]{"-text"} };
       if($__flag) {
          last if $__flag eq "last";
          next if $__flag eq "next";
          redo if $__flag eq "redo";
          die "p6_loop_CODE: bug";
       }
      }'."\n";
}
sub p6_last_CODE {
    "(throw Perl6::Run::OnPerl5::X1::Api::Exception::LoopControl('-text' => 'last'))";
}
sub p6_next_CODE {
    "(throw Perl6::Run::OnPerl5::X1::Api::Exception::LoopControl('-text' => 'next'))";
}
sub p6_redo_CODE {
    "(throw Perl6::Run::OnPerl5::X1::Api::Exception::LoopControl('-text' => 'redo'))";
}

sub p6_die  {my(@args)=@_; die @args;}

sub p6_to_perl {
    my($x)=@_;
    return 'undef' if !defined $x;
    my $s = eval {p6_as_str($x->perl)};
    defined $s ? $s : p6_as_str($x);
}

sub p6_defined {
    my($x)=@_;
    return 0 if !defined $x;
    return $x->defined if p6_is_perl6_object($x);
    return 1;
}

sub p6_declare_package {
    my($pkg,$kind)=@_;
    p6_initialize_package($pkg,$kind);
}

1;
__END__
