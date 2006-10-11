# PIL1 will go away in the hopefully not too distant future.
# So this is all throw-away code.
use strict;
package PIL::PIL1CPerl5::NodesList;
my $list;
sub get_name_rep_type_pairs_list { $list }

sub hlp {
    my($rep,$type,$name,@pairs)=@_;
    push(@$list,[$name,$rep,$type,@pairs]);
}
sub node_s { hlp('string',@_); }
sub node_a { hlp('array1',@_); }
sub node_a2{ hlp('array2',@_); }
sub node_h { hlp('hash',@_); }

BEGIN{
node_h 'PIL_Environment',      'PIL_Environment', 
  'pilGlob' ,'[PIL_Decl]',
  'pilMain' ,'PIL_Stmts';

node_s 'PIL_Stmts',            'PNil';
node_h 'PIL_Stmts',            'PStmts', 
  'pStmt'  ,'PIL_Stmt',
  'pStmts' ,'PIL_Stmts';
node_h 'PIL_Stmts',            'PPad', 
  'pScope' ,'Scope',
  'pSyms'  ,'[(VarName, PIL_Expr)]',
  'pStmts' ,'PIL_Stmts';

node_s 'PIL_Stmt',             'PNoop';
node_h 'PIL_Stmt',             'PStmt', 
  'pExpr' ,'PIL_Expr';
node_h 'PIL_Stmt',             'PPos', 
  'pPos'  ,'Pos',
  'pExp'  ,'Exp',
  'pNode' ,'PIL_Stmt';

node_h 'PIL_Expr',             'PRawName', 
  'pRawName' ,'VarName';
node_h 'PIL_Expr',             'PExp', 
  'pLV'   ,'PIL_LValue';
node_h 'PIL_Expr',             'PLit', 
  'pLit'  ,'PIL_Literal';
node_h 'PIL_Expr',             'PThunk', 
  'pThunk' ,'PIL_Expr';
node_h 'PIL_Expr',             'PCode', 
  'pType'    ,'SubType',
  'pParams'  ,'[TParam]',
  'pLValue'  ,'Bool',
  'pIsMulti' ,'Bool',
  'pBody'    ,'PIL_Stmts';

node_h 'PIL_Decl',             'PSub', 
  'pSubName'    ,'SubName',
  'pSubType'    ,'SubType',
  'pSubParams'  ,'[TParam]',
  'pSubLValue'  ,'Bool',
  'pSubIsMulti' ,'Bool',
  'pSubBody'    ,'PIL_Stmts';

node_h 'PIL_Literal',          'PVal', 
  'pVal'  ,'Val';

node_h 'PIL_LValue',           'PVar', 
  'pVarName' ,'VarName';
node_h 'PIL_LValue',           'PApp', 
  'pCxt'  ,'TCxt',
  'pFun'  ,'PIL_Expr',
  'pInv'  ,'Maybe PIL_Expr',
  'pArgs' ,'[PIL_Expr]';
node_h 'PIL_LValue',           'PAssign', 
  'pLHS'  ,'[PIL_LValue]',
  'pRHS'  ,'PIL_Expr';
node_h 'PIL_LValue',           'PBind', 
  'pLHS'  ,'[PIL_LValue]',
  'pRHS'  ,'PIL_Expr';

node_h 'TParam',               'MkTParam', 
  'tpParam'   ,'Param',
  'tpDefault' ,'Maybe PIL_Expr';

node_s 'TCxt',                 'TCxtVoid';
node_a 'TCxt',                 'TCxtLValue',
  'type' ,'Type';
node_a 'TCxt',                 'TCxtItem',
  'type' ,'Type';
node_a 'TCxt',                 'TCxtSlurpy',
  'type' ,'Type';
node_a 'TCxt',                 'TTailCall',
  'tcxt' ,'TCxt';

node_h 'TEnv',                 'MkTEnv', 
  'tLexDepth' ,'Int',
  'tTokDepth' ,'Int',
  'tCxt'      ,'TCxt',
  'tReg'      ,'(TVar (Int, String))',
  'tLabel'    ,'(TVar (Int))';


node_s 'Scope', 'SState';
node_s 'Scope', 'SMy';
node_s 'Scope', 'SOur';
node_s 'Scope', 'SLet';
node_s 'Scope', 'STemp';
node_s 'Scope', 'SGlobal';

node_s 'SubType', 'SubMethod';
node_s 'SubType', 'SubCoroutine';
node_s 'SubType', 'SubMacro';
node_s 'SubType', 'SubRoutine';
node_s 'SubType', 'SubBlock';
node_s 'SubType', 'SubPointy';
node_s 'SubType', 'SubPrim';

node_s 'Val', 'VUndef';
node_a 'Val', 'VBool' , 'value' ,'unk';
node_a 'Val', 'VInt'  , 'value' ,'unk';
node_a 'Val', 'VRat'  , 'value' ,'unk';
node_a 'Val', 'VNum'  , 'value' ,'unk';
node_a 'Val', 'VStr'  , 'value' ,'unk';
node_a 'Val', 'VList' , 'value' ,'unk';
node_a 'Val', 'VType' , 'value' ,'unk';

node_s 'Cxt', 'CxtVoid';
node_a 'Cxt', 'CxtItem'   , 'type' ,'Type';
node_a 'Cxt', 'CxtSlurpy' , 'type' ,'Type';

node_a 'Type', 'MkType',
  'typename', 'String';
node_a2 'Type', 'TypeOr',
  'lhs' ,'Type',
  'rhs' ,'Type';
node_a2 'Type', 'TypeAnd',
  'lhs' ,'Type',
  'rhs' ,'Type';

node_h 'Param', 'MkParam',
  'isInvocant'   ,'Bool',
  'isOptional'   ,'Bool',
  'isNamed'      ,'Bool',
  'isLValue'     ,'Bool',
  'isWritable'   ,'Bool',
  'isLazy'       ,'Bool',
  'paramName'    ,'String',
  'paramContext' ,'Cxt',
  'paramDefault' ,'Exp';

node_h 'Pos', 'MkPos',
  'posName'        ,'String',
  'posBeginLine'   ,'Int',
  'posBeginColumn' ,'Int',
  'posEndLine'     ,'Int',
  'posEndColumn'   ,'Int';
}

package PIL::PIL1CPerl5::NodesInfo::Object;
sub new {
    my($cls,$name,$rep,$type,$pairs)=@_;
    my @fields;
    my %field_type;
    for(my $i=0; $i < @$pairs; $i+=2) {
        my($fname,$ftype)=($pairs->[$i],$pairs->[$i+1]);
        push(@fields,$fname);
        $field_type{$fname} = $ftype;
    }
    my $self = {
        name => $name,
        rep  => $rep,
        type => $type,
        pairs => $pairs,
        fields => \@fields,
        field_type => \%field_type,
    };
    bless $self,$cls;
    $self;
}
sub rep_type  { $_[0]{'rep'} } # string array1 array2 hash
sub field_is_array {
    my($self,$fname)=@_;
    $self->{'field_type'}{$fname} =~ /^\[/ ? 1 : 0;
}
sub field_is_optional {
    my($self,$fname)=@_;
    $self->{'field_type'}{$fname} =~ /Maybe/ ? 1 : 0;
}
sub field_is_node {
    my($self,$fname)=@_;
    ($self->{'field_type'}{$fname} =~ /^(\w+)/
     && defined $PIL::PIL1CPerl5::NodesInfo::node_named{$1});
}
sub field_type_simple {
    my($self,$fname)=@_;
    my $ftype = $self->{'field_type'}{$fname};
    $ftype =~ s/Mabye //;
    $ftype =~ s/[\[\]]//g;
    $ftype;
}
# XXX - not sure what to do about TVar and pSyms.
package PIL::PIL1CPerl5::NodesInfo;
our %node_named;
our @nodes;
our %nodes_of_type;
sub init {
    my $list = PIL::PIL1CPerl5::NodesList::get_name_rep_type_pairs_list();
    my $code = "";
    for (@$list) {
        my($name,$rep,$type,@pairs)=@$_;
        my $o = PIL::PIL1CPerl5::NodesInfo::Object->new($name,$rep,$type,\@pairs);
        $node_named{$name} = $o;
        push(@nodes,$o);
        push(@{$nodes_of_type{$type}},$o);
    }
}
BEGIN{&init();}

package PIL::PIL1::NodeSet0;
sub gen_code {
    my($pkg_root)=@_;
    $pkg_root = 'PIL::PIL1::NodeSet0' if !$pkg_root;
    my $code = "";
    my(%created,%used);
    for my $n (@PIL::PIL1CPerl5::NodesInfo::nodes) {
        my $name = $n->{'name'};
        my $rep = ucfirst $n->rep_type;
        my $type = $n->{'type'};
	my $pkg_this = "${pkg_root}::$name";
	my $pkg_type = "${pkg_root}::$type";
	my $pkg_rep  = "${pkg_root}::NodeRep$rep";
	my $pkg_node = "${pkg_root}::Node";
	$pkg_type = "" if $pkg_type eq $pkg_this;
	$used{$pkg_type}=1 if $pkg_type;
	$used{$pkg_rep}=1;
	$used{$pkg_node}=1;
	$created{$pkg_this}=1;
        $code .= <<END;

package $pkg_this;
       \@${pkg_root}::${name}::ISA=qw(
          $pkg_type
          $pkg_rep
          $pkg_node);
{ my \$info = \$PIL::PIL1CPerl5::NodesInfo::node_named{'$name'};
  sub info { \$info } }
sub name { '$name' }
END
    }
$code .= "package ${pkg_root}::Node;\n".<<'END';
sub is_slurpy { $_[0]->name =~ /Slurpy/ }
END
    foreach my $pkg (keys %used) {
	next if $created{$pkg};
	$code .= "package $pkg;\n";
    }
    #print $code;
    $code;
}
BEGIN{ eval(gen_code); die $@ if $@; }

package PIL::PIL1CPerl5::Util;
sub rebless_with_prefix {
    my($o,$prefix)=@_;
    my $ref = ref $o;
    return $o if !$ref;
    return $o if $ref eq 'Math::BigInt';
    if($ref !~ /^([A-Z]+)$/) {
	$ref =~ s/::/_/; # PIL::Environment :(
        $ref = "$prefix$ref";
        bless $o,'AvoidAnyOverloading'.int(rand(10000000));
    }
    my $s = "$o";
    if($s =~ /ARRAY/) {
        for (@$o) { rebless_with_prefix($_,$prefix) }
    } elsif($s =~ /HASH/) {
        for (values %$o) { rebless_with_prefix($_,$prefix) }
    } else {die "bug"}
    bless $o,$ref;
    return $o;
}
sub rebless_with_prefix_and_cleanup {
    my($o,$prefix)=@_;
    my $ref = ref $o;
    return $o if !$ref;
    return $o if $ref eq 'Math::BigInt';
    if($ref !~ /^([A-Z]+)$/) {
	$ref =~ s/::/_/; # PIL::Environment :(
        $ref = "$prefix$ref";
        bless $o,'AvoidAnyOverloading'.int(rand(10000000));
    }
    my $s = "$o";
    if($s =~ /ARRAY/) {
        for (@$o) {
	    rebless_with_prefix_and_cleanup($_,$prefix);
	}
    } elsif($s =~ /HASH/) {
        for (keys %$o) {
	    my $v = $$o{$_};
	    if(ref($v)) {
		rebless_with_prefix_and_cleanup($v,$prefix);
	    } elsif(defined $v) {
		$$o{$_} = "$prefix$v"
		    if $v =~ /^(PNil|PNoop|TCxtVoid|VUndef|CxtVoid)$/;
	    }
	}
    } else {die "bug"}
    bless $o,$ref;
    return $o;
}


package PIL::PIL1CPerl5::Unparse_CPerl5;
sub recurse {
    my($x)=@_;
    my $s;
    return '(undef)' if !defined $x;
    my $ref = ref($x);
    if($ref eq 'ARRAY') {
	$s = '['.join(",",map{recurse($_)}@$x).']';
    } elsif($ref) {
	$s = $x->unparse;
    } elsif($x !~ /^\d+(\.\d+)?$/s) {
	$x =~ s/([\$\@\%\\\"])/\\$1/g;
	$s = "\"$x\"";
    } else { $s = $x }
    $s;
}
sub PIL::PIL1::NodeSet0::NodeRepString::unparse {
    my $n = $_[0]->name; "\"$n\"";
}
sub PIL::PIL1::NodeSet0::NodeRepArray1::unparse {
    my $n = $_[0]->name;
    my $v0 = $_[0][0]; my $s = recurse($v0);
    $s = "\"$v0\"" if $n eq 'VStr' && $s !~ /\A\"/;
    "bless([$s] , \"$n\")";
}
sub PIL::PIL1::NodeSet0::NodeRepArray2::unparse {
    my $n = $_[0]->name;
    my $v0 = $_[0][0]; $v0 = recurse($v0);
    my $v1 = $_[0][1]; $v1 = recurse($v1);
    "bless([$v0,$v1] , \"$n\")";
}
sub PIL::PIL1::NodeSet0::NodeRepHash::unparse {
    my $n = $_[0]->name;
    my @kv;
    for my $k (@{$_[0]->info->{'fields'}}) {
        my $v = $_[0]{$k};
	die "bug" if !defined $_[0];
	next if defined($v) && $v eq $_[0]; # where does this recursion arise?
	my $v2 = recurse($v);
	$v2 = "\"$v\"" if $k eq 'posName' && $v2 !~ /\A\"/;
        push(@kv,"$k => ".$v2);
    }
    my $kv = join(",",@kv);
    "bless({$kv} , \"$n\")";
}

package PIL::PIL1CPerl5::Util::Hacks;
sub pil_diff {
    my(@c01)=@_;
    for(@c01) { s/ /\n/g; s/\n*\z/\n/; }
    my $tof = sub { open(F,">$_[0]") or die; print F $_[1];close F;};
    $tof->("deleteme_pil0",$c01[0]);
    $tof->("deleteme_pil1",$c01[1]);
    system("diff -u --minimal deleteme_pil0 deleteme_pil1");
}
sub p2p {
    my($fn)=@_;
    my $c0 = `cat $fn`;
    my $p = eval($c0); die "$fn: $@" if $@;
    PIL::PIL1CPerl5::Util::rebless_with_prefix($p,'PIL::PIL1::NodeSet0::');
    return if !defined $p;
    my $c1 = $p->unparse;
    $c1 =~ s/PIL_Environment/PIL::Environment/g;
    if($c0 ne $c1) { pil_diff($c0,$c1) }
}

package PIL::PIL1CPerl5::Util::FilterNodeDefs;
our($package_name,$method_name);
sub import {
    my($pkg,$name)=@_;
    $package_name = $pkg || '__PACKAGE__';
    $method_name = $name || 'emit';
}
sub gen {
    my($name,$vars,$body)=@_;

    my $info = $PIL::PIL1CPerl5::NodesInfo::node_named{$name};
    die "bug $name" if !$info;
    my $chk = join(", ",map {"\$$_"} @{$info->{'fields'}});
    $vars eq $chk || die "bug: node field list mismatch:\n$vars\n$chk\n";
    my $keys = join(" ", @{$info->{'fields'}});

    my $code = "";
    $code .= "sub ${package_name}::${name}::${method_name} {";

    my $rep = $info->rep_type;
    if($vars eq "") {}
    elsif($rep eq 'string') {}
    elsif($rep eq 'hash') {
	$code .= " my \$self=\$_[0];my($vars)=\@{\$self}{qw($keys)};";}
    else { # array
	$code .= " my \$self=\$_[0];my($vars)=\@{\$self};";}

    $code .= "$body}";
    $code;
}
use Filter::Simple sub {
    s/\#.+//g;
    s/^NODE\s+(\S+)\s+\(([^\)]*)\)\s+\{(.+?)(?:^\}|\}\s*?$)/gen($1,$2,$3)/mseg;
    s/\bDOWN\((.+?)\)/${1}->emit()/g;
    #s/\bLOOK\((.+?)\)//g;
    #print;
    #print STDERR;
    $_;
};
# BEGIN { FILTER_ONLY code => sub {} }; also works?

1;
__END__
#----------------------------------------------------------------------
What is Exp?  And why is it in my tree? :)

