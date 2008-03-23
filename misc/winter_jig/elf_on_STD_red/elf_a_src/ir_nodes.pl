#line 2 ir_nodes.pl
# Warning: This file is mechanically written.  Your changes will be overwritten.
{ package IR::All;
  use Data::Dumper;
  sub describe_anything {
    my($this,$x)=@_;
    my $ref = ref($x);
    if($ref) {
      if(UNIVERSAL::can($x,'describe')) {
        $x->describe
      } elsif($ref eq 'ARRAY') {
        '['.join(",",map{$this->describe_anything($_)} @$x).']'
      } else {
        die "bug: $ref";
      }
    } else {
      local $Data::Dumper::Terse = 1;
      my $s = Dumper($x); $s =~ s/\n$//; $s;
    }
  }
}
{ package IR::CompUnit;
  @IR::CompUnit::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$statements)=@_;
    my %h;
    @h{'match','statements'}=($match,$statements);
    bless \%h,$cls;
  }
  sub node_name { 'CompUnit' }
  sub match { shift->{match} }
  sub field_names { qw{ statements } }
  sub field_values { my($self)=@_; @$self{'statements'} }
  sub describe {
    my($self)=@_;
    "CompUnit(".join(",",map{$self->describe_anything($_)}@$self{'statements'}).")"
  }
}
{ package IR::Val_Int;
  @IR::Val_Int::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$text)=@_;
    my %h;
    @h{'match','text'}=($match,$text);
    bless \%h,$cls;
  }
  sub node_name { 'Val_Int' }
  sub match { shift->{match} }
  sub field_names { qw{ text } }
  sub field_values { my($self)=@_; @$self{'text'} }
  sub describe {
    my($self)=@_;
    "Val_Int(".join(",",map{$self->describe_anything($_)}@$self{'text'}).")"
  }
}
{ package IR::PackageDeclarator;
  @IR::PackageDeclarator::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$kind,$name,$block)=@_;
    my %h;
    @h{'match','kind','name','block'}=($match,$kind,$name,$block);
    bless \%h,$cls;
  }
  sub node_name { 'PackageDeclarator' }
  sub match { shift->{match} }
  sub field_names { qw{ kind name block } }
  sub field_values { my($self)=@_; @$self{'kind','name','block'} }
  sub describe {
    my($self)=@_;
    "PackageDeclarator(".join(",",map{$self->describe_anything($_)}@$self{'kind','name','block'}).")"
  }
}
{ package IR::Block;
  @IR::Block::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$statements)=@_;
    my %h;
    @h{'match','statements'}=($match,$statements);
    bless \%h,$cls;
  }
  sub node_name { 'Block' }
  sub match { shift->{match} }
  sub field_names { qw{ statements } }
  sub field_values { my($self)=@_; @$self{'statements'} }
  sub describe {
    my($self)=@_;
    "Block(".join(",",map{$self->describe_anything($_)}@$self{'statements'}).")"
  }
}
{ package IR::Quote;
  @IR::Quote::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$concat)=@_;
    my %h;
    @h{'match','concat'}=($match,$concat);
    bless \%h,$cls;
  }
  sub node_name { 'Quote' }
  sub match { shift->{match} }
  sub field_names { qw{ concat } }
  sub field_values { my($self)=@_; @$self{'concat'} }
  sub describe {
    my($self)=@_;
    "Quote(".join(",",map{$self->describe_anything($_)}@$self{'concat'}).")"
  }
}
{ package IR::Val_Bit;
  @IR::Val_Bit::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$bit)=@_;
    my %h;
    @h{'match','bit'}=($match,$bit);
    bless \%h,$cls;
  }
  sub node_name { 'Val_Bit' }
  sub match { shift->{match} }
  sub field_names { qw{ bit } }
  sub field_values { my($self)=@_; @$self{'bit'} }
  sub describe {
    my($self)=@_;
    "Val_Bit(".join(",",map{$self->describe_anything($_)}@$self{'bit'}).")"
  }
}
{ package IR::Val_Num;
  @IR::Val_Num::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$num)=@_;
    my %h;
    @h{'match','num'}=($match,$num);
    bless \%h,$cls;
  }
  sub node_name { 'Val_Num' }
  sub match { shift->{match} }
  sub field_names { qw{ num } }
  sub field_values { my($self)=@_; @$self{'num'} }
  sub describe {
    my($self)=@_;
    "Val_Num(".join(",",map{$self->describe_anything($_)}@$self{'num'}).")"
  }
}
{ package IR::Val_Buf;
  @IR::Val_Buf::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$buf)=@_;
    my %h;
    @h{'match','buf'}=($match,$buf);
    bless \%h,$cls;
  }
  sub node_name { 'Val_Buf' }
  sub match { shift->{match} }
  sub field_names { qw{ buf } }
  sub field_values { my($self)=@_; @$self{'buf'} }
  sub describe {
    my($self)=@_;
    "Val_Buf(".join(",",map{$self->describe_anything($_)}@$self{'buf'}).")"
  }
}
{ package IR::Val_Char;
  @IR::Val_Char::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$char)=@_;
    my %h;
    @h{'match','char'}=($match,$char);
    bless \%h,$cls;
  }
  sub node_name { 'Val_Char' }
  sub match { shift->{match} }
  sub field_names { qw{ char } }
  sub field_values { my($self)=@_; @$self{'char'} }
  sub describe {
    my($self)=@_;
    "Val_Char(".join(",",map{$self->describe_anything($_)}@$self{'char'}).")"
  }
}
{ package IR::Val_Undef;
  @IR::Val_Undef::ISA = qw( IR::All );
  sub new {
    my($cls,$match)=@_;
    my %h;
    $h{'match'}=($match);
    bless \%h,$cls;
  }
  sub node_name { 'Val_Undef' }
  sub match { shift->{match} }
  sub field_names { return () }
  sub field_values { my($self)=@_; return () }
  sub describe {
    my($self)=@_;
    Val_Undef()
  }
}
{ package IR::Val_Object;
  @IR::Val_Object::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$clazz,$fields)=@_;
    my %h;
    @h{'match','clazz','fields'}=($match,$clazz,$fields);
    bless \%h,$cls;
  }
  sub node_name { 'Val_Object' }
  sub match { shift->{match} }
  sub field_names { qw{ clazz fields } }
  sub field_values { my($self)=@_; @$self{'clazz','fields'} }
  sub describe {
    my($self)=@_;
    "Val_Object(".join(",",map{$self->describe_anything($_)}@$self{'clazz','fields'}).")"
  }
}
{ package IR::Lit_Seq;
  @IR::Lit_Seq::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$seq)=@_;
    my %h;
    @h{'match','seq'}=($match,$seq);
    bless \%h,$cls;
  }
  sub node_name { 'Lit_Seq' }
  sub match { shift->{match} }
  sub field_names { qw{ seq } }
  sub field_values { my($self)=@_; @$self{'seq'} }
  sub describe {
    my($self)=@_;
    "Lit_Seq(".join(",",map{$self->describe_anything($_)}@$self{'seq'}).")"
  }
}
{ package IR::Lit_Array;
  @IR::Lit_Array::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$array)=@_;
    my %h;
    @h{'match','array'}=($match,$array);
    bless \%h,$cls;
  }
  sub node_name { 'Lit_Array' }
  sub match { shift->{match} }
  sub field_names { qw{ array } }
  sub field_values { my($self)=@_; @$self{'array'} }
  sub describe {
    my($self)=@_;
    "Lit_Array(".join(",",map{$self->describe_anything($_)}@$self{'array'}).")"
  }
}
{ package IR::Lit_Hash;
  @IR::Lit_Hash::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$hash)=@_;
    my %h;
    @h{'match','hash'}=($match,$hash);
    bless \%h,$cls;
  }
  sub node_name { 'Lit_Hash' }
  sub match { shift->{match} }
  sub field_names { qw{ hash } }
  sub field_values { my($self)=@_; @$self{'hash'} }
  sub describe {
    my($self)=@_;
    "Lit_Hash(".join(",",map{$self->describe_anything($_)}@$self{'hash'}).")"
  }
}
{ package IR::Lit_Pair;
  @IR::Lit_Pair::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$key,$value)=@_;
    my %h;
    @h{'match','key','value'}=($match,$key,$value);
    bless \%h,$cls;
  }
  sub node_name { 'Lit_Pair' }
  sub match { shift->{match} }
  sub field_names { qw{ key value } }
  sub field_values { my($self)=@_; @$self{'key','value'} }
  sub describe {
    my($self)=@_;
    "Lit_Pair(".join(",",map{$self->describe_anything($_)}@$self{'key','value'}).")"
  }
}
{ package IR::Lit_SigArgument;
  @IR::Lit_SigArgument::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$key,$value,$type,$has_default,$is_named_only,$is_optional,$is_slurpy,$is_multidimensional,$is_rw,$is_copy)=@_;
    my %h;
    @h{'match','key','value','type','has_default','is_named_only','is_optional','is_slurpy','is_multidimensional','is_rw','is_copy'}=($match,$key,$value,$type,$has_default,$is_named_only,$is_optional,$is_slurpy,$is_multidimensional,$is_rw,$is_copy);
    bless \%h,$cls;
  }
  sub node_name { 'Lit_SigArgument' }
  sub match { shift->{match} }
  sub field_names { qw{ key value type has_default is_named_only is_optional is_slurpy is_multidimensional is_rw is_copy } }
  sub field_values { my($self)=@_; @$self{'key','value','type','has_default','is_named_only','is_optional','is_slurpy','is_multidimensional','is_rw','is_copy'} }
  sub describe {
    my($self)=@_;
    "Lit_SigArgument(".join(",",map{$self->describe_anything($_)}@$self{'key','value','type','has_default','is_named_only','is_optional','is_slurpy','is_multidimensional','is_rw','is_copy'}).")"
  }
}
{ package IR::Lit_NamedArgument;
  @IR::Lit_NamedArgument::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$key,$value)=@_;
    my %h;
    @h{'match','key','value'}=($match,$key,$value);
    bless \%h,$cls;
  }
  sub node_name { 'Lit_NamedArgument' }
  sub match { shift->{match} }
  sub field_names { qw{ key value } }
  sub field_values { my($self)=@_; @$self{'key','value'} }
  sub describe {
    my($self)=@_;
    "Lit_NamedArgument(".join(",",map{$self->describe_anything($_)}@$self{'key','value'}).")"
  }
}
{ package IR::Lit_Code;
  @IR::Lit_Code::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$pad,$state,$sig,$body,$catch)=@_;
    my %h;
    @h{'match','pad','state','sig','body','catch'}=($match,$pad,$state,$sig,$body,$catch);
    bless \%h,$cls;
  }
  sub node_name { 'Lit_Code' }
  sub match { shift->{match} }
  sub field_names { qw{ pad state sig body catch } }
  sub field_values { my($self)=@_; @$self{'pad','state','sig','body','catch'} }
  sub describe {
    my($self)=@_;
    "Lit_Code(".join(",",map{$self->describe_anything($_)}@$self{'pad','state','sig','body','catch'}).")"
  }
}
{ package IR::Lit_Object;
  @IR::Lit_Object::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$clazz,$fields)=@_;
    my %h;
    @h{'match','clazz','fields'}=($match,$clazz,$fields);
    bless \%h,$cls;
  }
  sub node_name { 'Lit_Object' }
  sub match { shift->{match} }
  sub field_names { qw{ clazz fields } }
  sub field_values { my($self)=@_; @$self{'clazz','fields'} }
  sub describe {
    my($self)=@_;
    "Lit_Object(".join(",",map{$self->describe_anything($_)}@$self{'clazz','fields'}).")"
  }
}
{ package IR::Var;
  @IR::Var::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$sigil,$twigil,$name,$namespace)=@_;
    my %h;
    @h{'match','sigil','twigil','name','namespace'}=($match,$sigil,$twigil,$name,$namespace);
    bless \%h,$cls;
  }
  sub node_name { 'Var' }
  sub match { shift->{match} }
  sub field_names { qw{ sigil twigil name namespace } }
  sub field_values { my($self)=@_; @$self{'sigil','twigil','name','namespace'} }
  sub describe {
    my($self)=@_;
    "Var(".join(",",map{$self->describe_anything($_)}@$self{'sigil','twigil','name','namespace'}).")"
  }
}
{ package IR::Bind;
  @IR::Bind::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$parameters,$arguments)=@_;
    my %h;
    @h{'match','parameters','arguments'}=($match,$parameters,$arguments);
    bless \%h,$cls;
  }
  sub node_name { 'Bind' }
  sub match { shift->{match} }
  sub field_names { qw{ parameters arguments } }
  sub field_values { my($self)=@_; @$self{'parameters','arguments'} }
  sub describe {
    my($self)=@_;
    "Bind(".join(",",map{$self->describe_anything($_)}@$self{'parameters','arguments'}).")"
  }
}
{ package IR::Assign;
  @IR::Assign::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$parameters,$arguments)=@_;
    my %h;
    @h{'match','parameters','arguments'}=($match,$parameters,$arguments);
    bless \%h,$cls;
  }
  sub node_name { 'Assign' }
  sub match { shift->{match} }
  sub field_names { qw{ parameters arguments } }
  sub field_values { my($self)=@_; @$self{'parameters','arguments'} }
  sub describe {
    my($self)=@_;
    "Assign(".join(",",map{$self->describe_anything($_)}@$self{'parameters','arguments'}).")"
  }
}
{ package IR::Proto;
  @IR::Proto::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$name)=@_;
    my %h;
    @h{'match','name'}=($match,$name);
    bless \%h,$cls;
  }
  sub node_name { 'Proto' }
  sub match { shift->{match} }
  sub field_names { qw{ name } }
  sub field_values { my($self)=@_; @$self{'name'} }
  sub describe {
    my($self)=@_;
    "Proto(".join(",",map{$self->describe_anything($_)}@$self{'name'}).")"
  }
}
{ package IR::Call;
  @IR::Call::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$invocant,$hyper,$method,$arguments)=@_;
    my %h;
    @h{'match','invocant','hyper','method','arguments'}=($match,$invocant,$hyper,$method,$arguments);
    bless \%h,$cls;
  }
  sub node_name { 'Call' }
  sub match { shift->{match} }
  sub field_names { qw{ invocant hyper method arguments } }
  sub field_values { my($self)=@_; @$self{'invocant','hyper','method','arguments'} }
  sub describe {
    my($self)=@_;
    "Call(".join(",",map{$self->describe_anything($_)}@$self{'invocant','hyper','method','arguments'}).")"
  }
}
{ package IR::Apply;
  @IR::Apply::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$code,$arguments)=@_;
    my %h;
    @h{'match','code','arguments'}=($match,$code,$arguments);
    bless \%h,$cls;
  }
  sub node_name { 'Apply' }
  sub match { shift->{match} }
  sub field_names { qw{ code arguments } }
  sub field_values { my($self)=@_; @$self{'code','arguments'} }
  sub describe {
    my($self)=@_;
    "Apply(".join(",",map{$self->describe_anything($_)}@$self{'code','arguments'}).")"
  }
}
{ package IR::Return;
  @IR::Return::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$result)=@_;
    my %h;
    @h{'match','result'}=($match,$result);
    bless \%h,$cls;
  }
  sub node_name { 'Return' }
  sub match { shift->{match} }
  sub field_names { qw{ result } }
  sub field_values { my($self)=@_; @$self{'result'} }
  sub describe {
    my($self)=@_;
    "Return(".join(",",map{$self->describe_anything($_)}@$self{'result'}).")"
  }
}
{ package IR::If;
  @IR::If::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$test,$body,$elsif,$else)=@_;
    my %h;
    @h{'match','test','body','elsif','else'}=($match,$test,$body,$elsif,$else);
    bless \%h,$cls;
  }
  sub node_name { 'If' }
  sub match { shift->{match} }
  sub field_names { qw{ test body elsif else } }
  sub field_values { my($self)=@_; @$self{'test','body','elsif','else'} }
  sub describe {
    my($self)=@_;
    "If(".join(",",map{$self->describe_anything($_)}@$self{'test','body','elsif','else'}).")"
  }
}
{ package IR::While;
  @IR::While::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$test,$body)=@_;
    my %h;
    @h{'match','test','body'}=($match,$test,$body);
    bless \%h,$cls;
  }
  sub node_name { 'While' }
  sub match { shift->{match} }
  sub field_names { qw{ test body } }
  sub field_values { my($self)=@_; @$self{'test','body'} }
  sub describe {
    my($self)=@_;
    "While(".join(",",map{$self->describe_anything($_)}@$self{'test','body'}).")"
  }
}
{ package IR::Decl;
  @IR::Decl::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$decl,$type,$var,$default)=@_;
    my %h;
    @h{'match','decl','type','var','default'}=($match,$decl,$type,$var,$default);
    bless \%h,$cls;
  }
  sub node_name { 'Decl' }
  sub match { shift->{match} }
  sub field_names { qw{ decl type var default } }
  sub field_values { my($self)=@_; @$self{'decl','type','var','default'} }
  sub describe {
    my($self)=@_;
    "Decl(".join(",",map{$self->describe_anything($_)}@$self{'decl','type','var','default'}).")"
  }
}
{ package IR::Sig;
  @IR::Sig::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$invocant,$positional)=@_;
    my %h;
    @h{'match','invocant','positional'}=($match,$invocant,$positional);
    bless \%h,$cls;
  }
  sub node_name { 'Sig' }
  sub match { shift->{match} }
  sub field_names { qw{ invocant positional } }
  sub field_values { my($self)=@_; @$self{'invocant','positional'} }
  sub describe {
    my($self)=@_;
    "Sig(".join(",",map{$self->describe_anything($_)}@$self{'invocant','positional'}).")"
  }
}
{ package IR::Lit_Capture;
  @IR::Lit_Capture::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$invocant,$array,$hash)=@_;
    my %h;
    @h{'match','invocant','array','hash'}=($match,$invocant,$array,$hash);
    bless \%h,$cls;
  }
  sub node_name { 'Lit_Capture' }
  sub match { shift->{match} }
  sub field_names { qw{ invocant array hash } }
  sub field_values { my($self)=@_; @$self{'invocant','array','hash'} }
  sub describe {
    my($self)=@_;
    "Lit_Capture(".join(",",map{$self->describe_anything($_)}@$self{'invocant','array','hash'}).")"
  }
}
{ package IR::Lit_Subset;
  @IR::Lit_Subset::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$name,$base_class,$block)=@_;
    my %h;
    @h{'match','name','base_class','block'}=($match,$name,$base_class,$block);
    bless \%h,$cls;
  }
  sub node_name { 'Lit_Subset' }
  sub match { shift->{match} }
  sub field_names { qw{ name base_class block } }
  sub field_values { my($self)=@_; @$self{'name','base_class','block'} }
  sub describe {
    my($self)=@_;
    "Lit_Subset(".join(",",map{$self->describe_anything($_)}@$self{'name','base_class','block'}).")"
  }
}
{ package IR::Method;
  @IR::Method::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$name,$sig,$block)=@_;
    my %h;
    @h{'match','name','sig','block'}=($match,$name,$sig,$block);
    bless \%h,$cls;
  }
  sub node_name { 'Method' }
  sub match { shift->{match} }
  sub field_names { qw{ name sig block } }
  sub field_values { my($self)=@_; @$self{'name','sig','block'} }
  sub describe {
    my($self)=@_;
    "Method(".join(",",map{$self->describe_anything($_)}@$self{'name','sig','block'}).")"
  }
}
{ package IR::Sub;
  @IR::Sub::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$name,$sig,$block)=@_;
    my %h;
    @h{'match','name','sig','block'}=($match,$name,$sig,$block);
    bless \%h,$cls;
  }
  sub node_name { 'Sub' }
  sub match { shift->{match} }
  sub field_names { qw{ name sig block } }
  sub field_values { my($self)=@_; @$self{'name','sig','block'} }
  sub describe {
    my($self)=@_;
    "Sub(".join(",",map{$self->describe_anything($_)}@$self{'name','sig','block'}).")"
  }
}
{ package IR::Macro;
  @IR::Macro::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$name,$sig,$block)=@_;
    my %h;
    @h{'match','name','sig','block'}=($match,$name,$sig,$block);
    bless \%h,$cls;
  }
  sub node_name { 'Macro' }
  sub match { shift->{match} }
  sub field_names { qw{ name sig block } }
  sub field_values { my($self)=@_; @$self{'name','sig','block'} }
  sub describe {
    my($self)=@_;
    "Macro(".join(",",map{$self->describe_anything($_)}@$self{'name','sig','block'}).")"
  }
}
{ package IR::Coro;
  @IR::Coro::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$name,$sig,$block)=@_;
    my %h;
    @h{'match','name','sig','block'}=($match,$name,$sig,$block);
    bless \%h,$cls;
  }
  sub node_name { 'Coro' }
  sub match { shift->{match} }
  sub field_names { qw{ name sig block } }
  sub field_values { my($self)=@_; @$self{'name','sig','block'} }
  sub describe {
    my($self)=@_;
    "Coro(".join(",",map{$self->describe_anything($_)}@$self{'name','sig','block'}).")"
  }
}
{ package IR::P5Token;
  @IR::P5Token::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$regex)=@_;
    my %h;
    @h{'match','regex'}=($match,$regex);
    bless \%h,$cls;
  }
  sub node_name { 'P5Token' }
  sub match { shift->{match} }
  sub field_names { qw{ regex } }
  sub field_values { my($self)=@_; @$self{'regex'} }
  sub describe {
    my($self)=@_;
    "P5Token(".join(",",map{$self->describe_anything($_)}@$self{'regex'}).")"
  }
}
{ package IR::Token;
  @IR::Token::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$name,$regex,$sym)=@_;
    my %h;
    @h{'match','name','regex','sym'}=($match,$name,$regex,$sym);
    bless \%h,$cls;
  }
  sub node_name { 'Token' }
  sub match { shift->{match} }
  sub field_names { qw{ name regex sym } }
  sub field_values { my($self)=@_; @$self{'name','regex','sym'} }
  sub describe {
    my($self)=@_;
    "Token(".join(",",map{$self->describe_anything($_)}@$self{'name','regex','sym'}).")"
  }
}
{ package IR::Do;
  @IR::Do::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$block)=@_;
    my %h;
    @h{'match','block'}=($match,$block);
    bless \%h,$cls;
  }
  sub node_name { 'Do' }
  sub match { shift->{match} }
  sub field_names { qw{ block } }
  sub field_values { my($self)=@_; @$self{'block'} }
  sub describe {
    my($self)=@_;
    "Do(".join(",",map{$self->describe_anything($_)}@$self{'block'}).")"
  }
}
{ package IR::Begin;
  @IR::Begin::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$block)=@_;
    my %h;
    @h{'match','block'}=($match,$block);
    bless \%h,$cls;
  }
  sub node_name { 'Begin' }
  sub match { shift->{match} }
  sub field_names { qw{ block } }
  sub field_values { my($self)=@_; @$self{'block'} }
  sub describe {
    my($self)=@_;
    "Begin(".join(",",map{$self->describe_anything($_)}@$self{'block'}).")"
  }
}
{ package IR::Use;
  @IR::Use::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$mod,$perl5)=@_;
    my %h;
    @h{'match','mod','perl5'}=($match,$mod,$perl5);
    bless \%h,$cls;
  }
  sub node_name { 'Use' }
  sub match { shift->{match} }
  sub field_names { qw{ mod perl5 } }
  sub field_values { my($self)=@_; @$self{'mod','perl5'} }
  sub describe {
    my($self)=@_;
    "Use(".join(",",map{$self->describe_anything($_)}@$self{'mod','perl5'}).")"
  }
}
{ package IR::Rule;
  @IR::Rule::ISA = qw( IR::All );
  sub new {
    my($cls,$match)=@_;
    my %h;
    $h{'match'}=($match);
    bless \%h,$cls;
  }
  sub node_name { 'Rule' }
  sub match { shift->{match} }
  sub field_names { return () }
  sub field_values { my($self)=@_; return () }
  sub describe {
    my($self)=@_;
    Rule()
  }
}
{ package IR::Rule_Quantifier;
  @IR::Rule_Quantifier::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$term,$quant,$greedy,$ws1,$ws2,$ws3)=@_;
    my %h;
    @h{'match','term','quant','greedy','ws1','ws2','ws3'}=($match,$term,$quant,$greedy,$ws1,$ws2,$ws3);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_Quantifier' }
  sub match { shift->{match} }
  sub field_names { qw{ term quant greedy ws1 ws2 ws3 } }
  sub field_values { my($self)=@_; @$self{'term','quant','greedy','ws1','ws2','ws3'} }
  sub describe {
    my($self)=@_;
    "Rule_Quantifier(".join(",",map{$self->describe_anything($_)}@$self{'term','quant','greedy','ws1','ws2','ws3'}).")"
  }
}
{ package IR::Rule_Or;
  @IR::Rule_Or::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$terms)=@_;
    my %h;
    @h{'match','terms'}=($match,$terms);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_Or' }
  sub match { shift->{match} }
  sub field_names { qw{ terms } }
  sub field_values { my($self)=@_; @$self{'terms'} }
  sub describe {
    my($self)=@_;
    "Rule_Or(".join(",",map{$self->describe_anything($_)}@$self{'terms'}).")"
  }
}
{ package IR::Rule_Concat;
  @IR::Rule_Concat::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$concat)=@_;
    my %h;
    @h{'match','concat'}=($match,$concat);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_Concat' }
  sub match { shift->{match} }
  sub field_names { qw{ concat } }
  sub field_values { my($self)=@_; @$self{'concat'} }
  sub describe {
    my($self)=@_;
    "Rule_Concat(".join(",",map{$self->describe_anything($_)}@$self{'concat'}).")"
  }
}
{ package IR::Rule_Subrule;
  @IR::Rule_Subrule::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$metasyntax,$ident,$capture_to_array)=@_;
    my %h;
    @h{'match','metasyntax','ident','capture_to_array'}=($match,$metasyntax,$ident,$capture_to_array);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_Subrule' }
  sub match { shift->{match} }
  sub field_names { qw{ metasyntax ident capture_to_array } }
  sub field_values { my($self)=@_; @$self{'metasyntax','ident','capture_to_array'} }
  sub describe {
    my($self)=@_;
    "Rule_Subrule(".join(",",map{$self->describe_anything($_)}@$self{'metasyntax','ident','capture_to_array'}).")"
  }
}
{ package IR::Rule_SubruleNoCapture;
  @IR::Rule_SubruleNoCapture::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$metasyntax)=@_;
    my %h;
    @h{'match','metasyntax'}=($match,$metasyntax);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_SubruleNoCapture' }
  sub match { shift->{match} }
  sub field_names { qw{ metasyntax } }
  sub field_values { my($self)=@_; @$self{'metasyntax'} }
  sub describe {
    my($self)=@_;
    "Rule_SubruleNoCapture(".join(",",map{$self->describe_anything($_)}@$self{'metasyntax'}).")"
  }
}
{ package IR::Rule_Var;
  @IR::Rule_Var::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$sigil,$twigil,$name)=@_;
    my %h;
    @h{'match','sigil','twigil','name'}=($match,$sigil,$twigil,$name);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_Var' }
  sub match { shift->{match} }
  sub field_names { qw{ sigil twigil name } }
  sub field_values { my($self)=@_; @$self{'sigil','twigil','name'} }
  sub describe {
    my($self)=@_;
    "Rule_Var(".join(",",map{$self->describe_anything($_)}@$self{'sigil','twigil','name'}).")"
  }
}
{ package IR::Rule_Constant;
  @IR::Rule_Constant::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$constant)=@_;
    my %h;
    @h{'match','constant'}=($match,$constant);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_Constant' }
  sub match { shift->{match} }
  sub field_names { qw{ constant } }
  sub field_values { my($self)=@_; @$self{'constant'} }
  sub describe {
    my($self)=@_;
    "Rule_Constant(".join(",",map{$self->describe_anything($_)}@$self{'constant'}).")"
  }
}
{ package IR::Rule_Dot;
  @IR::Rule_Dot::ISA = qw( IR::All );
  sub new {
    my($cls,$match)=@_;
    my %h;
    $h{'match'}=($match);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_Dot' }
  sub match { shift->{match} }
  sub field_names { return () }
  sub field_values { my($self)=@_; return () }
  sub describe {
    my($self)=@_;
    Rule_Dot()
  }
}
{ package IR::Rule_SpecialChar;
  @IR::Rule_SpecialChar::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$char)=@_;
    my %h;
    @h{'match','char'}=($match,$char);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_SpecialChar' }
  sub match { shift->{match} }
  sub field_names { qw{ char } }
  sub field_values { my($self)=@_; @$self{'char'} }
  sub describe {
    my($self)=@_;
    "Rule_SpecialChar(".join(",",map{$self->describe_anything($_)}@$self{'char'}).")"
  }
}
{ package IR::Rule_Block;
  @IR::Rule_Block::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$closure)=@_;
    my %h;
    @h{'match','closure'}=($match,$closure);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_Block' }
  sub match { shift->{match} }
  sub field_names { qw{ closure } }
  sub field_values { my($self)=@_; @$self{'closure'} }
  sub describe {
    my($self)=@_;
    "Rule_Block(".join(",",map{$self->describe_anything($_)}@$self{'closure'}).")"
  }
}
{ package IR::Rule_InterpolateVar;
  @IR::Rule_InterpolateVar::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$var)=@_;
    my %h;
    @h{'match','var'}=($match,$var);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_InterpolateVar' }
  sub match { shift->{match} }
  sub field_names { qw{ var } }
  sub field_values { my($self)=@_; @$self{'var'} }
  sub describe {
    my($self)=@_;
    "Rule_InterpolateVar(".join(",",map{$self->describe_anything($_)}@$self{'var'}).")"
  }
}
{ package IR::Rule_NamedCapture;
  @IR::Rule_NamedCapture::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$rule,$ident,$capture_to_array)=@_;
    my %h;
    @h{'match','rule','ident','capture_to_array'}=($match,$rule,$ident,$capture_to_array);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_NamedCapture' }
  sub match { shift->{match} }
  sub field_names { qw{ rule ident capture_to_array } }
  sub field_values { my($self)=@_; @$self{'rule','ident','capture_to_array'} }
  sub describe {
    my($self)=@_;
    "Rule_NamedCapture(".join(",",map{$self->describe_anything($_)}@$self{'rule','ident','capture_to_array'}).")"
  }
}
{ package IR::Rule_Before;
  @IR::Rule_Before::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$rule,$assertion_modifier,$capture_to_array)=@_;
    my %h;
    @h{'match','rule','assertion_modifier','capture_to_array'}=($match,$rule,$assertion_modifier,$capture_to_array);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_Before' }
  sub match { shift->{match} }
  sub field_names { qw{ rule assertion_modifier capture_to_array } }
  sub field_values { my($self)=@_; @$self{'rule','assertion_modifier','capture_to_array'} }
  sub describe {
    my($self)=@_;
    "Rule_Before(".join(",",map{$self->describe_anything($_)}@$self{'rule','assertion_modifier','capture_to_array'}).")"
  }
}
{ package IR::Rule_After;
  @IR::Rule_After::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$rule,$assertion_modifier,$capture_to_array)=@_;
    my %h;
    @h{'match','rule','assertion_modifier','capture_to_array'}=($match,$rule,$assertion_modifier,$capture_to_array);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_After' }
  sub match { shift->{match} }
  sub field_names { qw{ rule assertion_modifier capture_to_array } }
  sub field_values { my($self)=@_; @$self{'rule','assertion_modifier','capture_to_array'} }
  sub describe {
    my($self)=@_;
    "Rule_After(".join(",",map{$self->describe_anything($_)}@$self{'rule','assertion_modifier','capture_to_array'}).")"
  }
}
{ package IR::Rule_NegateCharClass;
  @IR::Rule_NegateCharClass::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$chars)=@_;
    my %h;
    @h{'match','chars'}=($match,$chars);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_NegateCharClass' }
  sub match { shift->{match} }
  sub field_names { qw{ chars } }
  sub field_values { my($self)=@_; @$self{'chars'} }
  sub describe {
    my($self)=@_;
    "Rule_NegateCharClass(".join(",",map{$self->describe_anything($_)}@$self{'chars'}).")"
  }
}
{ package IR::Rule_CharClass;
  @IR::Rule_CharClass::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$chars)=@_;
    my %h;
    @h{'match','chars'}=($match,$chars);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_CharClass' }
  sub match { shift->{match} }
  sub field_names { qw{ chars } }
  sub field_values { my($self)=@_; @$self{'chars'} }
  sub describe {
    my($self)=@_;
    "Rule_CharClass(".join(",",map{$self->describe_anything($_)}@$self{'chars'}).")"
  }
}
{ package IR::Rule_Capture;
  @IR::Rule_Capture::ISA = qw( IR::All );
  sub new {
    my($cls,$match,$rule,$position,$capture_to_array)=@_;
    my %h;
    @h{'match','rule','position','capture_to_array'}=($match,$rule,$position,$capture_to_array);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_Capture' }
  sub match { shift->{match} }
  sub field_names { qw{ rule position capture_to_array } }
  sub field_values { my($self)=@_; @$self{'rule','position','capture_to_array'} }
  sub describe {
    my($self)=@_;
    "Rule_Capture(".join(",",map{$self->describe_anything($_)}@$self{'rule','position','capture_to_array'}).")"
  }
}
