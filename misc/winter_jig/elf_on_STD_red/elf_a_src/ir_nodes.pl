#line 2 ir_nodes.pl
# Warning: This file is mechanically written.  Your changes will be overwritten.
{ package IR::All;
}
{ package IR::CompUnit;
  @IR::CompUnit::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$statements)=@_;
    my %h;
    @h{'zmatch','statements'}=($zmatch,$statements);
    bless \%h,$cls;
  }
  sub node_name { 'CompUnit' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ statements } }
  sub field_values { my($self)=@_; @$self{'statements'} }
}
{ package IR::Val_Int;
  @IR::Val_Int::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$text)=@_;
    my %h;
    @h{'zmatch','text'}=($zmatch,$text);
    bless \%h,$cls;
  }
  sub node_name { 'Val_Int' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ text } }
  sub field_values { my($self)=@_; @$self{'text'} }
}
{ package IR::PackageDeclarator;
  @IR::PackageDeclarator::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$kind,$name,$block)=@_;
    my %h;
    @h{'zmatch','kind','name','block'}=($zmatch,$kind,$name,$block);
    bless \%h,$cls;
  }
  sub node_name { 'PackageDeclarator' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ kind name block } }
  sub field_values { my($self)=@_; @$self{'kind','name','block'} }
}
{ package IR::Block;
  @IR::Block::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$statements)=@_;
    my %h;
    @h{'zmatch','statements'}=($zmatch,$statements);
    bless \%h,$cls;
  }
  sub node_name { 'Block' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ statements } }
  sub field_values { my($self)=@_; @$self{'statements'} }
}
{ package IR::Quote;
  @IR::Quote::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$concat)=@_;
    my %h;
    @h{'zmatch','concat'}=($zmatch,$concat);
    bless \%h,$cls;
  }
  sub node_name { 'Quote' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ concat } }
  sub field_values { my($self)=@_; @$self{'concat'} }
}
{ package IR::Val_Bit;
  @IR::Val_Bit::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$bit)=@_;
    my %h;
    @h{'zmatch','bit'}=($zmatch,$bit);
    bless \%h,$cls;
  }
  sub node_name { 'Val_Bit' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ bit } }
  sub field_values { my($self)=@_; @$self{'bit'} }
}
{ package IR::Val_Num;
  @IR::Val_Num::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$num)=@_;
    my %h;
    @h{'zmatch','num'}=($zmatch,$num);
    bless \%h,$cls;
  }
  sub node_name { 'Val_Num' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ num } }
  sub field_values { my($self)=@_; @$self{'num'} }
}
{ package IR::Val_Buf;
  @IR::Val_Buf::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$buf)=@_;
    my %h;
    @h{'zmatch','buf'}=($zmatch,$buf);
    bless \%h,$cls;
  }
  sub node_name { 'Val_Buf' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ buf } }
  sub field_values { my($self)=@_; @$self{'buf'} }
}
{ package IR::Val_Char;
  @IR::Val_Char::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$char)=@_;
    my %h;
    @h{'zmatch','char'}=($zmatch,$char);
    bless \%h,$cls;
  }
  sub node_name { 'Val_Char' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ char } }
  sub field_values { my($self)=@_; @$self{'char'} }
}
{ package IR::Val_Undef;
  @IR::Val_Undef::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch)=@_;
    my %h;
    $h{'zmatch'}=($zmatch);
    bless \%h,$cls;
  }
  sub node_name { 'Val_Undef' }
  sub match { shift->{zmatch} }
  sub field_names { return () }
  sub field_values { my($self)=@_; return () }
}
{ package IR::Val_Object;
  @IR::Val_Object::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$clazz,$fields)=@_;
    my %h;
    @h{'zmatch','clazz','fields'}=($zmatch,$clazz,$fields);
    bless \%h,$cls;
  }
  sub node_name { 'Val_Object' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ clazz fields } }
  sub field_values { my($self)=@_; @$self{'clazz','fields'} }
}
{ package IR::Lit_Seq;
  @IR::Lit_Seq::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$seq)=@_;
    my %h;
    @h{'zmatch','seq'}=($zmatch,$seq);
    bless \%h,$cls;
  }
  sub node_name { 'Lit_Seq' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ seq } }
  sub field_values { my($self)=@_; @$self{'seq'} }
}
{ package IR::Lit_Array;
  @IR::Lit_Array::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$array)=@_;
    my %h;
    @h{'zmatch','array'}=($zmatch,$array);
    bless \%h,$cls;
  }
  sub node_name { 'Lit_Array' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ array } }
  sub field_values { my($self)=@_; @$self{'array'} }
}
{ package IR::Lit_Hash;
  @IR::Lit_Hash::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$hash)=@_;
    my %h;
    @h{'zmatch','hash'}=($zmatch,$hash);
    bless \%h,$cls;
  }
  sub node_name { 'Lit_Hash' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ hash } }
  sub field_values { my($self)=@_; @$self{'hash'} }
}
{ package IR::Lit_Pair;
  @IR::Lit_Pair::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$key,$value)=@_;
    my %h;
    @h{'zmatch','key','value'}=($zmatch,$key,$value);
    bless \%h,$cls;
  }
  sub node_name { 'Lit_Pair' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ key value } }
  sub field_values { my($self)=@_; @$self{'key','value'} }
}
{ package IR::Lit_SigArgument;
  @IR::Lit_SigArgument::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$key,$value,$type,$has_default,$is_named_only,$is_optional,$is_slurpy,$is_multidimensional,$is_rw,$is_copy)=@_;
    my %h;
    @h{'zmatch','key','value','type','has_default','is_named_only','is_optional','is_slurpy','is_multidimensional','is_rw','is_copy'}=($zmatch,$key,$value,$type,$has_default,$is_named_only,$is_optional,$is_slurpy,$is_multidimensional,$is_rw,$is_copy);
    bless \%h,$cls;
  }
  sub node_name { 'Lit_SigArgument' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ key value type has_default is_named_only is_optional is_slurpy is_multidimensional is_rw is_copy } }
  sub field_values { my($self)=@_; @$self{'key','value','type','has_default','is_named_only','is_optional','is_slurpy','is_multidimensional','is_rw','is_copy'} }
}
{ package IR::Lit_NamedArgument;
  @IR::Lit_NamedArgument::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$key,$value)=@_;
    my %h;
    @h{'zmatch','key','value'}=($zmatch,$key,$value);
    bless \%h,$cls;
  }
  sub node_name { 'Lit_NamedArgument' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ key value } }
  sub field_values { my($self)=@_; @$self{'key','value'} }
}
{ package IR::Lit_Code;
  @IR::Lit_Code::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$pad,$state,$sig,$body,$catch)=@_;
    my %h;
    @h{'zmatch','pad','state','sig','body','catch'}=($zmatch,$pad,$state,$sig,$body,$catch);
    bless \%h,$cls;
  }
  sub node_name { 'Lit_Code' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ pad state sig body catch } }
  sub field_values { my($self)=@_; @$self{'pad','state','sig','body','catch'} }
}
{ package IR::Lit_Object;
  @IR::Lit_Object::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$clazz,$fields)=@_;
    my %h;
    @h{'zmatch','clazz','fields'}=($zmatch,$clazz,$fields);
    bless \%h,$cls;
  }
  sub node_name { 'Lit_Object' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ clazz fields } }
  sub field_values { my($self)=@_; @$self{'clazz','fields'} }
}
{ package IR::Var;
  @IR::Var::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$sigil,$twigil,$name,$namespace)=@_;
    my %h;
    @h{'zmatch','sigil','twigil','name','namespace'}=($zmatch,$sigil,$twigil,$name,$namespace);
    bless \%h,$cls;
  }
  sub node_name { 'Var' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ sigil twigil name namespace } }
  sub field_values { my($self)=@_; @$self{'sigil','twigil','name','namespace'} }
}
{ package IR::Bind;
  @IR::Bind::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$parameters,$arguments)=@_;
    my %h;
    @h{'zmatch','parameters','arguments'}=($zmatch,$parameters,$arguments);
    bless \%h,$cls;
  }
  sub node_name { 'Bind' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ parameters arguments } }
  sub field_values { my($self)=@_; @$self{'parameters','arguments'} }
}
{ package IR::Assign;
  @IR::Assign::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$parameters,$arguments)=@_;
    my %h;
    @h{'zmatch','parameters','arguments'}=($zmatch,$parameters,$arguments);
    bless \%h,$cls;
  }
  sub node_name { 'Assign' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ parameters arguments } }
  sub field_values { my($self)=@_; @$self{'parameters','arguments'} }
}
{ package IR::Proto;
  @IR::Proto::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$name)=@_;
    my %h;
    @h{'zmatch','name'}=($zmatch,$name);
    bless \%h,$cls;
  }
  sub node_name { 'Proto' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ name } }
  sub field_values { my($self)=@_; @$self{'name'} }
}
{ package IR::Call;
  @IR::Call::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$invocant,$hyper,$method,$arguments)=@_;
    my %h;
    @h{'zmatch','invocant','hyper','method','arguments'}=($zmatch,$invocant,$hyper,$method,$arguments);
    bless \%h,$cls;
  }
  sub node_name { 'Call' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ invocant hyper method arguments } }
  sub field_values { my($self)=@_; @$self{'invocant','hyper','method','arguments'} }
}
{ package IR::Apply;
  @IR::Apply::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$code,$arguments)=@_;
    my %h;
    @h{'zmatch','code','arguments'}=($zmatch,$code,$arguments);
    bless \%h,$cls;
  }
  sub node_name { 'Apply' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ code arguments } }
  sub field_values { my($self)=@_; @$self{'code','arguments'} }
}
{ package IR::Return;
  @IR::Return::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$result)=@_;
    my %h;
    @h{'zmatch','result'}=($zmatch,$result);
    bless \%h,$cls;
  }
  sub node_name { 'Return' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ result } }
  sub field_values { my($self)=@_; @$self{'result'} }
}
{ package IR::If;
  @IR::If::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$cond,$body,$otherwise)=@_;
    my %h;
    @h{'zmatch','cond','body','otherwise'}=($zmatch,$cond,$body,$otherwise);
    bless \%h,$cls;
  }
  sub node_name { 'If' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ cond body otherwise } }
  sub field_values { my($self)=@_; @$self{'cond','body','otherwise'} }
}
{ package IR::While;
  @IR::While::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$cond,$body)=@_;
    my %h;
    @h{'zmatch','cond','body'}=($zmatch,$cond,$body);
    bless \%h,$cls;
  }
  sub node_name { 'While' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ cond body } }
  sub field_values { my($self)=@_; @$self{'cond','body'} }
}
{ package IR::Decl;
  @IR::Decl::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$decl,$type,$var)=@_;
    my %h;
    @h{'zmatch','decl','type','var'}=($zmatch,$decl,$type,$var);
    bless \%h,$cls;
  }
  sub node_name { 'Decl' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ decl type var } }
  sub field_values { my($self)=@_; @$self{'decl','type','var'} }
}
{ package IR::Sig;
  @IR::Sig::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$invocant,$positional)=@_;
    my %h;
    @h{'zmatch','invocant','positional'}=($zmatch,$invocant,$positional);
    bless \%h,$cls;
  }
  sub node_name { 'Sig' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ invocant positional } }
  sub field_values { my($self)=@_; @$self{'invocant','positional'} }
}
{ package IR::Lit_Capture;
  @IR::Lit_Capture::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$invocant,$array,$hash)=@_;
    my %h;
    @h{'zmatch','invocant','array','hash'}=($zmatch,$invocant,$array,$hash);
    bless \%h,$cls;
  }
  sub node_name { 'Lit_Capture' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ invocant array hash } }
  sub field_values { my($self)=@_; @$self{'invocant','array','hash'} }
}
{ package IR::Lit_Subset;
  @IR::Lit_Subset::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$name,$base_class,$block)=@_;
    my %h;
    @h{'zmatch','name','base_class','block'}=($zmatch,$name,$base_class,$block);
    bless \%h,$cls;
  }
  sub node_name { 'Lit_Subset' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ name base_class block } }
  sub field_values { my($self)=@_; @$self{'name','base_class','block'} }
}
{ package IR::Method;
  @IR::Method::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$name,$sig,$block)=@_;
    my %h;
    @h{'zmatch','name','sig','block'}=($zmatch,$name,$sig,$block);
    bless \%h,$cls;
  }
  sub node_name { 'Method' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ name sig block } }
  sub field_values { my($self)=@_; @$self{'name','sig','block'} }
}
{ package IR::Sub;
  @IR::Sub::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$name,$sig,$block)=@_;
    my %h;
    @h{'zmatch','name','sig','block'}=($zmatch,$name,$sig,$block);
    bless \%h,$cls;
  }
  sub node_name { 'Sub' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ name sig block } }
  sub field_values { my($self)=@_; @$self{'name','sig','block'} }
}
{ package IR::Macro;
  @IR::Macro::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$name,$sig,$block)=@_;
    my %h;
    @h{'zmatch','name','sig','block'}=($zmatch,$name,$sig,$block);
    bless \%h,$cls;
  }
  sub node_name { 'Macro' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ name sig block } }
  sub field_values { my($self)=@_; @$self{'name','sig','block'} }
}
{ package IR::Coro;
  @IR::Coro::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$name,$sig,$block)=@_;
    my %h;
    @h{'zmatch','name','sig','block'}=($zmatch,$name,$sig,$block);
    bless \%h,$cls;
  }
  sub node_name { 'Coro' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ name sig block } }
  sub field_values { my($self)=@_; @$self{'name','sig','block'} }
}
{ package IR::P5Token;
  @IR::P5Token::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$regex)=@_;
    my %h;
    @h{'zmatch','regex'}=($zmatch,$regex);
    bless \%h,$cls;
  }
  sub node_name { 'P5Token' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ regex } }
  sub field_values { my($self)=@_; @$self{'regex'} }
}
{ package IR::Token;
  @IR::Token::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$name,$regex,$sym)=@_;
    my %h;
    @h{'zmatch','name','regex','sym'}=($zmatch,$name,$regex,$sym);
    bless \%h,$cls;
  }
  sub node_name { 'Token' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ name regex sym } }
  sub field_values { my($self)=@_; @$self{'name','regex','sym'} }
}
{ package IR::Do;
  @IR::Do::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$block)=@_;
    my %h;
    @h{'zmatch','block'}=($zmatch,$block);
    bless \%h,$cls;
  }
  sub node_name { 'Do' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ block } }
  sub field_values { my($self)=@_; @$self{'block'} }
}
{ package IR::Begin;
  @IR::Begin::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$block)=@_;
    my %h;
    @h{'zmatch','block'}=($zmatch,$block);
    bless \%h,$cls;
  }
  sub node_name { 'Begin' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ block } }
  sub field_values { my($self)=@_; @$self{'block'} }
}
{ package IR::Use;
  @IR::Use::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$mod,$perl5)=@_;
    my %h;
    @h{'zmatch','mod','perl5'}=($zmatch,$mod,$perl5);
    bless \%h,$cls;
  }
  sub node_name { 'Use' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ mod perl5 } }
  sub field_values { my($self)=@_; @$self{'mod','perl5'} }
}
{ package IR::Rule;
  @IR::Rule::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch)=@_;
    my %h;
    $h{'zmatch'}=($zmatch);
    bless \%h,$cls;
  }
  sub node_name { 'Rule' }
  sub match { shift->{zmatch} }
  sub field_names { return () }
  sub field_values { my($self)=@_; return () }
}
{ package IR::Rule_Quantifier;
  @IR::Rule_Quantifier::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$term,$quant,$greedy,$ws1,$ws2,$ws3)=@_;
    my %h;
    @h{'zmatch','term','quant','greedy','ws1','ws2','ws3'}=($zmatch,$term,$quant,$greedy,$ws1,$ws2,$ws3);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_Quantifier' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ term quant greedy ws1 ws2 ws3 } }
  sub field_values { my($self)=@_; @$self{'term','quant','greedy','ws1','ws2','ws3'} }
}
{ package IR::Rule_Or;
  @IR::Rule_Or::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$terms)=@_;
    my %h;
    @h{'zmatch','terms'}=($zmatch,$terms);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_Or' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ terms } }
  sub field_values { my($self)=@_; @$self{'terms'} }
}
{ package IR::Rule_Concat;
  @IR::Rule_Concat::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$concat)=@_;
    my %h;
    @h{'zmatch','concat'}=($zmatch,$concat);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_Concat' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ concat } }
  sub field_values { my($self)=@_; @$self{'concat'} }
}
{ package IR::Rule_Subrule;
  @IR::Rule_Subrule::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$metasyntax,$ident,$capture_to_array)=@_;
    my %h;
    @h{'zmatch','metasyntax','ident','capture_to_array'}=($zmatch,$metasyntax,$ident,$capture_to_array);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_Subrule' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ metasyntax ident capture_to_array } }
  sub field_values { my($self)=@_; @$self{'metasyntax','ident','capture_to_array'} }
}
{ package IR::Rule_SubruleNoCapture;
  @IR::Rule_SubruleNoCapture::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$metasyntax)=@_;
    my %h;
    @h{'zmatch','metasyntax'}=($zmatch,$metasyntax);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_SubruleNoCapture' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ metasyntax } }
  sub field_values { my($self)=@_; @$self{'metasyntax'} }
}
{ package IR::Rule_Var;
  @IR::Rule_Var::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$sigil,$twigil,$name)=@_;
    my %h;
    @h{'zmatch','sigil','twigil','name'}=($zmatch,$sigil,$twigil,$name);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_Var' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ sigil twigil name } }
  sub field_values { my($self)=@_; @$self{'sigil','twigil','name'} }
}
{ package IR::Rule_Constant;
  @IR::Rule_Constant::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$constant)=@_;
    my %h;
    @h{'zmatch','constant'}=($zmatch,$constant);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_Constant' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ constant } }
  sub field_values { my($self)=@_; @$self{'constant'} }
}
{ package IR::Rule_Dot;
  @IR::Rule_Dot::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch)=@_;
    my %h;
    $h{'zmatch'}=($zmatch);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_Dot' }
  sub match { shift->{zmatch} }
  sub field_names { return () }
  sub field_values { my($self)=@_; return () }
}
{ package IR::Rule_SpecialChar;
  @IR::Rule_SpecialChar::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$char)=@_;
    my %h;
    @h{'zmatch','char'}=($zmatch,$char);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_SpecialChar' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ char } }
  sub field_values { my($self)=@_; @$self{'char'} }
}
{ package IR::Rule_Block;
  @IR::Rule_Block::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$closure)=@_;
    my %h;
    @h{'zmatch','closure'}=($zmatch,$closure);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_Block' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ closure } }
  sub field_values { my($self)=@_; @$self{'closure'} }
}
{ package IR::Rule_InterpolateVar;
  @IR::Rule_InterpolateVar::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$var)=@_;
    my %h;
    @h{'zmatch','var'}=($zmatch,$var);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_InterpolateVar' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ var } }
  sub field_values { my($self)=@_; @$self{'var'} }
}
{ package IR::Rule_NamedCapture;
  @IR::Rule_NamedCapture::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$rule,$ident,$capture_to_array)=@_;
    my %h;
    @h{'zmatch','rule','ident','capture_to_array'}=($zmatch,$rule,$ident,$capture_to_array);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_NamedCapture' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ rule ident capture_to_array } }
  sub field_values { my($self)=@_; @$self{'rule','ident','capture_to_array'} }
}
{ package IR::Rule_Before;
  @IR::Rule_Before::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$rule,$assertion_modifier,$capture_to_array)=@_;
    my %h;
    @h{'zmatch','rule','assertion_modifier','capture_to_array'}=($zmatch,$rule,$assertion_modifier,$capture_to_array);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_Before' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ rule assertion_modifier capture_to_array } }
  sub field_values { my($self)=@_; @$self{'rule','assertion_modifier','capture_to_array'} }
}
{ package IR::Rule_After;
  @IR::Rule_After::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$rule,$assertion_modifier,$capture_to_array)=@_;
    my %h;
    @h{'zmatch','rule','assertion_modifier','capture_to_array'}=($zmatch,$rule,$assertion_modifier,$capture_to_array);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_After' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ rule assertion_modifier capture_to_array } }
  sub field_values { my($self)=@_; @$self{'rule','assertion_modifier','capture_to_array'} }
}
{ package IR::Rule_NegateCharClass;
  @IR::Rule_NegateCharClass::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$chars)=@_;
    my %h;
    @h{'zmatch','chars'}=($zmatch,$chars);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_NegateCharClass' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ chars } }
  sub field_values { my($self)=@_; @$self{'chars'} }
}
{ package IR::Rule_CharClass;
  @IR::Rule_CharClass::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$chars)=@_;
    my %h;
    @h{'zmatch','chars'}=($zmatch,$chars);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_CharClass' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ chars } }
  sub field_values { my($self)=@_; @$self{'chars'} }
}
{ package IR::Rule_Capture;
  @IR::Rule_Capture::ISA = qw( IR::All );
  sub new {
    my($cls,$zmatch,$rule,$position,$capture_to_array)=@_;
    my %h;
    @h{'zmatch','rule','position','capture_to_array'}=($zmatch,$rule,$position,$capture_to_array);
    bless \%h,$cls;
  }
  sub node_name { 'Rule_Capture' }
  sub match { shift->{zmatch} }
  sub field_names { qw{ rule position capture_to_array } }
  sub field_values { my($self)=@_; @$self{'rule','position','capture_to_array'} }
}
