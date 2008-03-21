#line 1 ir_nodes.pl
# Warning: This file is mechanically written.  Your changes will be overwritten.
{ package PackageDeclarator;
  sub new {
    my($cls,$match,$kind,$name,$block)=@_;
    my %h = {};
    @h{'match','kind','name','block'}=($match,$kind,$name,$block);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ kind name block } }
  sub field_values { my($self)=@_; @$self{'kind','name','block'} }

}
{ package Block;
  sub new {
    my($cls,$match,$statements)=@_;
    my %h = {};
    @h{'match','statements'}=($match,$statements);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ statements } }
  sub field_values { my($self)=@_; @$self{'statements'} }

}
{ package Quote;
  sub new {
    my($cls,$match,$concat)=@_;
    my %h = {};
    @h{'match','concat'}=($match,$concat);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ concat } }
  sub field_values { my($self)=@_; @$self{'concat'} }

}
{ package CompUnit;
  sub new {
    my($cls,$match,$unit_type,$name,$traits,$attributes,$methods,$body)=@_;
    my %h = {};
    @h{'match','unit_type','name','traits','attributes','methods','body'}=($match,$unit_type,$name,$traits,$attributes,$methods,$body);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ unit_type name traits attributes methods body } }
  sub field_values { my($self)=@_; @$self{'unit_type','name','traits','attributes','methods','body'} }

}
{ package Val_Int;
  sub new {
    my($cls,$match,$int)=@_;
    my %h = {};
    @h{'match','int'}=($match,$int);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ int } }
  sub field_values { my($self)=@_; @$self{'int'} }

}
{ package Val_Bit;
  sub new {
    my($cls,$match,$bit)=@_;
    my %h = {};
    @h{'match','bit'}=($match,$bit);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ bit } }
  sub field_values { my($self)=@_; @$self{'bit'} }

}
{ package Val_Num;
  sub new {
    my($cls,$match,$num)=@_;
    my %h = {};
    @h{'match','num'}=($match,$num);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ num } }
  sub field_values { my($self)=@_; @$self{'num'} }

}
{ package Val_Buf;
  sub new {
    my($cls,$match,$buf)=@_;
    my %h = {};
    @h{'match','buf'}=($match,$buf);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ buf } }
  sub field_values { my($self)=@_; @$self{'buf'} }

}
{ package Val_Char;
  sub new {
    my($cls,$match,$char)=@_;
    my %h = {};
    @h{'match','char'}=($match,$char);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ char } }
  sub field_values { my($self)=@_; @$self{'char'} }

}
{ package Val_Undef;
  sub new {
    my($cls,$match)=@_;
    my %h = {};
    $h{'match'}=($match);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { return () }
  sub field_values { my($self)=@_; return () }

}
{ package Val_Object;
  sub new {
    my($cls,$match,$clazz,$fields)=@_;
    my %h = {};
    @h{'match','clazz','fields'}=($match,$clazz,$fields);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ clazz fields } }
  sub field_values { my($self)=@_; @$self{'clazz','fields'} }

}
{ package Lit_Seq;
  sub new {
    my($cls,$match,$seq)=@_;
    my %h = {};
    @h{'match','seq'}=($match,$seq);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ seq } }
  sub field_values { my($self)=@_; @$self{'seq'} }

}
{ package Lit_Array;
  sub new {
    my($cls,$match,$array)=@_;
    my %h = {};
    @h{'match','array'}=($match,$array);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ array } }
  sub field_values { my($self)=@_; @$self{'array'} }

}
{ package Lit_Hash;
  sub new {
    my($cls,$match,$hash)=@_;
    my %h = {};
    @h{'match','hash'}=($match,$hash);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ hash } }
  sub field_values { my($self)=@_; @$self{'hash'} }

}
{ package Lit_Pair;
  sub new {
    my($cls,$match,$key,$value)=@_;
    my %h = {};
    @h{'match','key','value'}=($match,$key,$value);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ key value } }
  sub field_values { my($self)=@_; @$self{'key','value'} }

}
{ package Lit_SigArgument;
  sub new {
    my($cls,$match,$key,$value,$type,$has_default,$is_named_only,$is_optional,$is_slurpy,$is_multidimensional,$is_rw,$is_copy)=@_;
    my %h = {};
    @h{'match','key','value','type','has_default','is_named_only','is_optional','is_slurpy','is_multidimensional','is_rw','is_copy'}=($match,$key,$value,$type,$has_default,$is_named_only,$is_optional,$is_slurpy,$is_multidimensional,$is_rw,$is_copy);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ key value type has_default is_named_only is_optional is_slurpy is_multidimensional is_rw is_copy } }
  sub field_values { my($self)=@_; @$self{'key','value','type','has_default','is_named_only','is_optional','is_slurpy','is_multidimensional','is_rw','is_copy'} }

}
{ package Lit_NamedArgument;
  sub new {
    my($cls,$match,$key,$value)=@_;
    my %h = {};
    @h{'match','key','value'}=($match,$key,$value);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ key value } }
  sub field_values { my($self)=@_; @$self{'key','value'} }

}
{ package Lit_Code;
  sub new {
    my($cls,$match,$pad,$state,$sig,$body,$catch)=@_;
    my %h = {};
    @h{'match','pad','state','sig','body','catch'}=($match,$pad,$state,$sig,$body,$catch);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ pad state sig body catch } }
  sub field_values { my($self)=@_; @$self{'pad','state','sig','body','catch'} }

}
{ package Lit_Object;
  sub new {
    my($cls,$match,$clazz,$fields)=@_;
    my %h = {};
    @h{'match','clazz','fields'}=($match,$clazz,$fields);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ clazz fields } }
  sub field_values { my($self)=@_; @$self{'clazz','fields'} }

}
{ package Var;
  sub new {
    my($cls,$match,$sigil,$twigil,$name,$namespace)=@_;
    my %h = {};
    @h{'match','sigil','twigil','name','namespace'}=($match,$sigil,$twigil,$name,$namespace);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ sigil twigil name namespace } }
  sub field_values { my($self)=@_; @$self{'sigil','twigil','name','namespace'} }

}
{ package Bind;
  sub new {
    my($cls,$match,$parameters,$arguments)=@_;
    my %h = {};
    @h{'match','parameters','arguments'}=($match,$parameters,$arguments);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ parameters arguments } }
  sub field_values { my($self)=@_; @$self{'parameters','arguments'} }

}
{ package Assign;
  sub new {
    my($cls,$match,$parameters,$arguments)=@_;
    my %h = {};
    @h{'match','parameters','arguments'}=($match,$parameters,$arguments);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ parameters arguments } }
  sub field_values { my($self)=@_; @$self{'parameters','arguments'} }

}
{ package Proto;
  sub new {
    my($cls,$match,$name)=@_;
    my %h = {};
    @h{'match','name'}=($match,$name);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ name } }
  sub field_values { my($self)=@_; @$self{'name'} }

}
{ package Call;
  sub new {
    my($cls,$match,$invocant,$hyper,$method,$arguments)=@_;
    my %h = {};
    @h{'match','invocant','hyper','method','arguments'}=($match,$invocant,$hyper,$method,$arguments);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ invocant hyper method arguments } }
  sub field_values { my($self)=@_; @$self{'invocant','hyper','method','arguments'} }

}
{ package Apply;
  sub new {
    my($cls,$match,$code,$arguments)=@_;
    my %h = {};
    @h{'match','code','arguments'}=($match,$code,$arguments);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ code arguments } }
  sub field_values { my($self)=@_; @$self{'code','arguments'} }

}
{ package Return;
  sub new {
    my($cls,$match,$result)=@_;
    my %h = {};
    @h{'match','result'}=($match,$result);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ result } }
  sub field_values { my($self)=@_; @$self{'result'} }

}
{ package If;
  sub new {
    my($cls,$match,$cond,$body,$otherwise)=@_;
    my %h = {};
    @h{'match','cond','body','otherwise'}=($match,$cond,$body,$otherwise);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ cond body otherwise } }
  sub field_values { my($self)=@_; @$self{'cond','body','otherwise'} }

}
{ package While;
  sub new {
    my($cls,$match,$cond,$body)=@_;
    my %h = {};
    @h{'match','cond','body'}=($match,$cond,$body);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ cond body } }
  sub field_values { my($self)=@_; @$self{'cond','body'} }

}
{ package Decl;
  sub new {
    my($cls,$match,$decl,$type,$var)=@_;
    my %h = {};
    @h{'match','decl','type','var'}=($match,$decl,$type,$var);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ decl type var } }
  sub field_values { my($self)=@_; @$self{'decl','type','var'} }

}
{ package Sig;
  sub new {
    my($cls,$match,$invocant,$positional)=@_;
    my %h = {};
    @h{'match','invocant','positional'}=($match,$invocant,$positional);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ invocant positional } }
  sub field_values { my($self)=@_; @$self{'invocant','positional'} }

}
{ package Lit_Capture;
  sub new {
    my($cls,$match,$invocant,$array,$hash)=@_;
    my %h = {};
    @h{'match','invocant','array','hash'}=($match,$invocant,$array,$hash);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ invocant array hash } }
  sub field_values { my($self)=@_; @$self{'invocant','array','hash'} }

}
{ package Lit_Subset;
  sub new {
    my($cls,$match,$name,$base_class,$block)=@_;
    my %h = {};
    @h{'match','name','base_class','block'}=($match,$name,$base_class,$block);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ name base_class block } }
  sub field_values { my($self)=@_; @$self{'name','base_class','block'} }

}
{ package Method;
  sub new {
    my($cls,$match,$name,$sig,$block)=@_;
    my %h = {};
    @h{'match','name','sig','block'}=($match,$name,$sig,$block);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ name sig block } }
  sub field_values { my($self)=@_; @$self{'name','sig','block'} }

}
{ package Sub;
  sub new {
    my($cls,$match,$name,$sig,$block)=@_;
    my %h = {};
    @h{'match','name','sig','block'}=($match,$name,$sig,$block);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ name sig block } }
  sub field_values { my($self)=@_; @$self{'name','sig','block'} }

}
{ package Macro;
  sub new {
    my($cls,$match,$name,$sig,$block)=@_;
    my %h = {};
    @h{'match','name','sig','block'}=($match,$name,$sig,$block);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ name sig block } }
  sub field_values { my($self)=@_; @$self{'name','sig','block'} }

}
{ package Coro;
  sub new {
    my($cls,$match,$name,$sig,$block)=@_;
    my %h = {};
    @h{'match','name','sig','block'}=($match,$name,$sig,$block);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ name sig block } }
  sub field_values { my($self)=@_; @$self{'name','sig','block'} }

}
{ package P5Token;
  sub new {
    my($cls,$match,$regex)=@_;
    my %h = {};
    @h{'match','regex'}=($match,$regex);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ regex } }
  sub field_values { my($self)=@_; @$self{'regex'} }

}
{ package Token;
  sub new {
    my($cls,$match,$name,$regex,$sym)=@_;
    my %h = {};
    @h{'match','name','regex','sym'}=($match,$name,$regex,$sym);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ name regex sym } }
  sub field_values { my($self)=@_; @$self{'name','regex','sym'} }

}
{ package Do;
  sub new {
    my($cls,$match,$block)=@_;
    my %h = {};
    @h{'match','block'}=($match,$block);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ block } }
  sub field_values { my($self)=@_; @$self{'block'} }

}
{ package Begin;
  sub new {
    my($cls,$match,$block)=@_;
    my %h = {};
    @h{'match','block'}=($match,$block);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ block } }
  sub field_values { my($self)=@_; @$self{'block'} }

}
{ package Use;
  sub new {
    my($cls,$match,$mod,$perl5)=@_;
    my %h = {};
    @h{'match','mod','perl5'}=($match,$mod,$perl5);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ mod perl5 } }
  sub field_values { my($self)=@_; @$self{'mod','perl5'} }

}
{ package Rule;
  sub new {
    my($cls,$match)=@_;
    my %h = {};
    $h{'match'}=($match);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { return () }
  sub field_values { my($self)=@_; return () }

}
{ package Rule_Quantifier;
  sub new {
    my($cls,$match,$term,$quant,$greedy,$ws1,$ws2,$ws3)=@_;
    my %h = {};
    @h{'match','term','quant','greedy','ws1','ws2','ws3'}=($match,$term,$quant,$greedy,$ws1,$ws2,$ws3);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ term quant greedy ws1 ws2 ws3 } }
  sub field_values { my($self)=@_; @$self{'term','quant','greedy','ws1','ws2','ws3'} }

}
{ package Rule_Or;
  sub new {
    my($cls,$match,$terms)=@_;
    my %h = {};
    @h{'match','terms'}=($match,$terms);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ terms } }
  sub field_values { my($self)=@_; @$self{'terms'} }

}
{ package Rule_Concat;
  sub new {
    my($cls,$match,$concat)=@_;
    my %h = {};
    @h{'match','concat'}=($match,$concat);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ concat } }
  sub field_values { my($self)=@_; @$self{'concat'} }

}
{ package Rule_Subrule;
  sub new {
    my($cls,$match,$metasyntax,$ident,$capture_to_array)=@_;
    my %h = {};
    @h{'match','metasyntax','ident','capture_to_array'}=($match,$metasyntax,$ident,$capture_to_array);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ metasyntax ident capture_to_array } }
  sub field_values { my($self)=@_; @$self{'metasyntax','ident','capture_to_array'} }

}
{ package Rule_SubruleNoCapture;
  sub new {
    my($cls,$match,$metasyntax)=@_;
    my %h = {};
    @h{'match','metasyntax'}=($match,$metasyntax);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ metasyntax } }
  sub field_values { my($self)=@_; @$self{'metasyntax'} }

}
{ package Rule_Var;
  sub new {
    my($cls,$match,$sigil,$twigil,$name)=@_;
    my %h = {};
    @h{'match','sigil','twigil','name'}=($match,$sigil,$twigil,$name);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ sigil twigil name } }
  sub field_values { my($self)=@_; @$self{'sigil','twigil','name'} }

}
{ package Rule_Constant;
  sub new {
    my($cls,$match,$constant)=@_;
    my %h = {};
    @h{'match','constant'}=($match,$constant);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ constant } }
  sub field_values { my($self)=@_; @$self{'constant'} }

}
{ package Rule_Dot;
  sub new {
    my($cls,$match)=@_;
    my %h = {};
    $h{'match'}=($match);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { return () }
  sub field_values { my($self)=@_; return () }

}
{ package Rule_SpecialChar;
  sub new {
    my($cls,$match,$char)=@_;
    my %h = {};
    @h{'match','char'}=($match,$char);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ char } }
  sub field_values { my($self)=@_; @$self{'char'} }

}
{ package Rule_Block;
  sub new {
    my($cls,$match,$closure)=@_;
    my %h = {};
    @h{'match','closure'}=($match,$closure);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ closure } }
  sub field_values { my($self)=@_; @$self{'closure'} }

}
{ package Rule_InterpolateVar;
  sub new {
    my($cls,$match,$var)=@_;
    my %h = {};
    @h{'match','var'}=($match,$var);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ var } }
  sub field_values { my($self)=@_; @$self{'var'} }

}
{ package Rule_NamedCapture;
  sub new {
    my($cls,$match,$rule,$ident,$capture_to_array)=@_;
    my %h = {};
    @h{'match','rule','ident','capture_to_array'}=($match,$rule,$ident,$capture_to_array);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ rule ident capture_to_array } }
  sub field_values { my($self)=@_; @$self{'rule','ident','capture_to_array'} }

}
{ package Rule_Before;
  sub new {
    my($cls,$match,$rule,$assertion_modifier,$capture_to_array)=@_;
    my %h = {};
    @h{'match','rule','assertion_modifier','capture_to_array'}=($match,$rule,$assertion_modifier,$capture_to_array);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ rule assertion_modifier capture_to_array } }
  sub field_values { my($self)=@_; @$self{'rule','assertion_modifier','capture_to_array'} }

}
{ package Rule_After;
  sub new {
    my($cls,$match,$rule,$assertion_modifier,$capture_to_array)=@_;
    my %h = {};
    @h{'match','rule','assertion_modifier','capture_to_array'}=($match,$rule,$assertion_modifier,$capture_to_array);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ rule assertion_modifier capture_to_array } }
  sub field_values { my($self)=@_; @$self{'rule','assertion_modifier','capture_to_array'} }

}
{ package Rule_NegateCharClass;
  sub new {
    my($cls,$match,$chars)=@_;
    my %h = {};
    @h{'match','chars'}=($match,$chars);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ chars } }
  sub field_values { my($self)=@_; @$self{'chars'} }

}
{ package Rule_CharClass;
  sub new {
    my($cls,$match,$chars)=@_;
    my %h = {};
    @h{'match','chars'}=($match,$chars);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ chars } }
  sub field_values { my($self)=@_; @$self{'chars'} }

}
{ package Rule_Capture;
  sub new {
    my($cls,$match,$rule,$position,$capture_to_array)=@_;
    my %h = {};
    @h{'match','rule','position','capture_to_array'}=($match,$rule,$position,$capture_to_array);
    bless \%h,$cls;
  }
  sub match { shift->{match} }
  sub field_names { qw{ rule position capture_to_array } }
  sub field_values { my($self)=@_; @$self{'rule','position','capture_to_array'} }

}
