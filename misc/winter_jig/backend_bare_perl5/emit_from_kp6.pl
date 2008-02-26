# WARNING
# This file is written by ./emit_from_kp6_pl_generate.
# YOUR EDITS TO THIS FILE WILL BE LOST.


package BackendBarePerl5;
use Class::Rebless;
use strict;
use warnings;

sub emit_ast {
    my($cls,$ast)=@_;
    Class::Rebless->rebase($ast,'BackendBarePerl5::Ast');
    $cls->new()->emit($ast);
}
use SimpleDispatchOnTypeSuffix;

sub new {
    my($cls)=@_;
    my $self = {
	dispatch => {}
    };
    bless $self,$cls;
    $self->initialize();
    $self;
}

sub emit {
    my($self,$obj,@rest)=@_;
    my $handler = $self->{dispatch}{emit}->lookup($obj);
    die "No handler for $obj" if not $handler;
    my $result = $handler->($self,$obj,@rest);
    $result;
}
sub initialize {
    my($self)=@_;
    $self->{dispatch}{emit} = SimpleDispatchOnTypeSuffix->new();
    $self->{dispatch}{emit}->dispatch_type_to('::CompUnit',\&emit_CompUnit);
    $self->{dispatch}{emit}->dispatch_type_to('::Val::Int',\&emit_Val_Int);
    $self->{dispatch}{emit}->dispatch_type_to('::Val::Bit',\&emit_Val_Bit);
    $self->{dispatch}{emit}->dispatch_type_to('::Val::Num',\&emit_Val_Num);
    $self->{dispatch}{emit}->dispatch_type_to('::Val::Buf',\&emit_Val_Buf);
    $self->{dispatch}{emit}->dispatch_type_to('::Val::Char',\&emit_Val_Char);
    $self->{dispatch}{emit}->dispatch_type_to('::Val::Undef',\&emit_Val_Undef);
    $self->{dispatch}{emit}->dispatch_type_to('::Val::Object',\&emit_Val_Object);
    $self->{dispatch}{emit}->dispatch_type_to('::Lit::Seq',\&emit_Lit_Seq);
    $self->{dispatch}{emit}->dispatch_type_to('::Lit::Array',\&emit_Lit_Array);
    $self->{dispatch}{emit}->dispatch_type_to('::Lit::Hash',\&emit_Lit_Hash);
    $self->{dispatch}{emit}->dispatch_type_to('::Lit::Pair',\&emit_Lit_Pair);
    $self->{dispatch}{emit}->dispatch_type_to('::Lit::SigArgument',\&emit_Lit_SigArgument);
    $self->{dispatch}{emit}->dispatch_type_to('::Lit::NamedArgument',\&emit_Lit_NamedArgument);
    $self->{dispatch}{emit}->dispatch_type_to('::Lit::Code',\&emit_Lit_Code);
    $self->{dispatch}{emit}->dispatch_type_to('::Lit::Object',\&emit_Lit_Object);
    $self->{dispatch}{emit}->dispatch_type_to('::Var',\&emit_Var);
    $self->{dispatch}{emit}->dispatch_type_to('::Bind',\&emit_Bind);
    $self->{dispatch}{emit}->dispatch_type_to('::Assign',\&emit_Assign);
    $self->{dispatch}{emit}->dispatch_type_to('::Proto',\&emit_Proto);
    $self->{dispatch}{emit}->dispatch_type_to('::Call',\&emit_Call);
    $self->{dispatch}{emit}->dispatch_type_to('::Apply',\&emit_Apply);
    $self->{dispatch}{emit}->dispatch_type_to('::Return',\&emit_Return);
    $self->{dispatch}{emit}->dispatch_type_to('::If',\&emit_If);
    $self->{dispatch}{emit}->dispatch_type_to('::While',\&emit_While);
    $self->{dispatch}{emit}->dispatch_type_to('::Decl',\&emit_Decl);
    $self->{dispatch}{emit}->dispatch_type_to('::Sig',\&emit_Sig);
    $self->{dispatch}{emit}->dispatch_type_to('::Lit::Capture',\&emit_Lit_Capture);
    $self->{dispatch}{emit}->dispatch_type_to('::Lit::Subset',\&emit_Lit_Subset);
    $self->{dispatch}{emit}->dispatch_type_to('::Method',\&emit_Method);
    $self->{dispatch}{emit}->dispatch_type_to('::Sub',\&emit_Sub);
    $self->{dispatch}{emit}->dispatch_type_to('::Macro',\&emit_Macro);
    $self->{dispatch}{emit}->dispatch_type_to('::Coro',\&emit_Coro);
    $self->{dispatch}{emit}->dispatch_type_to('::P5Token',\&emit_P5Token);
    $self->{dispatch}{emit}->dispatch_type_to('::Token',\&emit_Token);
    $self->{dispatch}{emit}->dispatch_type_to('::Do',\&emit_Do);
    $self->{dispatch}{emit}->dispatch_type_to('::BEGIN',\&emit_BEGIN);
    $self->{dispatch}{emit}->dispatch_type_to('::Use',\&emit_Use);
    $self->{dispatch}{emit}->dispatch_type_to('::Rule',\&emit_Rule);
    $self->{dispatch}{emit}->dispatch_type_to('::Rule::Quantifier',\&emit_Rule_Quantifier);
    $self->{dispatch}{emit}->dispatch_type_to('::Rule::Or',\&emit_Rule_Or);
    $self->{dispatch}{emit}->dispatch_type_to('::Rule::Concat',\&emit_Rule_Concat);
    $self->{dispatch}{emit}->dispatch_type_to('::Rule::Subrule',\&emit_Rule_Subrule);
    $self->{dispatch}{emit}->dispatch_type_to('::Rule::SubruleNoCapture',\&emit_Rule_SubruleNoCapture);
    $self->{dispatch}{emit}->dispatch_type_to('::Rule::Var',\&emit_Rule_Var);
    $self->{dispatch}{emit}->dispatch_type_to('::Rule::Constant',\&emit_Rule_Constant);
    $self->{dispatch}{emit}->dispatch_type_to('::Rule::Dot',\&emit_Rule_Dot);
    $self->{dispatch}{emit}->dispatch_type_to('::Rule::SpecialChar',\&emit_Rule_SpecialChar);
    $self->{dispatch}{emit}->dispatch_type_to('::Rule::Block',\&emit_Rule_Block);
    $self->{dispatch}{emit}->dispatch_type_to('::Rule::InterpolateVar',\&emit_Rule_InterpolateVar);
    $self->{dispatch}{emit}->dispatch_type_to('::Rule::NamedCapture',\&emit_Rule_NamedCapture);
    $self->{dispatch}{emit}->dispatch_type_to('::Rule::Before',\&emit_Rule_Before);
    $self->{dispatch}{emit}->dispatch_type_to('::Rule::After',\&emit_Rule_After);
    $self->{dispatch}{emit}->dispatch_type_to('::Rule::NegateCharClass',\&emit_Rule_NegateCharClass);
    $self->{dispatch}{emit}->dispatch_type_to('::Rule::CharClass',\&emit_Rule_CharClass);
    $self->{dispatch}{emit}->dispatch_type_to('::Rule::Capture',\&emit_Rule_Capture);
}

sub emit_CompUnit {
    my($self,$node)=@_;
    my($unit_type,$name,$traits,$attributes,$methods,$body)=($node->{unit_type},$node->{name},$node->{traits},$node->{attributes},$node->{methods},$node->{body});
    # $unit_type $name $traits $attributes $methods $body 
    $self->emit($body);
}
sub emit_Val_Int {
    my($self,$node)=@_;
    my($int)=($node->{int});
    # $int 
    $int;
}
sub emit_Val_Bit {
    my($self,$node)=@_;
    my($bit)=($node->{bit});
    # $bit 
    $bit;
}
sub emit_Val_Num {
    my($self,$node)=@_;
    my($num)=($node->{num});
    # $num 
    $num;
}
sub emit_Val_Buf {
    my($self,$node)=@_;
    my($buf)=($node->{buf});
    # $buf 
    $buf;
}
sub emit_Val_Char {
    my($self,$node)=@_;
    my($char)=($node->{char});
    # $char 
    "chr($char)";
}
sub emit_Val_Undef {
    my($self,$node)=@_;
    # 
    'undef';
}
sub emit_Val_Object {
    my($self,$node)=@_;
    my($class,$fields)=($node->{class},$node->{fields});
    # $class $fields 
    die "Even kp6 Emit/Perl5 doesn't implement $node";
}
sub emit_Lit_Seq {
    my($self,$node)=@_;
    my($seq)=($node->{seq});
    # $seq 
    '('.join(', ',map{$self->emit($_)} @{$seq}).')';
}
sub emit_Lit_Array {
    my($self,$node)=@_;
    my($array)=($node->{array});
    # $array 
    '['.join(', ',map{$self->emit($_)} @{$array}).']';
}
sub emit_Lit_Hash {
    my($self,$node)=@_;
    my($hash)=($node->{hash});
    # $hash 
    '{'.join(', ',map{$self->emit($_)} @{$hash}).'}';
}
sub emit_Lit_Pair {
    my($self,$node)=@_;
    my($key,$value)=($node->{key},$node->{value});
    # $key $value 
    "[$key,$value]";
}
sub emit_Lit_SigArgument {
    my($self,$node)=@_;
    my($key,$value,$type,$has_default,$is_named_only,$is_optional,$is_slurpy,$is_multidimensional,$is_rw,$is_copy)=($node->{key},$node->{value},$node->{type},$node->{has_default},$node->{is_named_only},$node->{is_optional},$node->{is_slurpy},$node->{is_multidimensional},$node->{is_rw},$node->{is_copy});
    # $key $value $type $has_default $is_named_only $is_optional $is_slurpy $is_multidimensional $is_rw $is_copy 
    
}
sub emit_Lit_NamedArgument {
    my($self,$node)=@_;
    my($key,$value)=($node->{key},$node->{value});
    # $key $value 

}
sub emit_Lit_Code {
    my($self,$node)=@_;
    my($pad,$state,$sig,$body,$CATCH)=($node->{pad},$node->{state},$node->{sig},$node->{body},$node->{CATCH});
    # $pad $state $sig $body $CATCH 
    join(";\n",map{$self->emit($_)} @{$body})
}
sub emit_Lit_Object {
    my($self,$node)=@_;
    my($class,$fields)=($node->{class},$node->{fields});
    # $class $fields 

}
sub emit_Var {
    my($self,$node)=@_;
    my($sigil,$twigil,$name,$namespace)=($node->{sigil},$node->{twigil},$node->{name},$node->{namespace});
    # $sigil $twigil $name $namespace 
    $sigil ne '&' ? '$'.$name : $name;
}
sub emit_Bind {
    my($self,$node)=@_;
    my($parameters,$arguments)=($node->{parameters},$node->{arguments});
    # $parameters $arguments 
    ('('.join(',',map{$self->emit($_)} @{$parameters}).')'.
     ' = '.
     '('.join(',',map{$self->emit($_)} @{$arguments}).')'.
     ';');
}
sub emit_Assign {
    my($self,$node)=@_;
    my($parameters,$arguments)=($node->{parameters},$node->{arguments});
    # $parameters $arguments 
    ('('.join(',',map{$self->emit($_)} @{$parameters}).')'.
     ' = '.
     '('.join(',',map{$self->emit($_)} @{$arguments}).')'.
     ';');
}
sub emit_Proto {
    my($self,$node)=@_;
    my($name)=($node->{name});
    # $name 
    die;
}
sub emit_Call {
    my($self,$node)=@_;
    my($invocant,$hyper,$method,$arguments)=($node->{invocant},$node->{hyper},$node->{method},$node->{arguments});
    # $invocant $hyper $method $arguments 
    ($self->emit($invocant).'->'.$method.
     '('.join(',',map{$self->emit($_)} @{$arguments}).')'.
     '');
}
sub emit_Apply {
    my($self,$node)=@_;
    my($code,$arguments)=($node->{code},$node->{arguments});
    # $code $arguments 
    my $f = $self->emit($code);
    ($f.($f =~ /^[\w:]+$/ ? "" : '->').
     '('.join(',',map{$self->emit($_)} @{$arguments}).')'.
     '');
}
sub emit_Return {
    my($self,$node)=@_;
    my($result)=($node->{result});
    # $result 
    'return('.$self->emit($result).');'
}
sub emit_If {
    my($self,$node)=@_;
    my($cond,$body,$otherwise)=($node->{cond},$node->{body},$node->{otherwise});
    # $cond $body $otherwise 
    ('if('.$self->emit($cond).") {\n".
     $self->emit($body).
     "\n}\nelse {\n".
     ($otherwise ? $self->emit($otherwise) : "").
     "\n}\n");
}
sub emit_While {
    my($self,$node)=@_;
    my($cond,$body)=($node->{cond},$node->{body});
    # $cond $body 
    ('while('.$self->emit($cond).") {\n".
     $self->emit($body).
     "\n}\n");
}
sub emit_Decl {
    my($self,$node)=@_;
    my($decl,$type,$var)=($node->{decl},$node->{type},$node->{var});
    # $decl $type $var 
    my $var_sigil = $var->{sigil};
    my $setup = {
	'$' => '',
	'@' => '[]',
	'%' => '{}',
    }->{$var_sigil} || '';
    $decl.' '.$self->emit($var).$setup.";";
}
sub emit_Sig {
    my($self,$node)=@_;
    my($invocant,$positional)=($node->{invocant},$node->{positional});
    # $invocant $positional 
    
}
sub emit_Lit_Capture {
    my($self,$node)=@_;
    my($invocant,$array,$hash)=($node->{invocant},$node->{array},$node->{hash});
    # $invocant $array $hash 

}
sub emit_Lit_Subset {
    my($self,$node)=@_;
    my($name,$base_class,$block)=($node->{name},$node->{base_class},$node->{block});
    # $name $base_class $block 

}
sub emit_Method {
    my($self,$node)=@_;
    my($name,$block)=($node->{name},$node->{block});
    # $name $block 
    'sub '.$name.' '.$self->emit($block);
}
sub emit_Sub {
    my($self,$node)=@_;
    my($name,$block)=($node->{name},$node->{block});
    # $name $block 
    'sub '.$name.' '.$self->emit($block);
}
sub emit_Macro {
    my($self,$node)=@_;
    my($name,$block)=($node->{name},$node->{block});
    # $name $block 
    die;
}
sub emit_Coro {
    my($self,$node)=@_;
    my($name,$block)=($node->{name},$node->{block});
    # $name $block 
    die;
}
sub emit_P5Token {
    my($self,$node)=@_;
    my($regex)=($node->{regex});
    # $regex 

}
sub emit_Token {
    my($self,$node)=@_;
    my($name,$regex,$sym)=($node->{name},$node->{regex},$node->{sym});
    # $name $regex $sym 

}
sub emit_Do {
    my($self,$node)=@_;
    my($block)=($node->{block});
    # $block 
    ("do{\n".$self->emit($block)."\n}");
}
sub emit_BEGIN {
    my($self,$node)=@_;
    my($block)=($node->{block});
    # $block 
    ("INIT{\n".$self->emit($block)."\n}\n");
}
sub emit_Use {
    my($self,$node)=@_;
    my($mod,$perl5)=($node->{mod},$node->{perl5});
    # $mod $perl5 
    ($mod =~ /^v6/) ? "# use $mod\n" : "use $mod\n";
}
sub emit_Rule {
    my($self,$node)=@_;
    # 

}
sub emit_Rule_Quantifier {
    my($self,$node)=@_;
    my($term,$quant,$greedy,$ws1,$ws2,$ws3)=($node->{term},$node->{quant},$node->{greedy},$node->{ws1},$node->{ws2},$node->{ws3});
    # $term $quant $greedy $ws1 $ws2 $ws3 

}
sub emit_Rule_Or {
    my($self,$node)=@_;
    my($or)=($node->{or});
    # $or 

}
sub emit_Rule_Concat {
    my($self,$node)=@_;
    my($concat)=($node->{concat});
    # $concat 

}
sub emit_Rule_Subrule {
    my($self,$node)=@_;
    my($metasyntax,$ident,$capture_to_array)=($node->{metasyntax},$node->{ident},$node->{capture_to_array});
    # $metasyntax $ident $capture_to_array 

}
sub emit_Rule_SubruleNoCapture {
    my($self,$node)=@_;
    my($metasyntax)=($node->{metasyntax});
    # $metasyntax 

}
sub emit_Rule_Var {
    my($self,$node)=@_;
    my($sigil,$twigil,$name)=($node->{sigil},$node->{twigil},$node->{name});
    # $sigil $twigil $name 

}
sub emit_Rule_Constant {
    my($self,$node)=@_;
    my($constant)=($node->{constant});
    # $constant 

}
sub emit_Rule_Dot {
    my($self,$node)=@_;
    # 
    '.';
}
sub emit_Rule_SpecialChar {
    my($self,$node)=@_;
    my($char)=($node->{char});
    # $char 

}
sub emit_Rule_Block {
    my($self,$node)=@_;
    my($closure)=($node->{closure});
    # $closure 

}
sub emit_Rule_InterpolateVar {
    my($self,$node)=@_;
    my($var)=($node->{var});
    # $var 

}
sub emit_Rule_NamedCapture {
    my($self,$node)=@_;
    my($rule,$ident,$capture_to_array)=($node->{rule},$node->{ident},$node->{capture_to_array});
    # $rule $ident $capture_to_array 

}
sub emit_Rule_Before {
    my($self,$node)=@_;
    my($rule,$assertion_modifier,$capture_to_array)=($node->{rule},$node->{assertion_modifier},$node->{capture_to_array});
    # $rule $assertion_modifier $capture_to_array 

}
sub emit_Rule_After {
    my($self,$node)=@_;
    my($rule,$assertion_modifier,$capture_to_array)=($node->{rule},$node->{assertion_modifier},$node->{capture_to_array});
    # $rule $assertion_modifier $capture_to_array 

}
sub emit_Rule_NegateCharClass {
    my($self,$node)=@_;
    my($chars)=($node->{chars});
    # $chars 

}
sub emit_Rule_CharClass {
    my($self,$node)=@_;
    my($chars)=($node->{chars});
    # $chars 

}
sub emit_Rule_Capture {
    my($self,$node)=@_;
    my($rule,$position,$capture_to_array)=($node->{rule},$node->{position},$node->{capture_to_array});
    # $rule $position $capture_to_array 

}
{package BackendBarePerl5::Ast::AllNodes; }
{package BackendBarePerl5::Ast::CompUnit; BEGIN{ push(@BackendBarePerl5::Ast::CompUnit::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Val::Int; BEGIN{ push(@BackendBarePerl5::Ast::Val::Int::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Val::Bit; BEGIN{ push(@BackendBarePerl5::Ast::Val::Bit::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Val::Num; BEGIN{ push(@BackendBarePerl5::Ast::Val::Num::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Val::Buf; BEGIN{ push(@BackendBarePerl5::Ast::Val::Buf::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Val::Char; BEGIN{ push(@BackendBarePerl5::Ast::Val::Char::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Val::Undef; BEGIN{ push(@BackendBarePerl5::Ast::Val::Undef::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Val::Object; BEGIN{ push(@BackendBarePerl5::Ast::Val::Object::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Lit::Seq; BEGIN{ push(@BackendBarePerl5::Ast::Lit::Seq::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Lit::Array; BEGIN{ push(@BackendBarePerl5::Ast::Lit::Array::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Lit::Hash; BEGIN{ push(@BackendBarePerl5::Ast::Lit::Hash::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Lit::Pair; BEGIN{ push(@BackendBarePerl5::Ast::Lit::Pair::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Lit::SigArgument; BEGIN{ push(@BackendBarePerl5::Ast::Lit::SigArgument::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Lit::NamedArgument; BEGIN{ push(@BackendBarePerl5::Ast::Lit::NamedArgument::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Lit::Code; BEGIN{ push(@BackendBarePerl5::Ast::Lit::Code::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Lit::Object; BEGIN{ push(@BackendBarePerl5::Ast::Lit::Object::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Var; BEGIN{ push(@BackendBarePerl5::Ast::Var::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Bind; BEGIN{ push(@BackendBarePerl5::Ast::Bind::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Assign; BEGIN{ push(@BackendBarePerl5::Ast::Assign::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Proto; BEGIN{ push(@BackendBarePerl5::Ast::Proto::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Call; BEGIN{ push(@BackendBarePerl5::Ast::Call::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Apply; BEGIN{ push(@BackendBarePerl5::Ast::Apply::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Return; BEGIN{ push(@BackendBarePerl5::Ast::Return::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::If; BEGIN{ push(@BackendBarePerl5::Ast::If::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::While; BEGIN{ push(@BackendBarePerl5::Ast::While::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Decl; BEGIN{ push(@BackendBarePerl5::Ast::Decl::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Sig; BEGIN{ push(@BackendBarePerl5::Ast::Sig::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Lit::Capture; BEGIN{ push(@BackendBarePerl5::Ast::Lit::Capture::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Lit::Subset; BEGIN{ push(@BackendBarePerl5::Ast::Lit::Subset::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Method; BEGIN{ push(@BackendBarePerl5::Ast::Method::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Sub; BEGIN{ push(@BackendBarePerl5::Ast::Sub::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Macro; BEGIN{ push(@BackendBarePerl5::Ast::Macro::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Coro; BEGIN{ push(@BackendBarePerl5::Ast::Coro::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::P5Token; BEGIN{ push(@BackendBarePerl5::Ast::P5Token::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Token; BEGIN{ push(@BackendBarePerl5::Ast::Token::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Do; BEGIN{ push(@BackendBarePerl5::Ast::Do::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::BEGIN; BEGIN{ push(@BackendBarePerl5::Ast::BEGIN::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Use; BEGIN{ push(@BackendBarePerl5::Ast::Use::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Rule; BEGIN{ push(@BackendBarePerl5::Ast::Rule::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Rule::Quantifier; BEGIN{ push(@BackendBarePerl5::Ast::Rule::Quantifier::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Rule::Or; BEGIN{ push(@BackendBarePerl5::Ast::Rule::Or::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Rule::Concat; BEGIN{ push(@BackendBarePerl5::Ast::Rule::Concat::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Rule::Subrule; BEGIN{ push(@BackendBarePerl5::Ast::Rule::Subrule::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Rule::SubruleNoCapture; BEGIN{ push(@BackendBarePerl5::Ast::Rule::SubruleNoCapture::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Rule::Var; BEGIN{ push(@BackendBarePerl5::Ast::Rule::Var::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Rule::Constant; BEGIN{ push(@BackendBarePerl5::Ast::Rule::Constant::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Rule::Dot; BEGIN{ push(@BackendBarePerl5::Ast::Rule::Dot::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Rule::SpecialChar; BEGIN{ push(@BackendBarePerl5::Ast::Rule::SpecialChar::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Rule::Block; BEGIN{ push(@BackendBarePerl5::Ast::Rule::Block::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Rule::InterpolateVar; BEGIN{ push(@BackendBarePerl5::Ast::Rule::InterpolateVar::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Rule::NamedCapture; BEGIN{ push(@BackendBarePerl5::Ast::Rule::NamedCapture::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Rule::Before; BEGIN{ push(@BackendBarePerl5::Ast::Rule::Before::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Rule::After; BEGIN{ push(@BackendBarePerl5::Ast::Rule::After::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Rule::NegateCharClass; BEGIN{ push(@BackendBarePerl5::Ast::Rule::NegateCharClass::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Rule::CharClass; BEGIN{ push(@BackendBarePerl5::Ast::Rule::CharClass::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::Rule::Capture; BEGIN{ push(@BackendBarePerl5::Ast::Rule::Capture::ISA, qw{ BackendBarePerl5::Ast::AllNodes }); }}
{package BackendBarePerl5::Ast::CompUnit;
sub all_fields {
    my($node)=@_;
    @$node{'unit_type','name','traits','attributes','methods','body'};
}
sub all_field_names {
    my($node)=@_;
    return('unit_type','name','traits','attributes','methods','body');
}
}
{package BackendBarePerl5::Ast::Val::Int;
sub all_fields {
    my($node)=@_;
    @$node{'int'};
}
sub all_field_names {
    my($node)=@_;
    return('int');
}
}
{package BackendBarePerl5::Ast::Val::Bit;
sub all_fields {
    my($node)=@_;
    @$node{'bit'};
}
sub all_field_names {
    my($node)=@_;
    return('bit');
}
}
{package BackendBarePerl5::Ast::Val::Num;
sub all_fields {
    my($node)=@_;
    @$node{'num'};
}
sub all_field_names {
    my($node)=@_;
    return('num');
}
}
{package BackendBarePerl5::Ast::Val::Buf;
sub all_fields {
    my($node)=@_;
    @$node{'buf'};
}
sub all_field_names {
    my($node)=@_;
    return('buf');
}
}
{package BackendBarePerl5::Ast::Val::Char;
sub all_fields {
    my($node)=@_;
    @$node{'char'};
}
sub all_field_names {
    my($node)=@_;
    return('char');
}
}
{package BackendBarePerl5::Ast::Val::Undef;
sub all_fields {
    my($node)=@_;
    return();
}
sub all_field_names {
    my($node)=@_;
    return();
}
}
{package BackendBarePerl5::Ast::Val::Object;
sub all_fields {
    my($node)=@_;
    @$node{'class','fields'};
}
sub all_field_names {
    my($node)=@_;
    return('class','fields');
}
}
{package BackendBarePerl5::Ast::Lit::Seq;
sub all_fields {
    my($node)=@_;
    @$node{'seq'};
}
sub all_field_names {
    my($node)=@_;
    return('seq');
}
}
{package BackendBarePerl5::Ast::Lit::Array;
sub all_fields {
    my($node)=@_;
    @$node{'array'};
}
sub all_field_names {
    my($node)=@_;
    return('array');
}
}
{package BackendBarePerl5::Ast::Lit::Hash;
sub all_fields {
    my($node)=@_;
    @$node{'hash'};
}
sub all_field_names {
    my($node)=@_;
    return('hash');
}
}
{package BackendBarePerl5::Ast::Lit::Pair;
sub all_fields {
    my($node)=@_;
    @$node{'key','value'};
}
sub all_field_names {
    my($node)=@_;
    return('key','value');
}
}
{package BackendBarePerl5::Ast::Lit::SigArgument;
sub all_fields {
    my($node)=@_;
    @$node{'key','value','type','has_default','is_named_only','is_optional','is_slurpy','is_multidimensional','is_rw','is_copy'};
}
sub all_field_names {
    my($node)=@_;
    return('key','value','type','has_default','is_named_only','is_optional','is_slurpy','is_multidimensional','is_rw','is_copy');
}
}
{package BackendBarePerl5::Ast::Lit::NamedArgument;
sub all_fields {
    my($node)=@_;
    @$node{'key','value'};
}
sub all_field_names {
    my($node)=@_;
    return('key','value');
}
}
{package BackendBarePerl5::Ast::Lit::Code;
sub all_fields {
    my($node)=@_;
    @$node{'pad','state','sig','body','CATCH'};
}
sub all_field_names {
    my($node)=@_;
    return('pad','state','sig','body','CATCH');
}
}
{package BackendBarePerl5::Ast::Lit::Object;
sub all_fields {
    my($node)=@_;
    @$node{'class','fields'};
}
sub all_field_names {
    my($node)=@_;
    return('class','fields');
}
}
{package BackendBarePerl5::Ast::Var;
sub all_fields {
    my($node)=@_;
    @$node{'sigil','twigil','name','namespace'};
}
sub all_field_names {
    my($node)=@_;
    return('sigil','twigil','name','namespace');
}
}
{package BackendBarePerl5::Ast::Bind;
sub all_fields {
    my($node)=@_;
    @$node{'parameters','arguments'};
}
sub all_field_names {
    my($node)=@_;
    return('parameters','arguments');
}
}
{package BackendBarePerl5::Ast::Assign;
sub all_fields {
    my($node)=@_;
    @$node{'parameters','arguments'};
}
sub all_field_names {
    my($node)=@_;
    return('parameters','arguments');
}
}
{package BackendBarePerl5::Ast::Proto;
sub all_fields {
    my($node)=@_;
    @$node{'name'};
}
sub all_field_names {
    my($node)=@_;
    return('name');
}
}
{package BackendBarePerl5::Ast::Call;
sub all_fields {
    my($node)=@_;
    @$node{'invocant','hyper','method','arguments'};
}
sub all_field_names {
    my($node)=@_;
    return('invocant','hyper','method','arguments');
}
}
{package BackendBarePerl5::Ast::Apply;
sub all_fields {
    my($node)=@_;
    @$node{'code','arguments'};
}
sub all_field_names {
    my($node)=@_;
    return('code','arguments');
}
}
{package BackendBarePerl5::Ast::Return;
sub all_fields {
    my($node)=@_;
    @$node{'result'};
}
sub all_field_names {
    my($node)=@_;
    return('result');
}
}
{package BackendBarePerl5::Ast::If;
sub all_fields {
    my($node)=@_;
    @$node{'cond','body','otherwise'};
}
sub all_field_names {
    my($node)=@_;
    return('cond','body','otherwise');
}
}
{package BackendBarePerl5::Ast::While;
sub all_fields {
    my($node)=@_;
    @$node{'cond','body'};
}
sub all_field_names {
    my($node)=@_;
    return('cond','body');
}
}
{package BackendBarePerl5::Ast::Decl;
sub all_fields {
    my($node)=@_;
    @$node{'decl','type','var'};
}
sub all_field_names {
    my($node)=@_;
    return('decl','type','var');
}
}
{package BackendBarePerl5::Ast::Sig;
sub all_fields {
    my($node)=@_;
    @$node{'invocant','positional'};
}
sub all_field_names {
    my($node)=@_;
    return('invocant','positional');
}
}
{package BackendBarePerl5::Ast::Lit::Capture;
sub all_fields {
    my($node)=@_;
    @$node{'invocant','array','hash'};
}
sub all_field_names {
    my($node)=@_;
    return('invocant','array','hash');
}
}
{package BackendBarePerl5::Ast::Lit::Subset;
sub all_fields {
    my($node)=@_;
    @$node{'name','base_class','block'};
}
sub all_field_names {
    my($node)=@_;
    return('name','base_class','block');
}
}
{package BackendBarePerl5::Ast::Method;
sub all_fields {
    my($node)=@_;
    @$node{'name','block'};
}
sub all_field_names {
    my($node)=@_;
    return('name','block');
}
}
{package BackendBarePerl5::Ast::Sub;
sub all_fields {
    my($node)=@_;
    @$node{'name','block'};
}
sub all_field_names {
    my($node)=@_;
    return('name','block');
}
}
{package BackendBarePerl5::Ast::Macro;
sub all_fields {
    my($node)=@_;
    @$node{'name','block'};
}
sub all_field_names {
    my($node)=@_;
    return('name','block');
}
}
{package BackendBarePerl5::Ast::Coro;
sub all_fields {
    my($node)=@_;
    @$node{'name','block'};
}
sub all_field_names {
    my($node)=@_;
    return('name','block');
}
}
{package BackendBarePerl5::Ast::P5Token;
sub all_fields {
    my($node)=@_;
    @$node{'regex'};
}
sub all_field_names {
    my($node)=@_;
    return('regex');
}
}
{package BackendBarePerl5::Ast::Token;
sub all_fields {
    my($node)=@_;
    @$node{'name','regex','sym'};
}
sub all_field_names {
    my($node)=@_;
    return('name','regex','sym');
}
}
{package BackendBarePerl5::Ast::Do;
sub all_fields {
    my($node)=@_;
    @$node{'block'};
}
sub all_field_names {
    my($node)=@_;
    return('block');
}
}
{package BackendBarePerl5::Ast::BEGIN;
sub all_fields {
    my($node)=@_;
    @$node{'block'};
}
sub all_field_names {
    my($node)=@_;
    return('block');
}
}
{package BackendBarePerl5::Ast::Use;
sub all_fields {
    my($node)=@_;
    @$node{'mod','perl5'};
}
sub all_field_names {
    my($node)=@_;
    return('mod','perl5');
}
}
{package BackendBarePerl5::Ast::Rule;
sub all_fields {
    my($node)=@_;
    return();
}
sub all_field_names {
    my($node)=@_;
    return();
}
}
{package BackendBarePerl5::Ast::Rule::Quantifier;
sub all_fields {
    my($node)=@_;
    @$node{'term','quant','greedy','ws1','ws2','ws3'};
}
sub all_field_names {
    my($node)=@_;
    return('term','quant','greedy','ws1','ws2','ws3');
}
}
{package BackendBarePerl5::Ast::Rule::Or;
sub all_fields {
    my($node)=@_;
    @$node{'or'};
}
sub all_field_names {
    my($node)=@_;
    return('or');
}
}
{package BackendBarePerl5::Ast::Rule::Concat;
sub all_fields {
    my($node)=@_;
    @$node{'concat'};
}
sub all_field_names {
    my($node)=@_;
    return('concat');
}
}
{package BackendBarePerl5::Ast::Rule::Subrule;
sub all_fields {
    my($node)=@_;
    @$node{'metasyntax','ident','capture_to_array'};
}
sub all_field_names {
    my($node)=@_;
    return('metasyntax','ident','capture_to_array');
}
}
{package BackendBarePerl5::Ast::Rule::SubruleNoCapture;
sub all_fields {
    my($node)=@_;
    @$node{'metasyntax'};
}
sub all_field_names {
    my($node)=@_;
    return('metasyntax');
}
}
{package BackendBarePerl5::Ast::Rule::Var;
sub all_fields {
    my($node)=@_;
    @$node{'sigil','twigil','name'};
}
sub all_field_names {
    my($node)=@_;
    return('sigil','twigil','name');
}
}
{package BackendBarePerl5::Ast::Rule::Constant;
sub all_fields {
    my($node)=@_;
    @$node{'constant'};
}
sub all_field_names {
    my($node)=@_;
    return('constant');
}
}
{package BackendBarePerl5::Ast::Rule::Dot;
sub all_fields {
    my($node)=@_;
    return();
}
sub all_field_names {
    my($node)=@_;
    return();
}
}
{package BackendBarePerl5::Ast::Rule::SpecialChar;
sub all_fields {
    my($node)=@_;
    @$node{'char'};
}
sub all_field_names {
    my($node)=@_;
    return('char');
}
}
{package BackendBarePerl5::Ast::Rule::Block;
sub all_fields {
    my($node)=@_;
    @$node{'closure'};
}
sub all_field_names {
    my($node)=@_;
    return('closure');
}
}
{package BackendBarePerl5::Ast::Rule::InterpolateVar;
sub all_fields {
    my($node)=@_;
    @$node{'var'};
}
sub all_field_names {
    my($node)=@_;
    return('var');
}
}
{package BackendBarePerl5::Ast::Rule::NamedCapture;
sub all_fields {
    my($node)=@_;
    @$node{'rule','ident','capture_to_array'};
}
sub all_field_names {
    my($node)=@_;
    return('rule','ident','capture_to_array');
}
}
{package BackendBarePerl5::Ast::Rule::Before;
sub all_fields {
    my($node)=@_;
    @$node{'rule','assertion_modifier','capture_to_array'};
}
sub all_field_names {
    my($node)=@_;
    return('rule','assertion_modifier','capture_to_array');
}
}
{package BackendBarePerl5::Ast::Rule::After;
sub all_fields {
    my($node)=@_;
    @$node{'rule','assertion_modifier','capture_to_array'};
}
sub all_field_names {
    my($node)=@_;
    return('rule','assertion_modifier','capture_to_array');
}
}
{package BackendBarePerl5::Ast::Rule::NegateCharClass;
sub all_fields {
    my($node)=@_;
    @$node{'chars'};
}
sub all_field_names {
    my($node)=@_;
    return('chars');
}
}
{package BackendBarePerl5::Ast::Rule::CharClass;
sub all_fields {
    my($node)=@_;
    @$node{'chars'};
}
sub all_field_names {
    my($node)=@_;
    return('chars');
}
}
{package BackendBarePerl5::Ast::Rule::Capture;
sub all_fields {
    my($node)=@_;
    @$node{'rule','position','capture_to_array'};
}
sub all_field_names {
    my($node)=@_;
    return('rule','position','capture_to_array');
}
}
1;
__END__
