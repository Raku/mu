# WARNING
# This file is written by ./emit_from_kp6_pl_generate.
# YOUR EDITS TO THIS FILE WILL BE LOST.


package BackendBarePerl5;
use strict;
use warnings;

use SimpleDispatchOnTypeSuffix;

sub new {
    my($cls)=@_;
    my $self = {
	dispatcher => SimpleDispatchOnTypeSuffix->new(),
    };
    bless $self,$cls;
    $self->initialize();
    $self;
}

sub dispatch_type_to {
    my($self,$type,$to)=@_;
    $self->{dispatcher}->dispatch_type_to($type,$to);
}

sub emit {
    my($self,$obj,@rest)=@_;
    my $handler = $self->{dispatcher}->lookup($obj);
    die "No handler for $obj" if not $handler;
    my $code = $handler->($self,$obj,@rest);
    $code;
}

sub initialize {
    my($self)=@_;
    $self->dispatch_type_to('::CompUnit',\&emit_kp6_CompUnit);
    $self->dispatch_type_to('::Val::Int',\&emit_kp6_Val_Int);
    $self->dispatch_type_to('::Val::Bit',\&emit_kp6_Val_Bit);
    $self->dispatch_type_to('::Val::Num',\&emit_kp6_Val_Num);
    $self->dispatch_type_to('::Val::Buf',\&emit_kp6_Val_Buf);
    $self->dispatch_type_to('::Val::Char',\&emit_kp6_Val_Char);
    $self->dispatch_type_to('::Val::Undef',\&emit_kp6_Val_Undef);
    $self->dispatch_type_to('::Val::Object',\&emit_kp6_Val_Object);
    $self->dispatch_type_to('::Lit::Seq',\&emit_kp6_Lit_Seq);
    $self->dispatch_type_to('::Lit::Array',\&emit_kp6_Lit_Array);
    $self->dispatch_type_to('::Lit::Hash',\&emit_kp6_Lit_Hash);
    $self->dispatch_type_to('::Lit::Pair',\&emit_kp6_Lit_Pair);
    $self->dispatch_type_to('::Lit::SigArgument',\&emit_kp6_Lit_SigArgument);
    $self->dispatch_type_to('::Lit::NamedArgument',\&emit_kp6_Lit_NamedArgument);
    $self->dispatch_type_to('::Lit::Code',\&emit_kp6_Lit_Code);
    $self->dispatch_type_to('::Lit::Object',\&emit_kp6_Lit_Object);
    $self->dispatch_type_to('::Var',\&emit_kp6_Var);
    $self->dispatch_type_to('::Bind',\&emit_kp6_Bind);
    $self->dispatch_type_to('::Assign',\&emit_kp6_Assign);
    $self->dispatch_type_to('::Proto',\&emit_kp6_Proto);
    $self->dispatch_type_to('::Call',\&emit_kp6_Call);
    $self->dispatch_type_to('::Apply',\&emit_kp6_Apply);
    $self->dispatch_type_to('::Return',\&emit_kp6_Return);
    $self->dispatch_type_to('::If',\&emit_kp6_If);
    $self->dispatch_type_to('::While',\&emit_kp6_While);
    $self->dispatch_type_to('::Decl',\&emit_kp6_Decl);
    $self->dispatch_type_to('::Sig',\&emit_kp6_Sig);
    $self->dispatch_type_to('::Lit::Capture',\&emit_kp6_Lit_Capture);
    $self->dispatch_type_to('::Lit::Subset',\&emit_kp6_Lit_Subset);
    $self->dispatch_type_to('::Method',\&emit_kp6_Method);
    $self->dispatch_type_to('::Sub',\&emit_kp6_Sub);
    $self->dispatch_type_to('::Macro',\&emit_kp6_Macro);
    $self->dispatch_type_to('::Coro',\&emit_kp6_Coro);
    $self->dispatch_type_to('::P5Token',\&emit_kp6_P5Token);
    $self->dispatch_type_to('::Token',\&emit_kp6_Token);
    $self->dispatch_type_to('::Do',\&emit_kp6_Do);
    $self->dispatch_type_to('::BEGIN',\&emit_kp6_BEGIN);
    $self->dispatch_type_to('::Use',\&emit_kp6_Use);
    $self->dispatch_type_to('::Rule',\&emit_kp6_Rule);
    $self->dispatch_type_to('::Rule::Quantifier',\&emit_kp6_Rule_Quantifier);
    $self->dispatch_type_to('::Rule::Or',\&emit_kp6_Rule_Or);
    $self->dispatch_type_to('::Rule::Concat',\&emit_kp6_Rule_Concat);
    $self->dispatch_type_to('::Rule::Subrule',\&emit_kp6_Rule_Subrule);
    $self->dispatch_type_to('::Rule::SubruleNoCapture',\&emit_kp6_Rule_SubruleNoCapture);
    $self->dispatch_type_to('::Rule::Var',\&emit_kp6_Rule_Var);
    $self->dispatch_type_to('::Rule::Constant',\&emit_kp6_Rule_Constant);
    $self->dispatch_type_to('::Rule::Dot',\&emit_kp6_Rule_Dot);
    $self->dispatch_type_to('::Rule::SpecialChar',\&emit_kp6_Rule_SpecialChar);
    $self->dispatch_type_to('::Rule::Block',\&emit_kp6_Rule_Block);
    $self->dispatch_type_to('::Rule::InterpolateVar',\&emit_kp6_Rule_InterpolateVar);
    $self->dispatch_type_to('::Rule::NamedCapture',\&emit_kp6_Rule_NamedCapture);
    $self->dispatch_type_to('::Rule::Before',\&emit_kp6_Rule_Before);
    $self->dispatch_type_to('::Rule::After',\&emit_kp6_Rule_After);
    $self->dispatch_type_to('::Rule::NegateCharClass',\&emit_kp6_Rule_NegateCharClass);
    $self->dispatch_type_to('::Rule::CharClass',\&emit_kp6_Rule_CharClass);
    $self->dispatch_type_to('::Rule::Capture',\&emit_kp6_Rule_Capture);
}

sub emit_ast {
    my($cls,$ast)=@_;
    $cls->new()->emit($ast);
}

sub emit_kp6_CompUnit {
    my($self,$obj)=@_;
    my($unit_type,$name,$traits,$attributes,$methods,$body)=($obj->{unit_type},$obj->{name},$obj->{traits},$obj->{attributes},$obj->{methods},$obj->{body});
{
    # $unit_type $name $traits $attributes $methods $body 
    $self->emit($body);
}
}
sub emit_kp6_Val_Int {
    my($self,$obj)=@_;
    my($int)=($obj->{int});
{
    # $int 
    $int;
}
}
sub emit_kp6_Val_Bit {
    my($self,$obj)=@_;
    my($bit)=($obj->{bit});
{
    # $bit 
    $bit;
}
}
sub emit_kp6_Val_Num {
    my($self,$obj)=@_;
    my($num)=($obj->{num});
{
    # $num 
    $num;
}
}
sub emit_kp6_Val_Buf {
    my($self,$obj)=@_;
    my($buf)=($obj->{buf});
{
    # $buf 
    $buf;
}
}
sub emit_kp6_Val_Char {
    my($self,$obj)=@_;
    my($char)=($obj->{char});
{
    # $char 
    "chr($char)";
}
}
sub emit_kp6_Val_Undef {
    my($self,$obj)=@_;
{
    # 
    'undef';
}
}
sub emit_kp6_Val_Object {
    my($self,$obj)=@_;
    my($class,$fields)=($obj->{class},$obj->{fields});
{
    # $class $fields 
    die "Even kp6 Emit/Perl5 doesn't implement $obj";
}
}
sub emit_kp6_Lit_Seq {
    my($self,$obj)=@_;
    my($seq)=($obj->{seq});
{
    # $seq 
    '('.join(', ',map{$self->emit($_)} @{$seq}).')';
}
}
sub emit_kp6_Lit_Array {
    my($self,$obj)=@_;
    my($array)=($obj->{array});
{
    # $array 
    '['.join(', ',map{$self->emit($_)} @{$array}).']';
}
}
sub emit_kp6_Lit_Hash {
    my($self,$obj)=@_;
    my($hash)=($obj->{hash});
{
    # $hash 
    '{'.join(', ',map{$self->emit($_)} @{$hash}).'}';
}
}
sub emit_kp6_Lit_Pair {
    my($self,$obj)=@_;
    my($key,$value)=($obj->{key},$obj->{value});
{
    # $key $value 
    "[$key,$value]";
}
}
sub emit_kp6_Lit_SigArgument {
    my($self,$obj)=@_;
    my($key,$value,$type,$has_default,$is_named_only,$is_optional,$is_slurpy,$is_multidimensional,$is_rw,$is_copy)=($obj->{key},$obj->{value},$obj->{type},$obj->{has_default},$obj->{is_named_only},$obj->{is_optional},$obj->{is_slurpy},$obj->{is_multidimensional},$obj->{is_rw},$obj->{is_copy});
{
    # $key $value $type $has_default $is_named_only $is_optional $is_slurpy $is_multidimensional $is_rw $is_copy 
    
}
}
sub emit_kp6_Lit_NamedArgument {
    my($self,$obj)=@_;
    my($key,$value)=($obj->{key},$obj->{value});
{
    # $key $value 

}
}
sub emit_kp6_Lit_Code {
    my($self,$obj)=@_;
    my($pad,$state,$sig,$body,$CATCH)=($obj->{pad},$obj->{state},$obj->{sig},$obj->{body},$obj->{CATCH});
{
    # $pad $state $sig $body $CATCH 
    join(";\n",map{$self->emit($_)} @{$body})
}
}
sub emit_kp6_Lit_Object {
    my($self,$obj)=@_;
    my($class,$fields)=($obj->{class},$obj->{fields});
{
    # $class $fields 

}
}
sub emit_kp6_Var {
    my($self,$obj)=@_;
    my($sigil,$twigil,$name,$namespace)=($obj->{sigil},$obj->{twigil},$obj->{name},$obj->{namespace});
{
    # $sigil $twigil $name $namespace 
    $sigil ne '&' ? '$'.$name : $name;
}
}
sub emit_kp6_Bind {
    my($self,$obj)=@_;
    my($parameters,$arguments)=($obj->{parameters},$obj->{arguments});
{
    # $parameters $arguments 
    ('('.join(',',map{$self->emit($_)} @{$parameters}).')'.
     ' = '.
     '('.join(',',map{$self->emit($_)} @{$arguments}).')'.
     ';');
}
}
sub emit_kp6_Assign {
    my($self,$obj)=@_;
    my($parameters,$arguments)=($obj->{parameters},$obj->{arguments});
{
    # $parameters $arguments 
    ('('.join(',',map{$self->emit($_)} @{$parameters}).')'.
     ' = '.
     '('.join(',',map{$self->emit($_)} @{$arguments}).')'.
     ';');
}
}
sub emit_kp6_Proto {
    my($self,$obj)=@_;
    my($name)=($obj->{name});
{
    # $name 
    die;
}
}
sub emit_kp6_Call {
    my($self,$obj)=@_;
    my($invocant,$hyper,$method,$arguments)=($obj->{invocant},$obj->{hyper},$obj->{method},$obj->{arguments});
{
    # $invocant $hyper $method $arguments 
    ($self->emit($invocant).'->'.$method.
     '('.join(',',map{$self->emit($_)} @{$arguments}).')'.
     '');
}
}
sub emit_kp6_Apply {
    my($self,$obj)=@_;
    my($code,$arguments)=($obj->{code},$obj->{arguments});
{
    # $code $arguments 
    my $f = $self->emit($code);
    ($f.($f =~ /^[\w:]+$/ ? "" : '->').
     '('.join(',',map{$self->emit($_)} @{$arguments}).')'.
     '');
}
}
sub emit_kp6_Return {
    my($self,$obj)=@_;
    my($result)=($obj->{result});
{
    # $result 
    'return('.$self->emit($result).');'
}
}
sub emit_kp6_If {
    my($self,$obj)=@_;
    my($cond,$body,$otherwise)=($obj->{cond},$obj->{body},$obj->{otherwise});
{
    # $cond $body $otherwise 
    ('if('.$self->emit($cond).") {\n".
     $self->emit($body).
     "\n}\nelse {\n".
     ($otherwise ? $self->emit($otherwise) : "").
     "\n}\n");
}
}
sub emit_kp6_While {
    my($self,$obj)=@_;
    my($cond,$body)=($obj->{cond},$obj->{body});
{
    # $cond $body 
    ('while('.$self->emit($cond).") {\n".
     $self->emit($body).
     "\n}\n");
}
}
sub emit_kp6_Decl {
    my($self,$obj)=@_;
    my($decl,$type,$var)=($obj->{decl},$obj->{type},$obj->{var});
{
    # $decl $type $var 
    my $var_sigil = $var->{sigil};
    my $setup = {
	'$' => '',
	'@' => '[]',
	'%' => '{}',
    }->{$var_sigil} || '';
    $decl.' '.$self->emit($var).$setup.";";
}
}
sub emit_kp6_Sig {
    my($self,$obj)=@_;
    my($invocant,$positional)=($obj->{invocant},$obj->{positional});
{
    # $invocant $positional 
    
}
}
sub emit_kp6_Lit_Capture {
    my($self,$obj)=@_;
    my($invocant,$array,$hash)=($obj->{invocant},$obj->{array},$obj->{hash});
{
    # $invocant $array $hash 

}
}
sub emit_kp6_Lit_Subset {
    my($self,$obj)=@_;
    my($name,$base_class,$block)=($obj->{name},$obj->{base_class},$obj->{block});
{
    # $name $base_class $block 

}
}
sub emit_kp6_Method {
    my($self,$obj)=@_;
    my($name,$block)=($obj->{name},$obj->{block});
{
    # $name $block 
    'sub '.$name.' '.$self->emit($block);
}
}
sub emit_kp6_Sub {
    my($self,$obj)=@_;
    my($name,$block)=($obj->{name},$obj->{block});
{
    # $name $block 
    'sub '.$name.' '.$self->emit($block);
}
}
sub emit_kp6_Macro {
    my($self,$obj)=@_;
    my($name,$block)=($obj->{name},$obj->{block});
{
    # $name $block 
    die;
}
}
sub emit_kp6_Coro {
    my($self,$obj)=@_;
    my($name,$block)=($obj->{name},$obj->{block});
{
    # $name $block 
    die;
}
}
sub emit_kp6_P5Token {
    my($self,$obj)=@_;
    my($regex)=($obj->{regex});
{
    # $regex 

}
}
sub emit_kp6_Token {
    my($self,$obj)=@_;
    my($name,$regex,$sym)=($obj->{name},$obj->{regex},$obj->{sym});
{
    # $name $regex $sym 

}
}
sub emit_kp6_Do {
    my($self,$obj)=@_;
    my($block)=($obj->{block});
{
    # $block 
    ("do{\n".$self->emit($block)."\n}");
}
}
sub emit_kp6_BEGIN {
    my($self,$obj)=@_;
    my($block)=($obj->{block});
{
    # $block 
    ("INIT{\n".$self->emit($block)."\n}\n");
}
}
sub emit_kp6_Use {
    my($self,$obj)=@_;
    my($mod,$perl5)=($obj->{mod},$obj->{perl5});
{
    # $mod $perl5 
    ($mod =~ /^v6/) ? "# use $mod\n" : "use $mod\n";
}
}
sub emit_kp6_Rule {
    my($self,$obj)=@_;
{
    # 

}
}
sub emit_kp6_Rule_Quantifier {
    my($self,$obj)=@_;
    my($term,$quant,$greedy,$ws1,$ws2,$ws3)=($obj->{term},$obj->{quant},$obj->{greedy},$obj->{ws1},$obj->{ws2},$obj->{ws3});
{
    # $term $quant $greedy $ws1 $ws2 $ws3 

}
}
sub emit_kp6_Rule_Or {
    my($self,$obj)=@_;
    my($or)=($obj->{or});
{
    # $or 

}
}
sub emit_kp6_Rule_Concat {
    my($self,$obj)=@_;
    my($concat)=($obj->{concat});
{
    # $concat 

}
}
sub emit_kp6_Rule_Subrule {
    my($self,$obj)=@_;
    my($metasyntax,$ident,$capture_to_array)=($obj->{metasyntax},$obj->{ident},$obj->{capture_to_array});
{
    # $metasyntax $ident $capture_to_array 

}
}
sub emit_kp6_Rule_SubruleNoCapture {
    my($self,$obj)=@_;
    my($metasyntax)=($obj->{metasyntax});
{
    # $metasyntax 

}
}
sub emit_kp6_Rule_Var {
    my($self,$obj)=@_;
    my($sigil,$twigil,$name)=($obj->{sigil},$obj->{twigil},$obj->{name});
{
    # $sigil $twigil $name 

}
}
sub emit_kp6_Rule_Constant {
    my($self,$obj)=@_;
    my($constant)=($obj->{constant});
{
    # $constant 

}
}
sub emit_kp6_Rule_Dot {
    my($self,$obj)=@_;
{
    # 
    '.';
}
}
sub emit_kp6_Rule_SpecialChar {
    my($self,$obj)=@_;
    my($char)=($obj->{char});
{
    # $char 

}
}
sub emit_kp6_Rule_Block {
    my($self,$obj)=@_;
    my($closure)=($obj->{closure});
{
    # $closure 

}
}
sub emit_kp6_Rule_InterpolateVar {
    my($self,$obj)=@_;
    my($var)=($obj->{var});
{
    # $var 

}
}
sub emit_kp6_Rule_NamedCapture {
    my($self,$obj)=@_;
    my($rule,$ident,$capture_to_array)=($obj->{rule},$obj->{ident},$obj->{capture_to_array});
{
    # $rule $ident $capture_to_array 

}
}
sub emit_kp6_Rule_Before {
    my($self,$obj)=@_;
    my($rule,$assertion_modifier,$capture_to_array)=($obj->{rule},$obj->{assertion_modifier},$obj->{capture_to_array});
{
    # $rule $assertion_modifier $capture_to_array 

}
}
sub emit_kp6_Rule_After {
    my($self,$obj)=@_;
    my($rule,$assertion_modifier,$capture_to_array)=($obj->{rule},$obj->{assertion_modifier},$obj->{capture_to_array});
{
    # $rule $assertion_modifier $capture_to_array 

}
}
sub emit_kp6_Rule_NegateCharClass {
    my($self,$obj)=@_;
    my($chars)=($obj->{chars});
{
    # $chars 

}
}
sub emit_kp6_Rule_CharClass {
    my($self,$obj)=@_;
    my($chars)=($obj->{chars});
{
    # $chars 

}
}
sub emit_kp6_Rule_Capture {
    my($self,$obj)=@_;
    my($rule,$position,$capture_to_array)=($obj->{rule},$obj->{position},$obj->{capture_to_array});
{
    # $rule $position $capture_to_array 

}
}
1;
__END__
