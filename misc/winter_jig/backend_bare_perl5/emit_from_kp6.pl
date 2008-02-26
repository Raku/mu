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
    my($self,$obj)=@_;
    my($unit_type,$name,$traits,$attributes,$methods,$body)=($obj->{unit_type},$obj->{name},$obj->{traits},$obj->{attributes},$obj->{methods},$obj->{body});
    # $unit_type $name $traits $attributes $methods $body 
    $self->emit($body);
}
sub emit_Val_Int {
    my($self,$obj)=@_;
    my($int)=($obj->{int});
    # $int 
    $int;
}
sub emit_Val_Bit {
    my($self,$obj)=@_;
    my($bit)=($obj->{bit});
    # $bit 
    $bit;
}
sub emit_Val_Num {
    my($self,$obj)=@_;
    my($num)=($obj->{num});
    # $num 
    $num;
}
sub emit_Val_Buf {
    my($self,$obj)=@_;
    my($buf)=($obj->{buf});
    # $buf 
    $buf;
}
sub emit_Val_Char {
    my($self,$obj)=@_;
    my($char)=($obj->{char});
    # $char 
    "chr($char)";
}
sub emit_Val_Undef {
    my($self,$obj)=@_;
    # 
    'undef';
}
sub emit_Val_Object {
    my($self,$obj)=@_;
    my($class,$fields)=($obj->{class},$obj->{fields});
    # $class $fields 
    die "Even kp6 Emit/Perl5 doesn't implement $obj";
}
sub emit_Lit_Seq {
    my($self,$obj)=@_;
    my($seq)=($obj->{seq});
    # $seq 
    '('.join(', ',map{$self->emit($_)} @{$seq}).')';
}
sub emit_Lit_Array {
    my($self,$obj)=@_;
    my($array)=($obj->{array});
    # $array 
    '['.join(', ',map{$self->emit($_)} @{$array}).']';
}
sub emit_Lit_Hash {
    my($self,$obj)=@_;
    my($hash)=($obj->{hash});
    # $hash 
    '{'.join(', ',map{$self->emit($_)} @{$hash}).'}';
}
sub emit_Lit_Pair {
    my($self,$obj)=@_;
    my($key,$value)=($obj->{key},$obj->{value});
    # $key $value 
    "[$key,$value]";
}
sub emit_Lit_SigArgument {
    my($self,$obj)=@_;
    my($key,$value,$type,$has_default,$is_named_only,$is_optional,$is_slurpy,$is_multidimensional,$is_rw,$is_copy)=($obj->{key},$obj->{value},$obj->{type},$obj->{has_default},$obj->{is_named_only},$obj->{is_optional},$obj->{is_slurpy},$obj->{is_multidimensional},$obj->{is_rw},$obj->{is_copy});
    # $key $value $type $has_default $is_named_only $is_optional $is_slurpy $is_multidimensional $is_rw $is_copy 
    
}
sub emit_Lit_NamedArgument {
    my($self,$obj)=@_;
    my($key,$value)=($obj->{key},$obj->{value});
    # $key $value 

}
sub emit_Lit_Code {
    my($self,$obj)=@_;
    my($pad,$state,$sig,$body,$CATCH)=($obj->{pad},$obj->{state},$obj->{sig},$obj->{body},$obj->{CATCH});
    # $pad $state $sig $body $CATCH 
    join(";\n",map{$self->emit($_)} @{$body})
}
sub emit_Lit_Object {
    my($self,$obj)=@_;
    my($class,$fields)=($obj->{class},$obj->{fields});
    # $class $fields 

}
sub emit_Var {
    my($self,$obj)=@_;
    my($sigil,$twigil,$name,$namespace)=($obj->{sigil},$obj->{twigil},$obj->{name},$obj->{namespace});
    # $sigil $twigil $name $namespace 
    $sigil ne '&' ? '$'.$name : $name;
}
sub emit_Bind {
    my($self,$obj)=@_;
    my($parameters,$arguments)=($obj->{parameters},$obj->{arguments});
    # $parameters $arguments 
    ('('.join(',',map{$self->emit($_)} @{$parameters}).')'.
     ' = '.
     '('.join(',',map{$self->emit($_)} @{$arguments}).')'.
     ';');
}
sub emit_Assign {
    my($self,$obj)=@_;
    my($parameters,$arguments)=($obj->{parameters},$obj->{arguments});
    # $parameters $arguments 
    ('('.join(',',map{$self->emit($_)} @{$parameters}).')'.
     ' = '.
     '('.join(',',map{$self->emit($_)} @{$arguments}).')'.
     ';');
}
sub emit_Proto {
    my($self,$obj)=@_;
    my($name)=($obj->{name});
    # $name 
    die;
}
sub emit_Call {
    my($self,$obj)=@_;
    my($invocant,$hyper,$method,$arguments)=($obj->{invocant},$obj->{hyper},$obj->{method},$obj->{arguments});
    # $invocant $hyper $method $arguments 
    ($self->emit($invocant).'->'.$method.
     '('.join(',',map{$self->emit($_)} @{$arguments}).')'.
     '');
}
sub emit_Apply {
    my($self,$obj)=@_;
    my($code,$arguments)=($obj->{code},$obj->{arguments});
    # $code $arguments 
    my $f = $self->emit($code);
    ($f.($f =~ /^[\w:]+$/ ? "" : '->').
     '('.join(',',map{$self->emit($_)} @{$arguments}).')'.
     '');
}
sub emit_Return {
    my($self,$obj)=@_;
    my($result)=($obj->{result});
    # $result 
    'return('.$self->emit($result).');'
}
sub emit_If {
    my($self,$obj)=@_;
    my($cond,$body,$otherwise)=($obj->{cond},$obj->{body},$obj->{otherwise});
    # $cond $body $otherwise 
    ('if('.$self->emit($cond).") {\n".
     $self->emit($body).
     "\n}\nelse {\n".
     ($otherwise ? $self->emit($otherwise) : "").
     "\n}\n");
}
sub emit_While {
    my($self,$obj)=@_;
    my($cond,$body)=($obj->{cond},$obj->{body});
    # $cond $body 
    ('while('.$self->emit($cond).") {\n".
     $self->emit($body).
     "\n}\n");
}
sub emit_Decl {
    my($self,$obj)=@_;
    my($decl,$type,$var)=($obj->{decl},$obj->{type},$obj->{var});
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
    my($self,$obj)=@_;
    my($invocant,$positional)=($obj->{invocant},$obj->{positional});
    # $invocant $positional 
    
}
sub emit_Lit_Capture {
    my($self,$obj)=@_;
    my($invocant,$array,$hash)=($obj->{invocant},$obj->{array},$obj->{hash});
    # $invocant $array $hash 

}
sub emit_Lit_Subset {
    my($self,$obj)=@_;
    my($name,$base_class,$block)=($obj->{name},$obj->{base_class},$obj->{block});
    # $name $base_class $block 

}
sub emit_Method {
    my($self,$obj)=@_;
    my($name,$block)=($obj->{name},$obj->{block});
    # $name $block 
    'sub '.$name.' '.$self->emit($block);
}
sub emit_Sub {
    my($self,$obj)=@_;
    my($name,$block)=($obj->{name},$obj->{block});
    # $name $block 
    'sub '.$name.' '.$self->emit($block);
}
sub emit_Macro {
    my($self,$obj)=@_;
    my($name,$block)=($obj->{name},$obj->{block});
    # $name $block 
    die;
}
sub emit_Coro {
    my($self,$obj)=@_;
    my($name,$block)=($obj->{name},$obj->{block});
    # $name $block 
    die;
}
sub emit_P5Token {
    my($self,$obj)=@_;
    my($regex)=($obj->{regex});
    # $regex 

}
sub emit_Token {
    my($self,$obj)=@_;
    my($name,$regex,$sym)=($obj->{name},$obj->{regex},$obj->{sym});
    # $name $regex $sym 

}
sub emit_Do {
    my($self,$obj)=@_;
    my($block)=($obj->{block});
    # $block 
    ("do{\n".$self->emit($block)."\n}");
}
sub emit_BEGIN {
    my($self,$obj)=@_;
    my($block)=($obj->{block});
    # $block 
    ("INIT{\n".$self->emit($block)."\n}\n");
}
sub emit_Use {
    my($self,$obj)=@_;
    my($mod,$perl5)=($obj->{mod},$obj->{perl5});
    # $mod $perl5 
    ($mod =~ /^v6/) ? "# use $mod\n" : "use $mod\n";
}
sub emit_Rule {
    my($self,$obj)=@_;
    # 

}
sub emit_Rule_Quantifier {
    my($self,$obj)=@_;
    my($term,$quant,$greedy,$ws1,$ws2,$ws3)=($obj->{term},$obj->{quant},$obj->{greedy},$obj->{ws1},$obj->{ws2},$obj->{ws3});
    # $term $quant $greedy $ws1 $ws2 $ws3 

}
sub emit_Rule_Or {
    my($self,$obj)=@_;
    my($or)=($obj->{or});
    # $or 

}
sub emit_Rule_Concat {
    my($self,$obj)=@_;
    my($concat)=($obj->{concat});
    # $concat 

}
sub emit_Rule_Subrule {
    my($self,$obj)=@_;
    my($metasyntax,$ident,$capture_to_array)=($obj->{metasyntax},$obj->{ident},$obj->{capture_to_array});
    # $metasyntax $ident $capture_to_array 

}
sub emit_Rule_SubruleNoCapture {
    my($self,$obj)=@_;
    my($metasyntax)=($obj->{metasyntax});
    # $metasyntax 

}
sub emit_Rule_Var {
    my($self,$obj)=@_;
    my($sigil,$twigil,$name)=($obj->{sigil},$obj->{twigil},$obj->{name});
    # $sigil $twigil $name 

}
sub emit_Rule_Constant {
    my($self,$obj)=@_;
    my($constant)=($obj->{constant});
    # $constant 

}
sub emit_Rule_Dot {
    my($self,$obj)=@_;
    # 
    '.';
}
sub emit_Rule_SpecialChar {
    my($self,$obj)=@_;
    my($char)=($obj->{char});
    # $char 

}
sub emit_Rule_Block {
    my($self,$obj)=@_;
    my($closure)=($obj->{closure});
    # $closure 

}
sub emit_Rule_InterpolateVar {
    my($self,$obj)=@_;
    my($var)=($obj->{var});
    # $var 

}
sub emit_Rule_NamedCapture {
    my($self,$obj)=@_;
    my($rule,$ident,$capture_to_array)=($obj->{rule},$obj->{ident},$obj->{capture_to_array});
    # $rule $ident $capture_to_array 

}
sub emit_Rule_Before {
    my($self,$obj)=@_;
    my($rule,$assertion_modifier,$capture_to_array)=($obj->{rule},$obj->{assertion_modifier},$obj->{capture_to_array});
    # $rule $assertion_modifier $capture_to_array 

}
sub emit_Rule_After {
    my($self,$obj)=@_;
    my($rule,$assertion_modifier,$capture_to_array)=($obj->{rule},$obj->{assertion_modifier},$obj->{capture_to_array});
    # $rule $assertion_modifier $capture_to_array 

}
sub emit_Rule_NegateCharClass {
    my($self,$obj)=@_;
    my($chars)=($obj->{chars});
    # $chars 

}
sub emit_Rule_CharClass {
    my($self,$obj)=@_;
    my($chars)=($obj->{chars});
    # $chars 

}
sub emit_Rule_Capture {
    my($self,$obj)=@_;
    my($rule,$position,$capture_to_array)=($obj->{rule},$obj->{position},$obj->{capture_to_array});
    # $rule $position $capture_to_array 

}
1;
__END__
