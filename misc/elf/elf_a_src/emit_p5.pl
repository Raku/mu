#line 2 emit_p5.pl
# Warning: This file is mechanically written.  Your changes will be overwritten.
{ package IR;
  sub emit_p5_for {
    my($this,$tree)=@_;
    if(not ref($tree)) {
      $tree;
    } else {
      my $ref = ref($tree);
      if($ref eq 'ARRAY') {
        [map{$this->emit_p5_for($_)} @$tree]
      }
      elsif($ref eq 'HASH') {
        my %h;
        for my $k (keys %$tree) {
          my $v = $tree->{$k};
          $h{$k} = $this->emit_p5_for($v);
        }
        \%h;
      }
      else {
        $tree->emit_p5
      }
    }
  }
}
{ package IR::All;
  sub emit_p5 {
    my $name = $_[0]->node_name;
    print STDERR "ERROR: emit_p5 is not defined for $name.\n";
    "***<$name>***";
  }
}
{ package IR::CompUnit; sub emit_p5 {
    my($n)=@_;
    local $whiteboard::in_package = [];
"\x23".'line 2 emitted_p5_'.int(rand(10000)).'
package main;
use Data::Dumper;
'.join(";\n",@{IR->emit_p5_for($n->{statements})})
  }
}
{ package IR::Val_Int; sub emit_p5 {
    my($n)=@_;
    IR->emit_p5_for($n->{text})
  }
}
{ package IR::Apply; sub emit_p5 {
    my($n)=@_;
    if(IR->emit_p5_for($n->{code}) =~ /^infix:(.+)$/) {
  my $op = $1;
  my($l,$r)=@{IR->emit_p5_for($n->{arguments})};
  if($op eq '~'){ "($l . $r)" }
  elsif($op eq ','){ "$l, $r" }
  elsif($op eq '=') {
    my $t = $n->{arguments}[0]{twigil};
    if($t && $t eq '.') {
$l.'('.$r.')'
    }
    else { "($l $op $r)" }
  }
  else { "($l $op $r)" }
}
elsif(IR->emit_p5_for($n->{code}) =~ /^circumfix:(.+)/) {
  my $op = $1;
  my($arg)=join(",",@{IR->emit_p5_for($n->{arguments})||[]});
  if(undef) {
  } else {
    $op =~ s/ /$arg/;
    $op  
  }
}
else {
  my $f = IR->emit_p5_for($n->{code});
  if($f =~ /^\$\w+$/) {
     $f.'->('.join(",",@{IR->emit_p5_for($n->{arguments})}).')';
  }elsif($f eq 'self') {
    '$self'
  }elsif($f eq 'last') {
    'last'
  }elsif($f =~ /^\w/) {
     '::'.$f.'('.join(",",@{IR->emit_p5_for($n->{arguments})}).')';
  }else{
     $f.'('.join(",",@{IR->emit_p5_for($n->{arguments})}).')';
  }
}
  }
}
{ package IR::Decl; sub emit_p5 {
    my($n)=@_;
    if(IR->emit_p5_for($n->{decl}) eq 'has') {
  my $default = IR->emit_p5_for($n->{default});
  if(defined $default) {
    $default = ", default => $default"
  } else {
    $default = ""
  }
  "has '".IR->emit_p5_for($n->{var}->{name})."' => (is => 'rw'$default);"
}
elsif(IR->emit_p5_for($n->{var}->{twigil}) eq '^') {
  my $name = IR->emit_p5_for($n->{var});
  $name =~ s/^(.)::/$1/;
  "{package main; use vars '".$name."'};".
  'local'.' '.IR->emit_p5_for($n->{var}).(IR->emit_p5_for($n->{default}) ? ' = '.IR->emit_p5_for($n->{default}) : '')
}
else {
  IR->emit_p5_for($n->{decl}).' '.IR->emit_p5_for($n->{var}).(IR->emit_p5_for($n->{default}) ? ' = '.IR->emit_p5_for($n->{default}) : '')
}
  }
}
{ package IR::Use; sub emit_p5 {
    my($n)=@_;
    ""
  }
}
{ package IR::Val_Buf; sub emit_p5 {
    my($n)=@_;
    local $Data::Dumper::Terse = 1;
my $s = Data::Dumper::Dumper(IR->emit_p5_for($n->{buf})); chomp($s);
$s;
  }
}
{ package IR::Val_Rx; sub emit_p5 {
    my($n)=@_;
    'qr/'.IR->emit_p5_for($n->{pat}).'/'
  }
}
{ package IR::Var; sub emit_p5 {
    my($n)=@_;
    my $s = IR->emit_p5_for($n->{sigil});
my $t = IR->emit_p5_for($n->{twigil})||'';
my $env = '';
$env = 'e' if $t eq '^';
my $pre = '';
$pre = 's_' if $s eq '$' && $env eq 'e';
$pre = 'a_' if $s eq '@';
$pre = 'h_' if $s eq '%';
my $name = $env.$pre.IR->emit_p5_for($n->{name});
if($t eq '.') {
  '$self->'.$name
}elsif($t eq '^') {
  $name =~ s/::/__/g;
  '$'.'::'.$name
}else{
  '$'.$name
}
  }
}
{ package IR::For; sub emit_p5 {
    my($n)=@_;
    'for('.IR->emit_p5_for($n->{expr})."->flatten){\n".IR->emit_p5_for($n->{body})."\n}"
  }
}
{ package IR::If; sub emit_p5 {
    my($n)=@_;
    ('if('.IR->emit_p5_for($n->{test}).") {\n".IR->emit_p5_for($n->{body})."\n}"
.join("",map{'elsif('.$_->[0].") {\n".$_->[1]."\n}"} @{IR->emit_p5_for($n->{elsif})})
.(IR->emit_p5_for($n->{else}) ?  "else {\n".IR->emit_p5_for($n->{else})->[0]."\n}" : ""))
  }
}
{ package IR::While; sub emit_p5 {
    my($n)=@_;
    'while('.IR->emit_p5_for($n->{test}).") {\n".IR->emit_p5_for($n->{body})."\n}"
  }
}
{ package IR::Block; sub emit_p5 {
    my($n)=@_;
    ''.join(";\n",@{IR->emit_p5_for($n->{statements})}).''
  }
}
{ package IR::Sub; sub emit_p5 {
    my($n)=@_;
    'sub '.IR->emit_p5_for($n->{name}).'{'.IR->emit_p5_for($n->{sig}).IR->emit_p5_for($n->{block}).'}'
  }
}
{ package IR::Method; sub emit_p5 {
    my($n)=@_;
    'sub '.IR->emit_p5_for($n->{name}).'{my $self=CORE::shift;'.IR->emit_p5_for($n->{sig}).IR->emit_p5_for($n->{block}).'}'
  }
}
{ package IR::Sig; sub emit_p5 {
    my($n)=@_;
    my @a = @{IR->emit_p5_for($n->{positional})};
if(!@a) { "" }
else {
  'my('.join(",",@{IR->emit_p5_for($n->{positional})}).')=@_;'."\n";
}
  }
}
{ package IR::Lit_SigArgument; sub emit_p5 {
    my($n)=@_;
    IR->emit_p5_for($n->{key})
  }
}
{ package IR::PackageDeclarator; sub emit_p5 {
    my($n)=@_;
    local $whiteboard::in_package = [@{(($whiteboard::in_package))},IR->emit_p5_for($n->{name})];
my $name = join('::',@{(($whiteboard::in_package))});
("\n{ package ".$name.";\n".
 "use Moose;\n".
 join("\n",@{IR->emit_p5_for($n->{traits})||[]}).
 IR->emit_p5_for($n->{block}).
 "; __PACKAGE__->meta->make_immutable();\n".
 "\n}\n");
  }
}
{ package IR::Trait; sub emit_p5 {
    my($n)=@_;
    if(IR->emit_p5_for($n->{verb}) eq 'is') {
  my $name = join('::',splice(@{(($whiteboard::in_package))},0,-1),IR->emit_p5_for($n->{expr}));
  "extends '".$name."';"
} else {
  print STDERR "ERROR: Emitting p5 for Trait verb ".IR->emit_p5_for($n->{verb})." has not been implemented.\n";
  "***Trait***"
}
  }
}
{ package IR::Call; sub emit_p5 {
    my($n)=@_;
    my $method = IR->emit_p5_for($n->{method});
if($method =~ 'postcircumfix:< >') {
  IR->emit_p5_for($n->{invocant}).'->'."{'".IR->emit_p5_for($n->{arguments})."'}";
}
elsif($method =~ 'postcircumfix:(.*)') {
  my $op = $1;
  my $arg = join(",",@{IR->emit_p5_for($n->{arguments})||[]});
  $op =~ s/ /$arg/;
  IR->emit_p5_for($n->{invocant}).'->'.$op;
} else {
  IR->emit_p5_for($n->{invocant}).'->'.IR->emit_p5_for($n->{method}).'('.join(",",@{IR->emit_p5_for($n->{arguments})||[]}).')'
}
  }
}
{ package IR::Lit_Hash; sub emit_p5 {
    my($n)=@_;
    '{'.join(",",@{IR->emit_p5_for($n->{hash})||[]}).'}'
  }
}
