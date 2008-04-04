
class SimpleEmit5 {

  has $.compiler;

  method prelude_for_entering_a_package () {
    "use autobox; use autobox::Core; use autobox UNDEF => 'UNDEF';\n"
  };

  method prelude ($n) {
  '#!/usr/bin/perl -w
package main;
use Data::Dumper; # Used to render Buf strings.
use Perl6::Say;
use autobox; use autobox::Core; use autobox UNDEF => "UNDEF";
use Moose::Autobox;
{package AssertCurrentModuleVersions;
 use Moose 0.40;
 use Moose::Autobox 0.06;
 use autobox 2.23;
 use autobox::Core 0.4;
}

our $a_ARGS = [@ARGV];

{package UNDEF;}
{package UNDEF; sub ref{"UNDEF"}}
{package UNIVERSAL; sub ref{CORE::ref($_[0]) || "SCALAR"} }
{package UNIVERSAL; sub WHAT{CORE::ref($_[0]) || "SCALAR"} }

sub ::undef{undef}

use Carp;
sub slurp{my($file)=@_; `cat $file`;}
sub unslurp{
  my($text,$file)=@_; open(F,">$file") or CORE::die $!; print F $text; close F;}
sub file_exists{-e $_[0]}
sub system{CORE::system(@_)}
sub eval_perl5{my($p5)=@_;my $res = eval($p5); croak($@) if $@; $res}
sub ::die{croak @_}
sub ::exit{CORE::exit(@_)}
sub ::defined{CORE::defined($_[0])}
sub ::substr ($$$){CORE::substr($_[0],$_[1],$_[2])}
sub ::not ($){CORE::not $_[0]}
sub ::exec{CORE::exec(@_)}
sub ::sleep{CORE::sleep(@_)}


# because the p5->p6 massage of ast_handlers isnt massaging join.
sub ::join{CORE::join(CORE::shift,@_)}
# end

{ package SCALAR;
sub re_gsub ($$$) {$_[0] =~ s/$_[1]/$_[2]/g; $_[0]}
sub re_sub  ($$$) {$_[0] =~ s/$_[1]/$_[2]/;  $_[0]}
}

{ package ARRAY;
# absent from autobox::Core
sub splice { my $a = CORE::shift; [CORE::splice(@{$a},$_[0],$_[1])] }
sub copy { my $a = CORE::shift; [@$a] }
# buggy in autobox::Core
BEGIN{my $x = *ARRAY::unshift; undef &$x;}
sub unshift (\@;@) { my $a = CORE::shift; CORE::unshift(@$a, @_); $a; }
}

sub parser_name{
  my $f = $0;
  $f =~ s/[^\/]+$//;
  $f."../STD_red/STD_red_run"
}

our $a_INC = ["."];
sub ::require {
  my($module)=@_;
  my $file = find_required_module($module);
  $file || CORE::die "Cant locate $module in ( ".CORE::join(" ",@$a_INC)." ).\n";
  eval_file($file);
};
sub ::find_required_module {
  my($module)=@_;
  my @names = ($module,$module.".pm",$module.".p6");
  for my $dir (@$a_INC) {
    for my $name (@names) {
      my $file = $dir."/".$name;
      if(-f $file) {
        return $file;
      }
    }
  }
  return undef;
}

our $compiler0;
our $compiler1;
sub ::eval_file {
  my($file)=@_;
  $compiler0->eval_file($file);
}
sub ::eval_perl6 {
  my($code)=@_;
  $compiler0->eval_perl6($code);
}

package main;
';
  };

  method e($x) {
    my $ref = $x.ref;
    if $ref eq 'UNDEF' { $x }
    elsif $ref eq 'SCALAR' { $x }
    elsif $ref eq 'ARRAY' { $x.map(sub($ae){$.e($ae)}) }
    else {$x.callback(self)}
  };


  method cb__CompUnit ($n) {
    my $^whiteboard::in_package = [];
    ("package main; # not Main, otherwise ::foo() hack for sub()s doesnt work.\n"~
     self.prelude_for_entering_a_package()~
     $.e($n<statements>).join(";\n"))
  };
  method cb__Block ($n) {
    ''~$.e($n<statements>).join(";\n")~''
  };

  method cb__Use ($n) {
    my $module = $n<module_name>;
    if $module eq 'v6-alpha' { "" }
    elsif $.compiler.hook_for_use($module) { "" }
    else { "***Unimplemented use()***" }
  };
  method cb__ClosureTrait ($n) {
    $n<kind>~'{'~$.e($n<block>)~'}'
  };

  method cb__PackageDecl ($n) {

    my $^whiteboard::in_package = [$^whiteboard::in_package.flatten,$n<name>];
    my $name = $^whiteboard::in_package.join('::');
    ("\n{ package "~$name~";\n"~
     "use Moose;\n"~
     self.prelude_for_entering_a_package()~
     $.e($n<traits>||[]).join("\n")~
     $.e($n<block>)~
     "; __PACKAGE__->meta->make_immutable();\n"~
     "\n}\n");
  };
  method cb__Trait ($n) {
    if($n<verb> eq 'is') {
      my $name = $^whiteboard::in_package.splice(0,-1).join('::')~'::'~$.e($n<expr>);
      "extends '"~$name~"';"
    } else {
      say "ERROR: Emitting p5 for Trait verb "~$n<verb>~" has not been implemented.\n";
      "***Trait***"
    }
  };

  method cb__VarDecl ($n) {
    if($n<scope> eq 'has') {
      my $default = $.e($n<default_expr>);
      if(defined $default) {
        $default = ", default => "~$default
      } else {
        $default = ""
      }
      "has '"~$.e($n<var><name>)~"' => (is => 'rw'"~$default~");"
    } else {
      my $default = "";
      if $n<default_expr> { $default = ' = '~$.e($n<default_expr>) }
      if($n<var><twigil> eq '^') {
        my $name = $.e($n<var>);
        $name.re_gsub('^(.)::','$1');
        ("{package main; use vars '"~$name~"'};"~
         'local'~' '~$.e($n<var>)~$default)
      }
      else {
        $n<scope>~' '~$.e($n<var>)~$default
      }
    }
  };
  method cb__SubDecl ($n) {
    'sub '~$.e($n<name>)~'{'~$.e($n<multisig>)~$.e($n<block>)~'}'
  };
  method cb__MethodDecl ($n) {
    'sub '~$.e($n<name>)~'{my $self=CORE::shift;'~$.e($n<multisig>)~$.e($n<block>)~'}'
  };
  method cb__Signature ($n) {
    if($n<parameters>.elems == 0) { "" }
    else {
      'my('~$.e($n<parameters>).join(",")~')=@_;'~"\n";
    }
  };
  method cb__Parameter ($n) {
    $.e($n<param_var>)
  };
  method cb__ParamVar ($n) {
    $n<sigil>~$.e($n<name>)
  };

  method cb__Call ($n) {
    my $method = $.e($n<method>);
    if($method =~ 'postcircumfix:< >') {
      $.e($n<invocant>)~'->'~"{'"~$.e($n<capture>)~"'}";
    }
    elsif($method =~ 'postcircumfix:(.*)') {
      my $op = $1;
      my $arg = $.e($n<capture>);
      $op.re_gsub(' ',$arg);
      $.e($n<invocant>)~'->'~$op;
    } else {
      $.e($n<invocant>)~'->'~$.e($n<method>)~'('~$.e($n<capture>)~')'
    }
  };
  method cb__Apply ($n) {
    if $n<function> =~ /^infix:(.+)$/ {
      my $op = $1;
      my $a = $.e($n<capture><arguments>);
      my $l = $a[0];
      my $r = $a[1];
      if($op eq '~') { "("~$l~" . "~$r~")" }
      elsif($op eq ',') { $l~", "~$r }
      elsif($op eq '=') {
        my $t = $.e($n<capture><arguments>[0]<twigil>);
        if($t && $t eq '.') {
          $l~'('~$r~')'
        }
        else { "("~$l~" "~$op~" "~$r~")" }
      }
      else { "("~$l~" "~$op~" "~$r~")" }
    }
    elsif($.e($n<function>) =~ /^circumfix:(.+)/) {
      my $op = $1;
      my $arg = $.e($n<capture>);
      $op.re_gsub(' ',$arg);
    }
    else {
      my $f = $.e($n<function>);
      if($f =~ /^\$\w+$/) {
         $f~'->('~$.e($n<capture>)~')';
      }elsif($f eq 'self') {
        '$self'
      }elsif($f eq 'last') {
        'last'
      }elsif($f =~ /^\w/) {
         '::'~$f~'('~$.e($n<capture>)~')';
      }else{
         $f~'('~$.e($n<capture>)~')';
      }
    }
  };
  method cb__Capture ($n) {
    $.e($n<arguments>||[]).join(",")
  };

  method cb__For ($n) {
    'for('~$.e($n<expr>)~"->flatten){\n"~$.e($n<block>)~"\n}"
  };
  method cb__Cond ($n) {
    my $els = '';
    if $n<default> { $els = "else {\n"~$.e($n<default>)~"\n}" }
    my $clauses = $.e($n<clauses>);
    my $first = $clauses.shift;
    my $first_test = $first[0];
    if $n<invert_first_test> { $first_test = "not("~$first_test~")" }
    ('if('~$first_test~") {\n"~$first[1]~"\n}"
    ~$clauses.map(sub($e){'elsif('~$e[0]~") {\n"~$e[1]~"\n}"}).join("")
    ~$els)
  };
  method cb__Loop ($n) {
    'while('~$.e($n<pretest>)~") {\n"~$.e($n<block>)~"\n}"
  };

  method cb__Var ($n) {
    my $s = $n<sigil>;
    my $t = $n<twigil>||'';
    #XXX $pkg:x -> s_pkg::x :(
    my $env = '';
    my $pre = '';
    if $t eq '^' { $env = 'e' };
    if $s eq '$' && $env eq 'e' { $pre = 's_' };
    if $s eq '@' { $pre = 'a_' }
    if $s eq '%' { $pre = 'h_' }
    my $name = $env~$pre~$.e($n<name>);
    if($t eq '.') {
      '$self->'~$name
    }elsif($t eq '^') {
      $name.re_gsub('::','__');
      '$'~'::'~$name
    }else{
      '$'~$name
    }
  };
  method cb__NumInt ($n) {
    $.e($n<text>)
  };
  method cb__Hash ($n) {
    '{'~$.e($n<hash>||[]).join(",")~'}'
  };
  method cb__Buf ($n) {
    my $s = eval_perl5('sub{local $Data::Dumper::Terse = 1; Data::Dumper::Dumper($_[0])}').($n<buf>);
    $s.chomp;
    $s;
  };
  method cb__Rx ($n) {
    'qr/'~$n<pat>~'/'
  };

};

