
class SimpleEmit5 {

  has $.compiler;

  method prelude ($n) {
  '
package main;
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


# because the p5->p6 massage of ast_handlers isnt massaging join.
sub ::join{CORE::join(CORE::shift,@_)}
# end

{ package SCALAR;
sub re_gsub ($$$) {$_[0] =~ s/$_[1]/$_[2]/g; $_[0]}
sub re_sub  ($$$) {$_[0] =~ s/$_[1]/$_[2]/;  $_[0]}
}

{ package ARRAY;
sub splice { my $a = CORE::shift; [CORE::splice(@{$a},$_[0],$_[1])] }
}

sub parser_name{
  my $f = $0;
  $f =~ s/[^\/]+$//;
  $f."../STD_red/STD_red_run"
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
     "use Data::Dumper;\n"~
     $.e($n<statements>).join(";\n"))
  };
  method cb__Val_Int ($n) {
    $.e($n<text>)
  };
  method cb__Apply ($n) {
    if $n<code> =~ /^infix:(.+)$/ {
      my $op = $1;
      my $a = $.e($n<arguments>);
      my $l = $a[0];
      my $r = $a[1];
      if($op eq '~') { "("~$l~" . "~$r~")" }
      elsif($op eq ',') { $l~", "~$r }
      elsif($op eq '=') {
        my $t = $.e($n<arguments>[0]<twigil>);
        if($t && $t eq '.') {
          $l~'('~$r~')'
        }
        else { "("~$l~" "~$op~" "~$r~")" }
      }
      else { "("~$l~" "~$op~" "~$r~")" }
    }
    elsif($.e($n<code>) =~ /^circumfix:(.+)/) {
      my $op = $1;
      my $arg = $.e($n<arguments>||[]).join(",");
      $op.re_gsub(' ',$arg);
    }
    else {
      my $f = $.e($n<code>);
      if($f =~ /^\$\w+$/) {
         $f~'->('~$.e($n<arguments>).join(",")~')';
      }elsif($f eq 'self') {
        '$self'
      }elsif($f eq 'last') {
        'last'
      }elsif($f =~ /^\w/) {
         '::'~$f~'('~$.e($n<arguments>).join(",")~')';
      }else{
         $f~'('~$.e($n<arguments>).join(",")~')';
      }
    }
  };
  method cb__Decl ($n) {
    if($n<decl> eq 'has') {
      my $default = $.e($n<default>);
      if(defined $default) {
        $default = ", default => "~$default
      } else {
        $default = ""
      }
      "has '"~$.e($n<var><name>)~"' => (is => 'rw'"~$default~");"
    } else {
      my $default = "";
      if $n<default> { $default = ' = '~$.e($n<default>) }
      if($n<var><twigil> eq '^') {
        my $name = $.e($n<var>);
        $name.re_gsub('^(.)::','$1');
        ("{package main; use vars '"~$name~"'};"~
         'local'~' '~$.e($n<var>)~$default)
      }
      else {
        $n<decl>~' '~$.e($n<var>)~$default
      }
    }
  };
  method cb__Use ($n) {
    my $module = $n<mod>;
    if $.compiler.dont_use($module) { "" }
    else { "***use() is unimplemented***" }
  };
  method cb__Val_Buf ($n) {
    my $s = eval_perl5('sub{local $Data::Dumper::Terse = 1; Data::Dumper::Dumper($_[0])}').($n<buf>);
    $s.chomp;
    $s;
  };
  method cb__Val_Rx ($n) {
    'qr/'~$n<pat>~'/'
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
  method cb__For ($n) {
    'for('~$.e($n<expr>)~"->flatten){\n"~$.e($n<body>)~"\n}"
  };
  method cb__If ($n) {
    my $els = '';
    if $n<else> { $els = "else {\n"~$.e($n<else>).[0]~"\n}" }
    ('if('~$.e($n<test>)~") {\n"~$.e($n<body>)~"\n}"
    ~$.e($n<elsif>).map(sub($e){'elsif('~$e[0]~") {\n"~$e[1]~"\n}"}).join("")
    ~$els)
  };
  method cb__While ($n) {
    'while('~$.e($n<test>)~") {\n"~$.e($n<body>)~"\n}"
  };
  method cb__Block ($n) {
    ''~$.e($n<statements>).join(";\n")~''
  };
  method cb__Sub ($n) {
    'sub '~$.e($n<name>)~'{'~$.e($n<sig>)~$.e($n<block>)~'}'
  };
  method cb__Method ($n) {
    'sub '~$.e($n<name>)~'{my $self=CORE::shift;'~$.e($n<sig>)~$.e($n<block>)~'}'
  };
  method cb__Sig ($n) {
    if($n<positional>.elems == 0) { "" }
    else {
      'my('~$.e($n<positional>).join(",")~')=@_;'~"\n";
    }
  };
  method cb__Lit_SigArgument ($n) {
    $.e($n<key>)
  };
  method cb__PackageDeclarator ($n) {
    my $^whiteboard::in_package = [$^whiteboard::in_package.flatten,$n<name>];
    my $name = $^whiteboard::in_package.join('::');
    ("\n{ package "~$name~";\n"~
     "use Moose;\n"~
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
  method cb__Call ($n) {
    my $method = $.e($n<method>);
    if($method =~ 'postcircumfix:< >') {
      $.e($n<invocant>)~'->'~"{'"~$n<arguments>~"'}";
    }
    elsif($method =~ 'postcircumfix:(.*)') {
      my $op = $1;
      my $arg = $.e($n<arguments>||[]).join(",");
      $op.re_gsub(' ',$arg);
      $.e($n<invocant>)~'->'~$op;
    } else {
      $.e($n<invocant>)~'->'~$.e($n<method>)~'('~$.e($n<arguments>||[]).join(",")~')'
    }
  };
  method cb__Lit_Hash ($n) {
    '{'~$.e($n<hash>||[]).join(",")~'}'
  };

};

