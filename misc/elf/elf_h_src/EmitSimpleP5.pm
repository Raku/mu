
class EmitSimpleP5 {

  method new_emitter($ignore,$compiler,$ignore2,$filename) {
    $.new('compiler',$compiler,'filename',$filename);
  };

  has $.compiler;
  has $.filename;

  method tidy($source) { private_tidy($source) }

  method prelude_for_entering_a_package () {
    "";
  };

  method prelude_lexical () {
    "use autobox ARRAY => 'ARRAY', HASH => 'HASH', CODE => 'CODE', INTEGER => 'INTEGER', FLOAT => 'FLOAT', STRING => 'STRING', UNDEF => 'UNDEF';  use encoding 'utf8';
      ";
  };

  method using_Moose() { 0 }
  method create_default_for($cls,$field_name,$default) {
   '$Object::DEFAULTS{'~$cls~"}\{'"~$field_name~"'} = sub \{"~$default~'};'
  }
  method prelude_oo () {
    if $.using_Moose {
      '{package AssertCurrentModuleVersions;  use Moose 0.44; }'
    }
    else {
'
{package Object;
    our %DEFAULTS;
    sub new {
        my $self = shift;
        my $class = ref $self || $self;
        my $obj = bless {@_}, $class;
        for (keys %{$DEFAULTS{$class}}) {
            $obj->{$_} = $DEFAULTS{$class}{$_}->() unless $obj->{$_};
        }
        $obj;
    }
    sub become {
      my($self,$cls)=@_;
      bless $self,$cls;
    }
}
';
    }
  };
  method prelude {
  '#!/usr/bin/env perl
use strict;
no strict "subs"; # XXX remove once Type-names are quoted. # say Int.isa(Any)
use warnings;

{package AssertCurrentModuleVersions;
 use autobox 2.51;
}
{ package NoSideEffects;
  use Class::Multimethods;
  use Data::Dumper;
}
'~$.prelude_oo~$.prelude_lexical~'

# Workaround autobox 2.53 api change. :(
if(!defined(&autobox::universal::type)) {
  eval q{package autobox::universal; sub type { autobox->type($_[0]) }};
}
{package UNIVERSAL; sub ref {CORE::ref($_[0]) || autobox::universal::type($_[0]) } } # For IRx1_FromAST.pm.
{package UNIVERSAL; sub WHAT {CORE::ref($_[0]) || autobox::universal::type($_[0]) } }

{ package Any;
  sub can { UNIVERSAL::can( (CORE::ref($_[0])||autobox::universal::type($_[0])) ,$_[1]) }
  sub isa { UNIVERSAL::isa( (CORE::ref($_[0])||autobox::universal::type($_[0])) ,$_[1]) }
  sub does { UNIVERSAL::isa( (CORE::ref($_[0])||autobox::universal::type($_[0])) ,$_[1]) }
  sub object_id {
    my $o = $_[0];
    if(ref($o)) {
      ("".$_[0]) =~ /\(0x(.+?)\)/ or die "bug";
      "o".$1;
    } else {
      (~$o&$o) eq 0 ? $o : "s".$o;
    }
  }
}  

# Avoid "use base" error: Base class package "Xxx" is empty. :/
{ package Num; our $_tell_use_base_i_am_not_empty_; }
{ package Int; our $_tell_use_base_i_am_not_empty_; }
{ package Str; our $_tell_use_base_i_am_not_empty_; }
{ package Array; our $_tell_use_base_i_am_not_empty_; }
{ package Hash; our $_tell_use_base_i_am_not_empty_; }
{ package Undef; our $_tell_use_base_i_am_not_empty_; }
{ package Code; our $_tell_use_base_i_am_not_empty_; }

{package UNDEF;
 use base "Undef";
 sub WHAT {"Undef"}
 sub perl { "undef" }
}
no warnings qw(redefine prototype);
{ package STRING;
  use base "Str";
  sub WHAT { "Str" }

  sub re_matchp { ($_[0] =~ m{$_[1]}) ? 1 : 0 }
  sub re_groups {
    my @g = $_[0] =~ m{$_[1]};
    @g ? \@g : undef;
  }

  sub re_gsub_pat { # slower, but needed for $1.
    my($s,$pat,$replace_pat)=@_;
    eval "\$s =~ s/$pat/$replace_pat/g";
    Carp::confess($@) if $@;
    $s;
  }
  sub re_gsub {
    my($s,$pat,$replace_str)=@_;
    $s =~ s/$pat/$replace_str/g;
    $s;
  }

  # unused
  sub re_sub         {
    my $expr = "\$_[0] =~ s/$_[1]/$_[2]/".($_[3]||"");
    eval $expr;
    Carp::confess($@) if $@;
    $_[0]
  }

  sub hex   { CORE::hex($_[0]); }

  # randomness taken from autobox::Core

  sub chomp    { CORE::chomp($_[0]); }
  sub chop     { CORE::chop($_[0]); }
  sub chr      { CORE::chr($_[0]); }
  sub crypt    { CORE::crypt($_[0], $_[1]); }
  sub index    { CORE::index($_[0], $_[1], @_[2.. $#_]); }
  sub lc       { CORE::lc($_[0]); }
  sub lcfirst  { CORE::lcfirst($_[0]); }

  sub bytes    { use bytes; CORE::length($_[0]); }
  sub chars    { CORE::length($_[0]); }
  sub codes    { my @c = unpack("U*",$_[0]); 0+@c }
  sub graphs   { require String::Multibyte; String::Multibyte->new("Grapheme")->length($_[0]) }

  sub ord      { CORE::ord($_[0]); }
  sub pack     { CORE::pack(@_); }
  sub reverse  { CORE::reverse($_[0]); }
  sub rindex   { CORE::rindex($_[0], $_[1], @_[2.. $#_]); }
  sub sprintf  { CORE::sprintf($_[0], $_[1], @_[2.. $#_]); }
  sub substr   { CORE::substr($_[0], $_[1], @_[2 .. $#_]); }
  sub uc       { CORE::uc($_[0]); }
  sub ucfirst  { CORE::ucfirst($_[0]); }
  sub unpack   { CORE::unpack($_[0], @_[1..$#_]); }
  sub undef    { $_[0] = undef }
  sub m        { [ $_[0] =~ m{$_[1]} ] }
  sub nm        { [ $_[0] !~ m{$_[1]} ] }
  sub s        { $_[0] =~ s{$_[1]}{$_[2]} }
  sub split    { [ split $_[1], $_[0] ] }

  sub primitive_Num { no warnings "numeric"; 0+$_[0] }
}

{ package FLOAT_and_INTEGER;
  use base "Num";
  sub abs   { CORE::abs($_[0]) }
  sub atan2 { CORE::atan2($_[0], $_[1]) }
  sub cos   { CORE::cos($_[0]) }
  sub exp   { CORE::exp($_[0]) }
  sub int   { CORE::int($_[0]) }
  sub log   { CORE::log($_[0]) }
  sub oct   { CORE::oct($_[0]) }
  sub hex   { CORE::hex($_[0]); }
  sub rand  { CORE::rand($_[0]) }
  sub sin   { CORE::sin($_[0]) }
  sub sqrt  { CORE::sqrt($_[0]) }

}
{ package FLOAT;
  use base "FLOAT_and_INTEGER";
  sub WHAT { "Num" }
}
{ package INTEGER;
  use base "FLOAT_and_INTEGER";
  use base "Int";

  sub WHAT { "Int" }
  sub to ($$) { $_[0] < $_[1] ? [$_[0]..$_[1]] : [CORE::reverse $_[1]..$_[0]]}
  sub upto ($$) { [ $_[0]..$_[1] ] }
  sub downto ($$) { [ CORE::reverse $_[1]..$_[0] ] }

  sub chr      { CORE::chr($_[0]); }
}
{ package ARRAY;
  use base "Array";
  sub WHAT {"Array"}

  sub shape { my $a = CORE::shift; 0+@$a } # ?
  sub end { my $a = CORE::shift; -1+@$a } # ?
  sub elems { my $a = CORE::shift; CORE::scalar @$a }
  sub delete { my $a = CORE::shift; @_ ? CORE::delete($a->[$_[0]]) : undef }
  sub exists { my $a = CORE::shift; @_ ? CORE::exists($a->[$_[0]]) : undef }
  sub pop   { CORE::pop @{$_[0]}; }
  sub shift { my $a = CORE::shift; CORE::shift(@$a) }
  sub push { my $a = CORE::shift; CORE::push(@$a,@_); $a }
  sub unshift { my $a = CORE::shift; CORE::unshift(@$a,@_) }
  sub splice {
    my $a = CORE::shift;
    my $offset = CORE::shift || 0;
    my $size = CORE::shift || 0;
    [CORE::splice(@{$a},$offset,$size,@_)]
  }
  sub keys { my $a = CORE::shift; [0..(@$a-1)] }
  sub kv { my $a = CORE::shift; my $idx = 0; [map{($idx++,$_)}@$a] }
  sub pairs { my $a = CORE::shift; my $idx = 0; [map{Pair->new("key"=>$idx++,"value"=>$_)}@$a] }
  sub values { my $a = CORE::shift; @$a }

  # Speculative

  sub clone { my $a = CORE::shift; [@$a] }

  # Non-spec

  sub grep  { my $arr = CORE::shift; my $sub = CORE::shift; [ CORE::grep { $sub->($_) } @$arr ]; }
  sub join  { my $arr = CORE::shift; my $sep = CORE::shift; CORE::join $sep, @$arr; }
  sub map  { my $arr = CORE::shift; my $sub = CORE::shift; [ CORE::map { $sub->($_) } @$arr ]; }
  sub reverse  { [ CORE::reverse @{$_[0]} ] }
  sub sort  { my $arr = CORE::shift; my $sub = CORE::shift() || sub { $a cmp $b }; [ CORE::sort { $sub->($a, $b) } @$arr ]; }
  sub max { my $arr = CORE::shift; my $max = $arr->[0]; foreach (@$arr) {$max = $_ if $_ > $max }; $max; }
  sub min { my $arr = CORE::shift; my $min = $arr->[0]; foreach (@$arr) {$min = $_ if $_ < $min }; $min; }

  sub concat { [map{@$_} @_] }

  # Internal

  sub flatten  { ( @{$_[0]} ) }
  sub flatten_recursively {
    map { my $ref = ref($_); ($ref && $ref eq "ARRAY") ? $_->flatten_recursively : $_ } @{$_[0]}
  }

}
{ package HASH;
  use base "Hash";
  sub WHAT {"Hash"}

  # randomness taken from autobox::Core

  sub delete  { my $hash = CORE::shift; my @res = (); CORE::foreach(@_) { push @res, CORE::delete $hash->{$_}; } CORE::wantarray ? @res : \@res }
  sub exists  { my $hash = CORE::shift; CORE::exists $hash->{$_[0]}; }
  sub keys  { [ CORE::keys %{$_[0]} ] }
  sub values  { [ CORE::values %{$_[0]} ] }

  sub each  {
    my $hash = CORE::shift;
    my $cb = CORE::shift;
    while((my $k, my $v) = CORE::each(%$hash)) {
      $cb->($k, $v);
    }
  }

  # spec

  sub kv { my $h = CORE::shift; [map{($_,$h->{$_})} CORE::keys %$h] }
  sub pairs { my $h = CORE::shift; [map{Pair->new("key"=>$_,"value"=>$h->{$_})} CORE::keys %$h] }

  # Speculative

  sub clone {
    my $h = CORE::shift;
    # Do not simplify this to "...ift; {%$h} }".  returns 0.  autobox issue?
    my $h1 = {%$h}; $h1
  }

  # Temporary

  sub dup { my $h = CORE::shift; my $h1 = {%$h}; $h1} # obsolete
}
{ package CODE;
  use base "Code";
  sub WHAT {"Code"}
}

{ package Any; sub Hash { $_[0] } } #X
{ package Hash; sub Hash { $_[0] } }
{ package Array; sub Array { $_[0] } }

{ package Any; #X
  sub postcircumfix__123_32_125 { @_ <= 2 ? $_[0]->{$_[1]} : do{$_[0]->{$_[1]} = $_[2]} }
  sub postcircumfix__60_32_62 { @_ <= 2 ? $_[0]->{$_[1]} : do{$_[0]->{$_[1]} = $_[2]} }
}
{ package HASH;
  sub postcircumfix__123_32_125 { @_ <= 2 ? $_[0]->{$_[1]} : do{$_[0]->{$_[1]} = $_[2]} }
  sub postcircumfix__60_32_62 { @_ <= 2 ? $_[0]->{$_[1]} : do{$_[0]->{$_[1]} = $_[2]} }
}
{ package ARRAY;
  sub postcircumfix__91_32_93 { @_ <= 2 ? $_[0]->[$_[1]] : do{$_[0]->[$_[1]] = $_[2]} }
}

{ package Match;
  sub postcircumfix__123_32_125 { @_ <= 2 ? $_[0]->{"match_hash"}->{$_[1]} : do{$_[0]->{"match_hash"}->{$_[1]} = $_[2]} }
  sub postcircumfix__60_32_62 { @_ <= 2 ? $_[0]->{"match_hash"}->{$_[1]} : do{$_[0]->{"match_hash"}->{$_[1]} = $_[2]} }
  sub postcircumfix__91_32_93 { @_ <= 2 ? $_[0]->{"match_array"}->[$_[1]] : do{$_[0]->{"match_array"}->[$_[1]] = $_[2]} }

  sub Str { $_[0]->{"match_string"} }
  sub Bool { $_[0]->{"match_boolean"} }
  sub Hash { $_[0]->{"match_hash"} }
  sub Array { $_[0]->{"match_array"} }
}

{ package Any; sub Bool { 1 } }
{ package UNDEF; sub Bool { undef } }
{ package ARRAY; sub Bool { @{$_[0]} ? 1 : undef } }
{ package HASH; sub Bool { %{$_[0]} ? 1 : undef } }
{ package FLOAT_and_INTEGER; sub Bool { $_[0] ? 1 : undef } }
{ package STRING; sub Bool { $_[0] ? 1 : undef } }

use warnings;

{ package Any; sub __make_not_empty_for_use_base{}}
{ package STRING; use base "Any";}
{ package INTEGER; use base "Any";}
{ package FLOAT; use base "Any";}
{ package ARRAY; use base "Any";}
{ package HASH; use base "Any";}
{ package CODE; use base "Any";}

{ package Private;
  # Taken from Perl6::Take 0.04.
  our @GATHER;
  sub gather (&) {local @GATHER = (@GATHER, []); shift->(); $GATHER[-1] }
  sub take (@) {push @{ $GATHER[-1] }, @_; undef }
}

{ package GLOBAL;
  { no warnings;
    *gather = \&Private::gather;
    *take   = \&Private::take;}

  our $a_ARGS = [map {encoding::decode("utf8",$_)} @ARGV];

  sub undef{undef}

  use Carp;
  sub slurp {
    my($file)=@_;
    local $/;
    open(my $fh,"<:utf8",$file);
    my $s = <$fh>;
    $s
  }
  sub unslurp {
    my($text,$file)=@_;
    open(my $fh,">:utf8",$file) or CORE::die $!;
    CORE::print $fh $text;
    close $fh;
  }
  sub file_exists{-e $_[0]}
  sub system{CORE::system(@_)}
  sub eval_runtime_code{
    my($p5,$env)=@_;
    if($env) { $env->($p5) }
    else {
      my $code = "package Main; ".$p5;
      my $res = eval($code); croak($@) if $@;
      $res
    }
  }
  sub eval_perl5{
    my($p5,$env)=@_;
    eval_runtime_code($p5,$env);
  }
  sub die{croak @_}
  sub exit{CORE::exit(@_)}
  sub defined{CORE::defined($_[0])}
  sub substr {CORE::substr($_[0],$_[1],$_[2])}
  sub not{$_[0] ? !($_[0]->Bool) : 1}
  sub exec{CORE::exec(@_)}
  sub sleep{CORE::sleep(@_)}

  sub split{[CORE::split($_[0],$_[1])]}
  sub push { my $a = CORE::shift; CORE::push(@$a,@_); $a }

  sub unlink{CORE::unlink(@_)}
  sub sprintf{CORE::sprintf(shift,@_)}
  sub print { CORE::print @_ }
  sub quotemeta { CORE::quotemeta($_[0]) }

  sub chmod_exe { CORE::chmod(0755,$_[0]) } # Hack for Compiler.

  sub private_tidy {
    eval("use Perl::Tidy");
    if ($@) { $_[0] }
    elsif ($ENV{ELF_DEV_NO_TIDY}) { $_[0] }
    else {
      my $source = $_[0];
      my $dest;
      Perl::Tidy::perltidy(argv=>[],source=>\$source,destination=>\$dest);
      $dest;
    }
  }

}

{ package GLOBAL;

  sub parser_format {"p5a"}
  sub parser_name{
    my $e = $ENV{ELF_STD_RED_RUN};
    return $e if $e;
    my $f = $0;
    $f =~ s/[^\/]+$//;
    # $f."elf_h_src/STD_red/STD_red_run"
    $f."../STD_red/STD_red_run"
  }

  our $a_INC = ["."];
  our $h_ENV = \%ENV;

  sub module_require {
    my($module)=@_;
    my $file = find_required_module($module);
    $file || CORE::die "Cant locate $module in ( ".CORE::join(" ",@$GLOBAL::a_INC)." ).\n";
    eval_file($file);
  };
  sub find_required_module {
    my($module)=@_;
    my @names = ($module,$module.".pm",$module.".p6");
    for my $dir (@$GLOBAL::a_INC) {
      for my $name (@names) {
        my $file = $dir."/".$name;
        if(-f $file) {
          return $file;
        }
      }
    }
    return undef;
  }
  sub import {
    my($module,@args)=@_;
    my $args = \@args;
    my $import = "if(defined(&".$module."::import)) { ".$module."->import(\$args); }";
    my $result = eval $import;
    Carp::confess($@) if $@;
    $result;
  }
  sub mkdir {
    my($dir) = @_;
    mkdir($dir);
  }

  our $compiler0;
  our $compiler1;
  our $parser0;
  our $parser1;
  our $ast2ir_0;
  our $ast2ir_1;
  our $emitter0;
  our $emitter1;

  sub eval_file {
    my($file)=@_;
    $GLOBAL::compiler0->eval_file($file);
  }
  sub eval_perl6 {
    my($code,$env)=@_;
    $GLOBAL::compiler0->eval_perl6($code,$env);
  }
  sub eval {
    my($code,$env)=@_;
    eval_perl6($code,$env);
  }
}

{ package Call; our $Api = 0; } # CallApi

{ package Fastundump;
  sub match {my($r,$s,$f,$t,$h)=@_; Match->make_from_rsfth($r,$s,$f,$t,$h)}
}
{ package GLOBAL;
  sub fastundump {my($dump)=@_; eval("package Fastundump; ".$dump);}
  sub mangle_name {my($name)=@_;
    $name =~ s/([^\w])/"_".CORE::ord($1)/eg;
    $name;
  }
  sub symbol_lookup : lvalue {
    my($name)=@_;
    my @parts = split(/::/,$name);
    $parts[0] =~ /\A([\$\@\%])(.*)/ || CORE::die("symbol_lookup: no sigil?: $name");
    $parts[0] = $2;
    my $sigil = $1;
    $parts[-1] = {"\$"=>"","\@"=>"a_","\%"=>"h_"}->{$sigil}.$parts[-1];
    my $var = join("::",@parts);
    no strict "refs";
    $$var;
  }
}

package Main;
';
  };

  method e($x) {
    my $ref = $x.WHAT;
    if $ref eq 'Undef' { $x }
    elsif $ref eq 'Str' || $ref eq 'Int' || $ref eq 'Num' { $x }
    elsif $ref eq 'Array' { $x.map(sub ($ae){$.e($ae)}) }
    else {$x.callback(self)}
  };


  method cb__CompUnit ($n) {
    $n.do_all_analysis();
    temp $whiteboard::current_emitter = self;
    say $whiteboard::current_emitter if 0; #X avoid "used only once".
    temp $whiteboard::in_package = [];
    temp $whiteboard::emit_pairs_inline = 0;
    temp $whiteboard::compunit_footer = [];
    temp $whiteboard::current_CallApi = 0;
    my $code = (
      "package Main;\n"~
      $.prelude_for_entering_a_package());
    my $stmts = $.e($n.statements);
    $stmts = $stmts.map(sub ($x){if defined($x) {$x} else {""}}); #XXX
    my $foot = $whiteboard::compunit_footer.join(";\n");
    $code ~ $stmts.join(";\n")~$foot~";\n";
  };
  method cb__Block ($n) {
    temp $whiteboard::emit_pairs_inline = 0;
    #'# '~$.e($n.notes<lexical_variable_decls>).join(" ")~"\n"~
    my $stmts = $.e($n.statements);
    $stmts = $stmts.map(sub ($x){if defined($x) {$x} else {""}}); #XXX
    '(do{'~$stmts.join(";\n")~'})'
  };

  method cb__Use ($n) {
    my $module = $.e($n.module_name);
    my $expr = $.e($n.expr);
    if $module eq 'v6-alpha' { "" }
    elsif $module eq 'v6' { "" }
    elsif $module eq 'lib' {
      my $name = $n.expr.buf;
      if $.compiler.hook_for_use_lib($name) { "" }
      else { "" }
    }
    elsif $.compiler.hook_for_use($module,$expr) { "" }
    else {
      "use " ~$module;
    }
  };
  method cb__ClosureTrait ($n) {
    temp $whiteboard::emit_pairs_inline = 0;
    $n.kind~'{'~$.e($n.block)~'}'
  };

  method cb__PackageDecl ($n) {
    my $in_package = [$whiteboard::in_package.flatten,$n.name];
    my $name = $in_package.join('::');
    my $base = 'use base "Any";';
    if $name eq 'Any' { $base = '' }
    if $name eq 'Object' { $base = '' }
    if $name eq 'Junction' { $base = '' }
    my $head = "\n\{ package "~$name~";\n";
    my $foot = "\n}\n";
    if $.using_Moose {
       $head = $head ~ "use Moose;"~" __PACKAGE__->meta->make_mutable();\n";
       $foot = ";\n__PACKAGE__->meta->make_immutable();\n"~ "\n}\n";
    } else {
    }
    $head = $head ~ $base~ $.prelude_for_entering_a_package();
    if $n.block {
      temp $whiteboard::in_package = $in_package; # my()
      $head ~ $.e($n.traits||[]).reverse.join("\n") ~ $.e($n.block) ~ $foot;
    } else {
      $whiteboard::in_package = $in_package; # not my()
      $whiteboard::compunit_footer.unshift($foot);
      $head ~ $.e($n.traits||[]).reverse.join("\n") ~ ";\n"
    }
  };
  method cb__Trait ($n) {
    if ($n.verb eq 'is' or $n.verb eq 'does') {
      my $pkgname = $whiteboard::in_package.join('::');
      my $name = $whiteboard::in_package.splice(0,-1).join('::')~'::'~$.e($n.expr);
      $name = $name.re_gsub('^::',''); # Moose 0.44 doesn't like these.
      "BEGIN\{unshift(@"~$pkgname~"::ISA,'"~$name~"');}\n";
    } else {
      say "ERROR: Emitting p5 for Trait verb "~$n.verb~" has not been implemented.\n";
      "***Trait***"
    }
  };

  method do_VarDecl_has ($n,$default) {
    my $name = $.e($n.var.name);
    if $.using_Moose {
      my $dflt = $default;
      if $dflt {
        $dflt = ', default => sub{ '~$default~" }"
      }
      "has '"~$name~"' => (is => 'rw'"~$dflt~");"
    } else {
      #my $code = "sub "~$name~': lvalue { $_[0]{'~"'"~$name~"'};}\n";
      my $x = '$_[0]{'~"'"~$name~"'"~'}';
      my $code = 'sub '~$name~' { if(@_==2){'~$x~'=$_[1]}else{'~$x~'}}';
      if $default {
        my $pkg = $whiteboard::in_package.join('::');
        $code = $code ~";\n"~ $.create_default_for($pkg,$name,$default);
      }
      $code;
    }
  };

  method cb__VarDecl ($n) {
    temp $whiteboard::emit_pairs_inline = 0;
    if ($n.scope eq 'has') {
      my $default = "";
      my $default_expr = $.e($n.default_expr);
      if $default_expr {
        $default = $default_expr;
      } else {
        if ($n.var.sigil eq '@') { $default = '[]' }
        if ($n.var.sigil eq '%') { $default = '{}' }
      }
      $.do_VarDecl_has($n,$default);
    } else {
      my $default = "";
      if $n.default_expr {
        if ((not($n.var.sigil eq '$')) &&
            $n.default_expr.isa('IRx1::Apply') &&
            ($n.default_expr.function eq 'circumfix:( )' ||
             $n.default_expr.function eq 'infix:,'))
        {
          my $pre = ''; my $post = '';
          if $n.is_array { $pre = '['; $post = ']' }
          if $n.is_hash  { $pre = '{'; $post = '}' }
          temp $whiteboard::emit_pairs_inline = 1;
          $default = ' = '~$pre~$.e($n.default_expr)~$post;
        } else {
          $default = ' = '~$.e($n.default_expr);
        }
      } else {
        if ($n.var.sigil eq '@') { $default = ' = [];' }
        if ($n.var.sigil eq '%') { $default = ' = {};' }
      }
      if ($n.is_context) {
        my $var = $n.var;
        my $nam = $.e($n.var);
        $nam = $nam.re_gsub_pat('^(.+)::','$1');
        ("\{package main; use vars '"~$nam~"'};"~
         'local'~' '~$.e($n.var)~$default)
      }
      elsif ($n.is_temp) {
        my $var = $n.var;
        my $nam = $.encode_varname($var.sigil,$var.twigil,$var.bare_name);
        my $pkg = $n.notes<crnt_package>;
        ("\{ package "~$pkg~"; use vars '"~$nam~"'};"~
        'local'~' '~$.e($n.var)~$default)
      }
      else {
        my $scope = $n.scope;
        if $scope eq 'constant' { $scope = 'my' }
        $scope~' '~$.e($n.var)~$default
      }
    }
  };


  method multimethods_using_hack ($n,$name,$param_types) {
    my $name = $.e($n.name);
    my $param_types = $n.multisig.parameters.map(sub ($p){
      my $types = $.e($p.type_constraints);
        if $types {
          if $types.elems != 1 { die("unsupported: parameter with !=1 type constraint.") }
          $types[0];
        } else {
          undef;
        }
    });
    my $type0 = $param_types[0];
    if not($type0) {
      die("implementation limitation: a multi method's first parameter must have a type: "~$name~"\n");
    }
    my $stem = '_mmd__'~$name~'__';
    my $branch_name = $stem~$type0;
    my $setup_name = '_reset'~$stem;
    my $code = "";
    $code = $code ~
    '
{ my $setup = sub {
    my @meths = __PACKAGE__->meta->compute_all_applicable_methods;
    my $h = {};
    for my $m (@meths) {
      next if not $m->{name} =~ /^'~$stem~'(\w+)/;
      my $type = $1;
      $h->{$type} = $m->{code}{q{&!body}};
    };
    my $s = eval q{sub {
      my $ref = ref($_[1]) || $_[1]->WHAT;
      my $f = $h->{$ref}; goto $f if $f;
      Carp::croak "multi method '~$name~' cant dispatch on type: ".$ref."\n";
    }};
    die $@ if $@;
    eval q{{no warnings; *'~$name~' = $s;}};
    die $@ if $@;
    goto &'~$name~';
  };
  eval q{{no warnings; *'~$setup_name~' = $setup;}};
  die $@ if $@;
  eval q{{no warnings; *'~$name~' = $setup;}};
  die $@ if $@;
};
';
    'sub '~$branch_name~'{my $self=CORE::shift;'~$.e($n.multisig)~$.e($n.block)~'}' ~ $code;
  };
  method multi_using_CM ($n,$is_method,$f_emitted) {
    my $name = $.e($n.name);
    my $enc_name = $.mangle_function_name($name);
    my $param_types = $n.multisig.parameters.map(sub ($p){
      my $types = $.e($p.type_constraints);
      if $types {
        if $types.elems != 1 { die("unsupported: parameter with !=1 type constraint.") }
        $types[0];
      } else {
        'Any'
      }
    });
    if $is_method {
      $param_types.unshift('Any');
    }
    my $sig = $param_types.map(sub ($t){
      # XXX C::M needs to be modified to work on both INTEGER and Int. :(
      if $t eq 'Any' { '*' }
      elsif $t eq 'Int' { '#' }
      elsif $t eq 'Num' { '#' }
      elsif $t eq 'Str' { '$' }
      else { $t }
    }).join(' ');
    'Class::Multimethods::multimethod '~$enc_name~
    ' => split(/\s+/'~",'"~$sig~"') => "~ $f_emitted ~';';
  };
  method cb__MethodDecl ($n) {
    my $body;
    if $n.traits && $n.traits[0].expr && $n.traits[0].expr eq 'p5' {
      $body = $n.block.statements[0].buf;
    }
    else {
      $body = $.e($n.block);
    }
    if $n.plurality && $n.plurality eq 'multi' {
      #$.multimethods_using_hack($n);
      my $ef = 'sub {my $self=CORE::shift;'~$.e($n.multisig)~$body~'}';
      $.multi_using_CM($n,1,$ef);
    }
    else {
      my $enc_name = $.mangle_function_name($.e($n.name));
      'sub '~$enc_name~'{my $self=CORE::shift;'~$.e($n.multisig)~$body~'}';
    }
  };

  method cb__SubDecl ($n) {
    temp $whiteboard::emit_pairs_inline = 0;
    my $name = $n.name;
    if $name { $name = $.e($name) } else { $name = "" }
    my $sig = $n.multisig;
    if $sig { $sig = $.e($sig) } else { $sig = "" }
    my $body;
    if $n.traits && $n.traits[0].expr && $n.traits[0].expr eq 'p5' {
      $body = $n.block.statements[0].buf;
    } else {
      $body = $.e($n.block);
    }
    if $n.plurality && $n.plurality eq 'multi' {
      my $ef = 'sub {'~$sig~$body~'}';
      $.multi_using_CM($n,0,$ef);
    } else {
      my $enc_name = $.mangle_function_name($name);
      'sub '~$enc_name~'{'~$sig~$body~'}';
    }
  };
  method signature_CallApi_handler ($n,$api_variable) { "" }
  method cb__Signature ($n) {
    temp $whiteboard::signature_declare_vars = [];
    temp $whiteboard::signature_inits = "";
    my $pl = $.e($n.parameters).join(",");
    my $decl = 'my('~$whiteboard::signature_declare_vars.join(",")~');';
    my $assign = '('~$pl~')=@_;'~$whiteboard::signature_inits~"\n";
    if ($n.parameters.elems == 0) { $decl = ""; $assign = ""; }
    ($decl~
     'if($Call::Api){my $__api__=$Call::Api;$Call::Api=undef;'~
     $.signature_CallApi_handler($n,'$__api__')~
     '}else{'~"\n"~
     $assign~"}\n");
  };
  method cb__Parameter ($n) {
    my $enc = $.e($n.param_var);
    $whiteboard::signature_declare_vars.push($enc);
    if $n.quant && $n.quant eq '*' {
      my $tmp = "@"~$n.param_var.name;
      $whiteboard::signature_declare_vars.push($tmp);
      $whiteboard::signature_inits = $whiteboard::signature_inits~"\n"~$enc~" = \\"~$tmp~";";
      $tmp;
    } else {
      if $n.default_expr {
        $whiteboard::signature_inits = $whiteboard::signature_inits~"\nif(!defined("~$enc~')){ '~$enc~' = '~$.e($n.default_expr)~' }';
      }
      $enc;
    }
  };
  method cb__ParamVar ($n) {
    my $s = $n.sigil;
    my $t = '';
    my $dsn = $.e($n.name);
    $.encode_varname($s,$t,$dsn);
  };

  method cb__Call ($n) {
    my $g;
    temp $whiteboard::emit_pairs_inline = 0;
    my $method = $.e($n.method);
    my $meth = $.mangle_function_name($method);
    if ($method eq 'postcircumfix:< >') {
      $.wrap_in_CallApi(0,$.e($n.invocant)~'->'~$meth~'('~"'"~$.e($n.capture)~"'"~')');
    } elsif ($method eq 'postcircumfix:( )') {
      $.wrap_in_CallApi(0,$.e($n.invocant)~'->'~'('~$.e($n.capture)~')');
    } else {
      $.wrap_in_CallApi(0,$.e($n.invocant)~'->'~$meth~'('~$.e($n.capture)~')');
    }
  };
  method mangle_function_name($name) {
     $name.split('(?<!fix)::').map(sub ($s){ #XX infix:<:=> sigh.
       my $s1 = $s.re_gsub_pat('^(\w+):(?!:)','${1}_'); #XX back compat
       mangle_name($s1)}).join('::')
     #$name = $name.re_gsub_pat('^(\w+):(?!:)','${1}_');
     #$name = mangle_name($name);
     #$name;
  }
  method wrap_in_CallApi ($api,$src) {
    #CAUTION: In perl 5.10.0, this
    #   return(do{local $buggy;$s->re_gsub(...)})  and this
    #   return(do{             $s->re_gsub(...)})
    # will occasionally behave differently.  For both local and my.
    # To reproduce, set $optimize_away = 0, and rebuild.  The symptom
    # is "Use of uninitialized value in concatenation (.) or string"
    # warnings and self-compilation failure.
    my $optimize_away = 1;
    if $api eq $whiteboard::current_CallApi && $optimize_away {
      $src
    } else {
      #X The \t is used in the "assignment to field" hack.
      '(do{local $Call::Api='~$api~';'~$src~"\t})";
    }
  }
  method cb__Apply ($n) {
    my $g;
    # temp $whiteboard::emit_pairs_inline = 0; #XXX depends on function :/
    my $fun = $.e($n.function);
    if $n.notes<lexical_bindings>{'&'~$fun} {
       my $fe = $.mangle_function_name($fun);
       return $.wrap_in_CallApi(0,''~$fe~'('~$.e($n.capture)~')');
    }
    if $g = $fun.re_groups('^infix:(.+)$') {
      my $op = $g[0];
      my $args = $n.capture.arguments;
      if $args.elems == 1 && $args[0].isa('IRx1::Apply') && $args[0].function eq 'infix:,' {
        $args = $args[0].capture.arguments;
      }
      my $a = $.e($args);
      my $l = $a[0];
      my $r = $a[1];
      if $a.elems && !defined($a[-1]) { $a.pop }
      if ($op eq '~') {
        my $s = $a.shift;
        while $a.elems { $s = "("~$s ~" . "~ $a.shift~")" }
        return $s;
      }
      if ($op eq ',') {
        my $s = $a.shift;
        while $a.elems { $s = $s ~", "~ $a.shift }
        return $s;
      }
      if ($op eq '=') {
        # assignment to field.
        if $args[0].isa("IRx1::Var") {
          my $t = $args[0].twigil;
          if ($t && $t eq '.') {
            return $l~'('~$r~')'
          }
        }
        if ($args[0].isa("IRx1::Call"))
        {
          if $args[0].capture.arguments.elems == 0 {
            my $meth = $.mangle_function_name($args[0].method);
            return $.wrap_in_CallApi(0,$.e($args[0].invocant)~'->'~$meth~'('~$r~')');
          } else {
            my $call = $.e($args[0]);
            my $g = $call.re_groups('\A((?s:.)*?)((?:\)\t\})?\))\z');
            if not $g { die "bug" }
            my $lvalue_call = $g[0]~', '~$r~$g[1];
            if $call eq $lvalue_call { die "bug" }
            return $lvalue_call;
          }
        }
      }
      if $op.re_matchp('^(\+|-|\*|\/)$') {
        my $s = $a.shift;
        while $a.elems { $s = "("~$s ~" "~$op~" "~ $a.shift~")" }
        return $s;
      }
      if $op.re_matchp('^(!=|ne|=|=~)$') && $a.elems == 2 {
        my $s = "("~$a.shift~" "~$op~" "~$a.shift~")";
        return $s;
      }
      if $op.re_matchp('^(<|>|==|lt|gt|eq)$')  && $a.elems == 2 {
        #X arity > 2 is obvious but unimplemented.
        my $s = "("~$a.shift~" "~$op~" "~$a.shift~")";
        return $s;
      }
      if $op.re_matchp('^(\|\||\&\&|and|or)$') {
        # Flatten args - STD_red gives &&(&&(2,3),4) vs STD's &&(2,3,4).
        my $argsf = $args.clone;
        while $argsf[0].WHAT eq 'IRx1::Apply' && $argsf[0].function eq $n.function {
          $argsf.unshift($argsf.shift.capture.arguments.flatten);
        }
        my $a = $.e($argsf);

        my $s1 = "";
        my $s2 = "";
        my $stop = ""; if $op eq '&&' || $op eq 'and' { $stop = '!' }
        my $a_end = $a.pop;
        while $a.elems {
          $s1 = ($s1~'$__tmp__=('~$a.shift~');if('~
                 $stop~'$__tmp__->Bool){no warnings "void";$__tmp__}else{');
          $s2 = $s2~'}';
        }
        my $s = ('(do{my '~$s1~
                 '$__tmp__=('~$a_end~');no warnings "void";$__tmp__'~
                 $s2~'})');
        return $s;
      }
    }
    elsif $g = $fun.re_groups('^prefix:(.+)$') {
      my $op = $g[0];
      my $a = $.e($n.capture.arguments);
      my $x = $a[0];
      if $op.re_matchp('^(-)$') {
        return  "("~$op~""~$x~")"
      }
    }
    elsif $g = $fun.re_groups('^statement_prefix:(.+)$') {
      my $op = $g[0];
      if $op eq 'do' {
        return 'do{'~$.e($n.capture.arguments[0])~'}'
      } elsif $op eq 'try' {
        return 'eval{'~$.e($n.capture)~'}'
      } elsif $op eq 'gather' {
        return 'GLOBAL::gather'~$.e($n.capture)~''
      } else {
        die $fun~': unimplemented';
      }
    }
    elsif $g = $fun.re_groups('^postfix:(.+)$') {
      my $op = $g[0];
      my $a = $.e($n.capture.arguments);
      my $x = $a[0];
      if $op.re_matchp('^(\+\+)$') {
        return "("~$x~""~$op~")"
      }
    }
    elsif $g = $fun.re_groups('^circumfix:(.+)') {
      my $op = $g[0];
      if $op eq '< >' {
        my $s = $n.capture.arguments[0];
        my $words = $s.split('\s+');
        if $words.elems == 0 {
          return '[]'
        } else {
          return "['"~$words.join("','")~"']"
        }
      }
      elsif $op.re_matchp('^(\( \)|\[ \])$') {
        my $arg = $.e($n.capture);
        return $op.re_gsub(' ',$arg);
      }
    }
    elsif ($fun eq 'self') {
      return '$self'
    }
    elsif ($fun eq 'next') {
      return 'next'
    }
    elsif ($fun eq 'last') {
      return 'last'
    }
    elsif ($fun eq 'return') {
      return 'return('~$.e($n.capture)~')';
    }
    elsif ($fun.re_matchp('^\$\w+$')) {
      return $.wrap_in_CallApi(0,$fun~'->('~$.e($n.capture)~')');
    }
    elsif ($fun.re_matchp('^sub\s*{')) {
      return $.wrap_in_CallApi(0,'('~$fun~')->('~$.e($n.capture)~')');
    }
    elsif $fun eq 'eval' {
      my $env = 'sub{my$s=eval($_[0]);Carp::carp($@)if$@;$s}';
      return 'GLOBAL::'~$fun~'('~$.e($n.capture)~','~$env~')'
    }
    elsif $fun eq '_inline_p5' { # Use is strongly discouraged.
      return $n.capture.arguments[0].buf;
    }

    if $fun.re_matchp('^\w') {
      my $fe = $.mangle_function_name($fun);
      return $.wrap_in_CallApi(0,'GLOBAL::'~$fe~'('~$.e($n.capture)~')');
    }
    else {
      return $.wrap_in_CallApi(0,$fun~'('~$.e($n.capture)~')');
    }
  };
  method cb__Capture ($n) {
    # temp $whiteboard::emit_pairs_inline = 0; XXX?
    my $a = $.e($n.arguments||[]).join(",");
    if $n.invocant {
      my $inv = $.e($n.invocant);
      if $a { $inv~", "~$a }
      else { $inv }
    }
    else { $a }
  };

  method cb__For ($n) {
    my $push = "";
    if $n.expr.WHAT ne 'IRx1::Apply' { $push = "->flatten"};
    my $pull = "";
    if $n.block.WHAT eq 'IRx1::SubDecl' { $pull = '->($_)'};
    'for(('~$.e($n.expr)~')'~$push~")\{\n"~$.e($n.block)~$pull~"\n}"
  };
  method cb__Loop ($n) {
    'while(('~$.e($n.pretest)~")->Bool) \{\n"~$.e($n.block)~"\n}"
  };
  method cb__Cond ($n) {
    my $els = '';
    if $n.default { $els = "else \{\n"~$.e($n.default)~"\n}" }
    my $clauses = $.e($n.clauses);
    my $first = $clauses.shift;
    my $first_test = $first[0];
    $first_test = "("~$first_test~")->Bool";
    if $n.invert_first_test { $first_test = "not("~$first_test~")" }
    ('if('~$first_test~") \{\n"~$first[1]~"\n}"
    ~$clauses.map(sub ($e){'elsif(('~$e[0]~")->Bool) \{\n"~$e[1]~"\n}"}).join("")
    ~$els)
  };
  method cb__Given ($n) {
    my $expr = $.e($n.expr);
    my $stmts = $.e($n.block.statements).map(sub ($s) {  #XX so When's elsif's work.
      if $s.re_matchp('\)\z') {$s~';'} else {$s} }).join("\n");
    '(do{ local $_ = '~$expr~";\n"~'if(0){}'~"\n"~$stmts~"\n})";
  }
  method cb__When ($n) {
    my $expr = $.e($n.expr);
    my $test;
    if not defined $expr { $test = "1" } # default {}
    elsif $n.expr.is_whatever { $test = "1" } # when * {}
    else { $test = '$_ eq '~$expr~'' }
    my $block = $.e($n.block);
    'elsif('~$test~') {'~$block~"}";
  }

  method encode_varname($s,$t,$dsn) {
    #XXX $pkg::x -> s_pkg::x :(
    my $env = '';
    my $pre = '';
    if $t eq '+' { $env = 'x' };
    if $s eq '$' && $env eq 'x' { $pre = 's_' };
    if $s eq '@' { $pre = 'a_' }
    if $s eq '%' { $pre = 'h_' }
    my $mn = $dsn.split('::').map(sub ($nam){mangle_name($nam)}).join("::");
    my $name = $env~$pre~$mn;
    if ($t eq '.') {
      '$self->'~$name
    } elsif ($t eq '+') {
      $name = $name.re_gsub('::','__');
      '$'~'::'~$name
    } elsif ($t eq '*') {
      $name = $name.re_gsub('::','__');
      '$'~'GLOBAL::'~$name
    } else {
      '$'~$name
    }
  };

  method cb__Var_selective_override ($n) { undef }

  method cb__Var ($n) {
    my $override = $.cb__Var_selective_override($n);
    if defined($override) { return $override; }
    my $s = $n.sigil;
    my $t = $n.twigil||'';
    if $n.is_context { $t = '+' }
    my $dsn = $.e($n.name);
    my $v = $s~$t~$dsn;
    if $v eq '$?PACKAGE' || $v eq '$?MODULE' || $v eq '$?CLASS' {
      my $pkgname = $whiteboard::in_package.join('::'); # XXX should use $n notes instead.
      $pkgname = $pkgname || 'Main';
      "'"~$pkgname~"'"
    } elsif $v eq '$?FILE' {
      "'"~$.filename~"'"
    } elsif $v eq '$?LINE' {
      '0' # XXX $n notes needs to provide this.
    } elsif $v eq '$?PERLVER' {
      "'elf / "~ primitive_runtime_version() ~ " / " ~ $.WHAT ~"'"
    } elsif $v eq '$¢' {
      $.encode_varname($s,$t,"GLOBAL::_cursor");
    } else {
      $.encode_varname($s,$t,$dsn);
    }
  };
  method cb__NumInt ($n) {
    $.e($n.text)
  };
  method cb__Hash ($n) {
    temp $whiteboard::emit_pairs_inline = 1;
    '{'~$.e($n.hash||[]).join(",")~'}'
  };
  method cb__Buf ($n) {
    my $s = $n.buf;
    '"' ~ quotemeta($s) ~ '"';
  };
  method cb__Rx ($n) {
    my $pat = $n.pat || '';
    'qr/'~$pat~'/'
  };
  method cb__Pair($n) {
    if $whiteboard::emit_pairs_inline {
      temp $whiteboard::emit_pairs_inline = 0;
      '('~$.e($n.key)~' => '~$.e($n.value)~')'
    } else {
      "Pair->new('key',"~$.e($n.key)~" => 'value',"~$.e($n.value)~")"
    }
  };

};

if not($*emitter0) { $*emitter0 = EmitSimpleP5.new}
$*emitter1 = EmitSimpleP5.new;
