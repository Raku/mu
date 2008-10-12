#!/usr/bin/perl
use strict;
use warnings;

my $output_file = "./IRx1_FromAST2.pm";
my $def = <<'END_DEF';
comp_unit
CompUnit.newp($m<statementlist>)

statementlist
$m<statement>

statement
$m<EXPR>

EXPR
$m<noun>

noun
$m<term> || $m<value>

value
*1*

number
*1*

integer
NumInt.newp(*text*)

term
if $o<args> {
  my $ident = 'sub_name_missing_from_STD_ast'; #XXX :(
  my $args = $m<args>;
  Apply.newp($ident,Capture.newp($args||[]))
}

args
$m<in> || $m<arglist>

arglist
# Actually an arg :/
$m<EXPR>

in
*1*

semilist
$m<statement> # actuall a statement list :(




END_DEF

my $header_code = <<'END_CODE';
# Warning: This file is mechanically written.  Your changes will be overwritten.

class IRx1_Build2 {
  has $.constructors;
  method add_constructor($k,$constructor) {
    if $.constructors {} else {
      my $h = {};
      $.constructors = $h;
    }
    $.constructors{$k} = $constructor;
  };
  method make_ir_from_Match_tree($m) {
    my $rule = $m.rule;
    my $constructor = $.constructors{$rule};
    if $constructor {
      $constructor.($m);
    } else {
      my $g = $rule.re_groups('\A([^:]+):');
      if $g { $constructor = $.constructors{$g[0]} }
      if $constructor {
        $constructor.($m);
      } else {
        die "Unknown rule: "~$rule~"\nIt needs to be added to ast_handlers.\n";
      }
    }
  };
};
class Match {
  method make_ir_from_Match_tree() {
    $main::irbuilder.make_ir_from_Match_tree(self)
  }
};
class ARRAY {
  method make_ir_from_Match_tree() {
    self.map(sub($e){$e.make_ir_from_Match_tree()})
  }
};
class STRING {
  method make_ir_from_Match_tree() {
    self
  }
};
class INTEGER {
  method make_ir_from_Match_tree() {
    self
  }
};
class FLOAT {
  method make_ir_from_Match_tree() {
    self
  }
};
class UNDEF {
  method make_ir_from_Match_tree() {
    self
  }
};

END_CODE


sub write_ast_handlers {
  my($def,$file)=@_;
  my $paragraphs = sub {
    my($text)=@_;
    $text =~ s/^[ ]*#.*\n//mg;
    $text =~ s/[ ]*#.*//g;
    $text =~ s/^([ \t]*\n)*//;
    my @x = split(/\n\n+/,$text);
    @x;
  };
  my @paragraphs = $paragraphs->($def);

  my $code = $header_code.unindent(<<'  END');
    package IRx1_Build2 {
      sub irbuild_ir ($x) {
        $x.make_ir_from_Match_tree()
      };

  END
  my $init = "";

  my %seen;
  for my $para (@paragraphs) {
    $para =~ /^(\w+(?::[\S]+)?)\n(.*)/s or die "bug\n$para";
    my($name,$body)=($1,$2);
    die "Saw an AST handler for '$name' twice!\n" if $seen{$name}++;

    $body =~ s/\bir\(/irbuild_ir\(/g;
    $body =~ s/(\$m(?:<\w+>)+)/irbuild_ir($1)/g;
    $body =~ s/\$o((?:<\w+>)+)/\$m$1/g;
    $body =~ s/<(\w+)>/.hash{'$1'}/g;
    $body =~ s/([A-Z]\w+\.new\w*)\(/IRx1::$1(\$m,/g;
    $body =~ s/\*text\*/(\$m.match_string)/g;
    if ($body =~ /\*1\*/) {
      $body =~ s/\*1\*/\$one/g;
      $body = unindent(<<'      END',"  ").$body;
        my $key;
        for $m.hash.keys {
          if $_ ne 'match' {
            if $key {
              die("Unexpectedly more than 1 field - dont know which to choose\n")
            }
            $key = $_;
          }
        }
        my $one = irbuild_ir($m.hash{$key});
      END
    }

    my $fname = $name;
    $fname =~ s/(\W)/"_".ord($1)/eg;
    $code .= "\n".unindent(<<"    END","    ");
      my \$construct_$fname = sub (\$m) {
        $body;
      };
    END
    $init .= "".unindent(<<"    END","    ");
      \$.add_constructor('$name', \$construct_$fname);
    END

  }
  $code .= unindent(<<"  END");
    method init {

  END
  $code .= unindent(<<"  END");
      $init
      self;
    }; # end init
  };

  END
  $code .= unindent(<<'  END');

  if not($*ast2ir_0) { $*ast2ir_0 = IRx1_Build.new.init; }
  $*ast2ir_1 = IRx1_Build.new.init;

  END
  open(F,">$file") or die $!; print F $code; close F;
}

#XXX doesn't actually work.
sub unindent {
  my($s,$leave_indent)=@_;
  $leave_indent ||= "";
  $s =~ /^( *)$leave_indent/;
  my $indent = $1;
  $s =~ s/^$indent//mg;
  $s;
}

write_ast_handlers($def,$output_file);
