#!/usr/bin/perl
use strict;
use warnings;

my $def = <<'END_DEF';

RxPat5 pat
RxExact text
RxMod_expr mods expr
RxMod_inline mods
RxBackref backref_n
RxCap expr
RxGrp expr
RxAlias target target_spec expr
RxQuant min max expr nongreedy
RxAlt exprs
RxConj exprs
RxSeq exprs
RxASpace aspace_inpkg text
RxSubrule created_in_pkg name exprs neg nocap
RxARegex modpat mods expr
RxBiind created_in_pkg name expr
RxNamespace created_in_pkg nsname bindings pkg
RxCode code
RxCodeRx code
RxIndependent expr
RxConditional test expr_then expr_else
RxLookaround is_forward is_positive expr
RxCommitSequence
RxCommitGroup
RxCommitRegex
RxCommitMatch

RxCategory name
RegexCategoryDecl name rx

END_DEF

sub write_ir_info5 {
  my($file)=@_;
  my $code = <<'END_CODE';
# Warning: This file is mechanically written.  Your changes will be overwritten.
package IRx1_Info::Node;

our @extra_fields = qw( match );
sub new {
  my($cls,$name,$fields)=@_;
  bless {
    name => $name,
    fields => $fields,
    all_fields => [@extra_fields,@$fields],
  }, $cls;
}
sub name { shift->{name} }
sub fields { @{shift->{fields}} }
sub all_fields { @{shift->{all_fields}} }

package IRx1_Info;
my $def = <<'END_DEF';
<<DEF>>
END_DEF

# IR nodes
our @nodes;
our %node_index;

sub nodes { @nodes }
sub node_named { my($cls,$name)=@_; $node_index{$name} }

sub load_ir_node_config {
  my $node_class = __PACKAGE__.'::Node';
  my $ir_config = $def;
  for my $line (split(/\n/,$ir_config)) {
    next if $line =~ /^\s*$|^\s*\#/;
    $line =~ s/#.*//;
    my($name,@fields)=eval('qw{'.$line.'}');
    my $node = $node_class->new($name,\@fields);
    push(@nodes,$node);
    $node_index{$name} = $node;
  }
}
load_ir_node_config();

1;
__END__
END_CODE
    $code =~ s/<<DEF>>/$def/;
    text2file($code,$file);
}


sub write_ir_nodes {
  my($file)=@_;
  my $code = "".unindent(<<'  END');
    # Warning: This file is mechanically written.  Your changes will be overwritten.
    package IRx1 {

      class RxBaseClass is Base {
      }

  END

  for my $node (IRx1_Info->nodes) {
    my($name,@fields)=($node->name,$node->fields);
    my @all = $node->all_fields;
    
    my $base = 'RxBaseClass';
    $base = "RxMixin${1}" if $name =~ /Rx([^_]+)_/;
    my $has = join("",map{"has \$.$_;\n        "} @all,'notes');
    my $params = join(',',map{"\$$_"}@all);
    my $init = join(', ',map{"'$_', \$$_"} @all);
    my $field_names = join(',',map{"'$_'"}@fields);
    my $field_values = join(',',map{'$.'.$_}@fields);

    $code .= unindent(<<"    END",'  ');
      class $name is $base {
        $has
        method newp($params) { self.new($init) }
        method callback(\$emitter) { \$emitter.cb__$name(self) }
        method node_name() { '$name' }
        method field_names() { [$field_names] }
        method field_values() { [$field_values] }
        method irx1_describe() {
          @{["'".$name."('~".join("','~",(map{'$.'.$_.'.irx1_describe~'}@fields))."')'"]}
        }

      }
    END
  }
  $code .= unindent(<<'  END');
    }
  END
  text2file($code,$file);
}


sub text2file {
  my($text,$file)=@_;
  open(F,">$file") or die "open() failed on $file: $!";
  print F $text;
  close F;
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

write_ir_info5("./nodes_info.p5");
require "./nodes_info.p5";
write_ir_nodes("./nodes.pm");
