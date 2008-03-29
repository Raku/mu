#!/usr/bin/perl -w
# This script writes the elf_one p6 code.
# It may also run that freshly generated code.

# Issues

# Notes
use strict;
use warnings;

require 'ir_nodes.pl';
our @ir_nodes = IR_Zero_Def::nodes();

main();

sub main {
    my $output_file = 'elf_one.p6';
    my $code = file_code();
    open(F,">$output_file") or die $!;
    print F $code; close(F);
    my $cmd = "./elf_zero -c $output_file -o elf_one";
    print STDERR '#',$cmd,"\n";
    system($cmd) or die $?;
    if(@ARGV) {
      exec("./$output_file",@ARGV)
    }
    exit();
}

sub file_code {
    (header().
     scraper_builder().
     ir_nodes().
     emit_p5().
     program()
    );
}

sub header { <<'END'; }
#!/usr/bin/env six
# WARNING - this file is mechanically generated.  Your changes will be overwritten.
# Usage: --help

class Strscan {
  # ...
}

class Tempfile {
  # ...
}

END

sub scraper_builder { <<'END'; }

class Scrape {
  has $.scanner;
  has $.string;    
  # ...
}

class BuildIR {
  # ...
}

END

sub ir_nodes {
    my $base = <<'END';
  class IR0::Base {
  }
  class IR0::Val_Base is Base {
  }
  class IR0::Lit_Base is Base {
  }
  class IR0::Rule_Base is Base {
  }
END
    my $nodes = "";
    for my $node (@ir_nodes) {
	my $name = $node->name;
	my $base = 'Base';
	$base = "${1}_Base" if $name =~ /([^_]+)_/;
	my(@idents);
	for my $field ($node->fields) {
	    my $fname = $field->identifier;
	    push(@idents,$fname)
	}
	my $has = join("",map{"has \$.$_;\n    "} @idents);
	my $params = join(",",map{"\$$_"} @idents);
	my $init = "";
	for my $ident (@idents) {
	    $init .= "\$.$ident = \$$ident;\n      ";
	}
	$nodes .= <<"END"
  class IR0::$name is $base {
    $has
    method new($params) {
      $init
    }
    method emit(\$emitter) { \$emitter.emit_$name(self) }
  }
END
    }
    <<"END";
$base
$nodes
# Constructors
#...
END
}

sub emit_p5 {
    <<'END';
class EmitSimpleP5 {
  #...
}
END
}

sub program { <<'END'; }
    
class Program {
  #...
}
Program.new().main(%ARGV)
END

