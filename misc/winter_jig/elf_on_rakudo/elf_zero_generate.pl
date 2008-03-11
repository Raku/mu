#!/usr/bin/perl -w
# This script writes the elf_zero ruby code.
# It may also run that freshly generated code.

# Issues
#  using Do for statementlist.  Which is perhaps not right?  Is there a statementlist node?
#  distinction between dump's Grammar nodes and nodes which are explicity Hash is lost.
#   what were the Hash's again?
#  should avoid using class methods, to simplify p6.

# Notes
#  some of the code is a bit weird because it's already being tuned for later
#    translation into p6.  though also because it's a crufty first fast draft.
use strict;
use warnings;

require 'ir_nodes.pl';
our @ir_nodes = IR_Zero_Def::nodes();

main();

sub main {
    my $output_file = 'elf_zero';
    my $code = file_code();
    open(F,">$output_file") or die $!;
    print F $code; close(F);
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
#!/usr/bin/env ruby
# WARNING - this file is mechanically generated.  Your changes will be overwritten.
# Usage: --help

require 'strscan'
require 'tempfile'

END

sub scraper_builder { <<'END'; }

class Scrape
  attr_accessor :scanner,:string
  def initialize
  end
  def scrape(string)
    @string=string
    @scanner=StringScanner.new(string)
    eat(/"parse" => /)
    scrape_thing
  end
  def where
    pos = @scanner.pos
    fm = pos - 20; fm = 0 if fm < 0
    to = pos + 20; to = @string.length if to > @string.length
    sample = @string.slice(fm,to-fm).tr("\n\t\r",'NTR')
    arrow = ' '*(pos - fm - 1) + '^'
    before = @string.slice(0,pos)
    line = before.lines.to_a.size
    before =~ /(.*)\z/
    column = $1.length + 1
    "#{pos}  #{line}:#{column}\n>#{sample}<\n>#{arrow}\n"
  end
  def scan(re)
    @scanner.scan(re)
  end
  def eat(re)
    @scanner.scan(re) or raise "failed to eat #{re} at #{where}"
  end
  def scrape_thing
    if scan(/(?=PMC 'Perl6::Grammar')/)
      scrape_node
    elsif scan(/(?=PMC 'PGE::Match')/)
      scrape_node
    elsif scan(/(?=ResizablePMCArray)/)
      scrape_array
    elsif scan(/(?=Hash)/)
      scrape_hash
    elsif scan(/(?=\")/)
      scrape_string
    elsif scan(/(?=\d)/)
      scrape_int
    elsif scan(/(?=PMC 'Sub')/)
      scrape_rest_of_line
    elsif scan(/(?=\\parse)/)
      scrape_rest_of_line
    else
      raise "scrape failed at #{where}"
    end
  end
  def scrape_rest_of_line
    eat(/[^\n]+?(?=,?\n)/); :ignored
  end
  def scrape_int; eval eat(/\d+/) end
  def scrape_string
    p = @scanner.pos
    eat(/\"/); eat(/([^\\"]|\\.)*/); eat(/\"/)
    s = @string.slice(p+1,@scanner.pos-p-2)
    s
  end
  def scrape_hash
    h = {}
    eat(/Hash {\n/)
    indentation = eat(/ +/)
    while s= scan(/\"\w+\" => /)
      s =~ /^\"(\w+)/ or raise "bug"
      k = $1
      v = scrape_thing
      h[k]=v
      eat(/,?\n/)
      eat(/ */) == indentation or scan(/(?=\})/) or raise "assert: maybe a bug #{@scanner.pos}"
    end
    eat(/\}/)
    h
  end
  def scrape_array
    a = []
    eat(/ResizablePMCArray \(size:\d+\) \[\n/)
    indentation = eat(/ +/)
    while not scan(/(?=\])/)
      a.push scrape_thing
      eat(/\n/)
      eat(/ */) == indentation or scan(/(?=\])/) or raise "assert: maybe a bug #{@scanner.pos}"
    end
    eat(/\]/)
    a
  end
  def scrape_node
    o = {}
    type = eat(/PMC 'Perl6::Grammar' => |PMC 'PGE::Match' => /)
    if type =~ /Match/
      o['_rakudo_type'] = 'match'
    end
    str = scrape_string
    eat(/ @ /)
    eat(/\d+/)
    if scan(/ {\n/)
      indentation = eat(/ +/)
      while s= scan(/<\w+> => |\[\d+\] => /)
        s =~ /^<(\w+)> => |\[(\d+)\] => / or raise "bug"
        k = $1 || $2
        v = scrape_thing
        o[k]=v if v != :ignored
        eat(/\n/)
        eat(/ */) == indentation or scan(/(?=\})/) or raise "assert: maybe a bug #{@scanner.pos}"
      end
      eat(/\}/)
    else
      o['text']=str
    end
    o
  end
end

class BuildIR
  attr_accessor :ast_data_file,:tag_map
  def initialize(ast_data_file)
    @ast_data_file=ast_data_file
    load_data_file
  end
  def tag_tree(tree)
    @saw_tags = {}
    tag_hash('parse',tree)
  end
  def tag_hash(name,h)
    tag = nil
    if h['_rakudo_type'] == 'match'
      h.delete '_rakudo_type'
      tag = 'match'
    else
      ks = h.keys.sort
      tag = "#{name}___#{ks.join('__')}"
    end
    h['_fields'] = h.keys.sort.join(' ')
    h['_tag'] = tag
    @saw_tags[tag] = true;
    h.each{|k,v|
      if v.is_a? Hash
        tag_hash(k,v)
      elsif v.is_a? Array
        v.each{|e| tag_hash(k,e) }
      end
    }
    h
  end
  def load_data_file
    @tag_map = {}
    lines = File.open(ast_data_file,'r'){|f| f.lines.to_a }
    lines = lines.select{|line| not line =~ /^\s*\#/ }
    lines.each{|line|
      line.sub!(/#.*$/,'')
      line =~ /^(\w+)\s+\|\s*(\S*)\s*$/ or raise "Broken line in #{ast_data_file}:\n#{line}"
      @tag_map[$1] = $2;
    }
  end
  def tag_report
    missing = @saw_tags.keys.select{|k| not tag_map.key? k}
    unspec = []
    tag_map.each{|k,v| unspec.push(k) if v == '' }
    out = ''
    out += "\nUnknown keys:\n"+missing.sort.join("\n")+"\n\n";
    out += "\nUnspeced keys:\n"+unspec.sort.join("\n")+"\n\n";
    out
  end
  def ir_from(tree)
    if tree.is_a? Array
      return tree.map{|e| ir_from(e)}
    elsif tree.is_a? String
      return tree
    end
    tag = tree['_tag']
    act = tag_map[tag]
    ret = nil
    debug = false
    if debug
      p "----"
      p tree
      p tag
      p act
    end
    if not act
      STDERR.print "# Warning: Faking tag #{tag} in #{tree}\n"
      ret = nil
    else
      fields = tree['_fields'].split(/\s+/)
      if act == 'pass'
        STDERR.print "# Warning: passing only first field of #{tag}\n" if fields.size > 1
        ret = ir_from(tree[fields[0]])
      elsif act == 'pass0'
        ret = ir_from(tree[fields[0]])
      else
        method = "#{act}_from__#{fields.join('__')}".to_sym
        down = tree.values_at(*fields)
        args = down.map{|n| ir_from(n) }
        ret = BadIR.send(method,*args)
      end
    end
    p "#{tag} -->",ret if debug
    ret
  end
end

END

sub ir_nodes {
    my $base = <<'END';
  class Base
  end
  class Val_Base < Base
  end
  class Lit_Base < Base
  end
  class Rule_Base < Base
  end
END
    my $nodes = "";
    for my $node (@ir_nodes) {
	my $name = $node->name;
	my $base = 'Base';
	$base = "${1}_Base" if $name =~ /([^_]+)_/;
	my(@symbols,@params,@attribs);
	for my $field ($node->fields) {
	    my $fname = $field->identifier;
	    push(@symbols,':'.$fname);
	    push(@attribs,'@'.$fname);
	    push(@params,$fname);
	}
	my $symbols = join(",",@symbols);
	my $params = join(",",@params);
	my $attribs = join(",",@attribs);
	my $init = $attribs eq '' ? "" : "$attribs = $params";
	$nodes .= <<"END"
  class $name < $base
    attr_accessor $symbols
    def initialize($params)
      $init
    end
    def emit(emitter); emitter.emit_$name(self) end
  end
END
    }
    <<"END";
module BadIR
$base
$nodes
# Constructors
def self.CompUnit_from__statement_block(b); CompUnit.new(nil,nil,nil,nil,nil,b) end
def self.Do_from__statementlist(sl); Do.new(sl) end
def self.Val_Int_from__text(s); Val_Int.new(s) end
end
END
}

sub emit_p5 {
    <<'END';
class EmitSimpleP5
  def emit(ir)
    ir.emit(self)
  end
  def emit_CompUnit(n); n.body.emit(self) end
  def emit_Do(n); n.block.map{|statement| statement.emit(self)+";\n"}.join("") end
  def emit_Val_Int(n); n.int end
  def method_missing(m,*a,&b)
    STDERR.print "# WARNING: Can't #{m}: FAKING IT.\n"
    '(fake())'
  end
end
END
}

sub program { <<'END'; }
    
class Program
  def print_usage_and_exit
    STDERR.print <<'end'; exit(2)
Usage: OPTIONS [ P6_FILE | -e P6_CODE ]

OPTIONS are as yet undefined.
end
  end
  def main(argv)
    p6_code = nil
    arg = argv.shift
    if not arg
      print_usage_and_exit
    elsif arg == '-e'
      p6_code = argv.shift or print_usage_and_exit
    elsif File.exists? arg
      p6_code = File.open(arg,'r'){|f|f.read}
    else
      print_usage_and_exit
    end
    print "\n"
    build = BuildIR.new('rakudo_ast.data')
    dump = parse(nil,p6_code)
    print dump
    tree = Scrape.new.scrape(dump)
    p tree
    ast = build.tag_tree(tree)
    p ast
    print build.tag_report
    ir = build.ir_from(ast)
    p ir
    p5 = EmitSimpleP5.new.emit(ir)
    print "\n----\n"
    print p5
    print "\n"
  end
  def parse(p6_file,p6_code)
    p6_code ||= File.open(p6_file,'r'){|f|f.read}
    parrot = ENV['PARROT_ROOT'] or
      raise "The environment variable PARROT_ROOT must be defined.\n"
    inp = Tempfile.new('p6')
    inp.print p6_code
    inp.close
    cmd = "cd #{parrot}/languages/perl6; ../../parrot perl6.pbc --target=parse #{inp.path}"
    dump = `#{cmd}`
  end
end

Program.new.main(ARGV)
END

