#! /usr/bin/env ruby1.9
# See README.
# Uses emacs ruby and outline mode. setq at bottom colorizes heredoc code.
# Disclaimer - most of this code was originally intended to be thown
#  away on a timescale of a couple of weeks.  Now... it's not clear.


###* RedSix

###** Dual boot, ruby/perl6.

 #(
$ruby_init_code = <<'_CODE'
  if not FileTest.directory?("redsix_cache")
    print "Creating missing cache directory: #{"redsix_cache"}\n"
    Dir.mkdir("redsix_cache")
  end
  def caller_file_line(n=1)
    file,line = caller[n].split(/:/); line = line.to_i
    [file,line+1]
  end
  def source_ruby(s)
    file,line = caller_file_line
    s_len = s.split(/\n/).size
    startline = line - s_len - 1
    eval(s,nil,file,startline)
  end
#  def source_perl(*a) end
#  def source_perl_finish(*a) end
_CODE
eval($ruby_init_code)
 #)
source_ruby(<<'_END'
  require 'digest/md5'
  module Kernel; def chksum(s) Digest::MD5.hexdigest(s) end end
  $perlcode_to_eval = []
  def source_perl_compiled(sum,rubycode,perlcode)
    file,line = *caller_file_line
    eval(rubycode,$P.P6_binding,file,line)
    #File.open("deleteme_02","w"){|f|f.write perlcode.split(//).join("\n")}
    if chksum(perlcode) != sum
      print "# Perl source #{caller_file_line.join(':')} changed.  I will eval() it later.\n"
      #p sum; p perlcode.length,chksum(perlcode)
      linepl = line + rubycode.split(/\n/).size
      $perlcode_to_eval.push([perlcode,[file,linepl]])
    end
  end
  def source_perl(perlcode)
    fileline = caller_file_line
    perl_eval(perlcode,fileline)
  end
  def source_perl_finish
    $perlcode_to_eval.each{|(c,fileline)| perl_eval(c,fileline)}
  end
  def perl_eval(perlcode,fileline=nil)
    $P.eval6q(perlcode,fileline) if
      not (ENV['TEST_ONLY_PARSE'] && ENV['TEST_ONLY_PARSE'] != 0)
  end
_END
);
;
source_ruby(<<'_END'

$LogP = File.new("deletemeparse.rb","w")

###** Regex bootstrap

###*** Engine

require 'strscan'
class Engine
  attr_accessor :scanner,:choicepoints,:rest,:stack,:match,:grammar
  def initialize(scanner,pat,mat,grammar)
    @scanner = scanner
    @choicepoints = []
    @rest = pat.dup
    @stack = []
    @match = mat.skim_state_copy
    @grammar = grammar
  end
  def run
    backtracking = false
    failed = false
    top_cp = nil
    debug = nil #||true
    parse_debug = nil #||true
    $LogP.print "Engine IN\n" if debug
    while true
      if @rest.empty?
        $LogP.print "Engine OUT - Success\n" if debug
        return @match
      end
      top = @rest.pop
      $LogP.print("===============================\n",
            @rest[-1].inspect.slice(0..50),"\n",
            top.inspect.slice(0..150),"\n") if debug
      case top
      when :apat
      when Regexp
        if @scanner.scan(top) || (@scanner.eos? && "" =~ top)
          if nil || parse_debug #||true
            @stack.each{|e| $LogP.print(e,"/") if e.class == String};$LogP.print "\n"
            p1 = @scanner.pos
            p0 = p1 - 20; p0 = 0 if p0 < 0
            s0 = @scanner.string.slice(p0,p1-p0)
            s1 = @scanner.string.slice(p1,50)
            s0.gsub!(/\n/){|w|'\n'};s0.gsub!(/\t/){|w|'\t'}
            s1.gsub!(/\n/){|w|'\n'};s1.gsub!(/\t/){|w|'\t'}
            $LogP.print "\033[0;7;34m#{s0}\033[0m#{s1}\n"
          end
        else
          if nil || parse_debug #||true
            path = ''
            @stack.each{|e| path += (e + "/") if e.class == String}
            if path != ''
              $LogP.print "\033[0;31m#{path}\033[0m"
              $LogP.print " #{top.inspect}\n"
            end
          end
          failed = true
        end
        $LogP.print top.inspect,' ',(failed ? 'failed' : 'ok'),"\n" if debug
      when Array
        case top[0]
        when :alt
          if not backtracking
            n = 0
            if top[1].size > 1
              @choicepoints.push([@scanner.pos,(@rest+[top]),@stack,@match,n])
            end
            fail "assert" if top[1].size < 1
            @rest += top[1][n].reverse
          else
            n = top_cp[-1]
            n += 1
            fail "bug" if n >= top[1].size
            if n < (top[1].size - 1)
              @choicepoints.push([@scanner.pos,(@rest+[top]),@stack,@match,n])
            end
            if n < top[1].size
              @rest += top[1][n].reverse
            end
          end
        when :rep_bstar
          _op,p,left=*top; if left > 0 then @rest += [[:alt,[p+[[:rep_bstar,p,left-1]],[] ] ]] end
        when :rep_bstar_ng
          _op,p,left=*top; if left > 0 then @rest += [[:alt,[[],p+[[:rep_bstar_ng,p,left-1]] ] ]] end
        when :rep
          _op,p,min,max,ng=*top
          @rest += ((p * min)+(ng ? [[:rep_bstar_ng,p,max-min]] : [[:rep_bstar,p,max-min]])).reverse
        when :seq
          @rest += top[1].reverse
        when :match_open
          #$LogP.print "------open\n"
          _op,rul,nam=*top
          @match = Match.new_from(@scanner.string,@scanner.pos)
          @match.rule = rul
          @stack = @stack.dup
          @stack.push("#{nam}")
        when :match_close
          #$LogP.print "------close\n"
          fail("splat\n#{caller.join("\n")}") if !@match
          @match = @match.skim_state_copy.close!(@scanner.pos)
          @stack = @stack.dup
          fail "assert" if not @stack.pop.class == String
        when :match_push
          @stack = @stack.dup
          @stack.push(@match.skim_state_copy)
        when :match_pop
          _op,nam=*top
          @stack = @stack.dup
          prev = @stack.pop.skim_state_copy
          if nam
            val = @match.skim_state_copy
            nam = val.rule if nam == true; fail "assert" if not nam
            if prev
              crnt = prev.hash[nam]
              if crnt
                crnt = [crnt] if crnt.class != Array
                val = crnt+[val]
              end
            end
            prev.hash[nam] =  val if nam
          end
          @match = prev
        when :skimC
          _op,name,argl=*top
          argl ||= []
          r = @grammar.method(name).call
          out = r.skimC(*argl)
          if out then @rest += out.reverse else failed = true end
        when :skimCo
          _op,obj,argl=*top
          argl ||= []
          out = obj.skimC(*argl)
          if out then @rest += out.reverse else failed = true end
        when :full_skimCf
          _op,out=*top
          res = out.skimCf(top,@scanner,@rest,@stack,@match,@choicepoints)
          if res then top,@scanner,@rest,@stack,@match,@choicepoints = *res else failed = true end
        when :proc_full
          _op,out=*top
          res = out.call(top,@scanner,@rest,@stack,@match,@choicepoints)
          if res then top,@scanner,@rest,@stack,@match,@choicepoints = *res else failed = true end
        else fail "bug '#{top[0]}'" end  
      else fail "bug >#{top}<" end
      backtracking=false
      $LogP.print "Pass #{failed ? 'failed' : 'worked'}\n" if debug
      $LogP.print "Choicepoints #{choicepoints.empty? ? 'empty' : @choicepoints.size}\n" if debug
      if failed
        if @choicepoints.empty?
          $LogP.print "Engine OUT - Failure\n" if debug
          return nil
        end
        top_cp = @choicepoints.pop
        @scanner.pos,@rest,@stack,@match = *top_cp
        $LogP.print "Backtracking to pos ",@scanner.pos," cp ",@choicepoints.size,"\n  ",top_cp.inspect,"\n  ",@rest[-1].inspect,"\n" if debug
        backtracking=true
        failed=false
      end
    end
  end
end

###*** Match

module MatchDescribe
  def match_describe(seen=nil)
    seen ||= {}
    return ("LOOP***"+match_describe_name) if seen.member?(self.object_id); seen[self.object_id] = true
    indent            = ->(s){ s ? s.gsub(/(?m)^(?!\Z)/,'  ') : '*nil*' }
    indent_except_top = ->(s){ s ? s.gsub(/(?m)^(?!\Z)/,'  ').sub(/^  /,'') : '*nil*' }
    n = match_describe_name
    b = as_b ? 'true' : 'false'
    s = "'"+indent_except_top.call(as_s).gsub(/([\\'])/){|w|"\\#{w}"}+"'"
    a = as_a.map{|m| "\n"+indent.call(m.match_describe(seen))+"," }.join("")
    a += "\n" if a != ""
    h = as_h.map{|k,v|
      vs = if v.instance_of?(Array)
             "[\n" + indent.call(v.map{|m|m.match_describe(seen)}.join(",\n"))+"\n]"
           else
             v.match_describe(seen)
           end
      "\n  #{k} => #{indent_except_top.call(vs)},"
    }.join("")
    h += "\n" if h != ""
    f = match_beg
    t = match_end
    "#{n}<#{b},#{s},#{f}-#{t},[#{a}],{#{h}}>"
  end
  def match_describe_name()
    "#{self.class}:#{object_id.to_s(36)}"
  end
end
class Match
  attr_accessor :on_str,:from,:to,:bool,:hash
  attr_writer :str
  attr_accessor :rule
  def initialize(on_str,from,to,bool,hash,str,rule=nil)
    @on_str,@from,@to,@bool,@hash,@str,@rule=on_str,from,to,bool,hash,str,rule
  end
  def self.new_from(on_str,from=0,to=nil)
    self.new(on_str,from,to,true,{},nil)
  end
  def skim_state_copy
    self.class.new(@on_str,@from,@to,@bool,@hash.skim_state_copy,@str,@rule)
  end
  def str(to=nil)
    @str || @on_str.slice(@from,(@to||to)-@from)
  end
  def close!(to)
    if @from # otherwise already .failed!
      @to = to
      @str = @on_str.slice(@from,@to-@from) if @from
    end
    self
  end
  def failed!
    @bool,@str,@hash,@from,@to = false,'',{},nil,nil
    self
  end
  def closer_proc
    SkimProc.new{|s,c| self.close!(s.pos); c.cont0(s)}
  end

  include MatchDescribe
  def as_b; @bool end; def as_s; @str end; def as_a; [] end; def as_h; @hash end
  def match_beg; @from end; def match_end; @to end
  def match_describe_name; "#{super}:#{rule ? rule.name : 'nil'}" end
  def inspect; match_describe end
end

class Object
  def skim_state_copy; self end
end
class Hash
  def skim_state_copy; cp = self.class.new; self.each{|k,v| cp[k]=v.skim_state_copy}; cp end
end
class Array
  def skim_state_copy; cp = dup; cp.map!{|e|e.skim_state_copy}; cp end
end

###*** Rule

class Rule
  attr_accessor :name,:raw_patA
  def initialize(*args) @name,@raw_patA=*args end
  def search(str,pos=0,can_skip=true)
    (pos..str.length).each{|p|
      scanner = StringScanner.new(str)
      scanner.pos = pos
      ev = Engine.new(scanner,[:apat,[:skimCo,self]],nil,$grammar).run
      return ev if ev && ev.bool
      break if !can_skip
    }
    return Match.new_from(str,pos).failed!
  end
  def skimC
    [:apat,[:seq,[
     [:match_open,self,@name],
     [:seq,@raw_patA],
     [:match_close]
    ]]]
  end    
end
class String
  def search(rule,pos=0,can_skip=true) rule.search(self,pos,can_skip) end
end

###*** OperatorPrecedenceParser

require 'digest/md5'

class OperatorPrecedenceParser
  attr_accessor :tokens,:ws
  def initialize(tokens,ws) @tokens,@ws=tokens,ws end
  def skimC(token_filter=nil)
    [:apat,[:full_skimCf,OperatorPrecedenceParser::Parsing.new(@tokens,@ws,token_filter)]]
  end
end
class OperatorPrecedenceParser::Parsing
  @@depth = 0
  def log(msg)
    if nil || true
      sp = "    " * @@depth
      id = Digest::MD5.hexdigest(object_id.to_s).slice(0,6)
      $LogP.print "#{sp}OPP #{id} #{msg}\n"
    end
  end
  class Edge;end
  BOS = Edge.new
  EOS = Edge.new
  attr_accessor :tokens,:ws,:s,:token_filter
  def initialize(tokens,ws,token_filter=nil)
    @tokens,@ws,@token_filter=tokens,ws,token_filter
  end
  def skimCf(*args)
    @@depth += 1
    scanner=args[1]
    @s=scanner
    ok = parse
    if ok
      log "OPP-PARSE succeeded\n"
      @@depth -= 1
      args[4] = ok
      args
    else 
      log "OPP-PARSE failed\n"
      @@depth -= 1
      false
    end
  end
  attr_accessor :saw_ws
  def eat_ws
    m = @ws.search(@s.string,@s.pos,false)
    if m.bool
      @s.pos = m.to
      m
    else false end
  end
  def lexeme_compatible_with_context?(l,prepost,ws)
    if ws != :dontcare
      lws = l.whitespace_on_left
      return false if not case lws
                          when nil; true
                          when :prohibit; !ws
                          when :require; !!ws
                          when :ws_irrelevant; true
                          else fail("assert: #{lws}") end
    end
    if prepost != nil
      lexp = l.expression_on_left
      return false if not case lexp
                          when :need_expr
                            case prepost
                            when :pre; nil
                            when :post; true
                            when :post_commalike; l.is_commalike
                            else fail('assert') end
                          when :no_expr_needed; prepost == :pre
                          else fail("assert: #{lexp}") end
    end
    return true
  end
  attr_accessor :prepost
  def lex(current_lexemes)
    log "LEX"; log "Lex'ing at #{@s.pos} >#{@s.string.slice(@s.pos,20)}"
    @saw_ws = eat_ws
    log "Lex saw ws" if @saw_ws
    log "Lex context: #{@prepost} #{@saw_ws ? 'ws' : 'nows'}"
    matches = []
    ss = StringScanner.new(@s.string); ss.pos = @s.pos
#    log "BEGIN ================================="
    current_lexemes.each{|l|
      key = l.competition_key
#      log "x# #{key.inspect} \t#{l.name}"
      len = nil
      if key && key.is_a?(Regexp)
        x = (ss.scan(key) || (ss.eos? && "".match(key) && ""))
#  log l.name.inspect+' '+(x ? 'OK' : "FAILED #{key.inspect}")+""
        next if not x
        ss.pos = @s.pos
        len = x.length
      end
      next if not lexeme_compatible_with_context?(l,nil,@saw_ws)
      m = l.search(@s.string,@s.pos,false)
      next if not m.bool
      len = m.hash[key].to - m.from if !len && key.is_a?(Symbol)
      len = m.to - m.from if !len
      matches.push([m,l,len])
    }
    log "Lex #{matches.size} candidates"
    ms = matches.sort{|a,b| b[2] <=> a[2]}
    if not ms.empty?
      at = ms[0][2]
      log "Lex target #{at}"
#      p "=================== ",at,ms[0][0].inspect,'-----'
      ms = ms.find_all{|a|
        log "...#{a[2]}   #{a[1].name}"
        at == a[2] and lexeme_compatible_with_context?(a[1],@prepost,:dontcare)
      }
    end
    log "Lex #{ms.size} matches"
    if ms.size > 1 then
      STDERR.print "\nWARNING: ambiguous parse: #{ms[0][0].as_s}\n"
      ms.each{|a| STDERR.print "  #{a[1].name}\n"}
    end
    m = ms.empty? ? nil : ms[0][0]
    log "Lex RESULT: #{m.inspect}"
    return nil if not m
    @s.pos = m.to
    log "Lex now at #{@s.pos}"
    @saw_ws = false
    m
  end
  attr_accessor :ops,:opands,:ends,:ends_sz
  attr_accessor :tok
  attr_accessor :lexemes
  def init
    @lexemes = @tokens.map{|t|t.first_lexeme}
    @lexemes_at_top_level = @lexemes
    if @token_filter
      top_level_tokens = @tokens.find_all{|t| @token_filter.call(t)}
      @lexemes_at_top_level = top_level_tokens.map{|t|t.first_lexeme}
    end
    @prepost = :pre
    @saw_ws = false

    @ops = [BOS]
    @opands = []
    @ends = []; @ends_sz = []
    @tok = nil
  end
  def parse
    log "OPP-PARSE"
    init
    catch(:parse_fail) {parser_loop}
  end
  def log_state
    log "Currently:\n\topands: #{@opands.inspect}\n\tops: #{@ops.inspect}\n\ttok: #{@tok.inspect}"
  end
  def parser_loop
    while true
      log "Loop"
      get_token
      log_state if nil #||true
      if @tok == EOS
        break
      end
      log @tok.rule.inspect if nil #||true
      if not @tok.rule.token.is_operator?
        operand
      elsif @ops.size == 1
        shift
      elsif @tok.rule == @ends[-1]
        crunch
      elsif (@ops[-1].rule.expression_on_right == :no_expr_needed &&
             !@ops[-1].rule.next_lexeme)
        reduce
      elsif @tok.rule.expression_on_left == :no_expr_needed
        shift
      else
        st = @ops[-1].rule.precedence_on_right
        tk = @tok.rule.precedence_on_left
        if    st < tk; shift
        elsif st > tk; reduce
        elsif st == 0 and tk == 0
          shift # eg, prelist with circumfix
        elsif @tok.rule.associativity == :right
          shift
        else
          reduce
        end
      end
    end
    log "EOS processing"
    reduce while @ops.size > 1
    error if not (@ops == [BOS] and @opands.size == 1 and @ends == [] and @tok == EOS)
    @s.pos = @saw_ws.from if @saw_ws
    @opands[0]
  end
  def get_token
    return if @tok
    close = @ends[-1]
    current_lexemes = close ? @lexemes + [close] : @lexemes_at_top_level
    @tok = lex(current_lexemes) || EOS
  end
  def operand
    log "operand"
    m = @tok; @tok = nil
    @opands.push(m)
    currently_argument_list_top_level = @token_filter && !@ends[-1]
    @prepost = m.rule.token.operand_post
    @prepost = :post if (@prepost == :post_commalike &&
                         !currently_argument_list_top_level)
  end
  def shift
    log "shift"
    m = @tok; @tok = nil
    @ops.push(m)
    eme = m.rule.next_lexeme
    if eme
      @ends.push(eme)
      @ends_sz.push(@ops.size)
    end
    @prepost = prepost_from_rule(@ops[-1].rule)
  end
  def reduce
    log "reduce"
    m0 = @ops.pop
    t = m0.rule.token
    count = t.part_count - 1 # all but m0 are on @opands
    if @opands.size < count
      log "reduce problem - not enough opands (#{@opands.size} vs #{count})"
      log @opands.inspect
      error 
    end
    parts = @opands.slice!(0-count,count)
    m = t.create_match([m0,*parts])
    @opands.push(m)
    @prepost = prepost_from_rule(@ops[-1].rule) if @ops[-1] != BOS
  end
  def crunch
    log "crunch"
    fail('assert') if @tok.rule != @ends[-1]
    m = @tok
    t = m.rule.token
    reduce while @ops.size > @ends_sz[-1]
    fail('assert') if @ops[-1].rule.token != @ends[-1].token
    @ends.pop; @ends_sz.pop
    @opands.push(@ops.pop)
    shift
  end
  def prepost_from_rule(r)
    r.expression_on_right == :need_expr ? :pre : :post
  end
  def error
    log "ERROR"
    log " op  #{@ops[-1].inspect}"
    log " tok #{@tok.inspect}"
    throw :parse_fail,nil
  end
end

###**** Tokens

class Lexeme < Rule
  attr_accessor :token
  attr_accessor :competition_key
  attr_accessor :whitespace_on_left
  attr_accessor :expression_on_left,:expression_on_right
  attr_accessor :precedence_on_left,:precedence_on_right
  attr_accessor :associativity
  attr_accessor :next_lexeme
  attr_accessor :is_commalike
  def initialize(name,f,len=nil)
    super(name,f)
    @competition_key = len
  end
end
class Token_Operand
  attr_accessor :first_lexeme
  attr_accessor :operand_post
  def is_operator?; false end
  def initialize(name,f,len=nil)
    @first_lexeme = l = Lexeme.new(name,f,len)
    l.token = self
    l.whitespace_on_left = :ws_irrelevant
    l.expression_on_left = :no_expr_needed
    l.expression_on_right = :no_expr_needed
    l.is_commalike = false
    @operand_post = :post
  end
end
class Token_Operator
  attr_accessor :fixity,:strings,:precedence,:assoc,:ws_policy
  attr_accessor :arity,:part_count
  attr_accessor :name
  attr_accessor :first_lexeme
  attr_accessor :fixity_extra
  def is_operator?; true end
  def initialize(*args)
    @name,@fixity,@fixity_extra,@strings,@precedence,@assoc,@ws_policy,@is_commalike=*args
    @fixity=@fixity.to_sym
    @precedence ||= @@precgen.default_precedence
    @name = @name || "#{@fixity}:#{@strings.join(' ')}"
    # build lexemes
    lexemes = [];
    regexps = @strings.each_with_index{|str_or_re,i|
      re=nil
      native_re = nil
      if str_or_re.is_a?(String)
        pat = str_or_re.gsub(/(\W)/){|w|"\\#{w}"};
        native_re = /#{pat}/
        re = [:apat,/#{pat}/]
      else
        re = str_or_re
      end
      lexemes.push(Lexeme.new("#{name}:#{i}",re,native_re))
    }
    # ... and connect them
    @first_lexeme = lexemes[0]
    lexemes[0].next_lexeme = lexemes[1] || nil
    lexemes[1].next_lexeme = nil if lexemes[1]
    # ... and set defaults
    lexemes.each{|l|
      l.token = self
      l.whitespace_on_left = nil
      l.associativity = @assoc
      l.is_commalike = @is_commalike
    }
    lexemes[0].whitespace_on_left = @ws_policy
    # finish up
    set = ->(n,el,pl,er,pr){
      lexemes[n].expression_on_left = el ? :need_expr : :no_expr_needed
      lexemes[n].expression_on_right = er ? :need_expr : :no_expr_needed
      lexemes[n].precedence_on_left = pl ? @precedence : 0
      lexemes[n].precedence_on_right = pr ? @precedence : 0
    }
    case @fixity
    when :prefix
      @arity = 1; @part_count = 2
      set[0, nil,nil, :e,:p]
    when :postfix
      @arity = 1; @part_count = 2
      set[0, :e,:p, nil,nil]
    when :circumfix
      @arity = 1; @part_count = 3
      set[0, nil,nil, :e,nil]
      set[1,:e,nil, nil,nil]
    when :infix
      @arity = 2; @part_count = 3
      set[0, :e,:p, :e,:p]
    when :ternary
      @arity = 3; @part_count = 5
      set[0, :e,:p, :e,nil]
      set[1, :e,nil, :e,:p]
    when :postcircumfix
      @arity = 2; @part_count = 4
      set[0, :e,:p, :e,nil]
      set[1, :e,nil, nil,nil]
    else fail "assert #{@fixity}"
    end
    lexemes[-1].precedence_on_right = 0 if @fixity_extra == :list
  end

  def create_match(parts)
$LogP.print parts.inspect
    parts.sort!{|a,b| a.from <=> b.from }
    m = Match.new_from(parts[0].on_str)
    m.rule = self
    m.hash[:parts] = parts
    m.from = parts[0].from
    m.close!(parts[-1].to)
    m
  end

  class PrecedenceGenerator
    attr :delta
    def initialize() @delta = 0.5 end
    def consume_a_delta() delta = @delta; @delta /= 2; delta end
    def default_precedence() 1 end
    def new_precedence_tighter_than(prec) prec + consume_a_delta end
    def new_precedence_looser_than (prec) prec - consume_a_delta end
  end
  @@precgen = PrecedenceGenerator.new
  def tighter; @@precgen.new_precedence_tighter_than(@precedence) end
  def looser;  @@precgen.new_precedence_looser_than(@precedence) end
  def equiv;   @precedence end
end

###** Six

###*** Grammar

class Grammar
  def self.abbrev(arg) arg.map{|a|a.is_a?(Symbol) ? sr(a) : a.is_a?(String) ? sym(a) : a.is_a?(Regexp) ? [:apat,a] : a } end
  def self.mangle(s) s.to_s.gsub(/z/,'zz').gsub(/([^a-z0-9])/i){|w|sprintf("z%0Xz",w[0].ord)} end

  def self.alt(*specs) [:apat,[:alt,abbrev(specs)]] end
  def self.seq(*spec) p=[:apat];abbrev(spec).map{|e|p.push(*e)}; p end
  def self.rep(r,min=0,max=2**24,ng=nil) [:apat,[:rep,abbrev([r])[0],min,max,ng]] end
  def self.star(*args) r = args.size > 1 ? seq(*args) : args[0]; rep(r,0) end
  def self.plus(*args) r = args.size > 1 ? seq(*args) : args[0]; rep(r,1) end
  def self.ques(*args) r = args.size > 1 ? seq(*args) : args[0]; rep(r,0,1) end
  def self.sr(rn,bindname=nil,argl=[])
    bindname = rn if bindname == nil # but not false
    [:apat,[:seq,[
     [:match_push],
     [:skimC,rn,argl],
     [:match_pop,bindname]
    ]]]
  end
  def self.sr?(rn,argl=[]) sr(rn,false,argl) end
  def self.sr!(rn,argl=[])
    fail "sr! unimplemented"
  end
  def self.sym(x)
    qx = x.class == Regexp ? "#{x}" : x.gsub(/(\W)/){|w|"\\#{w}"}
    qx += '\b' if qx =~ /\w\z/
    seq(ques(sr?(:ws)),ncap([/#{qx}/],:symbol),ques(sr?(:ws)))
  end
  def self.ncap(spec,bindname)
    [:apat,[:seq,[
     [:match_push],
     [:match_open,nil,:fake_symbol_rule],
     [:seq,seq(spec)],
     [:match_close],
     [:match_pop,bindname]
    ]]]
  end

  def self.defineA(name,f)
    vname = name.to_s.gsub(/\?/,'Q').gsub(/\!/,'B')
    eval "def #{name}; end" # essential magic (else action-at-a-distance bugs)
    eval %{ define_method(name) { @#{vname} ||= Rule.new(name,f) } },nil,"line #{__LINE__}, '#{name}'"
#    define_method(name) { Rule.new(name,f) }
  end
  def self.defineB(name,r)
    vname = name.to_s.gsub(/\?/,'Q').gsub(/\!/,'B')
    eval "def #{name}; end" # precautionary magic
    eval %{ define_method(name) { @#{vname} ||= r } },nil,"line #{__LINE__}, '#{name}'"
  end

  def self.named(name,*spec)
#     print "#{name} => ",seq(*spec).inspect,"\n\n"
    defineA(name,seq(*spec))
  end
  def self.term(name,len,*spec)
    defineB("tokens__#{name}",Token_Operand.new(name,seq(*spec),len))
  end
  def self.nterm(name,len,*spec)
    named(name,*spec)
    term(name,len,*spec)
  end
  def self.def_operator(fix,oppat,opdef)
    mangled = mangle(oppat.gsub(/\s+/,'_'))
    defineB("tokens__#{fix}_#{mangled}",opdef)
  end
  def self.option(n1,n2,*spec)
    named("#{n1}__#{n2}",*spec)
  end
end

class SixGrammar < Grammar
  def method_missing(method, *args)
    print "FAKING #{method}\n"
    Rule.new(method,[:apat,/(?!)/])
  end
  def method(method,*args)
    return super if self.class.method_defined?(method)
    print "FAKING #{method}\n"
    proc{Rule.new(method,[:apat,/(?!)/])}
  end

  def self.trait_name(trait)
    @trait_name_cache ||= {}
    @trait_name_cache[trait] ||= seq('is',trait,:qualified_identifier)
  end
  def self.bare_trait(trait)
    @bare_trait_cache ||= {}
    @bare_trait_cache[trait] ||= alt(trait_name(trait),
                                     seq(trait,:qualified_identifier,
                                         star(:ws,'of',:qualified_identifier)))
  end
  def self.comma_list(r)
    seq(r,star(',',r))
  end
  def self.reassembled_delimited_identifier(delim)
    seq(/\w+/,star(delim,/\w+/))
  end
  def self.maybeParens(*spec)
    r = seq(*spec)
    alt(seq('(',r,')'),r)
  end
  def self.verbatimBlah(o,r,c) seq(/#{"\\"+o}/,r,:ws?,/#{"\\"+c}/) end
  def self.verbatimBraces(r) verbatimBlah('{',r,'}') end
  def self.verbatimBrackets(r) verbatimBlah('[',r,']') end
  def self.verbatimParens(r) verbatimBlah('(',r,')') end


  # def ws; @ws ||= Rule.new('ws',SixGrammar.plus(SixGrammar.alt(*(methods.grep(/^ws__/).map{|m| self.class.sr(m)})))) end
  # option :ws,:simple,/\s+/
  # option :ws,:comment,/\#[^\n]*/
  # option :ws,:doc_block,/^=\w[^\n]*(?:\n(?!=cut)(?>[^\n]*))*/,alt(/\z/,seq(/\n=cut/,:whiteSpaceLine))
  # def ws!; ws end
  # named :ws?,ques(:ws)
  _paren = /\(\)/
  _paren = /(?:[^()]|\(#{_paren}\))*/
  _paren = /(?:[^()]|\(#{_paren}\))*/
  _paren = /(?:[^()]|\(#{_paren}\))*/
    ws_re = /
      (?: (?>\s+)
        | (?>(?<!\A|\n)\#\(#{_paren}\))
        | (?>\#[^\n]*)
        | ^=\w(?>[^\n]*)(?>(?:\n(?!=(cut|end))(?>[^\n]*))*)(?:\z|\n=(cut|end)[^\n]*)
       )+/x
  named :ws, ws_re
  named :ws!, ws_re
  named :ws?, /#{ws_re}?/
  named :before_ws, /(?=#{ws_re})/
  def tokens; @tokens ||= methods.grep(/^tokens__/).map{|m|method(m).call} end
  def expr; @expr ||= Rule.new('expr',[:apat,[:seq,[
                                       [:match_push],
                                       [:skimCo,OperatorPrecedenceParser.new(tokens,ws)],
                                       [:match_pop,:opp]
                                      ]]]) end
  def expr_opp_bare
    @expr_opp_bare ||= OperatorPrecedenceParser.new(tokens,ws)
  end
  named :expr_above_comma, sr(:expr_opp_bare,nil,[->(t){ if !t.is_operator? then true else t.precedence > $level__comma.precedence end}])

  #===========================================================================
  #-- Whitespace -------------------------------------------------------------

  named :whiteSpaceLine,/(?:(?!\n)\s)*$/

#  def self.colon_comma_list(r) seq(r,star(/,:/,r)) end
#  named :argument_list,colon_comma_list(:expr)
  named :argument_list,:expr
  optional_argument_list = ques(:argument_list)
  optional_expr_above_comma = ques(:expr_above_comma)
  acx = optional_argument_list
  ac1 = seq(:expr_above_comma)
  acq = optional_expr_above_comma
  ac0 = seq()
  term :apply_sub_notFolded,(:sub_name_bare),:sub_name_bare,/(?!\()/,
  [[:proc_full, ->(*args){
        rest = args[2] = args[2].dup
        m = args[4]
        n = m.hash[:sub_name_bare] || fail("assert")
        n = n.as_s.to_sym
        # Completely kludge arity, which has become critical. :/
        #ac = {:f1=>1,:fq=>:q,:f0=>0}[n]
        ac = {:ref=>1,:pi=>0,:shift=>1,:pop=>1,:try=>1}[n]
        case ac
        when nil; rest.push(*acx)
        when :s;  rest.push(*acx)
        when 1;   rest.push(*ac1)
        when :q;  rest.push(*acq)
        when 0;   rest.push(*ac0)
        else fail("assert") end
        args
      }]]
  term  :sub_name_term,(nil), :sub_name_bare,/(?=\()/

  named :method_args,ques(alt(seq(/:/,:argument_list),
                              seq(/\(/,optional_argument_list,:ws?,/\)/)))

  named :implicit_invocant,/(?=\.)/
  term :apply_method_term,(nil),:implicit_invocant,/\./,:qualified_identifier,:method_args

  #-- Declarations ------------------------------------------------
  named :scope, alt('state','my','our','let','temp','env')
  named :type,/(?:\w|::)[\w:&|?]*/  # related to :type_var and/or :type_literal?
  named :trait, alt('is','does'),ques(/::/),:qualified_identifier,ques(/\([^\)]*\)/)
  named :block,:ws?,/\{/,:statement_list,:ws?,/\}/
#  named :standalone_block,:block,:whiteSpaceLine
#  term  :code_block,(/\{/),:block
  term  :code_block,(/\{/),:block,ques(:whiteSpaceLine)
  #
  named :sub_declaration, seq(ques(:scope,ques(:identifier)),:sub_head,
                              ques('handles',:expr),
                              ques(bare_trait('returns')),
                              ques(:sub_parameters__ParensMandatory),
                              ques(bare_trait('returns')),
                              star(:trait),
                              :block)
  named :sub_head,alt(seq(ques('multi'),alt('sub','coro','submethod','method','macro')),'multi'),:sub_name_no_amp
  named :sub_parameters__ParensMandatory,'(',ques(comma_list(:formal_param)),')'
  named :sub_parameters__ParensOptional,ques(comma_list(:formal_param))
  named :param_name, alt(seq(/\&/,:sub_name_no_amp),seq(alt(/[\$\@\%]/,/::/),:twigil_opt,/\w+/))
  named :formal_param, seq(ques(:type,:ws),ques(/\\/),
                           ques(:param_foretaste),
                           :param_name,
                           ques(:param_optness),
                           star(:trait),
                           ques(:param_default),
                           ques('-->',:param_list__ParensOptional,alt(:formal_param,:type)))
  named :param_default, '=',:expr_above_comma
  named :param_foretaste, /\:|\*/
  named :param_optness, /\?|\!/
  
  #
  named :trusts_declaration, 'trusts', :qualified_identifier
  named :trait_declaration, :trait, :ws,/\z|(?=[;}])/
  named :member_declaration, seq('has',ques(:qualified_identifier),:var_name,
                                 star(:trait),ques('handles',:expr)),ques('=',:expr)
  #
  named :rule_declaration,'rule',:identifier,:adverb_hash,sr(:balanced,nil,['{'])
  #   rx_literal($<adverb_hash>,'{')
  #
  named :var_declaration, seq(:scope,ques(:qualified_identifier),
                              alt(:var_name,
                                  seq(/\(/,comma_list(alt(:var_name,:undef_literal)),/\)/)),
                              star(:trait),
                              ques(alt('=','.=',':=','::='), :ws?,
                                   alt(:var_declaration,:expr)))
  #
  named :package_block_declaration, :package_head,:block
  named :package_declaration, :package_head
  named :package_head, ques(:scope),alt('package','module','class','role','grammar'),:qualified_identifier,ques(:version_part,ques(:author_part)),:ws,star(:trait)
  #
  named :no_declaration, 'no',alt(:no_version,:use_package)
  named :use_declaration, 'use',alt(:use_version,:use_package)
  named :perl_version, /(?:v|Perl-)[\d\.]+/,ques(:author_part)
  named :use_version, :perl_version
  named :no_version, :perl_version
  named :use_package, alt(seq(/jsan:(?!:)/,:use_JSAN_module),
                          seq(/jsperl5:(?!:)/,:use_JSPerl5_module),
                          seq(ques(:identifier,/:(?!:)/),:use_perl_package))
  named :use_perl_package, :package_full_name,ques(alt(seq(/\(/,:ws,/\)/),
                                                       :expr))
  named :use_JSAN_module, seq(alt(:package_full_name,
                                  reassembled_delimited_identifier(".")),
                              alt(seq(/\(/,:ws,/\)/),
                                  ques(:expr)))
  named :use_JSPerl5_module,seq(alt(:package_full_name,
                                    reassembled_delimited_identifier("::")),
                                alt(seq(/\(/,:ws,/\)/),
                                    ques(:expr)))
  named :package_full_name, reassembled_delimited_identifier("::"),ques(:version_part),ques(:author_part)
  named :version_part, /-[\d\.\(\)]+/
  named :author_part, /-[\w\(\)]+/
  #
  named :inline_declaration, 'inline',:expr
  named :require_declaration, 'require',:package_full_name

  #-- Expressions / Terms ------------------------------------------------
  #   terms               42 "eek" $x /abc/ (1+2) a(1) :by(2) .meth listop

  term :fakestring,(/\'/), /\'(?:[^\'\\]|\\.)*\'/
  term :fakierstring,(/\"/), /\"(?:[^\"\\]|\\.)*\"/
  term :fakeEND,(/<<'_END'/), /<<'_END' *\n(.+?)\n_END *\n/m
  #
  named :dereference, /[\$\@\%\&]/,alt(:dereference,:sigiled_var,verbatimBraces(:expr))
  #
  nterm :angle_bracket_literal,(/<<?|\xab/),
  /<<(?:[^>\\]|\\.|>(?!>))*>>
  |<(?!<)(?:[^>\\]|\\.)*>
  |\xab(?:[^\xbb\\]|\\.)*\xbb/x
  named :hash_subscript_qw, :angle_bracket_literal
  named :hash_subscript_braces, /\{/,ques(:expr),/\}/
  named :hash_subscript, alt(:hash_subscript_braces,:hash_subscript_qw)
  fixity_re = /#{
'prefix: postfix: infix: circumfix: coerce: self: term:
 postcircumfix: rule_modifier: trait_verb: trait_auxiliary:
 scope_declarator: statement_control: infix_postfix_meta_operator:
 postfix_prefix_meta_operator: prefix_postfix_meta_operator:
 infix_circumfix_meta_operator:'.split(/\s+/).join("|")}/
  named :fixity, fixity_re
  named :operator_name, :fixity,alt(:identifier,:hash_subscript)
  named :sub_name_bare, /(?![A-Z])(?!(?:sub|coro|macro)\b)(?!(?:do)\b)/,alt(:operator_name,:qualified_identifier)
  named :sub_name_no_amp, :twigil_opt,alt(:operator_name,:qualified_identifier)
  named :sub_name_full, /\&/,:sub_name_no_amp
  named :identifier, /[a-z_]\w*/i
  named :qualified_identifier,/[a-z_]\w*(?:\:\:[a-z_]\w*)*/i
  named :twigil_opt,/[\^*?\.!+;]?/
  term :var_symbolic_deref,(/[\$\@\%\&]/), /[\$\@\%\&]/,plus(/::/,alt(/!|\//,seq(:twigil_opt,/\w+/)))
  nterm :var_sub,(nil), /\&/,:sub_name_no_amp # (nil) else prefix:& wins :/
  nterm :var_simple,(nil), /[\$\@\%]/,:twigil_opt,:qualified_identifier
  term :var_error,(/\$!/), /\$!/
  term :var_match_numbered,(/\$\d+/), /\$\d+/
  term :var_match_named,(/\$</), /\$<[^>]*>/
  term :var_match,(/\$\//), /\$\//
  named :var_name, alt(:var_sub,:var_simple)
  #
  #                       :apply__True,    #-- Folded metaoperators
  #
  term :do_block,(/do\b/), 'do',alt(:block, :statement)
  #
  nterm :block_formal_pointy,(/->/), /->/,:ws?,:sub_parameters__ParensOptional,star(:trait),:block
  nterm :block_formal_standard,(/(?:sub|coro|macro)\b/), sym(/sub|coro|macro/),ques(:sub_parameters__ParensMandatory),star(:trait),:block
  #
  fraction = /\.[\d_]+/
  expo = /[eE][-+]?\d+/
  number_re = /0(?:b[0-1]+|o[0-7]+|d[0-9]+|x[0-9a-fA-F]+)
              |[0-9][0-9_]*#{fraction}?#{expo}?
              |[-+]?(?:Inf|NaN)\b /x
  term :number,(number_re), number_re
  #
  term :empty_list_literal,(nil), verbatimParens(:ws?)
  # was /\(/, but that lost to postcircumfix_paren_empty(len 2),
  #  which needs to be 2 to compete with postcircumfix:( ):0(len 1).

#  term :empty_array_literal,(/\[/), verbatimBrackets(:ws?)
  #
  nterm :array_literal,(/\[/), verbatimBrackets(ques(:expr))
  #
  # :pair_literal is now unneeded -- alt(:pair_arrow,:pair_adverb)
  # :pair_arrow is now an op -- :identifier,'=>',:parseExpWithTightOps #infix:=>
  nterm :pair_adverb,(nil), /:/,alt(:shortcut_pair,:regular_pair)
  named :shortcut_pair, :var_name
  named :regular_pair_name, /\w+/
  named :regular_pair, :regular_pair_name,ques(alt(:valueDot,:noValue,:valueExp))
  named :valueDot, :ws,'.',ques(:valueExp)
  named :noValue, :ws
  named :valueExp, alt(verbatimParens(:expr),:array_literal,:angle_bracket_literal)
  #
  named :undef_literal, /undef\b/
  # 
  term :yada_literal,(/\.\.\.|\?\?\?|!!!/), alt('...','???','!!!')
  #
  #:q_literal,
  #
  named :adverb_hash,star(:pair_adverb)
  named :rx_pattern, seq(:ws?,alt(/\/(?:[^\/\\]|\\.)*\//,
                                  /\{(?:[^\}\\]|\\.)*\}/))
  # the \b_dot_ is to win against apply sub.
  term :rx_literal,(/(?:rx|m|rule)\b./), sym(/rx|m|rule/),:adverb_hash,:rx_pattern
  term :rx_literal_bare,(/\//), /(?=\/)/,:rx_pattern
  term :subst_literal,(/s\b/), 's',:adverb_hash,:rx_pattern,:q_literal1
  #
  #:nullary_literal,
  #:bareword_method,
  #
  nterm :closure_trait,(/(?:BEGIN|CHECK|INIT|FIRST|END)\b/), sym(/BEGIN|CHECK|INIT|FIRST|END/),:block # also stmt?!?
  #
  term :code_quotation,(/q:code/), /q:code/,ques('(:COMPILING)'),:block
  #
  term :type_var,(/::/), plus(/::/,alt(seq(/\(/,:expr,/\)/),
                                       seq(:twigil_opt,/\w+/)))
  #
  tl = /(?!Inf\b|NaN\b)(?!(?:BEGIN|CHECK|INIT|FIRST|END)\b)[A-Z]\w*(?:\:\:\w+)*/
  term :type_literal,(tl), tl
  #
  #:apply__False,   #-- Normal application
  #
  #term :parens,(/\(/), /\(/,ques(:expr),:ws?,/\)/
  ###

  #-- Constructs ------------------------------------------------

  named :for_construct,'for', maybeParens(:expr),ques(','),:expr
  named :loop_construct, 'loop',alt(:semi_loop_construct,:post_loop_construct)
  named :semi_loop_construct, maybeParens(ques(:expr),';',ques(:expr),';',ques(:expr)),:block
  named :post_loop_construct,:block,sym(/while|unitl/),:expr
  #
  named :cond_construct, sym(/if|unless/),:cond_body
  named :elsif_construct, 'elsif',:cond_body
  named :else_construct, 'else',:block
  named :cond_body, :cond_part,:block,ques(alt(:elsif_construct,:else_construct))
  named :cond_part, maybeParens(alt(:type_var,:type_literal,:expr))
  #
  named :while_until_construct,sym(/while|until/),:cond_part,:block
  named :given_construct, 'given',:cond_part,:block
  named :when_construct, 'when',:cond_part,:block
  named :default_construct, 'default',:block

  #-- :expr_statement ----------------------------------------

  named :expr_statement,:expr,ques(alt(:post_conditional,:post_loop,:post_iterate))
  named :post_conditional, sym(/if|unless/),:expr
  named :post_loop, sym(/while|until/),:expr
  named :post_iterate, 'for',:expr

  #-- Statements ----------------------------------------
  named :prog, :statement_list,:ws?
  named :statement_list,star(seq(:ws?,:statement,star(';')))

  def statement; @statement ||= Rule.new('statement',SixGrammar.alt(*(SixGrammar.statement_rules.map{|m| self.method(m).call.raw_patA}))) end
  def self.statement_rules; @statement_rules end
  def self.stmt(n)
    option :statement,n,n
    @statement_rules ||= []
    @statement_rules.push("statement__#{n}".to_sym)
  end
  # block_declaration
  stmt :sub_declaration
  stmt :closure_trait
  stmt :rule_declaration
  stmt :package_block_declaration
  # declaration
  stmt :package_declaration
  stmt :var_declaration
  stmt :member_declaration
  stmt :trait_declaration
  stmt :use_declaration
  stmt :no_declaration
  stmt :inline_declaration
  stmt :require_declaration
  stmt :trusts_declaration
  # construct
  stmt :for_construct
  stmt :loop_construct
  stmt :cond_construct
  stmt :while_until_construct
#  stmt :standalone_block
  stmt :given_construct
  stmt :when_construct
  stmt :default_construct
  # expr
  stmt :expr_statement

end

$grammar = SixGrammar.new

$grammar.tokens__code_block.operand_post = :post_commalike #

###*** Operator declarations

$level__last = nil
$level__eq = nil
$level__dot = nil
$level__comma = nil
def level(*args)
  fix,fixity_extra,assoc,ws_policy,commalike=nil
  level_equiv = nil
  args.each{|a|
    a = a.to_s
    case a
    when /\A(prefix|postfix|circumfix|infix|postcircumfix|ternary)\Z/; fix = $1
    when /\A(left|right|nonchain|chain)\Z/; assoc = $1.to_sym
    when /\A(list)\Z/; fixity_extra = $1.to_sym
    when /\A(nows)\Z/; ws_policy = :prohibit
    when /\A(needws)\Z/; ws_policy = :require
    when /\A(commalike)\Z/; commalike = true
    else
      a.split(/\s+/).each{|op|
        if fix =~ /\A(circumfix|postcircumfix|ternary)\Z/ then
          toks = op =~ /__/ ? op.split(/__/) : [op.slice(0..-2),op.slice(-1,1)]
        else
          toks = [op]
        end
        prec = (level_equiv ? level_equiv.precedence :
                $level__last ? $level__last.looser : nil)
        opdef = Token_Operator.new(nil,fix,fixity_extra,toks,prec,assoc,ws_policy,commalike)
        SixGrammar.def_operator(fix,op,opdef)
        if not level_equiv then
          level_equiv = opdef
          $level__last = opdef
          $level__eq = opdef if op == '='
          $level__dot = opdef if op == '.'
          $level__comma = opdef if op == ','
        end
      }
    end
  }
end
def define_assign_binops(opstr)
  prec = $level__eq.precedence
  opstr.split(/\s+/).each{|subop|
    op = "#{subop}="
    opdef = Token_Operator.new(nil,:infix,nil,[op],prec)
    SixGrammar.def_operator(:infix,op,opdef)
  }
end
def define_method_call
  op = SixGrammar.seq(/\./,:qualified_identifier,:method_args)
  prec = $level__dot.precedence
  opdef = Token_Operator.new(:apply_method.to_s,:postfix,nil,[op],prec)
  SixGrammar.def_operator(:postfix,'method_call',opdef)
end
def define_more_crud
  op = SixGrammar.seq(/\(/,:ws?,/\)/)
  prec = $level__dot.precedence
  opdef = Token_Operator.new(:postcircumfix_paren_empty.to_s,:postfix,nil,[op],prec)
  SixGrammar.def_operator(:postfix,'empty_paren_call',opdef)
end

# S03 - 7 Apr 2006
#   terms               42 "eek" $x /abc/ (1+2) a(1) :by(2) .meth listop
#   method postfix      . .+ .? .* .() .[] .{} .«» .=
#   autoincrement       ++ --
#   exponentiation      **
#   symbolic unary      ! + - ~ ? $ @ % & * ** +^ ~^ ?^ \ ^ =
#   multiplicative      * / % x xx +& +< +> ~& ~< ~>
#   additive            + - ~ +| +^ ~| ~^
#   junctive and (all)  &
#   junctive or (any)   | ^
#   named unary         rand sleep abs etc.
#   nonchaining binary  but does cmp <=> .. ^.. ..^ ^..^ ff ^ff ff^ ^ff^ fff ^fff etc.
#   chaining binary     != == < <= > >= ~~ !~ eq ne lt le gt ge =:= ===
#   tight and           &&
#   tight or            || ^^ //
#   ternary             ?? !!
#   assignment          = := ::= += -= **= xx= etc. (and also =>)
#   list item separator , ¥
#   list op (rightward) <== print push any all true not etc.
#   pipe forward        ==>
#   loose and           and andthen
#   loose or            or xor orelse
#   expr terminator     ; {} as control block, statement modifiers

#   terms               42 "eek" $x /abc/ (1+2) a(1) :by(2) .meth listop
level :circumfix, '(__)'
#   method postfix      . .+ .? .* .() .[] .{} .«» .=  
# .=
level :nows,:infix, '. .+ .? .*', :postcircumfix, '.() .[] .{} .«» .<>',
'() [] {} «» <>' # bogus?
;
#   autoincrement       ++ --
level :prefix, '++ --', :postfix, '++ --';
#   exponentiation      **
level :infix, '**';
#   symbolic unary      ! + - ~ ? $ @ % & * ** +^ ~^ ?^ \ ^ =
level :prefix, '! + - ~ ? $ @ % & * ** +^ ~^ ?^ \ ^ =';
#   multiplicative      * / % x xx +& +< +> ~& ~< ~>
level :infix, :left, '* / % x xx +& +< +> ~& ~< ~>';
#   additive            + - ~ +| +^ ~| ~^
level :infix, :left, '+ - ~ +| +^ ~| ~^';
#   junctive and (all)  &
level :infix, :left, '&';
#   junctive or (any)   | ^
level :infix, :left, '| ^';
#   named unary         rand sleep abs etc.
#XXX -------
#   nonchaining binary  but does cmp <=> .. ^.. ..^ ^..^ ff ^ff ff^ ^ff^ fff ^fff etc.
level :infix, :nonchain, 'but does cmp .. ^.. ..^ ^..^ ff ^ff ff^ ^ff^ fff ^fff fff^ ^fff^',:needws,'<=>';
#   chaining binary     != == < <= > >= ~~ !~ eq ne lt le gt ge =:= ===
level :infix, :chain, '!= == ~~ !~ eq ne lt le gt ge =:= ===',:needws,'< <= > >=';
#   tight and           &&
level :infix, '&&';
#   tight or            || ^^ //
level :infix, '|| ^^ //';
#   ternary             ?? !!
level :ternary, '??__!!';
#   assignment          = := ::= += -= **= xx= etc. (and also =>)
level :infix, :right, '= := ::=',:commalike,'=>';
#   list item separator , ¥
level :infix, :commalike, ', ¥';
#   list op (rightward) <== print push any all true not etc.
level :infix, :list, :needws, '<=='; #X?
#   pipe forward        ==>
level :infix, '==>';
#   loose and           and andthen
level :infix, 'and andthen';
#   loose or            or xor orelse
level :infix, 'or xor orelse';
#   expr terminator     ; {} as control block, statement modifiers
#XXX -------
#   end of table

define_assign_binops '. * / % x xx +& +< +> ~& ~< ~>   + - ~ +| +^ ~| ~^   &   | ^   || ^^ //';
define_method_call
define_more_crud


###** Past

###*** PastFromParse

class PastFromParse
  @@map = {}
  def self.mangle(s) s.to_s.gsub(/z/,'zz').gsub(/([^a-z0-9])/i){|w|sprintf("z%0Xz",w[0].ord)} end
  def self.node(name,p) @@map[mangle(name).to_sym] = p end
  node :rx_literal_bare,->(m){
    pat = m.hash[:rx_pattern].as_s
    mods = {}
    Past::Rx.new(m,mods,pat)
  }
  node :rx_literal,->(m){
    pat = m.hash[:rx_pattern].as_s
    mods = pair_list_to_hash(emit(m.hash[:adverb_hash]))
    Past::Rx.new(m,mods,pat)
  }
  node :adverb_hash,->(m){
    a = m.hash[:pair_adverb]
    return [] if not a
    a = [a] if not a === Array
    a.map{|p|emit(p)}
  }
  def self.pair_list_to_hash(pl)
    h = {}
    pl.each{|p|
      if p.is_a?(Past::PPair)
        h[p.name] = p.value
      elsif p.is_a?(Past::Infix) && p.fun.as_s == '=>'
        h[p.fargs[0]] = p.fargs[1]
      else fail("assert - pair_list_to_hash not given pair list") end
    }
    h
  end
  node :pair_adverb,->(m){
    rph = m.hash[:regular_pair].hash
    n = rph[:regular_pair_name].as_s
    v = ((rph[:valueExp] && (rph[:valueExp].hash[:expr] ||
                             rph[:valueExp].hash[:array_literal] ||
                             rph[:valueExp].hash[:angle_bracket_literal])
          ) ||
         nil)
    v = emit(v) if v
    if n =~ /^\d+\z/
      base = n.to_i
      val = v.src.as_s
      val.gsub!(/^[\<\[\{]|[\>\]\}]$/,'')
      num = case base
            when 16; eval(val).hex
            when 8; eval(val).oct
            when 10; eval(val).to_i
            else fail(":#{n}() unimplemented") end
      Past::Number.new(m,num)
    else
      Past::PPair.new(m,n,v)
    end
  }
  node :array_literal,->(m){
    l = m.hash[:expr] ? unpack_comma_list(m.hash[:expr].hash[:opp]) : []
    Past::ArrayLiteral.new(m,l)
  }
  node :type_literal,->(m){
    Past::TypeLiteral.new(m)
#    Past::String.new(m,m.as_s)
  }
  node :package_block_declaration,->(m){
    kind = m.hash[:package_head].hash[:symbol].as_s
    name = m.hash[:package_head].hash[:qualified_identifier].as_s
    body = emit(m.hash[:block])
    case kind
    when 'role'; Past::PRole.new(m,name,body)
    when 'class'; Past::PClass.new(m,name,body)
    when 'module'; Past::PModule.new(m,name,body)
    when 'package'; Past::PPackage.new(m,name,body)
    else fail("assert")
    end
  }
  node :member_declaration,->(m){
    var = emit(m.hash[:var_name])
    default = emitopt(m.hash[:expr])
    Past::MemberDeclaration.new(m,var,default)
  }
  node :var_error,->(m){
    Past::VarError.new(m)
  }
  node :closure_trait,->(m){
    Past::ClosureTrait.new(m,m.hash[:symbol].as_s,emit(m.hash[:block]))
  }
  node :do_block,->(m){
    b = emit(m.hash[:block])
    Past::DoBlock.new(m,b)
  }
#  node :standalone_block,->(m){
#    b = emit(m.hash[:block])
#    b.is_a?(Past::HashLiteral) ? b : Past::StandaloneBlock.new(m,b)
#  }
  node :code_block,->(m){
    b = emit(m.hash[:block])
    if b.is_a?(Past::HashLiteral)
      b
    elsif m.hash[:whiteSpaceLine]
      Past::StandaloneBlock.new(m,b)
    else
      Past::CodeBlock.new(m,b)
    end
  }
  node :block,->(m){
    begin
      e = m.hash[:statement_list].hash[:statement].hash[:expr_statement].hash[:expr].hash[:opp]
      h = pair_list_to_hash(unpack_comma_list(e))
      Past::HashLiteral.new(m,h)
    rescue
      Past::Block.new(m,emit(m.hash[:statement_list]))
    end
  }
  node :use_declaration,->(m){
    emit(m.hash[:use_version] || m.hash[:use_package])
  }
  node :use_version,->(m){
    Past::Comment.new(m,m.as_s)
  }
  node :use_package,->(m){
    Past::UsePackage.new(m,m.as_s)
  }
  node :undef_literal,->(m){ Past::UndefLiteral.new(m)}
  node :empty_list_literal,->(m){ Past::ListLiteral.new(m,[]) }
  node :prog,->(m){ emit(m.hash[:statement_list])}
  node :expr,->(m){
    if m.hash[:opp].rule.name == :sub_name_bare
      f = emit(m.hash.values[0])
      cap = make_capture(nil)
      Past::Apply.new(m,f,cap)
    else
      emit(m.hash.values[0])
    end
  }
  node :statement_list,->(m){
    lst = m.hash[:statement] || []
    lst = [lst] if !lst.kind_of?(Array)
    Past::StatementList.new(m,lst.map{|x|emit(x)}) }
  node :implicit_invocant,->(m){m.str = '$_'; Past::Variable.new(m)}
  node :apply_method_term,->(m){
    o = emit(m.hash[:implicit_invocant])
    f = emit(m.hash[:qualified_identifier])
    cap = emit(m.hash[:method_args])
    Past::ApplyMethod.new(m,o,f,cap)
  }
  node :apply_method,->(m){
    o = emit(m.hash[:parts][0])
    f = emit(m.hash[:parts][1].hash[:qualified_identifier])
    cap = emit(m.hash[:parts][1].hash[:method_args])
    Past::ApplyMethod.new(m,o,f,cap)
  }
  node :method_args,->(m){
    p = m.hash[:argument_list]
    make_capture(p)
  }
  node :apply_sub_notFolded,->(m){
    f = emit(m.hash[:sub_name_bare])
    p = (m.hash[:argument_list] ||
         (m.hash[:expr_above_comma] &&
          m.hash[:expr_above_comma].hash[:expr_opp_bare]))
    cap = make_capture(p)
    Past::Apply.new(m,f,cap)
  }
  node :parseParenParamList,->(m){ emitopt(m.hash.values[0]) }
  node :parseHasParenParamList,->(m){ emit(m.hash.values[0]) }
#  node :parens,->(m){ m.hash[:expr] ? emit(m.hash[:expr]) : Past::ListLiteral.new(m,[]) } #
  node :var_name,->(m){ emit(m.hash.values[0]) }
  node :var_simple,->(m){ Past::Variable.new(m) }
  node :var_declaration,->(m){
    vs = m.hash[:var_name]
    vs = [vs] if !vs.is_a?(Array)
    vars = vs.map{|v|emit(v)}
    vals = []
    if m.hash[:expr]
      vals = unpack_comma_list(m.hash[:expr].hash[:opp])
    elsif m.hash[:var_declaration]
      vals = [emit(m.hash[:var_declaration])]
    end
    sym = m.hash[:symbol]
    sym = sym[-1] if sym.is_a?(Array)
    sym = sym.as_s if sym
    sym = nil if sym == ','
    op = sym
    Past::VarDecl.new(m,m.hash[:scope].hash[:symbol].as_s,nil,vars,
                      emitopt(m.hash[:trait]),
                      op, vals)
  }
  node :for_construct,->(m){
    im = m.hash[:expr][0]
    l = unpack_comma_list(im.hash[:opp])
    iter = l.size == 1 ? l[0] : Past::ArrayLiteral.new(im,l)
    body = emit(m.hash[:expr][1])
    Past::For.new(m,iter,body)
  }
  def self.get_firstsense(m)
    sym = m.hash[:symbol].as_s
    firstsense = case sym
                 when 'if'; true; when 'unless'; false; else fail("assert") end
  end
  node :post_conditional,->(m){
    firstsense = get_firstsense(m)
    expr = emit(m.hash[:expr])
    [firstsense,expr]
  }
  node :cond_construct,->(m){
    firstsense = get_firstsense(m)
    body = emit(m.hash[:cond_body])
    Past::Cond.new(m,firstsense,body)
  }
  node :cond_body,->(m){
    this = [emit(m.hash[:cond_part]),
            emit(m.hash[:block])]
    rest = emitopt(m.hash[:elsif_construct] || m.hash[:else_construct])
    [this] + (rest ? rest : [])
  }
  node :elsif_construct,->(m){ emit(m.hash[:cond_body]) }
  node :else_construct,->(m){ [[emit(m.hash[:block])]] }
  node :cond_part,->(m){ emit(m.hash[:expr]) }
  def self.create_sig(m,kind)
    param = []
    tmp = (m.hash[:sub_parameters__ParensMandatory] ||
           m.hash[:sub_parameters__ParensOptional])
    if tmp
      param = tmp.hash[:formal_param]
      param = [] if not param
      param = [param] if param.class != Array
      param = param.map{|p|emit(p)}
    else
      t = nil
      slurpy = true
      n = Past::Variable.new('@_')
      optness = nil
      d = nil
      param = [Past::Parameter.new(nil,t,false,slurpy,false,n,optness,nil,d)]
    end
    Past::Signature.new(m,param,nil,kind)
  end
  node :sub_declaration,->(m){
    scope = m.hash[:scope] ? m.hash[:scope].hash[:symbol].as_s : nil
    kind = m.hash[:sub_head].hash[:symbol].as_s
    name = emit(m.hash[:sub_head].hash[:sub_name_no_amp])
    body = emit(m.hash[:block])
    sig = create_sig(m,kind)
    Past::SubDecl.new(m,scope,kind,name,sig,body)
  }
  node :block_formal_pointy,->(m){
    body = emit(m.hash[:block])
    sig = create_sig(m,nil)
    Past::SubDecl.new(m,nil,'sub',nil,sig,body)
  }
  node :block_formal_standard,->(m){
    body = emit(m.hash[:block])
    sig = create_sig(m,nil)
    Past::SubDecl.new(m,nil,'sub',nil,sig,body)
  }
  node :formal_param,->(m){
    t = emitopt(m.hash[:type])
    n = emit(m.hash[:param_name])
    d = emitopt(m.hash[:param_default])
    optness = emitopt(m.hash[:param_optness])
    foretaste = emitopt(m.hash[:param_foretaste])
    slurpy = foretaste =~ /\*/
    Past::Parameter.new(m,t,false,slurpy,false,n,optness,nil,d)
  }
  node :type,->(m){
    m.as_s
  }
  node :param_name,->(m){
    Past::Variable.new(m)
  }
  node :param_default,->(m){
    emit(m.hash[:expr_above_comma].hash[:expr_opp_bare])
  }
  node :param_foretaste,->(m){m.as_s}
  node :param_optness,->(m){m.as_s}
  node :sub_name_term,->(m){
    emit(m.hash[:sub_name_bare])
  }
  node :sub_name_bare,->(m){
    Past::Variable.new(m)
#    emit(m.hash[:qualified_identifier])
  }
  node :sub_name_no_amp,->(m){
    emit(m.hash[:qualified_identifier])
  }
  node :var_sub,->(m){
    Past::Variable.new(m)
#    Past::VarSub.new(m,m.hash[:sub_name_no_amp].as_s)
  }
  node :qualified_identifier,->(m){ Past::Identifier.new(m) }
#  node :bare_identifier,->(m){ emit(m.hash[:qualified_identifier]) }
  node :sub_parameters__ParensOptional,->(m){
    Past::Signature.new(tmp,param,nil,nil)
  }
  node :number,->(m){ Past::Number.new(m,m.as_s) }
  node :fakestring,->(m){
    md = m.as_s.match(/\A'(.*)'\z/m) or fail "bug\n#{m.as_s.inspect}"
    s = md[1]
    s.gsub!(/\\(.)/){|c|c}
    Past::String.new(m,s)
  }
  node :fakeEND,->(m){
    m.as_s =~ /^<<'_END' *\n(.+?)\n_END *\n\z/m or fail('assert')
    Past::String.new(m,$1)
  }
  node :fakierstring,->(m){
    md = m.as_s.match(/\A"(.*)"\z/m) or fail "bug #{m.inspect}"
    s = md[1]
    return Past::String.new(m,'') if s == ''
    s.gsub!(/\\(.)/){|c|c}
    ss = StringScanner.new(s)
    subs = []
    s0 = nil
    while not ss.eos?
      str = ss.scan(/(?:[^\\\$]|\\.)+/)
      if str
        str.gsub!(/\\n/,"\n")
        str.gsub!(/\\t/,"\t")
        s0 = str
        subs.push('"'+str+'"')
        next
      end
      code = ss.scan(/\$\w+/)
      if code
        subs.push(code)
        next
      end
      fail "assert"
    end
    if subs.size == 1 and s0
    then Past::String.new(m,s0)
    else
      subs.push('""') if subs.size == 1
      emit($grammar.expr.search(subs.join(" ~ "))) # offset
    end
  }
  node :angle_bracket_literal,->(m){
    s = m.as_s
    q = s.match(/^(?:<<?|\xab)/)[0]
    s = s.slice(q.length .. (-1 - q.length))
    Past::Quote.new(m,q,s)
  }
  node :term,->(m){ emit(m.hash.values[0]) }
  node :word,->(m){ Past::Word.new(m) }
  node :token,->(m){ emit(m.hash.values[0]) }
  node :statement,->(m){ emit(m.hash.values[0]) }
#  node :statement__expr_statement,->(m){ emit(m.hash.values[0]) }
  node :expr_statement,->(m){
    expr = emit(m.hash[:expr])
    post = emitopt(m.hash[:post_conditional] ||
                   m.hash[:post_loop] ||
                   m.hash[:post_iterate])
    if post
      firstsense,post_expr = *post
      Past::Cond.new(m,firstsense,[[post_expr,expr]])
    else
      expr
    end
  }
  node :variable,->(m){ Past::Variable.new(m) }
  node 'infix:,',->(m){
    l = unpack_comma_list(m)
    Past::ListLiteral.new(m,l)
  }
  node 'infix:+',->(m){ Past::Infix.new(m,m.hash[:parts][1],[0,2].map{|i|emit(m.hash[:parts][i])})}
  node 'prefix:+',->(m){ Past::Prefix.new(m,m.hash[:parts][0],[1].map{|i|emit(m.hash[:parts][i])})}
  node 'postfix:+',->(m){ Past::Postfix.new(m,m.hash[:parts][1],[0].map{|i|emit(m.hash[:parts][i])})}
  node 'postcircumfix:+',->(m){ Past::Postcircumfix.new(m,"#{m.hash[:parts][1].as_s} #{m.hash[:parts][3].as_s}",[0,2].map{|i|emit(m.hash[:parts][i])})}
  node 'postcircumfix:( )',->(m){
    f = emit(m.hash[:parts][0])
    p = m.hash[:parts][2]
    cap = make_capture(p)
    Past::Apply.new(m,f,cap)
  }
  node :postcircumfix_paren_empty,->(m){
    f = emit(m.hash[:parts][0])
    if m.hash[:parts][0].rule.name.to_sym == :apply_sub_notFolded
      f
    else
      cap = make_capture(nil)
      Past::Apply.new(m,f,cap)
    end
  }
  node 'ternary:?? !!',->(m){
    t = emit(m.hash[:parts][0])
    c = emit(m.hash[:parts][2])
    a = emit(m.hash[:parts][4])
    Past::Cond.new(m,true,[[t,c],[a]])
  }
  node 'circumfix:( )',->(m){
    e = emit(m.hash[:parts][1])
    if e.is_a?(Past::PPair) || (e.is_a?(Past::Infix) && e.fun.as_s == '=>')
    then Past::WrappedPPair.new(m,e)
    else e end
  }

  def self.emitopt(m)
    return nil if !m
    emit(m)
  end
  def self.emit(m)
    fail("boom\n#{caller.join("\n")}") if !m
    fail("thud\n#{caller.join("\n")}") if !m.bool
    name = m.rule.name.to_sym
    mname = mangle(name).to_sym
    mname2 = mname
    mname2 = mangle('infix:+').to_sym if name.to_s =~ /^infix:/
    mname2 = mangle('prefix:+').to_sym if name.to_s =~ /^prefix:/
    mname2 = mangle('postfix:+').to_sym if name.to_s =~ /^postfix:/
    mname2 = mangle('postcircumfix:+').to_sym if name.to_s =~ /^postcircumfix:/
    hand = @@map[mname] || @@map[mname2]
    if hand
      hand.call(m)
    else
      warn("#{self} FAILED: no handler for rules named '#{name}'")
      Past::SimpleString.new('THIS IS INVALID')
    end
  end
  def self.unpack_comma_list(m)
    name = m.rule.name.to_s
    if name == 'infix:,'
      unpack_comma_list(m.hash[:parts][0]) + [emit(m.hash[:parts][2])]
    else
      [emit(m)]
    end
  end
  def self.make_capture(m0)
    return Past::Capture.new(m0,[]) if not m0
    m1 = m0
    if m1.hash[:expr]
      m1 = m1.hash[:expr].hash[:opp]
    end
    return Past::Capture.new(m0,[]) if m1.rule.name.to_sym == :empty_list_literal
    #if m1.rule.name.to_sym == :parens
    #  m1 = m1.hash[:expr]
    #  m1 = m1.hash[:opp] if m1
    #end
    a = m1 ? unpack_comma_list(m1) : []
    Past::Capture.new(m0,a)
  end
end

###*** PastNodes

class Object; def as_s; to_s end end

module Past
  class PastObject
    attr_accessor :src
    attr_accessor :whiteboard
    def initialize(src) @src=src; @whiteboard={} end
    def inspect; src=@src;@src=@src.as_s;ret=super;@src=src;ret end
    def gensym; "v#{sprintf("%x",rand(1000000000))}" end
    def indent(s)
      s ? s.gsub(/(?m)^(?!\Z)/,'  ') : '*nil*'
    end
    def indent_except_top(s)
      s ? s.gsub(/(?m)^(?!\Z)/o,'  ').sub(/^  /,'') : '*nil*'
    end
    def down(m, *args, &block)
      if respond_to?(m)
        send(m,*args, &block)
      else
        down_not_self(m,*args,&block)
      end
    end
    def down_not_self(m, *args, &block)
      ms = m.to_s
      ms =~ /^(all|any|walk)_/ || fail("assert: #{ms}")
      any = $1 == 'any'
      all = $1 == 'all'
      ret = []
      instance_variables.each{|iv|
        n = iv.to_s.sub(/^@/,'')
        next if !self.respond_to?(n)
        node = self.send(n)
        nodes = nil
        if node.is_a?(PastObject)
          nodes = [node]
        elsif node.is_a?(Array)
          nodes = node.find_all{|e| e.is_a?(PastObject)}
        else next end
        nodes.each{|nd|
          res = nd.down(m,*args,&block)
          return res if any && res
          ret = ret + res if all
        }
      }
      ret
    end
    def walk_past_init(h=nil)
      h ||= {}
      down_not_self(:walk_past_init,h)
    end
    def walk_discarding_src
      @src = nil if instance_variable_defined? :@src
      down_not_self(:walk_discarding_src)
    end
  end
  def self.frob(s)
    s.split(/\s*\n/).each{|l|
      next if l =~ /^#/ or l =~ /^\s*$/
      l.gsub!(/^\s*/,'');
      cl,_fs=l.split(/\s+/);
      fs = _fs.split(/,\s*/); fs.shift if fs[0] == '-'
      as = ['src']+fs
      varcode = fs.empty? ? '' : <<-END
          attr_accessor #{fs.map{|f|":#{f}"}.join(',')}
          def initialize(#{as.join(',')})
            super(src)
            #{fs.map{|f|"@#{f}"}.join(',')} = #{fs.join(',')}
          end
      END
      code = <<-END 
        class #{cl} < PastObject #{"\n"+varcode}
        end
      END
      begin eval code; rescue Exception; fail "#{$!}:\n#{code}" end
    }
  end
  frob(%q{
Rx modifiers,pattern
PPair name,value
ArrayLiteral array
HashLiteral thehash
PRole name,body
PClass name,body
PModule name,body
PPackage name,body
UsePackage pkg
TypeLiteral -
MemberDeclaration var,default
VarError -
ClosureTrait flavor,block
DoBlock block
StandaloneBlock block
CodeBlock block
Comment comment
UndefLiteral -
ListLiteral list
Block statementlist
StatementList statements
ApplyMethod obj,f,arggen
Apply f,arggen
Capture argl
Identifier -
Number num
SimpleString -
String str
Quote quote,str
Variable -
VarSub name
VarDecl scope,mumble,vars,trait,op,vals
Cond sense_of_first_test,body
For enu,body
Word -
SubDecl scope,kind,name,sig,body
Signature params,returns,kind
Arguments theargs
Infix fun,fargs
Prefix fun,fargs
Postfix fun,fargs
Postcircumfix fun,fargs
WrappedPPair pair
})
# Parameter
end

###**** Past methods

module Past
  class VarDecl
    def my?;    scope == 'my' end
    def our?;   scope == 'our' end
    def state?; scope == 'state' end
    def let?;   scope == 'let' end
    def temp?;  scope == 'temp' end
    def env?;   scope == 'env' end
    def all_toplevel_vardecls; [self] end
  end
  module InitDecls
    def init_decls(h)
      decls = (h[:decls] || {}).dup
      down(:all_toplevel_vardecls).each{|vd|
        vars = vd.vars
        vars = [vars] if !vars.is_a?(Array)
        vars.each{|v|
          decls[v.perl] = vd
        }
      }
      h[:decls] = decls
    end
  end
  class StatementList
    include InitDecls
    def walk_past_init(h=nil)
      h ||= {}
      init_decls(h) if !h[:decls]
      down_not_self(:walk_past_init,h)
    end
  end
  class Block
    def all_toplevel_vardecls; [] end
    def block_toplevel_vardecls
      @statementlist.down_not_self(:all_toplevel_vardecls)
    end
    include InitDecls
    def walk_past_init(h)
      old_blk = h[:current_block]; h[:current_block] = self
      old_decls = h[:decls]; init_decls(h)
      block_toplevel_vardecls.each{|vd|
        vars = vd.vars
        vars = [vars] if !vars.is_a?(Array)
        vars.each{|v|
          h[:decls][v.perl] = vd
        }
      }
      @statementlist.down(:walk_past_init,h)
      h[:decls] = old_decls
      h[:current_block] = old_blk
    end
  end
  class Variable
    SIGIL_RE = /[$@%&]|::|@@/
    TWIGIL_RE = /[\.^*+?=!<]/ # $<foo>
    VARIABLE_RE = /^(?:(#{SIGIL_RE})(#{TWIGIL_RE})?)?(\w+(?:::\w+)*)$/
    attr_accessor :type,:sigil_actual,:twigil,:path
    def initialize(src,type=nil)
      super(src)
      @type = type
      initialize_from_string(@src.as_s)
    end
    def initialize_from_string(code)
      m = code.match(VARIABLE_RE) || fail("invalid variable: #{code}")
      @sigil_actual,@twigil,name = m[1..3]
      @path = name.split(/::/)
    end
    def sigil; @sigil_actual || '&' end
    def perl
      #t = type ? type.perl+' ' : '' #XXX
      t = type ? type+' ' : '' #XXX
      close = twigil == '<' ? '>' : ''
      "#{t}#{sigil_actual||''}#{twigil||''}#{path.join('::')}#{close}"
    end
    def pathname; path.join('::') end
    def leafname; path[-1] end
    def has_path; path.size > 1 end
    def scalar?;     sigil == '$' end
    def array?;      sigil == '@' end
    def hash?;       sigil == '%' end
    def exe?;        sigil == '&' end
    def type?;       sigil == '::' end
    def multislice?; sigil == '@@' end
    def accessor?;      twigil == '.' end # object attribute accessor
    def self_declared_parameter?; twigil == '^' end
    def global?;        twigil == '*' end
    def environmental?; twigil == '+' end
    def compiler?;      twigil == '?' end
    def pod?;           twigil == '=' end
    def match_capture?; twigil == '<' end
    def private?;       twigil == '!' end # aka explicitly_private?

    def walk_past_init(h)
      @whiteboard[:declaration] = h[:decls][self.perl] #|| fail("assert")
      down_not_self(:walk_past_init,h)
    end
    def declaration; @whiteboard[:declaration] end
  end
  class Parameter < PastObject
    attr_accessor :is_invocant,:is_slurpy,:is_name_only,
      :variable,:explicit_optness,:traits,:default
    attr_accessor :opt
    def initialize(src,*args)
      super(src)
      raise ArgumentError if args.size != 8
      type,@is_invocant,@is_slurpy,@is_name_only,
        variable,@explicit_optness,@traits,@default=*args
      @variable = variable #Variable.new(variable,type)
      @variable.type = type
      @opt = (@explicit_optness == '?' ||
              (@explicit_optness != '!' && @default))
    end
    def optional?; @opt end
    def required?; !@opt end
    def slurpy?; @is_slurpy end
    def positional?; !name_only? end
    def name_only?; @is_name_only end
    def invocant?; @is_invocant end
    def perl
      s = slurpy? ? '*' : ''
      n = name_only? ? ':' : ''
      o_concise = ''
      o_concise = '?' if optional? && !default
      o_concise = '!' if required? && default
      o_explicit = @explicit_optness
      o = o_explicit
      t = traits ? " "+traits.perl : ''
      d = default ? " = "+default.perl : ''
      "#{s}#{n}#{variable.perl}#{o}#{t}#{d}"
    end
    def slurpy_scalar?; slurpy? && scalar? end
    def slurpy_array?; slurpy? && array? end
    def slurpy_hash?; slurpy? && hash? end
    alias :method_missing_alias_Parameter :method_missing
    def method_missing(m,*args,&block)
      if @variable.respond_to?(m)
      then @variable.send(m,*args)
      else method_missing_alias_Parameter(m,*args,&block) end
    end
  end
  class Signature
    alias :initialize_Signature :initialize
    def initialize(*a)
      initialize_Signature(*a)
      add_implicit_self if kind == 'method' && invocants.empty?
    end
    def add_implicit_self
      implicit = Past::Parameter.new(nil,nil,
                                     true,false,false,
                                     Past::Variable.new('$self'),
                                     false,nil,nil)
      @params = [implicit] + (@params || [])
    end
    def perl
      r = returns ? ' --> '+returns.perl : ''
      ip = invocants.map{|p|p.perl}.join(', ')
      op = noninvocants.map{|p|p.perl}.join(', ')
      ip += ':' if ip
      "(#{ip}#{op}#{r})"
    end
    def invocants; ; params.select{|p| p.invocant?} end
    def noninvocants;params.select{|p| !p.invocant?} end
    def positionals; params.select{|p| p.positional?} end
    def name_onlys;  params.select{|p| p.name_only?} end
    def slurpys;     params.select{|p| p.slurpy?} end
    def types;       params.select{|p| p.type?} end
    def exes;        params.select{|p| p.exe?} end
    def slurpy_hash; params.select{|p| p.slurpy_hash?}[0] end
    def slurpy_positionals; params.select{|p| p.slurpy? && (p.scalar? || p.array?)} end
    def slurpy_scalar; params.select{|p| p.slurpy_scalar?}[0] end
    def slurpy_array; params.select{|p| p.slurpy_array?}[0] end
#    def named_names; nameds.map{|p|p.variable.leafname} end
    def positional_arity_min; positionals.select{|p| !p.optional?}.size end
    def positional_arity_max; slurpy_array ? Inf : positionals.size end
#    def nameds_required; nameds.select{|p| p.required?} end
  end
end


###*** PastToRb

module Past
  class PastObject
    def rb_type_mangle(tn)
      tn =~ /^Rb::/ ? tn.slice(4..-1) : ''+tn
    end
    def varname(n)
      n.sub(/^(?=[A-Z])/,'_')
    end
  end
  class Rx
    def emit_rb;
      pat = @pattern.dup
      if pat =~ /^{/
        pat = pat.gsub(/^./,'').gsub(/.$/,'').gsub(/\//,'\\/')
        pat = "/#{pat}/"
      end
      "#{pat}x"
    end
  end
  class PPair
    def emit_rb;
      v = @value ? @value.emit_rb : "1"
      "Pair.new(#{@name.inspect},#{v})"
    end
  end
  class WrappedPPair
    def emit_rb;
      "WrappedPair.new(#{pair.emit_rb})"
    end
  end
  class ArrayLiteral
    def emit_rb; '['+@array.map{|e|e.emit_rb}.join(', ')+'].delisty' end
  end
  class HashLiteral
    def emit_rb; '{'+@thehash.map{|k,v|"#{k.emit_rb}=>#{v.emit_rb}"}.join(', ')+'}' end
  end
  class PClass
    def emit_rb
      n = rb_type_mangle(@name)
      b = @body.emit_rb
      "class #{n}\n  init_class(self,Module.nesting)\n#{indent(b)}\nend\n"
    end
  end
  class MemberDeclaration
    def emit_rb
      vm = "#{var.emit_rb_def}m"
      code = "attr_accessor :#{vm}"
      va = "#{var.leafname}C"
      v = var.emit_rb
      code += "\ndef #{va}() ->(_S,selfS){ #{v} }end"
      gs = gensym
      sigex = var.rb_sigex
      code += "\nalias :initialize_#{gs} :initialize\ndef initialize(*a) initialize_#{gs}(*a); @#{vm} ||= BindBox#{sigex}.new end"
      code
    end
  end
  module NotClassHlp
    def emit_rb
      n = rb_type_mangle(@name)
      b = @body.emit_rb
      "module #{n}\n  init_nonclass(self,Module.nesting)\n#{indent(b)}\nend\n"
    end
  end
  class PRole; include NotClassHlp end
  class PModule; include NotClassHlp end
  class PPackage; include NotClassHlp end
  class UsePackage
    def emit_rb; "useC.call(nil,#{@pkg.inspect})\n" end
  end
  class TypeLiteral
    def emit_rb
      rb_type_mangle(@src.as_s)
    end
  end
  class VarError
    def emit_rb; '$scalar_ERROR' end
  end
  class ClosureTrait
    def emit_rb; "#{@flavor} {\n#{indent(@block.emit_rb)}\n}" end
  end
  class DoBlock
    def emit_rb; "proc{\n#{indent(@block.emit_rb)}\n}.call()" end
  end
  class StandaloneBlock
    def emit_rb; "proc{\n#{indent(@block.emit_rb)}\n}.call()" end
  end
  class CodeBlock
    def emit_rb
      a = @whiteboard[:need_an_arg] ? '_S' : ''
      a = a == '' ? "_S" : "_E,#{a}" 
      "->(#{a}){\n#{indent(@block.emit_rb)}\n}"
    end
  end
  class Comment
    def emit_rb; @comment.gsub(/^/,'# ').sub(/\n?\z/,"\n") end
  end
  class UndefLiteral
    def emit_rb; "pundef" end
  end
  class ListLiteral
    def emit_rb; 'Listy['+@list.map{|e|e.emit_rb}.join(",")+']' end
  end
  class Block
    def emit_rb
      b = @statementlist.emit_rb
      vs = []
      down(:block_toplevel_vardecls).map{|vd|
        next if !vd.my?
        vars = vd.vars
        vars.is_a?(Array) ? vs += vars.map{|v|v.emit_rb} : vs += [vars.emit_rb]
      }
      vs = vs.sort.uniq
      if vs.empty?
        b
      else
        vl = vs.join(',')
        nl = (['nil'] * vs.size).join(',')
        "->(#{vl}){\n#{indent(b)}\n}.call(#{nl})"
      end
    end
  end
  class StatementList
    def emit_rb; @statements.map{|x|x.emit_rb}.join(";\n") end
  end
  class ApplyMethod
    def emit_rb
      f = @f.emit_rb
      if f =~ /^rUBY/
        n = f.slice(4..-1)
        n.sub!(/_P$/,'?')
        "#{@obj.emit_rb}.#{n}(#{@arggen ? @arggen.emit_rb : ''})"
      else
        o = gensym
        n = varname("#{f}Cm")
        a = "#{@arggen ? @arggen.emit_rb : ''}"
        a = a == '' ? "#{o}" : "#{o},#{a}" 
        "(#{o}=#{@obj.emit_rb}).#{n}.call(#{a})"
      end
    end
  end
  class Apply
    def emit_rb
      f = @f.src.as_s
      if f =~ /^rUBY/
        n = f.slice(4..-1)
        n.sub!(/_P$/,'?')
        "#{n}(#{@arggen ? @arggen.emit_rb : ''})"
      elsif f == 'raw_rUBY'
        argl = @arggen.argl.dup
        code = argl.shift.str
        code = "#{code}.call(#{argl.map{|a|a.emit_rb}.join(',')})" if !argl.empty?
        code
      elsif f == 'source_ruby'
        argl = @arggen.argl.dup
        code = argl.shift.str
        code = "source_ruby(<<'_END'\n#{code}\n_END\n);\n"
        code
      elsif f == 'source_perl'
        argl = @arggen.argl.dup
        code6 = argl.shift.str
        rest = ''
        "source_perl(<<'_END'\n#{code6}\n_END\n#{rest});\n"
      elsif f == 'source_perl_compiled'
        argl = @arggen.argl.dup
        perlcode = argl.pop.str
        sum = chksum(perlcode+"\n")
        #print "Comp"+"iled: ",perlcode.length," ",sum,"\n"
        #File.open("deleteme_01","w"){|f|f.write perlcode.split(//).join("\n")}
        rubycode = $P.compile6(perlcode)
        coderb = rubycode.gsub(/\\/,'\\\\').gsub(/\'/,'\\rbbug').gsub(/rbbug/,'\'')
        coderb = "'\n"+coderb+"\n'"
        rest = "'#{sum}',#{coderb},"
        "source_perl_compiled(#{rest}<<'_END'\n#{perlcode}\n_END\n);\n"
      elsif f == 'source_perl_finish'
        "source_perl_finish()"
      else
        n = "#{f}C"
        a = "#{@arggen ? @arggen.emit_rb : ''}"
        a = a == '' ? "_S" : "_S,#{a}" 
        "#{@f.emit_rb}.call(#{a})"
      end
    end
  end
  class Capture
    def emit_rb; @argl.map{|a|a.emit_rb}.join(",") end
  end

  class VarDecl
    def emit_rb
      ini = []
      @vars.each_with_index{|v,i|
        bb = case v.sigil 
             when '$'; 'BindBox'
             when '@'; 'BindBoxa'
             when '%'; 'BindBoxh'
             when '&'; 'BindBoxc'
             else fail("assert") end
        vi = @vals[i]
        val1 = vi ? vi.emit_rb : ''
        val2 = vi ? val1 : 'pundef'
        r = case @op
            when nil; "#{bb}.new()"
            when '='; "#{bb}.new(#{val1})"
            when ':='; val2
            when '::='; val2
            else fail("assert #{op}") end
        ini.push(r)
      }
      lhs = if our?
              @vars.map{|v|v.emit_rb_symtab}.join(',')
            else
              @vars.map{|v|v.emit_rb}.join(',')
            end
      rhs = ini.empty? ? '' : " = "+ini.join(',')
      "#{lhs}#{rhs}"
    end
  end
  class Cond
    def emit_rb
      s = nil
      @body.each{|tc|
        test,cons=*tc
        if not s
          sym = @sense_of_first_test ? 'if' : 'unless'
          s = "#{sym} (#{test.emit_rb}).as_b\n#{indent(cons.emit_rb)}"
        elsif cons
          s += "\nelsif (#{test.emit_rb}).as_b\n#{indent(cons.emit_rb)}"
        else
          cons = test
          s += "\nelse\n#{indent(cons.emit_rb)}"
        end
      }
      s += "\nend"
      s
    end
  end
  class For
    def emit_rb;
      arity = 1
      arity = @body.sig.positional_arity_min if @body.respond_to?(:sig)
      if arity == 1
        @body.whiteboard[:need_an_arg] = 1
        "(#{@enu.emit_rb}).each{|__e__|(#{@body.emit_rb}).call(__e__,__e__)}"
      else
        a=gensym;i=gensym;v=gensym;
        "(#{a} = (#{@enu.emit_rb}); #{i}=0; while #{a}.exists?(#{i}); #{v} = (#{@body.emit_rb}).call(:deadbeef,*(#{a}.slice(#{i},#{arity}))); #{i} += #{arity}; #{v} end)"
      end
    end
  end
  class Identifier
    def emit_rb; @src.as_s end
  end
  class Number
    def emit_rb; "#{@num}" end
  end
  class SimpleString; def emit_rb; @src.as_s.inspect end end
  class String
    def emit_rb; @str.inspect end
  end
  class Quote
    def emit_rb
      case @quote
      when '<';
        s = @str
        "Listy[*('#{s}'.split(/\\s+/))]"
      else fail("assert or unimplemented") end
    end
  end
  class VarSub
    def emit_rb;
      "method('#{@name}')"
    end
  end
  class Variable
    def emit_rb_symtab
      tab = global? ? "symtab['::GLOBAL'].symtab" : "symtab"
      "#{tab}['#{sigil+leafname}']"
    end
    def rb_sigex
      {"\$"=>'S','@'=>'A','%'=>'H','&'=>'C'}[sigil]
    end
    def emit_rb_def
      v = @src.as_s
      decl = declaration
      if !decl || decl.my?
        return "_E" if v == '$+_'
        sigex = rb_sigex
        ns = pathname+sigex
        if twigil == '*'
          return "\$_#{ns}g"
        end
        ns
      else
        emit_rb_symtab
      end
    end
    def emit_rb
      c = emit_rb_def
      case twigil
      when '.'; "selfS.#{c}m"
      when '!'; "selfS.#{c}m"
      else c end
    end
  end
  class Infix
    def emit_rb
      a0 = fargs[0].emit_rb
      a1 = fargs[1].emit_rb
      fs = fun.as_s
      case fs
      when '~'; "(\"\#\{#{a0}}\" + \"\#\{#{a1}}\")"
      when ','; "#{a0} #{fs} #{a1}"
      when '.'; a1hack = fargs[1].src.as_s; "#{a0}#{fs}#{a1hack}"
      when '='; "(#{a0})._(#{a1})"
      when ':=';  "#{a0} = #{a1}"
      when '::='; "#{a0} = #{a1}"
      when '~~'; "smartmatch(#{a0},#{a1})"
      when '=>';
        s0 = fargs[0].src.as_s
        a0 = "'#{s0}'" if s0 =~ /^\w+$/ #ws
        "Pair.new(#{a0},#{a1})"
      when 'eq'; "#{a0}.as_s == #{a1}.as_s"
      when '=:='; "#{a0}.object_id == #{a1}.object_id"
      when '|'; "#{a0}.as_s + '_BOGUS_JUNCTION_' + #{a1}.as_s"
      #when '.='; '"UNIMPLEMENTED .= OP"'
      when '.='; "#{a0}._(#{a0}.#{a1})"
      when '~='; "#{a0}._(#{a0}.as_s + #{a1}.as_s)"
      when /^([-+*\/])=$/; "#{a0}._(#{a0} #{$1} #{a1})"
      else
        fs = {'eq'=>'==','ne'=>'!='}[fs] || fs
        "(#{a0} #{fs} #{a1})"
      end
    end
  end
  class Prefix
    def emit_rb
      as = fargs[0].emit_rb
      fs = fun.as_s
      case fs
      when '?'; "#{as}.as_b"
      when '!'; "!#{as}.as_b"
      when '+'; "#{as}.as_n"
      when '~'; "#{as}.as_s"
      when '%'; "#{as}.as_h0"
      when '++'; y=gensym;"(#{y}=#{as};#{as}._(#{y}+1);#{y})"
      when '--'; y=gensym;"(#{y}=#{as};#{as}._(#{y}-1);#{y})"
      when '\\'; "#{as}" #X
      else "#{fs}#{as}" end
    end
  end
  class Postfix
    def emit_rb
      as = fargs[0].emit_rb
      fs = fun.as_s
      case fs
      when '++'; y=gensym;"(#{y}=#{as};#{as}._(#{y}+1))"
      when '--'; y=gensym;"(#{y}=#{as};#{as}._(#{y}-1))"
      else "#{as}#{fs}" end
    end
  end
  class Postcircumfix
    def emit_rb
      a0 = fargs[0].emit_rb
      a1 = fargs[1].emit_rb
      fs = fun.as_s
      case fs
      when '[ ]'; "#{a0}.get(#{a1})"
      when '{ }'; "#{a0}.get(#{a1})"
      when '< >'; a1 = fargs[1].src.as_s; "#{a0}.get('#{a1}')"
      else "'INVALID POSTCIRCUMFIX #{fs}'" end
    end
  end
  class Word
    def emit_rb; @src.as_s end
  end
  class Parameter
  end
  class Signature
    def emit_rb_multi_pattern
      @params.map{|p|
        p.type ? p.type.src.as_s : 'Object'
      }.join(',')
    end
    def emit_rb_param_list
      assignment_needs_comma = @params.size == 1
      @params.map{|p|
        n = p.variable.emit_rb_def
        n = "*#{n}" if p.slurpy?
        assignment_needs_comma = false if p.slurpy?
        n
      }.join(',') + (assignment_needs_comma ? "," : "")
    end
    def emit_rb_param_list_with_nils
      @params.map{|p|
        n = p.variable.emit_rb_def
        if p.optional?
        then "#{n} = nil"
        else n end
      }.join(',')
    end
    def emit_rb_args_processing
      code = ""
      code += "_argl_,_argn_=_args_sep_(_args_);_arglsz_=_argl_.size\n"

      amin = positional_arity_min
      amax = positional_arity_max.as_s #Inf
      code += "fail(\"Incorrect argument count \#{_arglsz_} vs #{amin}..#{amax}\") if _arglsz_ > #{amax}\n"

      code += @params.size > 0 ? "#{emit_rb_param_list}=*_argl_.delisty\n" : ""

      @params.each{|p|
        v = p.variable.emit_rb_def
        n = p.variable.leafname
        code += "#{v} = ((x = _argn_.delete('#{n}')) && x[-1]) || #{v}\n"
      }
      if slurpy_hash
        code += "#{slurpy_hash.variable.emit_rb} = _argn_\n"
      else
        code += "fail(\"Unexpected named arguments \#{_argn_.keys.join(',')}\") if not _argn_.empty?\n"
      end

      @params.map{|p|
        v = p.variable.emit_rb_def
        n = p.variable.leafname
        if p.default
          code += "#{v} = #{p.default.emit_rb} if #{v} == nil\n"
        elsif p.optional?
          code += "#{v} = pundef if #{v} == nil\n"
        else
          code += "#{v} == nil && fail(\"Missing argument for required parameter #{n}\")\n"
        end
      }
      code
    end
    def arity; @params.size end
  end
  class SubDecl
    def emit_rb
      k = @kind.gsub(/\s+/,' ')
      k = "multi sub" if k == "multi"
      argsprocess = "#{@sig.emit_rb_args_processing}"
      lam = "->(*_args_){\n  ->(_S,_E){\n  _E=_args_.shift\n#{indent(argsprocess+@body.emit_rb)}\n}[nil,nil]\n}"
      lam2 = "->(*_args_){\n  ->(_S,_E,selfC){\n  _E=_args_.shift\n#{indent(argsprocess)}\n  selfC = ->(_S){selfS}\n#{indent(@body.emit_rb)}\n}[nil,nil,nil]\n}"
      if @scope == 'my'
        case k
        when 'sub'
          fail("assert") if not @name
          n = "#{@name.emit_rb}C"
          "#{n} = #{lam}\n"
        else
          fail("my @k not implemented")
        end
      else
        case k
        when 'sub'
          if @name
            n = "#{@name.emit_rb}C"
            "current_class.def_pkg_var(#{n.to_sym.inspect},#{lam})\n"
          else
            "#{lam}\n"
          end
        when 'method'
          n = varname("#{@name.emit_rb}Cm")
          "current_class.def_pkg_var(#{n.to_sym.inspect},#{lam2})\n"
        when 'submethod'
          n = "#{@name.emit_rb}Csm"
          "current_class.def_pkg_var(#{n.to_sym.inspect},#{lam})\n"
        when 'multi sub'
          n = varname("#{@name.emit_rb}Cm")
          nm = "#{@name.emit_rb}M"
          spc = @sig.emit_rb_multi_pattern
          "multi(#{nm.to_sym.inspect},Object,#{spc},#{lam})\ncurrent_class.def_pkg_var(#{n.to_sym.inspect},->(*args){#{nm}(*args)})\n"
        else fail("#{k} not implemented")
        end
      end
    end
  end
end
#class Module; public :define_method end

###** Runtime

###*** Needed by P6

class SymbolTable
  attr_accessor :hash,:up
  def initialize(up) @hash,@up = {},up end
  def [](i)
    if @hash.member?(i); @hash[i]
    elsif @up; @up[i]
    else fail("Undefined #{i}") end
  end
  def []=(i,v) @hash[i] = v end
end

module HasSymbolTableM
  attr_accessor :pkgname
  def init_symtab(n,up=nil)
    uptable = up ? up.symtab : nil
    @table = SymbolTable.new(uptable)
    @pkgname = n
    @up = up
  end
  def symtab; @table end
  def path; (up ? up.path : []) + [n] end
  def fullname
    if up
      '::*::' + path.join('::')
    else
      fail('assert') if @n != 'GLOBAL'
      '::*'
    end
  end
end

class RPackage
  def self.make_GLOBAL
    g = self.new('GLOBAL')
    g.symtab['::GLOBAL'] = g
    m = self.new('Main',g)
    g.symtab['::Main'] = m
  end
  def initialize(n,up=nil)
    init_symtab(n,up)
  end
  include HasSymbolTableM
  def thebinding; binding end
end

class Class
  include HasSymbolTableM
  def thebinding; binding end
  def init_class(cls,nest)
    cls.name =~ /([^:]+)\z/
    init_symtab($1,nest[1]) if !symtab
  end
end
class Module
  include HasSymbolTableM
  def thebinding; binding end
  def init_nonclass(cls,nest)
    cls.name =~ /([^:]+)\z/
    init_symtab($1,nest[1]) if !symtab
  end
end


###*** P6

require 'readline'
class P6
  attr_accessor :pkgspace
  attr_accessor :verbose
  attr_accessor :provide_yaml_ast
  def initialize
    @verbose=false
    @pkgspace = RPackage.make_GLOBAL
    @provide_yaml_ast = false
  end
  def P6_binding; @pkgspace.thebinding end
  def P6_binding_Main; @pkgspace.symtab['::Main'].thebinding end

  def note(head,val,insp=true)
    print "__________\n#{head}\n#{insp ? val.inspect : val}\n----------\n" if @verbose
  end
  def number_lines(s)
    cnt = 0
    s.split(/\n/).map{|l|cnt+=1; "#{cnt}\t#{l}\n"}.join("")
  end
  def file_prelude
    "\n \#(\n\$ruby_init_code = <<'_CODE'\n#{$ruby_init_code}_CODE\neval(\$ruby_init_code)\n #)\n"
  end
  def compile_file(src,dest)
    src = get_file(src)
    rb = compile6(src)
    f = File.open(dest,"w")
    f.write(file_prelude)
    f.write(rb)
    f.close_write
    rb
  end
  def get_file(fn)
    src = `cat #{fn}`
  end
  def parse(src)
    note :src,src
    tree = $grammar.prog.search(src,0,false)
    note :tree,tree
    tree
  end
  def clean
    (ENV['CLEAN'] && ENV['CLEAN'].to_i > 0) ? ENV['CLEAN'].to_i : nil
  end
  require 'digest/md5'
  def compile6(src,fileline=nil)
    if ENV['TEST_ONLY_PARSE'] && ENV['TEST_ONLY_PARSE'] != '0'
      print "1..2\n"
      tree = parse(src)
      if tree.bool
      then print "ok Parsed something...\n"
      else print "notok Parse failed.\n"
      end
      if tree.to == src.length
      then print "ok Parsed. #{tree.to} vs #{src.length}\n"
      else print "notok Partial\n"
      end
      exit
    end
    print "Parsing #{fileline[0]} line #{fileline[1]}.\n" if fileline and @verbose
    k = Digest::MD5.hexdigest(src)
    if (!clean||clean==2) && File.exists?("redsix_cache/#{k}.rb")
      print "Using redsix_cache/#{k}.rb\n"
      return `cat redsix_cache/#{k}.rb`
    end
    tree = parse(src)
    if not tree.bool
      print "Parse failed.\n"
      return
    end
    if not tree.to == src.length
      print "Parse partial.(#{tree.to} of #{src.length})\n"
    else
      print "Parsed.\n" if @verbose
    end
    ast = PastFromParse::emit(tree)
    ast.walk_past_init
    if provide_yaml_ast
      require "yaml"
      ast.walk_discarding_src
      print YAML::dump(ast)
      ""
    else
      note :ast,ast
      rbc = ast.emit_rb
      note :rbc,number_lines(rbc),false
      if !clean
        File.open("redsix_cache/#{k}.rb","w"){|f|f.print(rbc)}
      end
      rbc
    end
  end    
  def eval6_file(fn)
    fnc = "#{fn}.rb"
    if FileTest.exist?(fnc) && !clean
      print "Using precompiled #{fnc}\n"
      rbc = `cat #{fnc}`
      eval_safe(rbc,nil,fnc,0)
    else
      print "Loading #{fn}\n"
      src = get_file(fn)
      eval6(src,[fn,1])
    end
  end
  def eval6q(src,fileline=nil)
    v = @verbose; @verbose = false;
    res = eval6(src,fileline); @verbose = v;
    res
  end
  def eval6(src,fileline=nil)
    rbc = compile6(src,fileline)
    print "====================\n" if @verbose
    res = eval_safe(rbc,
                    nil,"compiled:#{fileline ? fileline[0] : ''}",
                    fileline ? fileline[1] : 0)
    print "=> ",res.inspect,"\n" if @verbose
    res
  end
  def eval_safe(rbc,binding=nil,file=nil,line=nil)
    binding ||= self.P6_binding_Main
    eval('_S = BindBox.new(pundef); _E = BindBox.new(pundef)',binding)
    begin res = eval(rbc,binding,file,line)
    rescue NameError => boom
      print number_lines(rbc),boom,"\n",boom.backtrace.join("\n"),"\n"
    rescue SyntaxError => boom
      print number_lines(rbc),boom,"\n",boom.backtrace.join("\n"),"\n"
    end
    res
  end
  def repl
    begin
      histfile = File::expand_path("deleteme_hist")
      if File::exists?(histfile)
        Readline::HISTORY.push(*(eval(IO.read(histfile))||[]))
      end
      while true
        #src = gets
        src = Readline.readline("\033[0;31m6\033[0m: ",true)
        #src = begin readline rescue "" end
        if src == "" || !src;print "\n"; break end
        if src == "\n"; break end
        res = eval6(src,['repl-input',1])
        print "=> ",res.inspect,"\n" if not @verbose # because eval6() did it if @verbose.
      end
    rescue Interrupt
      exit
    ensure
      h = Readline::HISTORY.to_a.reverse.uniq.slice(0,100).reverse.inspect
      open(histfile,"w"){|io|io.puts(h)}
    end
  end
end
$P = P6.new
def repl; $P.repl end

###*** Force include() to work with dynamic modules.

$module_included_by = {}
class Module
  alias :append_features_pre_ModuleIncludeGraph :append_features
  def append_features(m)
    d = $module_included_by[self] ||= []
    d.push(m)
    append_features_pre_ModuleIncludeGraph(m)
  end
  def self.refresh_includes_of(m0)
    todo = [m0]
    while !todo.empty?
      m = todo.shift
      d = $module_included_by[m] || []
      d.each{|m1|
        m.send(:append_features_pre_ModuleIncludeGraph,m1)
      }
      todo += d
    end
  end
  alias :append_features_pre_ModuleIncludeDynamic :append_features
  def append_features(m)
    append_features_pre_ModuleIncludeDynamic(m)
    Module.refresh_includes_of(m)
  end
end

###*** Multiple inheritance

$clasule_of = {}
module ClasuleInstanceM
  def list_is;   @@list_is   ||= [] end
  def list_does; @@list_does ||= [] end
end
module ClasuleM
  def _clasule_new
    m = Module.new
    m.module_eval('include ClasuleInstanceM')
    $clasule_of[self] = m
    include m
    def_is(superclass) if superclass
    m
  end
  def clasule; $clasule_of[self] || _clasule_new end
  def def_is(k)
    self.clasule.append_features(k.clasule)
    self.clasule.list_is.push(k)
  end
  def def_does(k)
    self.clasule.append_features(k.clasule)
    self.clasule.list_does.push(k)
  end
end
class Class; include ClasuleM end
class Module; include ClasuleM end
class Object
  def obj_clasule; (class << self; self end).clasule end
end
class Module
  def defc(sym,b) clasule.send(:define_method,sym,b) end
end


###*** Multi-method dispatch
#  Derived from Christopher Cyll's multi-0.1. (md5 ...30826914ae  multi.rb)
# Very not the right thing.

def multi(method_name, *patterns, &body)
  body ||= patterns.pop
  Multi::DISPATCHER.add(Multi::Dispatch, self, method_name, patterns, body)
end

module Multi
  class Dispatch
    def initialize(patterns, body)
      @patterns = patterns
      @body = body
    end
    
    def match?(params)
      pairs = params.zip(@patterns)
      return pairs.all? do |param, pattern|
        if pattern.kind_of?(Class)
          param.kind_of?(pattern)
        elsif pattern.instance_of?(Proc)
          begin
            pattern.call(param)
          rescue
            false
          end
        elsif pattern.instance_of?(Regexp)
          pattern.match(param)
        else
          param == pattern
        end
      end
    end
    
    def call(params, block)
      @body.call(*params, &block)
    end
  end

  class Dispatcher
    def initialize
      @map = {}
    end

    def add(type, obj, method_name, patterns, body)
      method_name = method_name.id2name if method_name.kind_of?(Symbol)
      body = patterns.pop if body.nil?
      klass = obj.kind_of?(Module) ? obj : class << obj; self end

      key = [klass, method_name]
      @map[key] ||= []
      @map[key].push(type.new(patterns, body))

      if ! obj.methods.include?(method_name)
        klass.send(:define_method,method_name.to_sym,proc{|*params, &block|
                          Multi::DISPATCHER.dispatch(klass, method_name, params, block)
                      })
      end
    end

    def dispatch(klass, method_name, params, block)
      candidates = @map[[klass, method_name]]
      handler = candidates.find{|candidate| candidate.match?(params) }
      if handler.nil?
        printed_params = params.map{|param| param.inspect}.join(', ')
        raise "No match for #{obj}.#{method_name}(#{printed_params})"
      end
      handler.call(params, block)
    end
  end

  DISPATCHER = Dispatcher.new()
end

###*** Dynamic env
# thanks to Tanaka Akira 

module Kernel
  def with_context(params)
    Thread.current[:dynamic] ||= []
    Thread.current[:dynamic].push params
    begin
      yield
    ensure
      Thread.current[:dynamic].pop
    end
  end

  def find_in_context(name)
    Thread.current[:dynamic].reverse_each {|params|
      return params[name] if params.has_key? name
    }
    raise "Can't find context value for #{name}"
  end
end

###*** Basics

module Kernel
  def current_class; self.is_a?(Class) ? self : self.class end #X
end

module Kernel
  def _args_sep_(args)
    pa = []; na = {}
    args.each do |a|
      if a.pair?
        (na[a.name] ||= []).push(a.value)
      else
        a = a.unwrap if a.wrapped_pair?
        pa.push(a)
      end
    end
    [pa,na]
  end
end
class Object
  def pair?; false end
  def wrapped_pair?; false end
end
class Pair; def pair?; true end end

class Module
  def def_pkg_var(sym,val)
#    p self,self.class,methods.grep(/#{sym}/)
#    p begin eval "#{sym}" rescue nil end
    class_eval %{
      def #{sym}; @@#{sym} end
      def #{sym}=(v); @@#{sym} = v end
    }
#    p self,self.class,methods.grep(/#{sym}/),methods.grep(/#{sym}/)
    class_eval %{
      @@#{sym} = ObjectSpace._id2ref(#{val.object_id})
    }
  end
end

module Kernel
  def _S; pundef end
end

class Object; def listy?; false end end

###*** BindBox

require 'delegate'
class BetterDelegator < Delegator; end
class << Object
  alias :pre_BetterDelegator_method_added :method_added
  def method_added(id)
    #print "method_added(#{id.id2name}) on #{self}\n"
    if self == Object
      #print "punting #{id.id2name}\n"
      BetterDelegator.send(:undef_method,id)
    end
    pre_BetterDelegator_method_added(id)
  end
end

class Object; def __getobj__; self end end
module ScalarSetM
  def _(*opt)
    o, = *opt
    o, = *o.to_a if o.listy?
    o ||= pundef
    __setobj__(o)
  end
end

class BindBoxKeyed < BetterDelegator
  attr_accessor :aggregate,:key
  def initialize(aggregate,key) @aggregate,@key=aggregate,key end
  def __getobj__; @aggregate[@key] || pundef end
  def __setobj__(o) @aggregate[@key] = o end
  include ScalarSetM
end
class BindBox < BetterDelegator
  attr_accessor :__getobj__
  def initialize(*args)
    super(nil)
    _(*args)
  end
  def __setobj__(o)
    @__getobj__= o
  end
  include ScalarSetM
end
class BindBoxa < BindBox
  def _(*args)
    a = []
    args.each{|e|
      if e.listy? || e.is_a?(Array) || e.is_a?(Range)
      then a.push(*e.to_a) 
      else a.push(e) end
    }
    super(a)
  end
end
class BindBoxh < BindBox
  def self._fudge_init(a)
    o = {}
    a.pop if a == [nil]
    a = a.delisty
    while not a.empty?
      k = a.shift
      v = nil
      if k.isaCm.call(nil,"Pair").as_b
        k,v = k.name,k.value
      elsif k.wrapped_pair?
        p = k.unwrap
        k,v = p.name,p.value
      else
        fail("Odd number of elements initializing hash") if a.empty?
        v = a.shift
      end
      o[k] = v
    end
    o
  end
  def _(*a)
    o = BindBoxh._fudge_init(a)
    super(o)
  end
end
class BindBoxc < BindBox
end
BindBoxS = BindBox
BindBoxA = BindBoxa
BindBoxH = BindBoxh
BindBoxC = BindBoxc

###*** some subs
class Undef; end
module Kernel; def pundef; Undef.new end end

def df(sym,b)
  sym = "#{sym}C".to_sym
  bb = BindBox.new(b)
  eval %{
    module Kernel
      def_pkg_var(#{sym.to_sym.inspect},ObjectSpace._id2ref(#{bb.object_id}))
    end
  }
end

df :say,->(_E,*args){ print *args.map{|a|a.as_s},"\n"}
df :defined,->(_E,x){ x.is_defined}
df :substr,->(_E,s,off,len){ s.slice(off,len)}
df :split,->(_E,re,s){ s.split(re)}
df :die,->(_E,*args){ fail(*args)}
df :try,->(_E,b){
  $scalar_ERROR = pundef
  begin
    b.call(_E)
  rescue Exception
    $scalar_ERROR = "#{$!}"
    pundef
  end
}
df :use,->(_E,c){
  puse(c)
}
def puse(n)
  fn = "#{n}.pm"
  if fn == "Test.pm" and not FileTest.exist?("Test.pm")
    fn = "misc/pX/Common/redsix/Test.pm"
  end
  $P.eval6_file(fn)
end

###*** misc
###**** is_defined
class BindBoxKeyed; def is_defined; __getobj__.is_defined end end
class BindBox; def is_defined; @__getobj__.is_defined end end
class Object; def is_defined; true end end
class Undef;  def is_defined; false end end
class Object; def definedCm;->(_S){self.is_defined}end end
class NilClass; def is_defined; false end end #X
###**** as_s
class BindBoxKeyed; def as_s; __getobj__.as_s end end
class BindBox; def as_s; @__getobj__.as_s end end
# class Object; def as_s; is defined earlier.
class Array; def as_s; map{|e|e.as_s}.join(" ") end end
class Undef; def as_s; "" end end
class Pair; def as_s; "#{name.as_s}\t#{value.as_s}" end end
# class Float; def as_s; is defined elsewhere.
###**** as_i
class BindBoxKeyed; def as_i; __getobj__.as_i end end
class BindBox; def as_i; @__getobj__.as_i end end
class Object; def as_i; to_i end end
class Array;  def as_i; size end end
class Listy;  def as_i; size end end
###**** as_b
class BindBoxKeyed; def as_b; __getobj__.as_b end end
class BindBox; def as_b; @__getobj__.as_b end end
class Object; def as_b; true end end
class Fixnum; def as_b; self == 0 ? false : true end end
class FalseClass; def as_b; false end end
class NilClass; def as_b; false end end
class Undef; def as_b; false end end
###**** as_h
class Hash; def as_h0; self end end
class Array;
  def as_h0;
    fail("Odd number of elements in array") if size % 2 != 0
    Hash[*self]
  end
end
###**** as_n
# is elsewhere
###**** ref
class BindBox; def refCm;proc{ @__getobj__.refCm.call }end end
class Object; def refCm;proc{ "#{self.class}" }end end
class String; def refCm;proc{ "Str" }end end
class TrueClass;  def refCm;proc{ "Bool" }end end
class FalseClass; def refCm;proc{ "Bool" }end end
class Proc; def refCm;proc{ "Code" }end end
###**** true false
class Object
  class Blah
    attr_accessor :trueC,:falseC
    def initialize
      @trueC = BindBox.new(->(_E){ 1 == 1})
      @falseC = BindBox.new(->(_E){ 1 == 2})
    end
    self
  end
  BLAH = Blah.new
  def bool; BLAH end
end
###**** isa
module Any; end
class Object; include Any; end
module Bool; end
class TrueClass; include Bool end
class FalseClass; include Bool end
module Str; end
class String; include Str; end
module Num; end
module Int; include Num end
class Fixnum; include Int; end
class Float;  include Num; end
module Sub; end
module Block; end
module Code; end
class Proc;  include Code; end
module NotAModuleHack; end
class Object
  # also used by Array/List isaCm
  def isa_hlp(t) t.is_a?(Module) ? t : begin eval("#{t}")
                                       rescue Exception; NotAModuleHack end 
  end
  def isaCm;->(_E,s){ self.is_a?(isa_hlp(s)) }end
end
class BindBox; def isaCm;->(_E,s){ @__getobj__.isaCm.call(_E,s) }end end
###**** goto
class Method; def gotoCm;->(*args){ call(*args) }end end
class Proc; def gotoCm;->(*args){ call(*args) }end end
$scalar_ERROR = pundef
###**** arity
module Code; def arityCm;->(_E){ self.arity }end end
 #... but sub() procs all have *args so -1 arity.
 # So need to wrap and label them.  Something for another time.
###**** Range
class Range
  def [](i) self.to_a[i] end
  def to_i; self.to_a.size end
end
###**** List
# Object#listy? is above.
class Array
  def exists?(n) n >= 0 && n < self.size end
  def listyconcat(*as) ListyThing.new(self,*as) end
  def delisty
    if find{|e| e.listy?} 
      a = []
      each{|e| e.listy? ? a.push(*e.to_a) : a.push(e)}
      a
    else self end
  end
end
class Range; def listy?; true end end
module ListyThingM; end
class Listy
  def listy?; true end
  include ListyThingM
  def self.[](*elems) new(*elems) end
  attr_accessor :parts,:offsets
  def initialize(*elems)
    @parts = []
    normal = nil
    elems.each{|e|
      if e.kind_of?(ListyThingM)
        @parts.push(normal) if normal; normal = nil
        @parts.push(e)
      else
        normal ||= []
        normal.push(e)
      end
    }
    @parts.push(normal) if normal
    @parts.push([]) if @parts.empty?
    @offsets = [0]
  end
  def [](n)
    pi,off = _find_index(n)
    pi ? parts[pi][off] : nil
  end
  def []=(n,v)
    pi,off = _find_index(n)
    pi ? (parts[pi][off] = v) : nil
  end
  def size
    if parts.size > offsets.size
      off = offsets[-1]
      (offsets.size..(parts.size-1)).each{|pi|
        off = offsets[pi] = off + parts[pi-1].size
      }
    end
    offsets[-1] + parts[-1].size
  end
  def _find_index(n)
    if n < 0
      npos = size+n
      return npos < 0 ? nil : self._find_index(size+n)
    end
    pi = @offsets.size-1
    pi -= 1 while @offsets[pi] > n
    while true
      off = n - @offsets[pi]
      return [pi,off] if (@parts.size == @offsets.size ||
                          @parts[pi].exists?(off))
      @offsets[pi+1] = @offsets[pi] + @parts[pi].size
      pi += 1
    end
  end
  def exists?(n) n >= 0 && n < size end #X
  def listyconcat(*as) self.class.new(self,*as) end
  def each(&b) parts.each{|p| p.each(&b)};self end
  def to_a; a=[]; each{|e| a.push(e)}; a end
  def map(&b) to_a.map(&b) end
end
###**** Array
module ArrayM
  def get(k) BindBoxKeyed.new(self,k) end
  def elemsCm;->(_E){ self.size }end
  def endCm;->(_E){ self.size-1 }end
  def chompCm;->(_E){ self.map{|e|e.chomp} }end
  def pushCm;->(_E,*args){
      a = args.delisty
      a = a[0] if a.size == 1 && a[0].is_a?(Array) #X!
      self.push(*a)
    }end
  def unshiftCm;->(_E,*args){
      a = args.delisty
      a = a[0] if a.size == 1 && a[0].is_a?(Array) #X!
      self.unshift(*a)
    }end
  def shiftCm;->(_E){ self.shift() || pundef }end
  def popCm;->(_E){ self.pop() || pundef }end
  def isaCm;->(_E,s){
      (s == List || s == 'List' ||
       s == Array || s == 'Array' ||
       self.is_a?(isa_hlp(s)))
    }end
  def existsCm;->(_E,k){
      k = size+k if k < 0
      k >= 0 && exists?(k)
    }end
  def sortCm;->(_E){
      sort{|a,b| a.as_s <=> b.as_s}
    }end
  def joinCm;->(_E,s){
      map{|e|e.as_s}.join(s.as_s)
    }end
  def mapCm;->(_E,b){
      map{|e| b.call(e)} #X?
    }end
  def deleteCm;->(_E,*ks){
      ks.map{|k|
        e = self[k];
        (k == -1 || k == size - 1) ? delete_at(k) : (self[k] = pundef);
        e
      }
    }end
  def keysCm;->(_E){(0..(size-1)).to_a}end
  def valuesCm;->(_E){dup}end
  def kvCm;->(_E){a=[];each_with_index{|v,k|a.push(k,v)};a}end
  def pairsCm;->(_E){a=[];each_with_index{|v,k|a.push(Pair.new(k,v))};a}end
  def splice(off,len=nil,*v)
    off = 0 if not off
    len = size if not len
    r = self[off,len]
    self[off,len]=v
    r
  end
  def spliceCm;->(_E,off=nil,len=nil,*lst){splice(off,len,*lst)} end
end
List = Listy
class Listy
  include ArrayM
end
class Array
  include ArrayM
end
class Range
  include ArrayM
  def size; self.to_a.size end
end
###**** Hash
class Hash
  def get(k) BindBoxKeyed.new(self,k) end
  def existsCm;->(_E,k){
      self[k] && true
    }end
  def pairsCm;->(_E){
      map{|k,v|Pair.new(k,v)}
    }end
  def deleteCm;->(_E,*ks){
      ks.map{|k| e = delete(k); e || pundef}
    }end
  def keysCm;->(_E){keys}end
  def valuesCm;->(_E){values}end
  def kvCm;->(_E){a=[];each{|k,v|a.push(k,v)};a}end
  def sortCm;->(_E){
      sort{|a,b| a[0].as_s <=> b[0].as_s}
    }end
end
###**** Pair
class Pair
  attr_accessor :name,:value
  def initialize(name,value) @name,@value=name,value end
  def nameCm;->(_E){ @name }end
  def keyCm;->(_E){ @name }end
  def valueCm;->(_E){ @value }end
  def keysCm;->(_E){ [@name] }end
  def valuesCm;->(_E){ [@value] }end
  def kvCm;->(_E){ [@name,@value] }end
  def pairsCm;->(_E){[self]}end
  def <=>(o) c = name <=> o.name; c != 0 ? c : value <=> o.value end
end
class WrappedPair
  attr_accessor :pair
  def initialize(pair) @pair=pair end
  def unwrap; @pair end
  def wrapped_pair?; true end
  def __getobj__; @pair end
end
###**** smartmatch
class P6
#  multi(:smartmatch,Object,Object){|a,b| a == b}
#  multi(:aaa,Object){ 42 }
#  fail('assert') if aaa(3) != 42
#  p self,"===================="
end
###**** ...

class Fixnum; def hex; "#{self}".hex end end

class Fixnum; def as_n; self end end
class Float; def as_n; self end end
class String
  def as_n
    case self
    when /^\+?Inf(inity)?$/; Inf
    when /^\-Inf(inity)?$/; -Inf
    when 'NaN'; NaN
    when /^(0x[0-9a-f]+)/i; eval($1)
    when /^(0o[0-7]+)/i; eval($1)
    when /\.|^[0-9]+e/; f = to_f; f == f.floor ? f.to_i : f
    else to_i end
  end
end
class Float
  def as_s
    if nan?; 'NaN'
    elsif infinite?; self < 0 ? '-Inf' : 'Inf'
    else self == floor ? to_i.to_s : to_s
    end
  end
end
class Fixnum
  def infinite?; false end
  def nan?; false end
end
Kernel::Inf = 1.0/0
Kernel::NaN = 0.0/0
class Range; def as_n; to_a.size end end
class Array; def as_n; size end end
class Listy; def as_n; size end end
class Hash; def as_n; size end end
  
class Object; def asCm;->(_S,f){sprintf(f,self)}end end
class String
  def asCm;->(_S,f){
      begin
        sprintf(f,self)
      rescue ArgumentError
        sprintf(f,self.to_f)
      end
    }
  end
  def lcfirstCm;->(_S){gsub(/^./){|c|c.downcase}} end
  def lcCm;->(_S){downcase} end
  def ucfirstCm;->(_S){gsub(/^./){|c|c.upcase}} end
  def ucCm;->(_S){upcase} end
  def capitalizeCm;->(_S){gsub(/\w+/){|w|w.capitalize}} end
  def bytesCm;->(_S){length} end
  def charsCm;->(_S){length} end
  def indexCm;->(_S,ss,from=0){index(ss,from)||-1} end
  def rindexCm;->(_S,ss,from=0){rindex(ss,from)||-1} end
  def chompCm;->(_S){chomp} end
  def transCm;->(_S,*m){s=self;m.each{|p|s = s.tr(p.name,p.value)};s} end
end
class Array; def asCm;->(_S,f,j){ self.map{|e|e.asCm.call(_S,f)}.join(j) }end end
class Listy; def asCm;->(_S,f,j){ self.map{|e|e.asCm.call(_S,f)}.join(j) }end end
class Hash; def asCm;->(_S,f,j){ self.map{|k,v|sprintf(f,k,v)}.join(j) }end end
class Pair; def asCm;->(_S,f){ sprintf(f,name,value) }end end

require 'rbconfig' # for Config::CONFIG

class Object; def self.newCm;->(_S,_self,*a){ new(*a) }end end

###** main
#print $grammar.parse.search((" "*1000)+"4").inspect

# $P.eval6q($six_rt) if not (ENV['TEST_ONLY_PARSE'] && ENV['TEST_ONLY_PARSE'] != 0)

def main
  # Deal with arguments ./pugs -Bredsix and test-redsix provide.
  #  STDERR.print ARGV.join(", "),"\n"
  while(not ARGV.empty? and ARGV[0] =~ /^--pugs=|^-Bredsix$|^-w$/)
    ARGV.shift() 
  end
  if not ARGV.empty? and ARGV[0] =~ /^-B$/
    ARGV.shift()
    ARGV.shift()
  end
  if not ARGV.empty? and ARGV[0] =~ /^-v$/
    ARGV.shift()
    $P.verbose = true
  end
  if not ARGV.empty? and ARGV[0] =~ /^--yaml$/
    ARGV.shift()
    $P.provide_yaml_ast = true
  end

  if ARGV.empty?
    $P.repl
  else
    #STDERR.print ARGV.inspect,"\n\n"
    exec("perl",*ARGV) if ARGV[0] == '-le' # pugs "make test" :/
    ARGV.shift if ARGV[0] == '-w'
    if ARGV[0] == '-c'
      print_usage_and_exit if ARGV.size != 3
      $P.compile_file(ARGV[1],ARGV[2])
    elsif ARGV[0] == '-e'
      $P.eval6(ARGV[1])
    elsif ARGV[0] =~ /^-/
      print_usage_and_exit
    else
      print "Parseing #{ARGV[0]}\n"
      $P.eval6_file(ARGV[0])
    end
  end
end
def print_usage_and_exit
  STDERR.print <<_USAGE; exit(2)

#{$0} [ARGS]

With no ARGS, runs an interactive read-eval-print-loop.

  -v      Turn on verbose.

  --yaml  Print ast as yaml, instead of running.

  -e EXPRESSION_TO_RUN

  FILE_TO_RUN

  -c FILE_TO_COMPILE TO_FILENAME

_USAGE
end
_END
);
;
source_perl_compiled(0,'',<<'_END'
#say("Welcome to red clam.")
sub abs($n=$+_) { $n.rUBYabs }
sub sign($n) { $n <=> 0 }
sub rand($n=1) { rUBYrand($n.rUBYas_i) }
sub exp($n) { Rb::Math.rUBYexp($n.rUBYto_f) }
sub sqrt($n) { Rb::Math.rUBYsqrt($n.rUBYto_f) }
sub log($n) { Rb::Math.rUBYlog($n.rUBYto_f) }
sub log10($n) { Rb::Math.rUBYlog10($n.rUBYto_f) }
sub int($x) {
  my $n = $x.rUBYas_n;
  if $n.rUBYinfinite_P { $n }
  elsif $n.rUBYnan_P { $n }
  elsif $n >= 0 { $n.rUBYfloor }
  else { $n.rUBYceil }
}
sub shift($o){$o.shift}
sub pop($o){$o.pop}
sub isa($o,$t){$o.isa($t)}
sub ref($o){$o.ref}
sub push($o,*@a){$o.push(*@a)}
sub unshift($o,*@a){$o.unshift(*@a)}
sub splice($o,*@a){$o.splice(*@a)}
sub eval($code){rUBYeval6q($code)}
sub sprintf($f,*@a){rUBYsprintf($f,*@a)}
sub hash(*@a){ my %h = *@a; %h }
sub elems($a){ $a.elems }
sub end($a){ $a.end }
sub undefine($x){$x = undef}
sub undef(){rUBYpundef}
sub lcfirst($s=$+_){$s.lcfirst}
sub lc($s=$+_){$s.lc}
sub ucfirst($s=$+_){$s.ucfirst}
sub uc($s=$+_){$s.uc}
sub capitalize($s=$+_){$s.capitalize}
sub bytes($s=$+_){$s.bytes}
sub chars($s=$+_){$s.chars}
sub index($s=$+_,$ss,$from=0){$s.index($ss,$from)}
sub rindex($s=$+_,$ss,$from=0){$s.rindex($ss,$from)}
#sub chomp($a){ $a.chomp }
sub chomp($s=$+_){$s.chomp}
sub trans($s,*@m){$s.trans(*@m)}
sub keys($o){$o.keys}
sub values($o){$o.values}
sub kv($o){$o.kv}
sub sort($o){$o.sort}
sub qw(*@a){@a}
sub pi(){raw_rUBY('Math::PI')}
sub sin($n){raw_rUBY('Math.sin(nS.as_n)')}
sub cos($n){raw_rUBY('Math.cos(nS.as_n)')}
sub tan($n){raw_rUBY('Math.tan(nS.as_n)')}
sub asin($n){raw_rUBY('Math.asin(nS.as_n)')}
sub acos($n){raw_rUBY('Math.acos(nS.as_n)')}
sub atan($n,$n2=undef){
  $n2 ?? raw_rUBY('Math.atan2(nS.as_n,n2S.as_n)') !! raw_rUBY('Math.atan(nS.as_n)') }
our $*OS = raw_rUBY('Config::CONFIG["host_os"]');
our $*OSVER = undef;
our $*PERVER = "5.13.0";
our $*PUGS_BACKEND = "BACKEND_PUGS"; # lie for tests :(
our @*ARGS = raw_rUBY('ARGV');
#our %*ENV = raw_rUBY('ENV');
our $*IN  = raw_rUBY('STDIN');
our $*OUT = raw_rUBY('STDOUT');
our $*ERR = raw_rUBY('STDERR');
our $*PID = raw_rUBY('Process.pid');
our $*UID = raw_rUBY('Process.uid');
our $*GID = raw_rUBY('Process.gid');
say "Welcome! :)"
_END
);
;
source_perl_finish();
source_ruby(<<'_END'
main
_END
);
;
source_ruby(<<'_END'
# (setq ruby-here-doc-beg-re "<<\\(-\\)?\\(\\([a-zA-Z0-9][a-zA-Z0-9_]*\\)\\|[\"]\\([^\"]+\\)[\"]\\|[']\\([^'_][^']*\\)[']\\)")
###* Footer
#; Local Variables:
#; mode: outline-minor
#; outline-regexp: "###[*]+"
#; ruby-indent-level: 2
#; perl-indent-level: 2
#; perl-continued-statement-offset: 2
#; perl-continued-brace-offset: -2
#; indent-tabs-mode: nil
#; End:
#; vim: shiftwidth=2:
_END
);
