require 'strscan'

require 'match'


# Perl grammar helpers

def say(*args); print *args;print "\n"; end

class Object
  def bool; true; end
end
class FalseClass; def bool; false; end; end
class NilClass; def bool; false; end; end

module Kernel
  def my(v); end
end

class Grammar
  attr_accessor :permit_partial_parse
  def initialize(orig,at=0)
    @scanner = StringScanner.new(orig)
    @str = orig
    @eat_cache = {}
    @ws_from = @ws_to = false
    @scanner.pos = at
    if at != 0
      @permit_partial_parse = true
      $env_vars.scope_enter(:unitstopper)
      $env_vars[:unitstopper] = "_EOS"
    end
  end
  def pos; @scanner.pos; end
  def fail_at(n); @scanner.pos = n; false; end
  def scan(re); @scanner.scan(re); end
  def eat(str); @scanner.scan((@eat_cache[str] ||= Regexp.new(Regexp.quote(str)))); end
  def let_pos(&blk)
    b = @scanner.pos
    v = blk.call()
    @scanner.pos = b if not v
    v
  end
  def rul(&blk)
    let_pos{ wsp and v= blk.call() and wsp and v }
  end
    


  def _match_from(from,h=nil,rule=nil)
    h ||= {}
    Match.new(@str,from,@scanner.pos,true,h,nil,rule)
  end
  def _match_pat(re,rule=nil)
    b = @scanner.pos
    return false if !@scanner.scan(re)
    _match_from(b,nil,rule)
  end

  def _line_and_indent_of_offset(off)
    lines = @str.slice(0,off).split(/\n/)
    sz = lines.length
    if sz == 0
      [1,0]
    else
      [sz,lines[-1].length]
    end
  end
  def _picture_of_offset(off)
    bot = off - 30;  bot = 0 if bot < 0
    below = @str.slice(bot,off-bot)
    above = @str.slice(off,30)
    below = below.gsub(/\t/,'\t').gsub(/\n/,'\n')
    above = above.gsub(/\t/,'\t').gsub(/\n/,'\n')
    prefix = "WHERE:"
    prefix1 = " "
    indent = below.size > 0 ? prefix1+(" " * (below.size-1)) : ""
    prefix+prefix1+below+above+"\n"+prefix+indent+"/\\<-- HERE\n"
  end
  def panic(msg)
    line_num,char_in_line = _line_and_indent_of_offset(pos)
    picture = _picture_of_offset(pos)
    raise "panic at line #{line_num} column #{char_in_line} (pos #{pos}): #{msg}\n#{picture}"
  end

  def null; true; end

  def before(re=nil,&blk)
    if re
      @scanner.check(re)
    else
      b = @scanner.pos
      v = blk.call()
      @scanner.pos = b
      v ? true : false
    end
  end
  def after(re)
    # no look-behind in 1.8 :(
    s = @str.slice(0,pos)
    Regexp.new("#{re}\\z").match(s) ? true : false
  end

  def seqTOK(f=nil,&blk)
    fun = f || blk
    b = pos
    v = fun.call() ? v : fail_at(b)
  end

  def quesTOK(f=nil,&blk)
    fun = f || blk
    v = fun.call()
    v ? [v] : []
  end
  def starTOK(f=nil,&blk)
    fun = f || blk
    a = []
    while v = fun.call(); a.push(v); end; a
  end
  def plusTOK(f=nil,&blk)
    fun = f || blk
    v = fun.call() or return false
    a = [v]
    while v = fun.call(); a.push(v); end; a
  end

  def quesRULE(f=nil,&blk)
    fun = f || blk
    wsp
    v = fun.call(); wsp
    v ? [v] : []
  end
  def starRULE(f=nil,&blk)
    fun = f || blk
    a = []
    p = pos
    wsp
    while v = fun.call()
      a.push(v); wsp
      p1 = pos; break if not p < p1; p = p1
    end
    a
  end
  def plusRULE(f=nil,&blk)
    fun = f || blk
    wsp
    v = fun.call() or return false
    a = [v]
    p = pos
    wsp
    while v = fun.call()
      a.push(v); wsp
      p1 = pos; break if not p < p1; p = p1
    end
    a
  end

  def quesRX(fun,&more)
    before_fun = pos
    v = fun.call()
    if not v
      if not more
        [[]]
      else
        result = more.call() or return false
        result.unshift([])
      end
    else
      if not more
        [[v]]
      else
        result = more.call()
        if result
          result.unshift([v])
        else
          @scanner.pos = before_fun
          result = more.call() or return false
          result.unshift([])
        end
      end
    end
  end
  def starRX(fun,&more)
    result = plusRX(fun,more) and return result
    if not more
      [[]]
    else
      result = more.call() or return false
      result.unshift([])
    end
  end
  def plusRX(fun,&more)
    before_fun = pos
    v = fun.call() or return false
    if result = starRX(fun,more)
      result[0].unshift(v)
      result
    else
      fail_at(before_fun)
    end
  end
end


# Tokens

class Grammar

  def self.def_precedence(precedence,precedence_hash)
    @@__precedence_hashes__ ||= {}
    @@__precedence_hashes__[precedence] = precedence_hash
    eval("H#{precedence} = precedence_hash")
  end
  def self.def_precedence_alias(precedence,defequiv_precedence)
    precedence_hash = @@__precedence_hashes__[defequiv_precedence]
    def_precedence(precedence,precedence_hash)
  end

  def self.token_category(category,*args)
    eval "@@matcher_for_#{category} = CategoryMatcher.new"
    _help_def_category(category,args)
  end
  def self.rule_category(category,*args)
    eval "@@matcher_for_#{category} = CategoryMatcher.new(true)"
    _help_def_category(category,args)
  end
  def self._help_def_category(category,args)

    # endsym's
    if not args.empty?
      if args[0].instance_of?(Hash)
        k = args[0].keys[0]
        v = args[0].values[0]
      else
        k = args[0]
        v = nil
      end
      case k
      when 'nofat' # / >> <nofat> /
        print "# caveat: #{category} endsym nofat unimplemented\n" if not $quiet
      when 'nofat_space' # / \s+ <nofat> /
        print "# caveat: #{category} endsym nofat_space unimplemented\n" if not $quiet
      when 'unspacey' # / <.unsp>? /
        print "# caveat: #{category} endsym unspacey unimplemented\n" if not $quiet
      when 'endsym'
        print "# caveat: #{category} endsym unimplemented\n" if not $quiet
      else
        p category, k
        raise "bug"
      end
    end

    eval "@@__precedences_for_#{category}_symbols__ = {}"

    eval <<-END
      def #{category}
        b = @scanner.pos
        sym,v = @@matcher_for_#{category}.longest_token_match(self,@scanner)
        sym or return false

        if v.instance_of?(Match)
          v.rule = "#{category}:\#{v.rule||"kludge_node"}"
          m = v
        else
          h = v.is_a?(TrueClass) ? nil : {:kludge_name =>v}
          m = _match_from(b,h,'#{category}')
        end

        if precedence = @@__precedences_for_#{category}_symbols__[sym]
          if precedence_hash = @@__precedence_hashes__[precedence]
            precop_method(m,precedence_hash)
          else
            precedence_hash.is_a?(FalseClass) or
              raise "bug: unknown return precedence class: \#{precedence.capitalize}"
          end
        end
        m
      end
    END

  end
    


  def self.def_tokens_simple(category, precedence, syms)
    syms.each{|sym| _def_token(category, sym, nil, precedence) }
  end
  def self.def_rules_rest(category, left_syms, rest_code)
    def_tokens_rest(category, false, left_syms, 'wsp; '+rest_code)
  end
  def self.def_tokens_rest(category, precedence, left_syms, common_rest_code)
    left_syms.each{|sym|
      rest = _methodify_rest_code(category,sym,common_rest_code)
      _def_token(category, sym, nil, precedence, rest)
    }
  end
  def self.def_tokens_before(category, precedence, syms, common_rest_code=nil)
    syms.each{|sym|
      re = Regexp.new("(?=#{Regexp.quote(sym)})")
      rest = _methodify_rest_code(category,sym,common_rest_code)
      _def_token(category, sym, re, precedence, rest)
    }
  end
  def self.def_token_full(category, precedence, name, leading_re, common_rest_code)
      rest = _methodify_rest_code(category,name,common_rest_code)
      _def_token(category, name, leading_re, precedence, rest)
  end

  def self._methodify_rest_code(category,sym,common_rest_code)
    return true if not common_rest_code
    rest_code = common_rest_code.gsub(/<sym>/,sym)
    rest_method_name = "__#{category}_#{rand(10000000)}"
    code = "def #{rest_method_name}(start); #{rest_code}; end"
    begin
      eval code
    rescue Exception
      STDERR.print code,"\n" if not $quiet
      raise
    end
    rest = rest_method_name.to_sym
  end
  def self._def_token(category,name,leading_re,precedence,rest=true)
    leading_re ||= Regexp.new(Regexp.quote(name))
    _def_token_precedence(category,name,precedence)
    _def_category_member(category,name,leading_re,rest)
  end
  def self._def_token_precedence(category,sym,precedence)
    (eval "@@__precedences_for_#{category}_symbols__")[sym] = precedence
  end
  def self._def_category_member(category,name,leading_re,rest)
    if (eval "@@matcher_for_#{category}").declared?(name)
      print "# WARNING: #{category}:#{name} redefined.\n" if not $quiet
    end
    (eval "@@matcher_for_#{category}").declare(name,leading_re,rest)
  end
end

class CategoryMatcher
  def initialize(is_rule_category=false)
    @is_rule_category=is_rule_category
    @member_index = {}
  end
  class CategoryMember
    attr_accessor :name,:leading_re,:rest
    def initialize(name,leading_re,rest)
      @name,@leading_re,@rest=name,leading_re,rest
    end
  end
  def declare(name,leading_re,rest)
    cm = CategoryMember.new(name,leading_re,rest)
    @member_index[name] = cm
    @member_cache = nil
  end
  def declared?(name)
    @member_index.key? name
  end
  def longest_token_match(gram,scanr)
    @member_cache ||= @member_index.values.sort{|a,b|
      b.leading_re.to_s.length <=> a.leading_re.to_s.length}
    b0 = b1 = scanr.pos
    if @is_rule_category
      gram.wsp or return false
      b1 = scanr.pos
    end
    @member_cache.each{|cm|
      scanr.scan(cm.leading_re) or next
      rest = cm.rest
      if rest.is_a?(Symbol)
        if u = gram.send(rest,b1) #and ((not @is_rule_category) or gram.wsp)
          return [cm.name,u]
        end
      #elsif rest.respond_to?(:call)
      else
        if true #and ((not @is_rule_category) or gram.wsp)
          return [cm.name,rest]
        end
      end
      scanr.pos = b1
    }
    scanr.pos = b0
    false
  end
end


# Dynamically scoped $+foo environment variables

class EnvVars
  def initialize
    @stack = []
  end
  def scope_enter(*vars)
    @stack.unshift({})
    vars.each{|var|def_var(var)}
  end
  def scope_leave
    @stack.shift or raise "bug"
  end
  def def_var(var)
    @stack[-1][var] = nil
  end
  def _find_defining_env(k)
    @stack.each{|e| return e if e.key? k}
    nil
  end
  def defined?(k)
    _find_defining_env(k) ? true : false
  end
  def [](k)
    e = _find_defining_env(k)
    return nil if not e
    e[k]
  end
  def []=(k,v)
    e = _find_defining_env(k)
    raise("Environment variable #{k} was not declared") if not e
    e[k] = v
  end
end

$env_vars = EnvVars.new


# Interactive REPL

require 'readline'
class Repl
  def initialize(history_filename="deleteme_hist")
    @histfile = File::expand_path(history_filename)
    if File::exists?(@histfile)
      Readline::HISTORY.push(*(eval(IO.read(@histfile))||[]))
    end
  end
  def save_history
    h = Readline::HISTORY.to_a.reverse.uniq.slice(0,100).reverse.inspect
    open(@histfile,"w"){|io|io.puts(h)}
  end
  def ruby
    while true
      s = Readline.readline("rb: ",true)
      break if not s or s == ""
      p Object.module_eval(s)
    end
    save_history
  end
  def expr
    while true
      s = Readline.readline("expr: ",true)
      break if not s or s == ""
      p Perl.new(s)._EXPR(false)
    end
    save_history
  end
  def parser_rule
    while true
      print "Example rules: _UNIT  _EXPR  infix  integer\n"
      s = Readline.readline("rule: ",true)
      break if not s or s == ""
      rule = s
      while true
        s = Readline.readline("input: ",true)
        break if not s or s == ""
        eval("p Perl.new(s).#{rule}")
      end
    end
    save_history
  end
  def parser_input
    while true
      s = Readline.readline("input: ",true)
      break if not s or s == ""
      input = s
      while true
        s = Readline.readline("rule: ",true)
        break if not s or s == ""
        rule = s
        eval("p Perl.new(input).#{rule}")
      end
    end
    save_history
  end
end
