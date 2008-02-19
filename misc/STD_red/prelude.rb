require 'strscan'

require 'match'


def say(*args); print *args,"\n"; end

class Object
  def bool; true; end
end
class FalseClass; def bool; false; end; end
class NilClass; def bool; false; end; end

module Kernel
  def my(v); end
end

class Grammar
  def initialize(orig)
    @scanner = StringScanner.new(orig)
    @str = orig
  end
  def pos; @scanner.pos; end
  def fail_at(n); @scanner.pos = n; false; end
  def scan(re); @scanner.scan(re); end
  def let_pos(&blk)
    b = @scanner.pos
    v = blk.()
    @scanner.pos = b if not v
    v
  end
  def rul(&blk)
    let_pos{ dot_ws and blk.() and dot_ws }
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

  def _line_and_indent_of_offset(off); lines = @str.slice(0,off).split(/\n/); [lines.length,lines[-1].length]; end
  def panic(msg)
    line_num,char_in_line = _line_and_indent_of_offset(pos)
    raise "panic at #{line_num}:#{char_in_line} (#{pos}): #{msg}"
  end

  def null; true; end

  def before(re=nil,&blk)
    if re
      @scanner.check(re)
    else
      b = @scanner.pos
      v = blk.()
      @scanner.pos = b
      v ? true : false
    end
  end
  def after(re)
    # no look-behind in 1.8 :(
    s = @str.slice(0,pos)
    Regexp.new("#{re}\z").match(s) ? true : false
  end

  def seqTOK(f=nil,&blk)
    fun = f || blk
    b = pos
    v = fun.() ? v : fail_at(b)
  end

  def quesTOK(f=nil,&blk)
    fun = f || blk
    v = fun.()
    v ? [v] : []
  end
  def starTOK(f=nil,&blk)
    fun = f || blk
    a = []
    while v = fun.(); a.push(v); end; a
  end
  def plusTOK(f=nil,&blk)
    fun = f || blk
    v = fun.() or return false
    a = [v]
    while v = fun.(); a.push(v); end; a
  end

  def quesRULE(f=nil,&blk)
    fun = f || blk
    dot_ws
    v = fun.(); dot_ws
    v ? [v] : []
  end
  def starRULE(f=nil,&blk)
    fun = f || blk
    a = []
    p = pos
    dot_ws
    while v = fun.()
      a.push(v); dot_ws
      p1 = pos; break if not p < p1; p = p1
    end
    a
  end
  def plusRULE(f=nil,&blk)
    fun = f || blk
    dot_ws
    v = fun.() or return false
    a = [v]
    p = pos
    dot_ws
    while v = fun.()
      a.push(v); dot_ws
      p1 = pos; break if not p < p1; p = p1
    end
    a
  end

  def quesRX(fun,more)
    before_fun = pos
    v = fun.()
    if not v
      if not more
        [[]]
      else
        result = more.() or return false
        result.unshift([])
      end
    else
      if not more
        [[v]]
      else
        result = more.()
        if result
          result.unshift([v])
        else
          @scanner.pos = before_fun
          result = more.() or return false
          result.unshift([])
        end
      end
    end
  end
  def starRX(fun,more)
    result = plusRX(fun,more) and return result
    if not more
      [[]]
    else
      result = more.() or return false
      result.unshift([])
    end
  end
  def plusRX(fun,more=nil)
    before_fun = pos
    v = fun.() or return false
    if result = starRX(fun,more)
      result[0].unshift(v)
      result
    else
      fail_at(before_fun)
    end
  end


  def self.prec_op(type,init)
    @@__types__ ||= {}
    @@__types__[type] = init
    eval("H#{type} = init")
  end
  def self._proto_common(category,args)
    if not args.empty?
      if args[0].instance_of?(Hash)
        k = args[0].keys[0]
        v = args[0].values[0]
      else
        k = args[0]
        v = nil
      end
      case k
      when 'defequiv'
        type = v
        prec_op(category,@@__types__[type])
      when 'gtgt_nofat'
        print "# caveat: #{category} endsym gtgt_nofat unimplemented\n"
      when 'endsym'
        print "# caveat: #{category} endsym unimplemented\n"
      when nil
        p category
        #R XXX why is this case needed??
      else
        raise "bug"
      end
    end
  end
  def self.proto_token(category,*args)
    _proto_common(category,args)
    eval "@@#{category} = TokenHash.new"
    _token_category(category)
  end
  def self.proto_rule(category,*args)
    _proto_common(category,args)
    eval "@@#{category} = RuleHash.new"
    _token_category(category)
  end
  def self._token_category(category)
    eval "@@__sym_return_type__#{category} = {}"
    eval <<-END
      def #{category}
        b = @scanner.pos
        tmp = @@#{category}.longest_token_match(self,@scanner) or return false
        sym_re,v =  tmp

        if v.instance_of?(Match)
          v.rule = "#{category}:\#{v.rule||"kludge"}"
          m = v
        else
          stuff = v.is_a?(TrueClass) ? nil : {:kludge =>v}
          m = _match_from(b,stuff,'#{category}')
        end

        if return_type = @@__sym_return_type__#{category}[sym_re]
          if init = @@__types__[return_type]
            precop_method(m,init)
          else
            init.is_a?(FalseClass) or
              raise "bug: unknown return type class: \#{return_type.capitalize}"
          end
        end
        m
      end
    END
  end
    

  def self.def_tokens_simple(category, return_type, syms)
    syms.each{|sym| _token(category, return_type, sym) }
  end
  def self.def_tokens_before(category, return_type, syms)
    syms.each{|sym| _token(category, return_type, sym) }
  end
  def self.def_tokens_circum(return_type, left_syms, rest_code)
    def_tokens_rest(:circumfix, return_type, left_syms, rest_code)
  end
  def self.def_tokens_rest(category, return_type, left_syms, common_rest_code)
    left_syms.each{|sym|
      rest_code = common_rest_code.gsub(/<sym>/,sym)
      rest_method_name = "__#{category}_#{rand(10000000)}"
      eval "def #{rest_method_name}(start); #{rest_code}; end"
      rest = rest_method_name.to_sym
      _token(category, return_type, sym, rest)
    }
  end
  def self.def_category_rules(category,syms,common_rest_code)
    syms.each{|sym|
      rest_code = common_rest_code.gsub(/<sym>/,sym)
      rest_method_name = "__#{category}_#{rand(10000000)}"
      eval "def #{rest_method_name}(start); #{rest_code}; end"
      rest = rest_method_name.to_sym
      _token(category, false, sym, rest)
    }
  end
  def self._token(category, return_type, sym, rest=true)
    sym_re = Regexp.new(Regexp.quote(sym))
    (eval "@@__sym_return_type__#{category}")[sym_re] = return_type
    if (eval "@@#{category}").key?(sym_re); print "# Warning: #{category}:#{sym_re} redefined.\n"; end
    (eval "@@#{category}")[sym_re] = rest
  end

  def self.def_tokens_full(*a)
    print "# caveat: ignored #{a}\n"
  end
  def self.def_rules_rest(category, left_syms, rest_code)
    print "# caveat: ignored #{category}:#{left_syms}\n"
  end
end

class TokenHash < Hash
  def longest_token_match(gram,scanr)
    @cache ||= keys.sort{|a,b| b.to_s.length <=> a.to_s.length}
    @cache.each{|k|
      b = scanr.pos
      scan_key(gram,scanr,k) or next
      hv = self[k]
      if hv.is_a? Symbol
        (v = gram.send(hv,b)) and return [k,v]
      elsif hv.is_a? String
        scanr.scan(v) and return [k,true]
      #elsif hv.respond_to?(:call)
      #  (v = hv.()) and return [k,v]
      else
        return [k,hv]
      end
      scanr.pos = b
    }
    false
  end
  def scan_key(gram,scanr,k)
    scanr.scan(k)
  end
  alias :_TokenHash_set :[]=
  def []=(k,v)
    @cache = nil
    _TokenHash_set(k,v)
  end
end
class RuleHash < TokenHash
  def scan_key(gram,scanr,k)
    gram.let_pos{
      gram.dot_ws
      v = scanr.scan(k) or return false
      gram.dot_ws
      v
    }
  end
end

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
