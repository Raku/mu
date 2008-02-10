require 'strscan'
require 'match'

def say(*args); print *args,"\n"; end

class Object
  def bool; true; end
  def WHAT; to_s; end ##?
end

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

  def _match_from(from,h=nil)
    h ||= {}
    Match.new(@str,from,@scanner.pos,true,h,nil)
  end
  def _match_pat(re)
    b = @scanner.pos
    return false if !@scanner.scan(re)
    _match_from(b)
  end

  def before(re)
    @scanner.check(re)
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
    dot_ws
    while v = fun.(); a.push(v); dot_ws; end
    a
  end
  def plusRULE(f=nil,&blk)
    fun = f || blk
    dot_ws
    v = fun.() or return false
    a = [v]
    dot_ws
    while v = fun.(); a.push(v); dot_ws; end
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


  def self.prec_op(*a)
  end
  def self.proto_token_simple(name)
    _token_category(name)
  end
  def self.proto_token_defequiv(name,other)
    _token_category(name)
  end
  def self.proto_token_endsym(name,pat)
    _token_category(name)
  end
  def self.proto_rule_endsym(name,pat)
    _token_category(name)
  end
  def self.proto_token_gtgt_nofat(name)
    _token_category(name)
  end
  def self.proto_rule_gtgt_nofat(name)
    _token_category(name)
  end
  def self._token_category(name)
    eval "@@#{name} = RxHash.new"
    eval "def #{name}; @@#{name}.longest_token_match(@scanner); end"
  end

  def self.def_tokens_simple(fix,type,syms)
    syms.each{|sym| _token(fix,type,sym) }
  end
  def self.def_tokens_before(fix,type,syms)
    syms.each{|sym| _token(fix,type,sym) }
  end
  def self._token(fix,type,sym)
    h = eval "@@#{fix}"
    h[Regexp.new(Regexp.quote(sym))] = nil
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


class RxHash < Hash
  def longest_token_match(scanner)
    @cache ||= keys.sort{|a,b| b.to_s.length <=> a.to_s.length}
    @cache.each{|k| scanner.scan(k) and return k }
    nil
  end
  alias :_RxHash_set :[]=
  def []=(k,v)
    @cache = nil
    _RxHash_set(k,v)
  end
end
