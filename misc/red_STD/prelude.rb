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
      failed_at(before_fun)
    end
  end

end

class Env
  def scope_enter(*vars)
  end
  def scope_leave
  end
  def [](k)
  end
  def []=(k,v)
  end
end

$env = Env.new
