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

  def ques(f=nil,&blk)
    fun = f || blk
    v = fun.()
    v ? [v] : []
  end
  def star(f=nil,&blk)
    fun = f || blk
    a = []
    while v = fun.(); a.push(v); end; a
  end
  def plus(f=nil,&blk)
    fun = f || blk
    v = fun.() or return false
    a = [v]
    while v = fun.(); a.push(v); end; a
  end
end
