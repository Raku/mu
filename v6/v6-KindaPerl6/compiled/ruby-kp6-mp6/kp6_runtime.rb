
# Runtime
module Ruddy; end

def cx(*args)
  Ruddy::Capture.new(args)
end
def na(k,v)
  Ruddy::NamedArgument.new(k,v)
end

class Ruddy::NamedArgument
  attr_accessor :key, :value
  def initialize(key,value)
    @key,@value = key,value
  end
end

class Ruddy::Capture
  attr_accessor :args
  def initialize(args)
    @args=args
  end
  def positional
    @args.find_all {|a| !a.is_a?(Ruddy::NamedArgument) }
  end
  def named
    @args.find_all {|a|  a.is_a?(Ruddy::NamedArgument) }
  end
  def pos(n=nil)
    #return @args
    p = positional
    n = @args.length if not n
    if p.length != n
      raise "boom"
    end
    p
  end
end

def si(sigil,twigle,name,default,*options_list)
  options = {}
  options_list.each {|e| options[e] = true}
  Ruddy::SignatureItem.new(sigil,twigle,name,default,options)
end
class Ruddy::SignatureItem
  attr_accessor :sigil,:twigle,:name,:default,:options
  def initialize(sigil,twigle,name,default,options)
    @sigil,@twigle,@name,@default,@options = sigil,twigle,name,default,options
  end
  def encoded_name
    {'$'=>'s_','@'=>'a_','%'=>'h_','&'=>'c_'}[@sigil] + @name #'
  end
end
class Ruddy::Signature
  attr_accessor :binder
  def initialize(invocant,args,returns)
    code = "->(env,cap){"
    code += "}"
    #@binder = eval(code)
    @binder = ->(env,cap){
      v = cap.pos[0]
      id = v.object_id
      eval("s_n._(ObjectSpace._id2ref(#{id}))",env)
    }
  end
  def bind(env,cap)
    @binder.(env,cap)
  end
end  

# Runtime/Perl5/Array
class Array
  def map_n(f,n=nil)
    n ||= f.arity
    i = 0
    result = []
    while true
      s = self.slice(i,n)
      return result if not s
      if n > s.length
        s.push(*Array.new(n - s.length){|i|Undef.new})
      end
      result.push(*f.(*s))
    end
  end

  # new
  def m_values; ->(){self.values}; end
  # FETCH eager INDEX
  def m_elems; ->(){self.length}; end
  def m_push; end
  def m_pop; ->(){self.pop}; end
  def m_shift; ->(){self.shift}; end
  def m_unshift; end
  def m_sort; ->(f){}; end
  def m_map; ->(f){self.map_n(f)}; end
end

#

class Object
  def is_true6?; (not self or self == 0) ? false : true; end
end


# random cruft

class Undef; end

class Bit
  attr_accessor :bit
  def initialize(bit=false); @bit = bit ? true : false; end
  def to_int; @bit ? 1 : 0; end
  alias :method_missing_Bit :method_missing
  def method_missing(m,*args,&block)
    if 1.respond_to?(m)
    then to_int.send(m,*args)
    else method_missing_Bit(m,*args,&block) end
  end
  def coerce other
    if other.respond_to?(:+)
      [to_int, other]
    else
      [self, other]
    end
  end
end

class Pair
  attr_accessor :key, :value
  def initialize(key,value)
    @key,@value = key,value
  end
end


def c_say; ->(c){print *c.pos,"\n"}; end


def def_infix_op(op)
  encoded_name = op.split(//).map{|c|"_#{c.ord}_"}.join
  code = "def c_infix_58__60_#{encoded_name}_62_; ->(cap){a=cap.pos; a[0] #{op} a[1]}; end"
  eval(code)
end
'+ - * / < > <= >= =='.split.map{|op| def_infix_op op}
#infix:<~>
def c_infix_58__60__126__62_; ->(c){c.pos.join("")}; end


module Kernel
  def current_class; self.is_a?(Class) ? self : self.class end #X
end
class Module
  def def_pkg_var(sym,val)
    class_eval %{
      def #{sym}; @@#{sym} end
      def #{sym}=(v); @@#{sym} = v end
      @@#{sym} = ObjectSpace._id2ref(#{val.object_id})
    }
  end
end


class Object; def __getobj__; self end end

# Is there now a better alternative to this?
require 'delegate'
class BetterDelegator < Delegator; end
class << Object
  alias :pre_BetterDelegator_method_added :method_added
  def method_added(id)
    #print "method_added(#{id.id2name}) on #{self}\n"
    ##if self == Object
    ##  #print "punting #{id.id2name}\n"
    ##  BetterDelegator.send(:remove_method,id)
    ##end
    pre_BetterDelegator_method_added(id)
  end
end

class Variable < BetterDelegator
  attr_accessor :__getobj__
  def initialize(*args)
    super(nil)
    _(*args)
  end
  def _(*opt)
    o, = *opt
    #o = *o.to_a if o.listy?
    o ||= Undef.new
    __setobj__(o)
  end
  def __setobj__(o)
    @__getobj__= o
  end
end

class Scalar < Variable; end
class ArrayContainer < Variable; end
class HashContainer < Variable; end
class Routine < Variable; end
