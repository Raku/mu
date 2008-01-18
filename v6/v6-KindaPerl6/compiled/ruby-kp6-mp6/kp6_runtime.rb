
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

class NamedArgument < Pair
end


def c_say; ->(*a){print *a,"\n"}; end
#infix:<~>
def c_infix_58__60__126__62_; ->(*a){a.join("")}; end
#infix:<+>
def c_infix_58__60__43__62_; ->(a,b){a+b}; end
#infix:<==>
def c_infix_58__60__61__61__62_; ->(a,b){a==b}; end



class Object; def __getobj__; self end end

# Is there now a better alternative to this?
require 'delegate'
class BetterDelegator < Delegator; end
class << Object
  alias :pre_BetterDelegator_method_added :method_added
  def method_added(id)
    #print "method_added(#{id.id2name}) on #{self}\n"
    if self == Object
      #print "punting #{id.id2name}\n"
      BetterDelegator.funcall(:undef_method,id)
    end
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


