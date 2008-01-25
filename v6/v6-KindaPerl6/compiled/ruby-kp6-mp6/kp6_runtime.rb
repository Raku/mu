
## Containers

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
    o, = args
    o ||= default_value
    __setobj__(o)
  end
  def _(*opt)
    o, = *opt
    __setobj__(o)
  end
  def __setobj__(o)
    @__getobj__= o
  end
  def default_value; Undef.new; end
end

class Scalar < Variable; end
class ArrayContainer < Variable; end
class HashContainer < Variable; end
class Routine < Variable; end

class ArrayContainer
  def default_value; []; end
end
class HashContainer
  def default_value; {}; end
end

# obj.containerize(), for implementing bind.
class Object;   def containerize; Scalar.new(self); end end
class Array;    def containerize; ArrayContainer.new(self); end end
class Hash;     def containerize; HashContainer.new(self); end end
class Variable; def containerize; self; end end
class Proc; def containerize; self; end end #X


## Captures and Signatures

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

# none of this is actually used at the moment.
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

## The crufty work in progress of porting Runtime/Perl5/*.pm
# Runtime/Perl5/Array
class Array
  def map_n(f,n=nil)
    n ||= 1 #f.arity
    i = 0
    result = []
    len = self.length
    while true
      return result if i >= len
      s = self.slice(i,n)
      if n > s.length
        s.push(*Array.new(n - s.length){|i|Undef.new})
      end
      result.push(*f.(cx(*s)))
      i += n
    end
  end

  def mc_values; ->(cap){self.values} end
  # FETCH eager
  def mc_INDEX; ->(cap){a=cap.pos;
      n = a[0]
      v = self[n]
      vc = v.containerize
      if v.object_id != vc.object_id
        self[n] = vc
      end
      vc
    }
  end
  def mc_elems; ->(cap){self.length} end
  def mc_push; ->(cap){a=cap.pos; self.push(*a)} end
  def mc_pop; ->(cap){self.pop} end
  def mc_shift; ->(cap){self.shift} end
  def mc_unshift; ->(cap){a=cap.pos; self.unshift(*a)} end
  def mc_sort; ->(cap){a=cap.pos; self.sort(a[0])} end
  def mc_map; ->(cap){a=cap.pos; self.map_n(a[0])} end
  #
  def mc_join; ->(cap){a=cap.pos; self.join(a[0])} end
end

# Runtime/Perl5/Hash
class Hash
  def mc_LOOKUP; ->(cap){a=cap.pos;
      n = a[0]
      v = self[n]
      vc = v.containerize
      if v.object_id != vc.object_id
        self[n] = vc
      end
      vc
    }
  end
  def mc_elems; ->(cap){self.length} end
  def mc_pairs; ->(cap){self.each{|k,v|Pair.new(k,v)}} end
end

## random cruft created while getting started

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

# Truth
class Variable; def is_true6?; __getobj__.is_true6?; end end
class Object;   def is_true6?; true; end end
class Undef;    def is_true6?; false; end end
class NilClass; def is_true6?; false; end end
class FalseClass; def is_true6?; false; end end
class Numeric;  def is_true6?; self == 0 ? false : true; end end
class Array;    def is_true6?; not self.empty?; end end
class Hash;     def is_true6?; not self.empty?; end end

class Object; def mc_true; ->(cap){self.is_true6?} end end
class Fixnum; def mc_true; ->(cap){self.is_true6? ? 1 : 0} end end


# operators
def def_infix_op(op)
  encoded_name = op.gsub(/[^a-zA-Z0-9_:]/){|c|"_#{c.ord}_"}
  code = "def c_infix_58__60_#{encoded_name}_62_; ->(cap){a=cap.pos; a[0] #{op} a[1]}; end"
  eval(code)
end
'+ - * / < > <= >= == !='.split.map{|op| def_infix_op op}

def c_say; ->(c){print *c.pos,"\n"}; end

#infix:<~>
def c_infix_58__60__126__62_; ->(cap){cap.pos.join("")}; end
def c_infix_58__60_eq_62_; ->(cap){a=cap.pos; a[0] == a[1]}; end
def c_infix_58__60_ne_62_; ->(cap){a=cap.pos; a[0] != a[1]}; end

def c_prefix_58__60__126__62_; ->(cap){a=cap.pos; a[0].to_s}; end
def c_prefix_58__60__43__43__62_; ->(cap){a=cap.pos; a[0]._(a[0]+1); a[0]}; end

# misc
Str = String
class Object
  def mc_WHAT; ->(cap){self.to_s} end
  def mc_isa; ->(cap){a=cap.pos; x=eval(a[0]); self.is_a?(x)} end #XXX SECURITY
end
def c_chars; ->(cap){a=cap.pos; s=a[0]; s.length} end
def c_substr; ->(cap){a=cap.pos; s=a[0]; s.slice(a[1],a[2]||s.length)} end
def c_print; ->(cap){print *c.pos} end
def c_Inf; ->(cap){1.0/0.0} end
def c_NaN; ->(cap){0.0/0.0} end



## OO
module Kernel
  def current_class; self.is_a?(Class) ? self : self.class end #X
  def def_our(*args)
    current_class.def_pkg_var(*args)
  end
end

class Module
  def def_pkg_var(sym,val)
    class_eval %{
      def #{sym}; @@#{sym} end
      def #{sym}=(v); @@#{sym} = v end
      @@#{sym} = ObjectSpace._id2ref(#{val.object_id})
    }
  end
  def def_has(name,initializer)
    varname = name
    basename = name.to_s.sub(/^ci._/,'')
    class_eval %{
      def #{varname}=(v); @#{varname} = v; end
      def mc_#{basename}; ->(cap){#{varname}} end
    }
    # What's the real way to do this?
    # Can't wrap the method def in a closure, so...?
    id = initializer.object_id
    $avoid_gc.incr_refcount(id)
    class_eval %{
      def #{varname}
        if not @#{varname}
          @#{varname} = ObjectSpace._id2ref(#{id}).()
          $avoid_gc.decr_refcount(#{id})
        end
        @#{varname}
      end
    }
  end
end

#eep
class AvoidGC
  attr_accessor :counts, :objects
  def initialize
    @counts = {}
    @objects = {}
  end
  def incr_refcount(id)
    if not @counts.key?(id)
      @counts[id] = 0
      @objects[id] = ObjectSpace._id2ref(id)
    end
    @counts[id] += 1
  end
  def decr_refcount(id)
    count = @counts[id] -= 1
    if count == 0
      @counts.delete(id)
      @objects.delete(id)
    end
  end
end
$avoid_gc = AvoidGC.new


class Class
  def mc_new
    @cached_mc_new ||=
      (->(*ignored){
         o = new()
         o
       })
  end
end
