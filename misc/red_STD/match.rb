# snarfed from redsix.  much can probably be discarded,
# and much is probably non-spec, but we'll get to both in a bit.

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
  def as_b; @bool end; def as_s; str end; def as_a; [] end; def as_h; @hash end
  def match_beg; @from end; def match_end; @to end
  def match_describe_name; "#{super}:#{rule ? rule.name : 'nil'}" end
  def inspect; match_describe end
end
