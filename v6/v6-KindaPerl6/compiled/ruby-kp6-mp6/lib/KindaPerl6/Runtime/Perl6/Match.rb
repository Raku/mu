# Machine-generated ruby code.
# Ruby version >= 1.9.0 2007-12-25 is needed. unit_type = class
require 'kp6_runtime'

class Match
def_has(:cis_from,->(obj){Scalar.new})
; def_has(:cis_to,->(obj){Scalar.new})
; def_has(:cis_result,->(obj){Scalar.new})
; def_has(:cis_bool,->(obj){Scalar.new})
; def_has(:cis_match_str,->(obj){Scalar.new})
; def mc_perl; ->(cap){->(s_self){s_self = self; ->(a__,s_self){
p = cap.pos

 'Match.new( ... )'
}.(ArrayContainer.new,Scalar.new)}.(nil)}
end
; def mc_Str; ->(cap){->(s_self){s_self = self; ->(a__,s_self){
p = cap.pos

if (cis_result).is_true6? 
 return(cis_result.mc_Str.(cx()))
 else; Bit.new(false); 
end
;  c_ternary_58__60__63__63__32__33__33__62_.(cx(cis_bool,  c_substr.(cx(cis_match_str, cis_from,  c_infix_58__60__45__62_.(cx(cis_to, cis_from))
)),
  Undef.new()))

}.(ArrayContainer.new,Scalar.new)}.(nil)}
end
; def mc_scalar; ->(cap){->(s_self){s_self = self; ->(a__,s_self){
p = cap.pos

if (cis_result).is_true6? 
 return(cis_result)
 else 
return(s_self.mc_Str.(cx()))
 
end

}.(ArrayContainer.new,Scalar.new)}.(nil)}
end
; def mc_true; ->(cap){->(s_self){s_self = self; ->(a__,s_self){
p = cap.pos

return(cis_bool.mc_true.(cx()))

}.(ArrayContainer.new,Scalar.new)}.(nil)}
end

end

