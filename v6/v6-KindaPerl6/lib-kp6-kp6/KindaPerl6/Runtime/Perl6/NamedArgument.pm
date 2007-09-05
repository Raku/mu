use v6-alpha;
class NamedArgument {
    has $._argument_name_;
    has $.value;
    method perl {
        '( ' ~ $._argument_name_.perl ~ ' => ' ~ $.value.perl ~ ' )' 
    };
    method str {
        $._argument_name_ ~ '   ' ~ $.value 
    };
    method true { true };
    #method kv   { ( $._argument_name_, $.value ) };
    method int  { $.value.int };
}
