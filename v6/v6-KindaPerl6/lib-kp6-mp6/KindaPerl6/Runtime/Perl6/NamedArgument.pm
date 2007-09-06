use v6-alpha;
class NamedArgument {
    has $._argument_name_;
    has $.value;
    method perl {
        $._argument_name_.perl ~ ' => ' ~ $.value.perl 
    };
    method str {
        self.perl
    };
}
