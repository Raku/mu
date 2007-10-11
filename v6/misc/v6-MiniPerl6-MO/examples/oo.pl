use v6-alpha;

# accessors, token
class x { 
        method a_method { 3 };
        token  a_token  { 4 }; 
        sub    a_sub    { 5 };
        has    $.instance_var;
        my     $.class_var;
        has    $._private_instance_var;
        my     $._private_class_var;
        my     $plain_my_var;
}
# TODO - extend an existing class
#class x { 
#        method b_token { 6 }; 
#}

# new, coercion
class YY {
        has $.v;
}
class Main {
        my $a := ::YY( v => 2 );
        say $a.v;
        
        # TODO
        #$a := ::YY.new( v => 3 );
        #say $a.v;
}
