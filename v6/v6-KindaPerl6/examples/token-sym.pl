class Main {

sub mysub { 123 };

proto token xyz {}; 

token xyz:sym<abc> { aaa }
token xyz:sym<abf> { aaa }
token xyz:sym<abger> { aaa }

$_ = "abfbbb";
# Main.xyz;   XXX - AST bug
xyz();

}

