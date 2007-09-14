class Main {

proto token xyz {}; 

token xyz:sym<abc> { aaa }
token xyz:sym<abf> { aaa }
token xyz:sym<abger> { aaa }

$_ = "abfbbb";
# Main.xyz;   XXX - AST bug
xyz();

}

