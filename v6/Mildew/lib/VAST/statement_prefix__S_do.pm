use v5.10;
use MooseX::Declare;
use utf8;
class VAST::statement_prefix__S_do {
    use AST::Helpers;
    method emit_m0ld {
        call 'postcircumfix:( )'=>code($self->{blast}{block}),[capturize];
    }
}
