use v6-alpha;

use Test;

plan 14;

# L<S12/"Multisubs and Multimethods">
# L<S12/"Multi dispatch">

class Foo {

    multi method bar() {
        return "Foo.bar() called with no args";
    }

    multi method bar(Int $int) {
        return "Foo.bar() called with Int : $int";
    }
    
    multi method bar(Num $num) {
        return "Foo.bar() called with Num : $num";
    }    
    
    multi method bar(Rat $rat) {
        return "Foo.bar() called with Rat : $rat";
    }    
    
    multi method bar(Str $str) {
        return "Foo.bar() called with Str : $str";
    } 
       
    multi method bar(Bool $bool) {
        return "Foo.bar() called with Bool : {$bool.perl}";
    } 

    multi method bar(Sub $sub) {
        return "Foo.bar() called with Sub : {$sub()}";
    } 

    multi method bar(Array @array) {
        return "Foo.bar() called with Array : { join(', ', @array) }";
    } 

    multi method bar(Hash %hash) {
        return "Foo.bar() called with Hash : { join(', ', %hash.keys) }";
    } 
    
    multi method bar(IO $fh) {
        return "Foo.bar() called with IO";
    }     

}


my $foo = Foo.new();
is($foo.bar(), 'Foo.bar() called with no args', '... multi-method dispatched on no args');

is($foo.bar(5), 'Foo.bar() called with Int : 5', '... multi-method dispatched on Int');
my $num = '4';
is($foo.bar(+$num), 'Foo.bar() called with Num : 4', '... multi-method dispatched on Num');
is($foo.bar(1.5), 'Foo.bar() called with Rat : 1.5', '... multi-method dispatched on Rat');

is($foo.bar("Hello"), 'Foo.bar() called with Str : Hello', '... multi-method dispatched on Str');
is($foo.bar(1 == 1), 'Foo.bar() called with Bool : \\Bool::True', '... multi-method dispatched on Bool');

is($foo.bar(sub { "my sub" }), 'Foo.bar() called with Sub : my sub', '... multi-method dispatched on Sub');

my @array = ('foo', 'bar', 'baz');
is($foo.bar(@array), 'Foo.bar() called with Array : foo, bar, baz', '... multi-method dispatched on Array');

my %hash = ('foo' => 1, 'bar' => 2, 'baz' => 3);
is($foo.bar(%hash), 'Foo.bar() called with Hash : foo, bar, baz', '... multi-method dispatched on Array', :todo<bug>);

is($foo.bar($*ERR), 'Foo.bar() called with IO', '... multi-method dispatched on IO');

my $yaml_tests = eval(q{
- 
    m2: *%h
    call: a => 'b'
    expect: 2
-
    m1: %h
    m2: @a?
    call: <1 2 3>
    expect: 2
-
    m1: %h
    m2: @a
    call: <1 2 3>
    expect: 2
-
    m1: @a?
    expect: 2 
}, :lang<yaml>);

for each($yaml_tests) -> %h {
    # I think Perl6 is supposed to offer us a way to just
    # pass %h on through...
    test_dispatch( 
        m1     => %h<m1>,
        m2     => %h<m2>,
        call   => %h<call>,
        expect => %h<expect>,
     );
}

sub test_dispatch (
    Str $m1,  
    Str $m2,
    Str $call,
    Int $expect,
    ) {

    my $got = eval q:s/ { 
                            class Foo { 
                                multi method a ($m1) {1}
                                multi method a ($m2) {2}
                            };
                            Foo.new.a($call);
                }/;

    if defined $got {
        is($got , $expect, "Arguments ($call) to signatures 1. ($m1) and 2. ($m2) calls $expect");
    }
    else {
        ok(0, "Failed to compile test! error was was: $!" )
    }


}





