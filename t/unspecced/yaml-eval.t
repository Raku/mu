use Test;
 
plan 5;
 
my $yaml_tests = eval(q{
- 
    m2: %h
    call: "{a => 'b'}"
    expect: 2
-
    m1: %h
    m2: @a?
    call: "[1,2,3]"
    expect: 2
-
    m1: @a?
    expect: 2 
-
    m1: %h
    m2: @a
    call: "[1,2,3]"
    expect: 2
-   
    m1: 
    m2: @a?
    call: "[1,2,3]"
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

    state $cls = 'Foo000';
    $cls++;

    my $got = eval qq/
        class $cls \{
            multi method a ($m1) \{1\}
            multi method a ($m2) \{2\}
        \};
        {$cls}.a($call);
    /;

    if defined $got {
        is($got , $expect, "Arguments ($call) to signatures 1. ($m1) and 2. ($m2) calls $expect");
    }
    else {
        ok(0, "Failed to compile test! error was was: $!" )
    }

}
