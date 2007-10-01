grammar Foo {
token simple {
    'simple'
}
token foo {
    ['foo'|'ba']'baz'
}
token inner {
    inner
}
token outer {
    'outer.' <inner>
}
token code_block {
    foo{return 0}
}
}
say \(1:2,3,4).perl
say (Match.new()).isa('Capture');
say (Foo.simple("xsimple",1)).perl;
say (Foo.simple("complex",0)).perl;
say (Foo.outer("outer.inner",0)).perl;
say (Foo.code_block("foo",0)).perl;
