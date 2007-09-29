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
say Foo.simple("xsimple",1);
say Foo.simple("complex",0);
say Foo.outer("outer.inner",0);
say Foo.code_block("foo",0);
