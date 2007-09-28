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
