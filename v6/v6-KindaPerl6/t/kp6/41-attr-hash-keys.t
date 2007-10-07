class MyClass {
    has %.somehash;
    method test {
        %.somehash = { a => 1 };
        for %.somehash.keys -> $key {
            say 'ok ' ~ %.somehash{$key};
        }
    }
}
say '1..1';
my $a = MyClass.new();
$a.test();
