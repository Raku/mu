use v6;
sub foo($a,$b,$c) {
    $OUT.print($a,$b,$c,"\n");
}
::MildewSOLoader.new.load('Prelude.mildew.so',$LexicalPrelude.FETCH);
my $multi = ::Multi.new;
my $multi2 = ::Multi.new;
$multi.name = 'foo';
$multi2.name = 'bar';
$multi.variants = ::Array.new;
$multi.variants.push(&foo);
$multi.(1,2,3);
