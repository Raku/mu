use v6;
::MildewSOLoader.new.load('Prelude.mildew.so',$LexicalPrelude.FETCH);
my $multi = ::Multi.new;
my $multi2 = ::Multi.new;
$OUT.print("before name\n");
$multi.name = 'foo';
$multi2.name = 'bar';
$multi.(1,2,3);
