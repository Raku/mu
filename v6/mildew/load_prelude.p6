::MildewSOLoader.new.load('Prelude.mildew.so',$LexicalPrelude.FETCH);
my $multi = ::Multi.new;
$OUT.print("before name\n");
$multi.name = 'foo';
$OUT.print("after name\n");
