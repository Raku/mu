use Test::More;
ok -d 'tmp';
ok -d 'tmp/syml';
ok -e 'tmp/syml/DefinedBySMOP.syml';
ok -e 'tmp/syml/MildewCORE.syml';
done_testing;
