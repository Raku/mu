use strict;
use warnings;

use FindBin '$Bin';
use File::Slurp;
use Test::More tests => 2;

chdir "$Bin/timeout";
unlink('out', 'out2');
is system("sh test.sh > out 2>&1"), 0;
my $content = read_file('out');
$content =~ s/Killing \d+/Killing 1234/sg;
$content =~ s/test\.sh: line \d+:.*\n//sg;
write_file('out2', $content);
my $out = `diff out2 out.std`;
is $out, '', 'no diff';

#END { unlink('out', 'out2'); }

