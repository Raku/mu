#!/usr/bin/pugs

use v6;
require Test;

plan 48;


my $filename = 'tempfile';

{ # write the file first
    my $fh = open(">$filename");
    for (1 .. 6) -> $num {
        $fh.print("$num\n");
    }
    $fh.close();
}

{ # now read it in and check
    my $fh = open($filename);
    my $num = 1;
    while ($num < 6) {
        my $line = =$fh;
        is($line, "$num\n", '... got the right line (array controlled loop)');
        $num++;
    }
    $fh.close();
}

{ # now read it in with the $fh controling the loop
    my $fh = open($filename);
    my $num = 1;
    my $line;
    while ($line = =$fh) {
        is($line, "$num\n", '... got the right line (=$fh controlled loop)');
        $num++;
    }
    $fh.close();
}

