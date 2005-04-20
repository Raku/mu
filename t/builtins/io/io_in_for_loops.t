#!/usr/bin/pugs

use v6;
require Test;

plan 49;


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
    for (1 .. 6) -> $num {
        my $line = =$fh;
        is($line, "$num\n", '... got the right line (array controlled loop)');
    }
    $fh.close();
}

{ # now read it in with the $fh controling the loop
    my $fh = open($filename);
    my $num = 1;
    for (=$fh) -> $line {
        is($line, "$num\n", '... got the right line ((=$fh) controlled loop)');
        $num++;
    }
    $fh.close();
}

{ # now read it in with the $fh controling the loop w/out parens
    my $fh = open($filename);
    my $num = 1;
    for =$fh -> $line {
        is($line, "$num\n", '... got the right line (=$fh controlled loop)');
        $num++;
    }
    $fh.close();
}

## more complex loops

{ # now read it in and check
    my $fh = open($filename);
    my $num = 1;
    for (1 .. 3) -> $_num {
        my $line = =$fh;
        is($line, "$num\n", '... got the right line (array controlled loop)');
        $num++;
        my $line2 = =$fh;
        is($line2, "$num\n", '... got the right line2 (array controlled loop)');        
        $num++;        
    }
    $fh.close();
}

### Pugs Bugs FIXME
### these two test groups below will fail
### with the error:
###
###    pugs: tempfile: hGetLine: illegal operation (handle is closed)
###
### it seems that accessing it when it is 
### in the for loop is not okay.

{ # now read it in with the $fh controling the loop but call 
  # the =$fh inside the loop inside parens (is this list context??)
    my $fh = open($filename);
    my $num = 1;
    for (=$fh) -> $line {
        is($line, "$num\n", '... got the right line ((=$fh) controlled loop)');
        $num++;
        my $line2 = =$fh;
        is($line2, "$num\n", '... got the right line2 ((=$fh) controlled loop)');
        $num++;
    }
    $fh.close();
}

{ # now read it in with the $fh controling the loop but call 
  # the =$fh inside the loop w/out parens (is this scalar context??)
    my $fh = open($filename);
    my $num = 1;
    for =$fh -> $line {
        is($line, "$num\n", '... got the right line (=$fh controlled loop)');
        $num++;
        my $line2 = =$fh;
        is($line2, "$num\n", '... got the right line2 (=$fh controlled loop)');
        $num++;
    }
    $fh.close();
}

ok(?unlink($filename), 'file has been removed');
