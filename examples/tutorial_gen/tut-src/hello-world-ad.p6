use v6;

say 'Hello world!';
print 'Hello world!'; print "\n";

say '@ {} $ " % ';
say 'You say \'bla bla\' !'; 

# This is comment
my $str = 'Hello';
my $space = ' ';

say "$str world!";
say $str ~ ' world!';
say $str ~ ' ' ~ 'world' ~ '!';

print "$str world!";

# say "$str$spaceworld!";
say "{$str}{$space}world!";

say '{$str}{$space}world!';
say '\n';
