use v6-alpha;

use Test;

=pod

2006-10-05
A temporary file.
See an_oddity.t and http://colabti.de/irclogger/irclogger_log/perl6?date=2006-10-05,Thu .

=cut

plan 1;

{
    is eval(q{
        my $str;
        for 1..2 {
            my $sub = {
                START { $str ~= $_ };
            };
            $sub();
            $sub();
        }
        $str;
    }), '12');
};
