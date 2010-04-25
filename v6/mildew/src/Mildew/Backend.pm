use v5.10;
use MooseX::Declare;
role Mildew::Backend {
    method output($what,$where) {
        if ($where) {
            open(my $out,">",$where);
            binmode $out, ':utf8';
            print $out $what;
        } else {
            binmode STDOUT, ':utf8';
            print $what;
        }
    }
}
