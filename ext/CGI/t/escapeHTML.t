use v6-alpha;
use Test; plan 2; 
use CGI;
my $q = CGI.new;

{
    my $in is ro = '<';
    my   $out = $q.escapeHTML($in);
    is($out, '&lt;', "basic escapeHTML test");
    my $back = $q.unescapeHTML($out);
    is($back, $in, "basic unescapeHTML test");
}
