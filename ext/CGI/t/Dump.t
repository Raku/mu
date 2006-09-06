use Test; plan 1;
use CGI;

my $q = CGI.new( a => 1, b => [3,4] );

is(
    $q.Dump,
    '<ul>
<li><strong>a </strong></li>
 <ul>
<li>1 </li>
</ul>
<li><strong>b </strong></li>
 <ul>
<li>3 </li>
<li>4 </li>
</ul>
</ul>',
    "Dump produces expected output"
)

