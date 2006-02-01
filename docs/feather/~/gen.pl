#!/usr/bin/perl -w
use strict;
use Fatal qw(open close);

open my $fh, '>index.html';
print { $fh } <<'END';
<html>
<head>
<title>Public HTML</title>
</head>
<script>
function openNewWindows() {
    var links = document.getElementsByTagName('A');
    for (var i = 0; i < links.length; i++) {
        var href = links[i].href;
        if (href.match(/\/~/)) window.open(href);
    }
}
</script>
<body>
<h1>~</h1>
<p>(<a href="/">Back to /</a>)</p>
<p>
These users have something in their <tt>~/public_html</tt> directory:
</p>
<ul>
END

my @dir = glob "/home/*/public_html/*";
my @users = keys %{ { map { (split m[/])[2] => undef } @dir } };

print { $fh } qq[<li><a href="/~$_/">$_</a></li>\n] for sort @users;

print { $fh } <<'END';
</ul>
<p>
Or <a href="#" onclick="openNewWindows(); return false;">open each link in a
new window</a>.  (Useful with Firefox 1.5's Preferences/Tabs/Force/NewTab
setting.)
</p>
<p>
To update this listing, run <tt>perl gen.pl</tt> in <tt>pugs/docs/feather/~</tt>
and wait for the automatic update.
</p>
</body>
</html>
END
