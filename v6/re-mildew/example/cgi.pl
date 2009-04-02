my $p5 = ::P5Interpreter.new();
$p5.eval('use CGI;');
my $q = $p5.eval("CGI->new");
$OUT.print($q.header('-charset','utf8').Str,$q.start_html.Str,$q.h1("hello world\n").Str,$q.end_html.Str);
