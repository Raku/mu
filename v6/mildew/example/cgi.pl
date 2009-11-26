use CGI:from<perl5>;
my $q = ::CGI.new;
say $q.header('-charset','utf8'), $q.start_html, $q.h1('hello world'), $q.end_html;
