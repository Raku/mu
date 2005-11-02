#!/usr/local/bin/pugs
BEGIN {unshift @*INC, "/home/eric256/auto/pugs/ext/CGI/lib" };
use CGI;
#use perl5:HTML::Template;
#use perl5:Text::Textile qw(textile);

print header();
#my $template  = HTML::Template.new(filename => "wiki.html");

my $action    = param('action');
my %wiki      = evalfile("wiki.db");
my $wikiwords = rx:P5/\b([A-Z]\w+[A-Z]\w+)/;
my $topic     = join("\n", param('topic')) || 'FrontPage';
my $entry      = join("\n", param('entry') || %wiki{$topic});
if ($topic eq "OverView") {
    $action = "";
    $entry = %wiki.keys.sort.join("\n");
}
my $entry_html = $entry;
$entry_html ~~ s:g:perl5/\b([A-Z]\w+[A-Z]\w*)/{ '<a href="?topic=' ~ $0 ~ '">' ~ $0 ~ '</a>'}/;

given $action {
    when 'new'   {
        print template( "wiki.tmpl", topic => '', entry => '', entry_html => '');
    }
    when 'edit' {
        print template( "wiki_edit.tmpl", topic => $topic, entry => $entry );
    }
    when 'update' { 
        say "Updating";
#        die "No topic no entry!\n" unless $topic;
#        die "Topci $topic isn't a WikiWoord!\n" unless $topic ~~ $wikiwords;
        %wiki{$topic} = $entry;
        my $fh = open("wiki.db", :w);
        $fh.print(%wiki.perl);
        $fh.close;
        show();
    }
    default      {
        show();
    }
};

sub show () {
    print template( "wiki.tmpl", topic => $topic, entry => $entry, entry_html => $entry_html );
}

sub template ($filename, *%params) {
    my $template = slurp $filename;
    $template ~~ s:g:perl5/\[\%(.+)\%\]/{ %params{$0} || '' }/;
    return $template;
}
