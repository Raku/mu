#!/usr/bin/perl

$dir='/usr/lib/perl5/site_perl/5.8.6/Perl6/Bible/';

opendir (DIR, $dir);
@files = sort( grep /\.pod$/, readdir(DIR) );
closedir(DIR);

open (IDX, ">index.html");

print IDX "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n";
print IDX "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n";
print IDX "<head>\n";
print IDX "<title>A Perl 6 Site</title>\n";
print IDX "<link rel=\"stylesheet\" href=\"perl6.css\" type=\"text/css\" />\n";
print IDX "</head>\n";
print IDX "\n";
print IDX "<body>\n";

foreach (@files) {
    my($in)=$dir.$_;
    my($out)=$_; $out=~s/\.pod$/\.html/;
    print "$in\n";
    print IDX "<a href=\"$out\">$out</a><br />\n";
    `pod2html -css perl6.css $in >$out`
}

print IDX "</body>\n";
print IDX "</html>\n";

close(IDX);