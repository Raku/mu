use v6-alpha;

# ------------------------ E M I T T E R ------------------------ #

class Talk {
    has $.presenter;
    has $.title;
    method emit {
        '<talk><title>' ~ $.title ~ '</title><presenter>' ~ $.presenter ~ '</presenter></talk>';
    }
}

# --------------------------- P L A N --------------------------- #

package main;

use v6-alpha;
use Test;

plan 11 + (2 * 2);

if !eval('("a" ~~ /a/)') {
  skip_rest "skipped tests - rules support appears to be missing";
  exit;
}

# ------------------------ G R A M M A R ------------------------ #

rule embedded_title {
    <title>
}

rule presentation  { 
    <title>
    <presenter> 
}

rule talk  { 
    <title> 
    <presenter> 
    { return ::Talk(:$$<title>, :$$<presenter>) }
}

token presenter {
    <'<span class="speaker">'> <'<tt>'>? <( <alpha>+ [<ws> <alpha>+]+ )>
}

token title {
    <'<span class="talk">'> <link> <'</span>'>
    { return $<link><label> }
}

token link {
    <'<a href="'>
    $<url> := ([\/]? \w+ [\/ \w+]+ \. \w+) <'">'>
    $<label> := (<alpha>+ [<ws> <alpha>+]+) <'</a>'>
}

# ------------------------- S O U R C E ------------------------- #

# snippet from http://pghpw.org/schedule.html
my $content = '
    <tr>
      <td class="time">9:45 AM</td>
      
      <td class="beginner"
        rowspan="2"
        
      >
        <span class="talk"><a href="schedule/making_perl_work_for_you.html">Making Perl Work for You</a></span>
        <span class="speaker"><tt>brian d foy</tt></span>
      </td>

      
      <td class="advanced"
        
        
      >
        <span class="talk"><a href="schedule/make_your_database_work_for_you.html">Make your database work for you</a></span>
        <span class="speaker">Beth Skwarecki</span>
      </td>

    </tr>
';

# -------------------------- T E S T S -------------------------- #

my @expected = (
    'Making Perl Work for You',
    'Make your database work for you',
);

# L<S05/Subrule captures>
my $presenter = ($content ~~ m/<presenter>/);
is(~$presenter, 'brian d foy', 'match presenter');

my $title = ($content ~~ m/<title>/);
is(~$title, 'Making Perl Work for You', 'match title');

# L<S05/Match objects/"This returned object is also automatically assigned to the lexical $/ variable">
my $embedded = ($content ~~ m/<embedded_title>/);
ok($embedded);
# $embedded behaves just like $/, so you have to specify the <embedded_title> layer
is(~$embedded<embedded_title><title>, 'Making Perl Work for You', 'match embedded.title');

my $presentation = ($content ~~ m/<presentation>/);
ok($presentation);
is(~$presentation<presentation><title>, 'Making Perl Work for You', 'match presentation.title');
is(~$presentation<presentation><presenter>, 'brian d foy', 'match presentation.presenter');

# L<S05/Match objects/"you can override that by calling return inside a regex">
my $talk = ($content ~~ m/<talk>/);
ok($talk);
my $title_value; eval('$title_value = $talk.title');
is($title_value, 'Making Perl Work for You', 'match talk.title');
my $presenter_value; eval('$presenter_value = $talk.presenter');
is($presenter_value, 'brian d foy', 'match talk.presenter');
my $emit_value; eval('$emit_value = $talk.emit');
is($emit_value, '<talk><title>Making Perl Work for You</title><presenter>brian d foy</presenter></talk>', 'talk.emit');

# make sure we can find multiple presentations in our $content
# L<S05/Match objects/"Match object can produce the rest of the results lazily">
my $c = 0;
for $content ~~ m:g/<presentation>/ -> $match {
    # should loop through two matches
    ok($match);
    is(~$match<presentation><title>, @expected[$c], "presentation $c title");
    $c++;
}
