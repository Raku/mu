#!/usr/bin/perl6
# This is a simple IRC log to HTML converter.
# It accepts only logfiles in ilogger2 format, such as those provided by
# http://colabti.de/irclogger/irclogger_logs/perl6 (click on "raw text").
use v6;

# This is our class which calculates the colors of the nicks.
class Chat {
  # 16 different colors should suffice.
  my $POOL_SIZE = 16;

  has @.pool;
  has @.color;

  # $id is the person id, $time is some kind of time, which is, in this class,
  # not necessary. But I plan to add a subclass, which does evaluate $time.
  method tick(Int $id, Int $time) {
    # As we don't have submethod BUILD support yet, we have to initialize
    # @.pool now.
    @.color //= precalc_colors($POOL_SIZE);

    # If we haven't allocated a color for $id...
    unless defined @.color[$id]  {
      # Take one from the pool (pop), assign in to $id, and unshift it.
      @.pool.unshift(@.color[$id] = @.pool.pop);
    }
  }

  # Precalculate the pool.
  sub precalc_colors(Int $num) {
    my @colors = 0..$num-1;

    @colors .= map:{ [calc_color($^i, $num)] };

    return @colors;
  }

  # calc_color copied from irclog2html.pl
  # (http://freshmeat.net/projects/irclog2html.pl/), Copyleft (C) 2000-2002 Jeff
  # Waugh, licensed under the Terms of the GNU General Public License, version 2
  # or higher.
  # calc_color expects the total number of colors to assign ($_[2]) and the color
  # id ($_[1]) and returns a HTML-("#foreground", "#background")-pair with nice
  # contrast etc.
  # Take calc_color as a sub w/o errors.
  sub calc_color(Int $i, Int $ncolors is copy) {
    $ncolors = 1 if $ncolors == 0; # No division /0.

    my $a = 0.95;     # tune these for the starting and ending concentrations of R,G,B
    my $b = 0.5;
    my $rgb = [ [$a,$b,$b], [$b,$a,$b], [$b,$b,$a], [$a,$a,$b], [$a,$b,$a], [$b,$a,$a] ];
    my $rgbmax = 125;   # tune these two for the outmost ranges of colour depth
    my $rgbmin = 240;

    my $n = $i % (+$rgb);
    my $m = $rgbmin + ($rgbmax - $rgbmin) * ($ncolors - $i) / $ncolors;

    my @c = 0 .. 2;
    @c   .= map:{ $rgb[$n][$_] * $m };
    my $g = @c[0] * 0.3 + @c[1] * 0.59 + @c[2] * 0.11;
    my $f = $g > 127 ?? "#000000" :: "#ffffff";
    my $h = sprintf "#%02x%02x%02x", *@c;

    return [$f, $h];
  }
}

# Stop if we weren't given a logfile to process.
@*ARGS or die "Usage: $*PROGRAM_NAME logfile\n";

my $chat = Chat.new;
my ($i, %nick2num) = (1);

# Pass I
my $fh = open @*ARGS[0] err die "Couldn't open \"@*ARGS[0]\": $!\n";
my $total = 0;

# We read the input file in and populate %nick2num.
# %nick2num is a Hash with nicknames as keys and IDs, suitable for $chat.tick,
# as values.
for =$fh -> {
  my ($time, $nick, $type, $text) = parse_ilogger2($_) or next;
  $time ~~ rx:Perl5/^(\d\d):(\d\d)$/;
  my $utime = $0 * 60 + $1;

  # We allocate a color only if $nick has said something (e.g. not, if he has
  # only joined, etc.).
  if $type eq "PRIVMSG"|"NOTICE" {
    %nick2num{$nick} //= $i++;
    $chat.tick(%nick2num{$nick}, $utime);
  }

  # If $nick has changes its nick, his color should stay.
  my $nid = %nick2num{$nick};
  %nick2num{$text} = %nick2num{$nick} if $type eq "NICK";
  $total++;
}

close $fh;

# Pass I
$fh = open @*ARGS[0] err die "Couldn't open \"@*ARGS[0]\": $!\n";

# This is the main coderef which processes a logline and returns HTML.
my $process = -> $time, $nick, $type, $text {
  my $htext;

  given $type {
    # PRIVMSG is the standard type of messages.
    when "PRIVMSG" {
      # If it was a /ME, we format it differently.
      $htext = $text ~~ m:Perl5/^\001ACTION (.*)\001$/
	?? "$nick {qhtml $0}"
	:: qhtml $text;
    }

    # Somebody set the topic.
    when "TOPIC" {
      $htext = "TOPIC: {qhtml $text}";
    }

    # It's some other event (JOIN, PART, etc.).
    default {
      $htext = chars $text ?? "$type: {qhtml $text}" :: $type;
    }
  }

  # These are the colors of the nick.
  # If we don't have a ID for $nick, $nick has never said anything, so we
  # default to foreground #000 and background #fff.
  my @nickc = %nick2num{$nick} ?? $chat.color[%nick2num{$nick}] :: ("#000", "#fff");

  # Now we give our variables to the template.
  tmpl_logline(
    # Global foreground/background color
    globfg => "black",
    globbg =>
      $type eq "PRIVMSG"    
	?? $text ~~ rx:Perl5/^\001ACTION/ ?? "#eaeaea" :: "#f5f5f5"
	:: "#dddddd",

    # Nick foreground/background color
    nickfg => @nickc[0],
    nickbg => @nickc[1],

    # Nick, time, type of the event
    nick   => $nick,
    time   => $time,
    type   => $type,

    # Text
    text   => $htext,

    # Sigil: One of "<" (user has left), ">", (user has joined"), " " (normal
    # message), or "*" (/ME)
    sigil  =>
      $type eq "QUIT"    ?? qhtml "<" ::
      $type eq "PART"    ?? qhtml "<" ::
      $type eq "JOIN"    ?? qhtml ">" ::
      $type eq "PRIVMSG"
	?? ($text ~~ rx:Perl5/^\001ACTION/ ?? qhtml "*" :: "")
	:: qhtml "*",
  );
};

# First, we output the header.
print tmpl_header("Log of «@*ARGS[0]»");
print tmpl_logstart();

# Then we iterate over $fh and process each logline.
for =$fh {
  my ($time, $nick, $type, $text) = parse_ilogger2($_) or next;

  print
    $process(time => $time, type => $type, nick => $nick, text => $text);
}

# Finally, we output the footer.
print tmpl_logend();
print tmpl_end();

# This is the sub which expects a logline in ilogger2 format and returns
# ($time, $type, $nick, $text).
sub parse_ilogger2(Str $line is copy) {
  $line .= chomp;
  $line ~~ rx:Perl5/^\[(\d\d:\d\d)\] (.*)$/ or
    die "Couldn't parse line »$line«!";
  my ($time, $rest) = @$/;
  # We want to see if we progress.
  $*ERR.say($rest);

  given $rest {
    when rx:Perl5/^\*\*\* ([^ ]+) has joined ([^ ]+)/ {
      return ($time, $0, "JOIN", $1);
    }

    when rx:Perl5/^\*\*\* ([^ ]+) has left/ {
      return ($time, $0, "PART");
    }

    when rx:Perl5/^\*\*\* ([^ ]+) has quit IRC \((.*)\)/ {
      return ($time, $0, "QUIT", $1);
    }

    when rx:Perl5/^\*\*\* ([^ ]+) is now known as ([^ ]+)/ {
      return ($time, $0, "NICK", $1);
    }

    when rx:Perl5/^<([^>]+)> (.*)/ {
      return ($time, $0, "PRIVMSG", $1);
    }

    when rx:Perl5/^\* <([^>]+)> (.*)/ {
      # We reformat /MEs as CTCP ACTIONs.
      return ($time, $0, "PRIVMSG", "\001ACTION $1\001");
    }
  }

  return;
}

# Quote HTML
# E.g. "a<b" → "a&lt;b"
sub qhtml (Str $str is copy) returns Str {
  $str ~~ s:Perl5:g/([&<>"'-])/{ #"#--vim
    $0 eq "&" ?? "&amp;"  ::
    $0 eq "<" ?? "&lt;"   ::
    $0 eq ">" ?? "&gt;"   ::
    $0 eq '"' ?? "&quot;" ::
    $0 eq "'" ?? "&#39;"  ::
    $0 eq "-" ?? "&#45;"  :: die
  }/;
  $str;
}

# Here-docs not yet implemented, so we have to use multi-line literals...
sub tmpl_header($title) {"
<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"de\">
<head>
<title>{qhtml $title}</title>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />
<style>{'
  body { font-family: Sans; background-color: white; color: black; margin: 0; }

  h1 { background-color: #41347B; color: #52fe3b; font-family: monospace; margin: 0; text-align: center; font-size: 200%; }
  h2 { background-color: #41347B; color: #52fe3b; font-family: monospace; margin: 0; text-align: center; font-size: 120%; }
  h3 { font-size: 150%; margin: 0; }
  h4 { font-size: 080%; margin: 0; }

  a                { text-decoration: none; }
  a:hover          { text-decoration: underline; }
  h2 a             { color: #00dd00; }
  h2 a:hover       { color: #00ff00; }
  .sidebar a       { color: #00dd00; }
  .sidebar a:hover { color: #00ff00; }
  .footer a        { color: #00dd00; }
  .footer a:hover  { color: #00ff00; }
  .text a          { color: #0000dd; }
  .text a:hover    { color: #0000ff; }

  .sidebar { display: none; position: absolute; right: 0; left: 85%; background-color: #41347B; color: white; }
  .text { padding: 10px; }

  .abstract { background-color: #7B59DE; border: 1px solid black; color: white; padding: 3px; }
  pre       { background-color: #DEDEFF; border: 1px solid black; color: black; padding: 3px; font-family: monospace; }
  .footer   { background-color: #41347B; margin: 0; padding: 3px; color: white; font-size: 80%; }

  ul.nav { list-style-type: none; margin: 0; padding: 0; }

  th, td { vertical-align: top; }
  div.msg { overflow: auto; }

  a.link_0, a.link_0:hover { color: gray; }
'}</style>
<link rel=\"stylesheet\" href=\"/style.css\" />
<script type=\"text/javascript\" src=\"/info.js\"></script>
</head>
<body>

<h1>IRC Log</h1>
<h2>generated by Pugs</h2>

<div class=\"text\">
  <h3>{qhtml $title}</h3>
"}

sub tmpl_logstart() {'
  <table style="width: 100%;">
    <tr>
      <th>From/To</th>
      <th>@</th>
      <th>&nbsp;</th>
      <th style="width: 80%;">Text</th>
    </tr>
'}

sub tmpl_logend() {'
  </table>
'}

sub tmpl_logline(
  Str $globfg, Str $globbg,
  Str $nickbg, Str $nickfg,
  Str $time,
  Str $type,
  Str $sigil,
  Str $text,
  Str $nick,
) {"
    <tr style=\"color: $globfg; background-color: $globbg\">
      <td style=\"background-color: $nickbg; color: $nickfg; text-align: center;\">
	{qhtml $nick}
      </td>
      <td>{$time}</td>
      <td title=\"$type\">{$sigil}</td>
      <td>{$text}</td>
    </tr>
"}

sub tmpl_end {'
</div>

<div class="footer">
  Valid <a href="http://validator.w3.org/check/referer">XHTML 1.1</a>.<br />
  Created using <a href="http://www.pugscode.org/">Pugs</a>, a <a
  href="http://dev.perl.org/perl6/">Perl 6</a> compiler.
</div>

</body>
</html>
'}
