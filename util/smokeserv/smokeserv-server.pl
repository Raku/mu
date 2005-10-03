#!/usr/bin/perl

use warnings;
use strict;

use CGI;
use CGI::Carp   qw<fatalsToBrowser>;
use Fcntl       qw<:DEFAULT :flock>;
use Storable    qw<store_fd fd_retrieve freeze>;
use Digest::MD5 qw<md5_hex>;
use HTML::Template;
use Algorithm::TokenBucket;
use Time::Piece;
use Time::Seconds;

# All dates should be displayed as UTC (Time::Piece honours $*ENV<TZ>).
$ENV{TZ} = "UTC";

require_compression_modules();

use constant {
  VERSION     => 0.4,
  MAX_SIZE    => 2**20 * 3.0,  # MiB limit
  BASEDIR     => "/var/www/iblech/stuff/pugs-smokes/",
  BASEHTTPDIR => "/iblech/stuff/pugs-smokes/",
  BUCKET      => "bucket.dat",
  MAX_RATE    => 1 / 30,       # Allow a new smoke all 30s
  BURST       => 5,            # Set max burst to 5
  MAX_SMOKES_OF_SAME_CATEGORY => 5,
};
$CGI::POST_MAX = MAX_SIZE;
chdir BASEDIR or die "Couldn't chdir into \"@{[ BASEDIR ]}\": $!\n";

my $CGI = CGI->new;
if($CGI->param("upload")) {
  process_upload();
} else {
  process_list();
}

exit;

sub process_upload {
  print "Content-Type: text/plain\n\n";

  limit_rate();
  validate_params();
  add_smoke();
  clean_obsolete_smokes();

  print "ok";
}

sub validate_params {
  if(not $CGI->param("version") or $CGI->param("version") != VERSION) {
    print "Versions do not match!";
    exit;
  }

  if(not $CGI->param("smoke")) {
    print "No smoke given!";
    exit;
  }

  uncompress_smoke();
  unless($CGI->param("smoke") =~ /^<!DOCTYPE html/) {
    print "The submitted smoke does not look like a smoke!";
    exit;
  }
}

sub uncompress_smoke {
  $CGI->param("smoke",
    Compress::Zlib::memGunzip($CGI->param("smoke")) ||
    Compress::Bzip2::memBunzip($CGI->param("smoke")) ||
    $CGI->param("smoke"));
}

sub require_compression_modules {
  no strict 'refs';
  eval { require Compress::Zlib } or
    *Compress::Zlib::memGunzip = sub { return };
  eval { require Compress::Bzip2 } or
    *Compress::Bzip2::memBunzip = sub { return };
}

sub add_smoke {
  my $html = $CGI->param("smoke");

  my $id = md5_hex $html;
  if(glob "pugs-smoke-*-$id.html") {
    print "The submitted smoke was already submitted!";
    exit;
  }

  my %smoke;
  $html =~ /pugs_versnum: ([\d.]+)/ and $smoke{pugs_version}  = $1;
  $html =~ /pugs_revision: (\d+)/   and $smoke{pugs_revision} = $1;
  $html =~ /osname: ([\w\d]+)/      and $smoke{osname}        = $1;
  $html =~ /duration: (\d+)/        and $smoke{duration}      = $1;
  $html =~ /pugs-path: (.+)$/m      and $smoke{runcore}       = pugspath2runcore($1);
  $html =~ /summary="(\d+) test cases: (\d+) ok, (\d+) failed, (\d+) todo, (\d+) skipped and (\d+) unexpectedly succeeded"/    and $smoke{summary}       = {
    total    => $1,
    ok       => $2,
    failed   => $3,
    todo     => $4,
    skipped  => $5,
    unexpect => $6,
  };

  if(grep { not $smoke{$_} } qw<pugs_version osname duration summary runcore>) {
    print "The submitted smoke has an invalid format!";
    exit;
  }

  $smoke{pugs_revision} ||= 0;
  $smoke{timestamp}       = time;
  $smoke{id}              = $id;
  my $filename            = pack_smoke(%smoke);

  open my $fh, ">", $filename or
    die "Couldn't open \"$filename\" for writing: $!\n";
  print $fh $html or
    die "Couldn't write to \"$filename\": $!\n";
  close $fh or
    die "Couldn't close \"$filename\": $!\n";
}

sub clean_obsolete_smokes {
  my $category = sub {
    return join "-",
      (map { $_[0]->{$_} } qw<pugs_version osname runcore>),
      $_[0]->{pugs_revision} == 0 ? "release" : "dev",
  };

  my %cats;
  my @smokes = map { unpack_smoke($_) } glob "pugs-smoke-*.html";
  push @{ $cats{$category->($_)} }, $_ for @smokes;

  $cats{$_} = [
    (sort {
      $b->{pugs_revision} <=> $a->{pugs_revision} ||
      $b->{timestamp}[0]  <=> $a->{timestamp}[1]
    } @{ $cats{$_} })
    [0..MAX_SMOKES_OF_SAME_CATEGORY-1]
  ] for keys %cats;

  my %delete = map { $_->{filename} => 1 } @smokes;
  for(map { @$_ } values %cats) {
    next unless $_;

    delete $delete{$_->{filename}};
  }

  unlink keys %delete;
}

sub process_list {
  my $tmpl = HTML::Template->new(filehandle => *DATA, die_on_bad_params => 0);

  print "Content-Type: text/html\n\n";
  #$tmpl->output(print_to => *STDOUT);

  my $category = sub {
    return sprintf "%s / %s",
      $_[0]->{pugs_revision} == 0 ? "release" : "repository snapshot",
      $_[0]->{osname};
  };

  my @smokes  = map { unpack_smoke($_) } glob "pugs-smoke-*.html";
  my %runcores;
  push @{ $runcores{$_->{runcore}}{$category->($_)} }, $_ for @smokes;

  foreach my $runcore (keys %runcores) {
    foreach my $cat   (keys %{ $runcores{$runcore} }) {
      $runcores{$runcore}{$cat} = [
        map  {{ %$_, timestamp => $_->{timestamp}[1] }}
        sort {
          $b->{pugs_revision} <=> $a->{pugs_revision} ||
          lc $a->{osname}     cmp lc $b->{osname}     ||
          $b->{timestamp}[0]  <=> $a->{timestamp}[0]
        } @{ $runcores{$runcore}{$cat} }
      ];
    }

    $runcores{$runcore} = [
      map {{
        catname => $_,
        smokes  => $runcores{$runcore}{$_},
      }} sort { lc $a cmp lc $b } keys %{ $runcores{$runcore} }
    ];
  }

  $tmpl->param(runcores => [
    map {{
      name       => $_,
      categories => $runcores{$_},
    }} sort keys %runcores
  ]);
  print $tmpl->output;
}

sub pack_smoke {
  my %smoke = @_;

  return sprintf "pugs-smoke-%s-r%d-%s-%s--%d-%d--%d-%d-%d-%d-%d-%d--%s.html",
    (map { $smoke{$_}          } qw<pugs_version pugs_revision osname runcore timestamp duration>),
    (map { $smoke{summary}{$_} } qw<total ok failed todo skipped unexpect>),
    $smoke{id};
}

sub unpack_smoke {
  my $name = shift;

  /^pugs-smoke-([\d.]+)-r(\d+)-([\w\d]+)-(\w+)--(\d+)-(\d+)--(\d+)-(\d+)-(\d+)-(\d+)-(\d+)-(\d+)--([a-f0-9]+).html$/
    and return {
      pugs_version  => $1,
      pugs_revision => $2,
      osname        => $3,
      runcore       => runcore2human($4),
      timestamp     => [
        $5,
        do { 
          my $str = localtime($5)->strftime("%d %b %Y %H:%M %a");
          $str =~ s/ /&nbsp;/g;
          # hack, to make the timestamps not break so the smoke reports look
          # good even on 640x480
          $str;
        },
      ],
      duration      => sprintf("%.02f", Time::Seconds->new($6)->minutes) . "&nbsp;min",
      summary       => [{
        total       => $7,
        ok          => $8,
        failed      => $9,
        todo        => $10,
        skipped     => $11,
        unexpect    => $12,
      }],
      percentage    => sprintf("%.02f", $8 / ($7||1) * 100),
      id            => $13,
      filename      => $name,
      link          => BASEHTTPDIR . $name, 
    };
  return ();
}

sub pugspath2runcore {
  local $_ = shift;

  /PIL2JS/i  and return "pil2js";
  /PIL-RUN/i and return "pilrun";
  /PIR/i     and return "pir";
  return "normal";
}

sub runcore2human {
  local $_ = shift;

  $_ eq "pil2js" and return "PIL2JS (Perl 6 on JavaScript)";
  $_ eq "pilrun" and return "PIL-Run (Perl 6 on Perl 5)";
  $_ eq "pir"    and return "PIR (Perl 6 on Parrot)";
  $_ eq "normal" and return "Normal runcore (Perl 6 on Haskell)";
  die;
}

# Rate limiting
sub limit_rate {
  # Open the DB and lock it exclusively. See perldoc -q lock.
  sysopen my $fh, BUCKET, O_RDWR|O_CREAT
    or die "Couldn't open \"@{[ BUCKET ]}\": $!\n";
  flock $fh, LOCK_EX
    or die "Couldn't flock \"@{[ BUCKET ]}\": $!\n";

  my $data   = eval { fd_retrieve $fh };
  $data    ||= [MAX_RATE, BURST];
  my $bucket = Algorithm::TokenBucket->new(@$data);

  my $exit;
  unless($bucket->conform(1)) {
    print "Rate limiting -- please wait a bit and try again, thanks.";
    $exit++;
  }
  $bucket->count(1);

  seek     $fh, 0, 0  or die "Couldn't rewind \"@{[ BUCKET ]}\": $!\n";
  truncate $fh, 0     or die "Couldn't truncate \"@{[ BUCKET ]}\": $!\n";

  store_fd [$bucket->state] => $fh or
    die "Couldn't serialize bucket to \"@{[ BUCKET ]}\": $!\n";

  exit if $exit;
}

__DATA__
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN"
  "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
<head>
  <title>Pugs Smoke Reports</title>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />

  <style type="text/css">
    body {
      background-color: white;
      margin:           0;

      font-family: sans-serif;
      line-height: 1.3em;
      font-size:   95%;
    }

    h1, h2 {
      background-color: #313052;
      color:            white;
      padding:          10px;
    }

    th       { text-align: left; }
    .indent0 { padding-top:  30px; border-bottom: 2px solid #313052; }
    .indent1 { padding-top:  10px; border-bottom: 1px solid #313052; }
    .indent2 { padding-left: 40px; }
    .indent3 { padding-left: 80px; padding-bottom: 10px; }

    p, dl, pre, table { margin:      15px; }
    dt    { font-weight: bold; }
    dd+dt { margin-top:  1em;  }
    .leftsep  { padding-left: 10px;  }
    .num      { text-align:   right; }

    .details  { display: none; }
    .expander { color: blue; cursor: pointer; }  /* hack? */

    .tests_ok       { color: #050; }
    .tests_failed   { color: #500; }
    .tests_todo     { color: #030; }
    .tests_skipped  { color: #555; }
    .tests_unexpect { color: #550; }
  </style>

  <script type="text/javascript">//<![CDATA[[
    function toggle_visibility (id) {
      var elem     = document.getElementById("details_"  + id),
          expander = document.getElementById("expander_" + id);
      if(elem.className == "details") {
	elem.className = "";  /* hack? */
	expander.innerHTML = "&laquo;";
      } else {
	elem.className = "details";
	expander.innerHTML = "&raquo;";
      }
    }
  //]]></script>
</head>

<body>
  <h1>Pugs Smoke Reports</h1>

  <p>
    Here's a list of recently submitted <a
    href="http://www.pugscode.org/">Pugs</a> smoke reports. These smokes are
    automatically generated and show how much a given backend (normal
    Haskell runcore, <a
    href="http://svn.openfoundry.org/pugs/perl5/PIL-Run/">Perl 6 on Perl 5</a>,
    <a href="http://svn.openfoundry.org/pugs/perl5/PIL2JS/">Perl 6 on
    JavaScript</a>, ...) supports of <a
    href="http://svn.openfoundry.org/pugs/t/">Pugs's testsuite</a>.
  </p>

  <p>
    Submitting your own smoke is easy,
  </p>

  <pre class="indent2">$ make smoke
$ ./util/smokeserv/smokeserv-client.pl ./smoke.html</pre>

  <p>
    should suffice. See the <a
    href="http://search.cpan.org/dist/Perl6-Pugs/lib/pugs/hack.pod">pugs::hack</a>
    manpage and <a
    href="http://svn.openfoundry.org/pugs/util/smokeserv/README">smokeserv's
    README</a> for details.
  </p>

  <p>
    Note that old smoke reports are automatically deleted, so you may not want
    to link directly to a smoke.
  </p>

  <p>
    (Timezone is UTC.<br />
    Also note that, depending on the backend, the
    <code>ext/</code> tests may or may not be run, causing durations and the ok
    percentage to not be comparable.<br />
    The percentage of passed tests is calculated using the total number of
    tests run -- for example, if a backend only ran three tests, which it
    passed, this page would report 100 % test passes.)
  </p>

  <table>
    <tmpl_loop name=runcores>
      <tr><th colspan="11" class="indent0"><tmpl_var name=name></th></tr>
      <tmpl_loop name=categories>
        <tr><th colspan="11" class="indent1"><tmpl_var name=catname></th></tr>
        <tmpl_loop name=smokes>
          <tr>
            <td class="indent2">Pugs&nbsp;<tmpl_var name=pugs_version></td>
            <td>
              <tmpl_if name=pugs_revision>
                r<tmpl_var name=pugs_revision>
              </tmpl_if>
            </td>
            <td class="leftsep"><tmpl_var name=timestamp></td>
            <td class="leftsep num"><tmpl_var name=duration></td>
            <td class="leftsep num"><tmpl_var name=percentage>&nbsp;%&nbsp;ok</td>
	    <tmpl_loop name=summary>
	      <td class="leftsep num tests_total"><span title="<tmpl_var name=total> total"><tmpl_var name=total></span>:</td>
	      <td class="num tests_ok"><span title="<tmpl_var name=ok> ok"><tmpl_var name=ok></span>,</td>
	      <td class="num tests_failed"><span title="<tmpl_var name=failed> failed"><tmpl_var name=failed></span>,</td>
	      <td class="num tests_todo"><span title="<tmpl_var name=todo> todo"><tmpl_var name=todo></span>,</td>
	      <td class="num tests_skipped"><span title="<tmpl_var name=skipped> skipped"><tmpl_var name=skipped></span>,</td>
	      <td class="num tests_unexpect"><span title="<tmpl_var name=unexpect> unexpectedly succeeded"><tmpl_var name=unexpect></span></td>
	    </tmpl_loop>
	    <td><span title="Details" class="expander" onclick="toggle_visibility('<tmpl_var name=id>')" id="expander_<tmpl_var name=id>">&raquo;</span></td>
	    <td><a style="text-decoration: none" href="<tmpl_var name=link>" title="Full smoke report">&raquo;</a></td>
          </tr>
          <tr class="details" id="details_<tmpl_var name=id>">
            <td colspan="11" class="indent3">
              <tmpl_loop name=summary>
                <span class="tests_total"><tmpl_var name=total> test cases</span>:<br />
		<span class="tests_ok"><tmpl_var name=ok> ok</span>,
		<span class="tests_failed"><tmpl_var name=failed> failed</span>,
		<span class="tests_todo"><tmpl_var name=todo> todo</span>,<br />
                <span class="tests_skipped"><tmpl_var name=skipped> skipped</span> and
		<span class="tests_unexpect"><tmpl_var name=unexpect> unexpectedly succeeded</span>
              </tmpl_loop><br />
              <a href="<tmpl_var name=link>" title="Full smoke report">View full smoke report</a>
            </td>
          </tr>
        </tmpl_loop>
      </tmpl_loop>
    </tmpl_loop>
  </table>
</body>
</html>
