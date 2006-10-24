package Web::Terminal::Dispatcher;

use vars qw( $VERSION );
$VERSION = '0.2.0';
use strict;
use utf8;
use YAML::Syck;
#
# based on testmsg.pl from "Advanced Perl Programming"
#
use Web::Terminal::Settings;
use Web::Terminal::Msg;
use Exporter;

our @ISA         = qw( Exporter );
our @EXPORT   = qw(send );
our @EXPORT_OK   = qw(send );
our %EXPORT_TAGS = (
	ALL     => [qw( send )],
	DEFAULT => [],
);

sub send {
	my $id = shift;
	my $ip = shift;
    my $app=shift;
    my $interactive=shift;
    my $cmds  = shift;
	my $host = $Web::Terminal::Settings::host;
	my $port = $Web::Terminal::Settings::port;
	my $cmd=$cmds;#'';
=old    
    # we only consider the last line with a prompt
	my @cmdlines=split("\n",$cmds);
	for my $cmdline (reverse @cmdlines) {
		$cmdline=~/^\s*$/ && next;
		#$cmdline=~/^(pugs|\.\.\.\.)\>\s+/ && do {
		$cmdline=~/$Web::Terminal::Settings::prompt_pattern/ && do {
			$cmd=$cmdline;
			#$cmd=~s/^(pugs|\.\.\.\.)\>\s+//;
			$cmd=~s/$Web::Terminal::Settings::prompt_pattern//;
			chomp $cmd;
			last;
		};
	} 
=cut
    
#   We're using PUGS_SAFEMODE=1 instead 
#    if ($Web::Terminal::Settings::filter and
#    $cmd=~/$Web::Terminal::Settings::filter_pattern/) {
#    if ($cmd=~/\b(system|exec|fork|wait|open|slurp|eval|kill)\b|(\`)/) {
#    my $offending_command=$1||$2;
#    return "Sorry, \'$offending_command\' is not allowed.\npugs> ";
#    return "Sorry, \'$offending_command\' is not
#    allowed.\n$Web::Terminal::Settings::prompt";
 #   } else {
    my $conn;
#    my $ntries=5;
#    for (1..$ntries) {
	$conn = Web::Terminal::Msg->connect( $host, $port, \&rcvd_msg_from_server );
#	die "Client could not connect to $host:$port ($wd)\n" unless $conn;
    if (not $conn) {
 #       # Assume server has died
#WV: disabled, too dangerous
#       system("/usr/bin/perl ../bin/termserv.pl");
#       sleep 5;
#    } else {last;}
        return "Sorry, the pugs server is not running.";
    } else {
	my $msg = YAML::Syck::Dump({ id=> $id, ip=> $ip, app=>$app,ia=>$interactive,cmd=> $cmd});
	$conn->send_now($msg);
	( my $rmesg, my $err ) = $conn->rcv_now();
#	( my $rid, my $reply ) = split( "\n", $rmesg, 2 );
    my $rmesgref= YAML::Syck::Load($rmesg);
     my $rid=$rmesgref->{id};
      my $reply=$rmesgref->{msg};
      my $histref=$rmesgref->{recent};
      my $prompt=$rmesgref->{prompt};
    $conn->disconnect();
	if ( "$id" ne  "$rid" ) {
#		die "Terminal server returned wrong id: $rid, should be $id";
        return "Sorry, the pugs session died.";
	}
	return ($reply,$prompt,$histref);
   }
}

sub rcvd_msg_from_server {
	my ( $conn, $msg, $err ) = @_;
	if ( defined $msg ) {
		die "Strange... shouldn't really be coming here\n";
	}
}
