package Web::Terminal::Dispatcher3;

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
my $v=1-$Web::Terminal::Settings::daemon;

sub send {
	my $id = shift;
	my $ip = shift;
    my $app=shift;
    my $interactive=shift;
    my $cmds  = shift;
	my $host = $Web::Terminal::Settings::host;
	my $port = $Web::Terminal::Settings::port;
	my $cmd=$cmds;#'';
    
#WV:   We're using PUGS_SAFEMODE=1 instead 
#    if ($Web::Terminal::Settings::filter and
#    $cmd=~/$Web::Terminal::Settings::filter_pattern/) {
#    if ($cmd=~/\b(system|exec|fork|wait|open|slurp|eval|kill)\b|(\`)/) {
#    my $offending_command=$1||$2;
#    return "Sorry, \'$offending_command\' is not allowed.\npugs> ";
#    return "Sorry, \'$offending_command\' is not
#    allowed.\n$Web::Terminal::Settings::prompt";
 #   } else {
    my $conn;
	$conn = Web::Terminal::Msg->connect( $host, $port, \&rcvd_msg_from_server );
    if (not $conn) {
#WV: disabled, too dangerous
#       system("/usr/bin/perl ../bin/termserv.pl");
#       sleep 5;
#    } else {last;}
        return "Sorry, the pugs server is not running.";
    } else {
	my $msg = YAML::Syck::Dump({ id=> $id, ip=> $ip, app=>$app,ia=>$interactive,cmd=> $cmd});
    print STDERR "Sending message to server: $msg\n",'#' x 70,"\n" if $v;
	$conn->send_now($msg);
    print STDERR "done\n" if $v;
	( my $rmesg, my $err ) = $conn->rcv_now();
    print STDERR "Received reply from server: $rmesg (Error msg:$err)\n",'#' x 70,"\n" if $v;
    my $rmesgref= YAML::Syck::Load($rmesg);
     my $rid=$rmesgref->{id};
      my $reply=$rmesgref->{msg};
      my $histref=$rmesgref->{recent};
      my $prompt=$rmesgref->{prompt};
    $conn->disconnect();
	if ( "$id" ne  "$rid" ) {
		print "Terminal server returned wrong id: $rid, should be $id";
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
1;
__END__

=head1 NAME

Web::Terminal::Dispatcher -- Dispatches commands to a  terminal
session
Requires YAML::Syck.

=head1 SYNOPSIS

   use Web::Terminal::Dispatcher;
   use Web::Terminal::Settings;

    ($reply, $prompt, $histref) =
    &Web::Terminal::Dispatcher::send($sessionid,$ip,$version,$interactive,$cmd);

=head1 DESCRIPTION

This module exports a single subroutine C<send>. The arguments are:
    $sessionid: a string identifying the session
    $ip: the IP address of the client as a dotted-decimal string
    $version: an integer indicating the version of the terminal application to
    be used. The value is the index in the list of C<commands> (see L<Settings.pm>). 
    $interactive: if 1, the session is interactive, else it's batch mode
    $cmd: the actual command to be sent to the terminal application.

The call to C<send> returns a list containing:
    $reply: a string containing the reply from the terminal application
    $prompt: a string containing the prompt from the terminal application
    $histref: a reference to a list of the most recent commands (see
    Settings.pm)

=head1 SEE ALSO

L<Web::Terminal::Settings>,
L<Web::Terminal::Server>,
L<Web::Terminal::Server::Session>,
L<Web::Terminal::Msg>

=head1 AUTHOR

Wim Vanderbauwhede <wim.vanderbauwhede@gmail.com>

=head1 COPYRIGHT

Copyright (c) 2006. Wim Vanderbauwhede. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
