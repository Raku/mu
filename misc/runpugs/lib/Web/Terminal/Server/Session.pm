package Web::Terminal::Server::Session;

use vars qw( $VERSION );
$VERSION='0.2.0';
use strict;
use utf8;

use Web::Terminal::Settings;

#A thin wrapper around Net::Telnet
#new() starts the session;
#write() sends commands to it.

$SIG{CHLD}='IGNORE';
## Constructor
sub new {
	my $invocant = shift;
	my $class    = ref($invocant) || $invocant;
	my $self     = {@_};
    my $prompt ='/'.$Web::Terminal::Settings::prompt.'/';
    $self->{'prompt'}=$prompt;
    $self->{'error'}=0;
    $self->{'recent'}=[];
	## Start pugs
    my $app=$self->{'app'};
    my $command=$Web::Terminal::Settings::commands[$app];
    if ($self->{'ia'}==0) {
        #1. Create a file with the content of $cmd using $id.p6 for name, store in data
        my $id=$self->{'id'};
        open(P6,">$Web::Terminal::Settings::tmp_path/$id.p6") or ($self->{'error'}=1);
        if($self->{'error'}==0) {
        print P6 $self->{'cmds'};
        close P6;
            $command.=" $Web::Terminal::Settings::tmp_path/$id.p6";
        }
    }
	( $self->{'pty'},$self->{'pid'} ) = &spawn($command);    # spawn() defined below
    if ( $self->{'pty'}==-1 and  $self->{'pid'}==0) {
        $self->{'output'}= "\nThere was a problem starting pugs. Please try again later.";
        $self->{'error'}=1;
        if ($self->{'ia'}==0) {
            my $id=$self->{'id'};
            unlink "$Web::Terminal::Settings::tmp_path/$id.p6";
        }
    } else {
	## Create a Net::Telnet object to perform I/O on pugs's tty.
	use Net::Telnet;
	$self->{'pugs'} = new Net::Telnet(
		-fhopen => $self->{'pty'},
		-timeout => $Web::Terminal::Settings::timeout_call,
		-prompt => $prompt,
		-telnetmode      => 0,
		-cmd_remove_mode => 0,
	);
#    my $error='';
#    ( my $p, my $m ) = $self->{'pugs'}->waitfor(
#		-match   => $self->{'pugs'}->prompt,
#		-errmode => "return"
#	  ) or do {
#          $self->{'error'}=1;
#          $error="\nThere was a problem starting pugs. Please try again later.";
#          # should close the TTY
#          $self->{'pugs'}->close();
#      };
	bless($self,$class);
    my $m=$self->readlines();
      if ($self->{'error'}==1) {
          # should close the TTY
          $self->{'pty'}->close() unless ($self->{'pty'}==-1);
          $self->{'pugs'}->close();
      }
        if ($self->{'ia'}==0) {
        my $id=$self->{'id'};
        unlink "$Web::Terminal::Settings::tmp_path/$id.p6";
          # should close the TTY
          $self->{'pty'}->close() unless ($self->{'pty'}==-1);
          $self->{'pugs'}->close();
        }
	$self->{'output'}= $m; 
    }
	#bless($self,$class);
	return $self;
}

sub readlines {
	my $obj = shift;
	my $ps = '';
	my $i     = 1;
	my $lline = '';
    my $pugs=$obj->{'pugs'};
    $pugs->errmode(sub {kill 9,$obj->{'pid'}; });
    #$pugs->errmode('die');
#    print "readlines()\n";
	while ($i<$Web::Terminal::Settings::nlines) {
    my $char='';
    my $line='';
    my $j=0;
    while ($char ne "\n" and ($j<$Web::Terminal::Settings::nchars)) {
#    print "getting...\n";
    $char=$pugs->get();
#    print "got $j>$char<\n";
    $j++;
    last if $char eq '';
    $line.=$char;
    last if $line eq $Web::Terminal::Settings::prompt;
    }
        #print $line;
        if ($j>=$Web::Terminal::Settings::nchars-1) {
        $line.="Generated output is limited to $Web::Terminal::Settings::nchars characters. Aborted.\n";
       $obj->{pugs}->close();
        kill 9, $obj->{'pid'};
        $obj->{'error'}=1;
        $lline .= $line;
        last;
        }
        my $msg=$pugs->errmsg;
	    if($msg=~/timed/) {
            $msg='';
            $pugs->errmsg([]);
            if ($obj->{'ia'}==1) {
            $lline="${Web::Terminal::Settings::prompt} Sorry, that took too long! Aborted.\n";
            $pugs->close();
            $ps=$Web::Terminal::Settings::prompt;
            $obj->{'error'}=1;
            } else {
            $lline.=$line;
            }
            last;
        }
        $msg='';
        if ( ($line =~ /$Web::Terminal::Settings::prompt_pattern/ or
        ($line=~/$Web::Terminal::Settings::quit_pattern/)) and $i > 1 ) { $ps = $1; last }
		$lline .= $line unless $line =~
        /$Web::Terminal::Settings::prompt_pattern/;
		$i++;
#        print "$i\n";
        last if $line eq '';
	}
   if ($i>=$Web::Terminal::Settings::nlines-1) {
       $obj->{pugs}->close();
        kill 9, $obj->{'pid'};
        $lline.="Generated output is limited to $Web::Terminal::Settings::nlines lines. Aborted.\npugs";
        $obj->{'error'}=1;
    }
    chomp $ps; # a hack!
	$obj->{prompt}=$ps;
	return $lline;
} # end readlines method

sub write {
	my $obj = shift;
	my $cmd  = shift;
#	print "CMD1: $cmd\n";
#    $cmd=pack("U0C*", unpack("C*",$cmd));    
#	print "CMD2: $cmd\n";
    chomp $cmd;
	my $ps = '';

	if ( $cmd eq $Web::Terminal::Settings::quit_command ) {
        $obj->{pugs}->close();
		kill 9, $obj->{'pid'};		
		return "\n$Web::Terminal::Settings::quit_message\n";
	}
	
	my $i     = 1;
	my $lline = '';
    my $pugs=$obj->{'pugs'};
    $pugs->errmode(sub {kill 9,$obj->{'pid'};});

	$pugs->print($cmd);
	while ($i<$Web::Terminal::Settings::nlines) {
		my $line = $pugs->getline;
        my $msg=$pugs->errmsg;
#	    print "L:",$line,":",$msg;
	    if($msg=~/timed/) {
            $msg='';
            $pugs->errmsg([]);
            $lline="${Web::Terminal::Settings::prompt} Sorry, that took too long! Aborted.\n";
            $pugs->close();
            $ps=$Web::Terminal::Settings::prompt;
            $obj->{'error'}=1;
            last;
        }
        $msg='';
        if ( ($line =~ /$Web::Terminal::Settings::prompt_pattern/ or
        ($line=~/$Web::Terminal::Settings::quit_pattern/)) and $i > 1 ) { $ps = $1; last }
		$lline .= $line unless $line =~
        /$Web::Terminal::Settings::prompt_pattern/;
		$i++;
	}

	#$lline .= "\n$ps>";
   if ($i>=$Web::Terminal::Settings::nlines-1) {
       $obj->{pugs}->close();
        kill 9, $obj->{'pid'};
        $lline.="Generated output is limited to $Web::Terminal::Settings::nlines lines. Aborted.\npugs";
        $obj->{'error'}=1;
    }

    chomp $ps; # a hack!
#	$lline .= $ps;
	$obj->{prompt}=$ps;
	return $lline;
}    # end write method

#sub run {
#	my $obj = shift;
#    my $cmd = shift;
#my $error=0;
#my $id=$obj->{'id'};
##1. Create a file with the content of $cmd using $id.p6 for name, store in data
#open(P6,">$Web::Terminal::Settings::tmp_path/$id.p6") or ($error=1);
##2. Create a Net::Telnet object 
#
##3. Read back the result
##4. Close the Net::Telnet object
#
#}

sub spawn {
	my (@cmd) = @_;
	my ( $pid, $pty, $tty, $tty_fd );
    my $error=0;
	## Create a new pseudo terminal.
	use IO::Pty ();
	$pty = new IO::Pty
	  or do {
          return ( -1, 0 );
      };
      #die $!;
    binmode $pty, ":utf8"; 
	## Execute the program in another process.
	unless ( $pid = fork ) {    # child process
	#	die "problem spawning program: $!\n" unless defined $pid;
     if (not defined $pid) {
        $pty->close();
        $error=1;
    } else { # all is well
		## Disassociate process from existing controlling terminal.
		use POSIX ();
		POSIX::setsid
		  or ($error=1);#die "setsid failed: $!";

		## Associate process with a new controlling terminal.
		$tty    = $pty->slave;
        binmode $tty, ":utf8";
		$tty_fd = $tty->fileno;
		close $pty;

		## Make stdio use the new controlling terminal.
		open STDIN,  "<&$tty_fd" or ($error=1);#die $!;
		open STDOUT, ">&$tty_fd" or ($error=1);#die $!;
		open STDERR, ">&STDOUT"  or ($error=1);#die $!;
        binmode STDIN, ":utf8";
        binmode STDOUT, ":utf8";
        binmode STDERR, ":utf8";
		close $tty;

		## Execute requested program.
		exec @cmd
		  or  ($error=1);#die "problem executing $cmd[0]\n";
          }
	}    # end child process

    if($error==1) {
        $pty=-1;
        $pid=0;
    }

	return ( $pty, $pid );
}    # end sub spawn

__END__

=head1 NAME

Web::Terminal::Server::Session -- Session object to encapsulate terminal
application.
Requires Net::Telnet.

=head1 SYNOPSIS

    package  Web::Terminal::Server
    use Web::Terminal::Settings;
    use Web::Terminal::Server::Session;

    $terminals{$id} = 
    new Web::Terminal::Server::Session(
        id=>$id,
        app=>$app,
        ia=>$ia,
        cmds=>$cmd
        );

=head1 DESCRIPTION

This module provides a Session object. The object encapsulates the
actual terminal session. The session is configured via L<Settings.pm>.
The object's C<new> method takes following arguments:
    id: a string identifying the session
    app: an integer indicating the version of the terminal application to
    be used. The value is the index in the list of C<commands> (see L<Settings.pm>). 
    ia: if 1, the session is interactive, else it's batch mode
    cmds: the actual command or script to be sent to the terminal application.

The object constructor forks off a process for the terminal application and
communicates with it via a pseudo-tty created with L<IO::Pty>. Reading the
datastream, timeouts and errors are handled via L<Net::Telnet>


=head1 SEE ALSO

L<Web::Terminal::Settings>,
L<Web::Terminal::Server>,
L<Web::Terminal::Dispatcher>,
L<Web::Terminal::Msg>

=head1 AUTHOR

Wim Vanderbauwhede <wim.vanderbauwhede@gmail.com>

=head1 COPYRIGHT

Copyright (c) 2006. Wim Vanderbauwhede. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
