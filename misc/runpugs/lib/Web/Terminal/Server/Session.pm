package Web::Terminal::Server::Session;

use vars qw( $VERSION );
$VERSION='0.2.0';
use strict;
use utf8;

use Web::Terminal::Settings;
=pod
A thin wrapper around Net::Telnet
new() starts the session;
write() sends commands to it.
=cut

$SIG{CHLD}='IGNORE';
## Constructor
sub new {
	my $invocant = shift;
	my $class    = ref($invocant) || $invocant;
	my $self     = {@_};
	#my $prompt = '/\>\ /';
    my $prompt ='/'.$Web::Terminal::Settings::prompt.'/';
    $self->{'prompt'}=$prompt;
   # my $prompt= '/'.$Web::Terminal::Settings::init_pattern.'/';
    #my $prompt= '/'.$Web::Terminal::Settings::prompt_pattern.'/';
    $self->{'error'}=0;
    $self->{'recent'}=[];
	## Start pugs
#    $ENV{PUGS_SAFEMODE}=1;# Must be in CGI script!
	( $self->{'pty'},$self->{'pid'} ) =
    &spawn($Web::Terminal::Settings::command);    # spawn() defined below
    if ( $self->{'pty'}==-1 and  $self->{'pid'}==0) {
        $self->{'output'}= "\nThere was a problem starting pugs. Please try again later.";
        $self->{'error'}=1;
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
    my $error='';
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
      #die "starting pugs failed: ", $self->{'pugs'}->lastline;
	$self->{'output'}= $m; #$p.$m.$error;#$self->{'pugs'}->prompt;
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
    $pugs->errmode(sub {kill 9,$obj->{'pid'};});

	while ($i<$Web::Terminal::Settings::nlines) {
    my $char='';
    my $line='';
    while ($char ne "\n") {
    $char=$pugs->get;
    $line.=$char;
    last if $line eq $Web::Terminal::Settings::prompt;
    }
#		my $line = $pugs->getline;
#        chomp $line;
        print $line;
        my $msg=$pugs->errmsg;
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

   if ($i>=$Web::Terminal::Settings::nlines-1) {
       $obj->{pugs}->close();
        kill 9, $obj->{'pid'};
        $lline.="Generated output is limited to $Web::Terminal::Settings::nlines lines. Aborted.\npugs";
        $obj->{'error'}=1;
    }
    chomp $ps; # a hack!
	#$lline .= $ps;
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
