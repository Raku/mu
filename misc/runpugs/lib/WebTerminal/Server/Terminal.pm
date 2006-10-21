package WebTerminal::Server::Terminal;
use vars qw( $VERSION );
$VERSION='0.1.0';
use strict;
use utf8;
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
	my $prompt = '/>\ /';
    $self->{'error'}=0;
	## Start pugs
#    $ENV{PUGS_SAFEMODE}=1;# Must be in CGI script!
	( $self->{'pty'},$self->{'pid'} ) = &spawn("/usr/bin/nice /usr/bin/pugs");    # spawn() defined below
if ( $self->{'pty'}==-1 and  $self->{'pid'}==0) {
    $self->{'init'}= "\nThere was a problem starting pugs. Please try again later.";
    $self->{'error'}=1;
} else {
	## Create a Net::Telnet object to perform I/O on pugs's tty.
	use Net::Telnet;
	$self->{'pugs'} = new Net::Telnet(
		-fhopen => $self->{'pty'},
        -timeout => 20,
		-prompt => $prompt,
		-telnetmode      => 0,
		-cmd_remove_mode => 0,
	);
	#( $self->{'init'}, my $m ) = $self->{'pugs'}->waitfor(
    my $error='';
	( my $p, my $m ) = $self->{'pugs'}->waitfor(
		-match   => $self->{'pugs'}->prompt,
		-errmode => "return"
	  ) or do {
#      die "starting pugs failed: ", $self->{'pugs'}->lastline;
      $error="\nThere was a problem starting pugs. Please try again later.";
      $self->{'error'}=1;
      # should close the TTY
      $self->{'pugs'}->close();
      };
	$self->{'init'}= $p.$m.$error;#$self->{'pugs'}->prompt;
    }
	bless($self,$class);
	return $self;
}

sub write {
	my $obj = shift;
	my $cmd  = shift;
	chomp $cmd;
	my $ps = '';

	if ( $cmd eq ':q' ) {
		kill 9, $obj->{'pid'};		
		return "\nLeaving pugs.\n";
	}
	
	my $i     = 1;
	my $lline = '';
    my $pugs=$obj->{'pugs'};
    $pugs->errmode(sub {kill 9,$obj->{'pid'};});

	$pugs->print($cmd);
	while ($i<256) {
		my $line = $pugs->getline;
        my $msg=$pugs->errmsg;
#	    print "L:",$line,":",$msg;
	    if($msg=~/timed/) {
            $msg='';
            $pugs->errmsg([]);
            $lline="pugs> Sorry, that took too long! Aborted.\n";
#            $pugs->close();
            $ps='pugs';
            $obj->{'error'}=1;
            last;
        }
        $msg='';
        if ( ($line =~ /(pugs|\.\.\.\.)\>/ or ($line=~/^Leaving\ pugs./)) and $i > 1 ) { $ps = $1; last }
		$lline .= $line unless $line =~ /(pugs|\.\.\.\.)\>/;
		$i++;
	}
    if ($i>=255) {
#            $pugs->close();
     kill 9, $obj->{'pid'};
            $obj->{'error'}=1;
     $lline.="Generated output is limited to 100 lines. Aborted.\npugs";
    }

	#$lline .= "\n$ps>";
	$lline .= "$ps> ";
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
          return -1,0;
      };
      #die $!;

	## Execute the program in another process.
	unless ( $pid = fork ) {    # child process
		#die "problem spawning program: $!\n" unless defined $pid;
        if (not defined $pid) {
            $pty->close();
            $error=1;
        } else { # all is well
		## Disassociate process from existing controlling terminal.
		use POSIX ();
		POSIX::setsid
		  or ($error=1); #die "setsid failed: $!";

		## Associate process with a new controlling terminal.
		$tty    = $pty->slave;
		$tty_fd = $tty->fileno;
		close $pty;

		## Make stdio use the new controlling terminal.
		open STDIN,  "<&$tty_fd" or ($error=1);#die $!;
		open STDOUT, ">&$tty_fd" or ($error=1);#die $!;
		open STDERR, ">&STDOUT"  or ($error=1);#die $!;
		close $tty;

		## Execute requested program.
		exec @cmd
		  or ($error=1);#die "problem executing $cmd[0]\n";
          }
	}    # end child process
    if($error==1) {
        $pty=-1;
        $pid=0;
    }
	return ( $pty, $pid );
}    # end sub spawn
