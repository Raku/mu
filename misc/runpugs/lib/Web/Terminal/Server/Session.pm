package Web::Terminal::Server::Session;

use vars qw( $VERSION );
$VERSION='0.3.0';
use Moose;
#use strict;
#use utf8;
use lib '.','../../..';
use Web::Terminal::Settings;

#A thin wrapper around Net::Telnet
#new() starts the session;
#write() sends commands to it.

$SIG{CHLD}='IGNORE';

my $v=1;#(1-$Web::Terminal::Settings::daemon)*(1-$Web::Terminal::Settings::test);

    my $prompt ='/'.$Web::Terminal::Settings::prompt.'/';
    has 'prompt' => (is=>'rw',isa=>'Str', default => $prompt);
    has 'error' => (is=>'rw',isa=>'Int',default=>0);
 	has 'output' => (is=>'rw',isa=>'Str');    
    has 'recent'=> (is=>'rw',isa=>'ArrayRef',default=>sub {[]});
    has 'id' => (is=>'ro',isa=>'Str');
    has 'ip' => (is=>'ro',isa=>'Str');
    has 'app' => (is=>'ro',isa=>'Int');
    has 'ia' => (is=>'ro',isa=>'Int');
    has 'cmds' => (is=>'ro',isa=>'Str');
   	has 'pugs' => (is=>'rw',isa=>'Str');
    has 'pty' => (is=>'rw',isa=>'Any');
    has 'pid' => (is=>'rw',isa=>'Int');
    has 'motd' => (is=>'rw',isa=>'Str');
    
    sub BUILD {
    	my $self=shift;
    print "Starting pugs session for ",$self->id,"\n" if $v; 
	## Start pugs
    my $app=$self->app;
    my $command=$Web::Terminal::Settings::commands[$app];
    if ($Web::Terminal::Settings::test!=1) {
    if ($self->ia==0) {
        #1. Create a file with the content of $cmd using $id.p6 for name, store in data
        my $id=$self->id;
        open(P6,">$Web::Terminal::Settings::tmp_path/$id.p6") or $self->error(1);
        if($self->error==0) {
        print P6 $self->cmds;
        close P6;
            $command.=" $Web::Terminal::Settings::tmp_path/$id.p6";
        }
    }
	( $self->{'pty'},$self->{'pid'} ) = &spawn($command);    # spawn() defined below
    if ( $self->pty==-1 and  $self->pid==0) {
        print "There was a problem starting pugs. Please try again later.\n"
        if $v;
        $self->output("\nThere was a problem starting pugs. Please try again later.");
        $self->error(1);
        if ($self->ia==0) {
            my $id=$self->id;
            unlink "$Web::Terminal::Settings::tmp_path/$id.p6";
        }
    } else {
	## Create a Net::Telnet object to perform I/O on pugs's tty.
	print "Pugs started successfully. Creating Net::Telnet object as session handler...\n" if $v;
	use Net::Telnet;
	$self->{'pugs'} = new Net::Telnet(
		-fhopen => $self->pty,
		-timeout => $Web::Terminal::Settings::timeout_call,
		-prompt => $prompt,
		-telnetmode      => 0,
		-cmd_remove_mode => 0,
		-input_record_separator => "\n",
	);
    }
    } else {
    	$self->{'pid'}=42;
    	$self->{'pugs'} ="Test flag is set -- no session started for $command.";
    }
	print "Reading Pugs startup message...\n" if $v;
    my $m=$self->readlines();
    $self->{'motd'}=$m;
      if ($self->{'error'}==1 and not $Web::Terminal::Settings::test) {
          # should close the TTY
          $self->{'pty'}->close() unless ($self->{'pty'}==-1);
          $self->{'pugs'}->close();
      }
        if ($self->{'ia'}==0 and not $Web::Terminal::Settings::test) {
        my $id=$self->{'id'};
        unlink "$Web::Terminal::Settings::tmp_path/$id.p6";
          # should close the TTY
          $self->{'pty'}->close() unless ($self->{'pty'}==-1);
          $self->{'pugs'}->close();
        }
	$self->{'output'}= $m;
	print "\nStatus is ",($self->{'error'}==1)?'ERROR':'OK',"\n" if $v; 
print "Returning with output:\n",$m,"\n\n" if $v;
	return $self;
} # END of new() constructor method



### Constructor
#sub new {
#	my $invocant = shift;
#	my $class    = ref($invocant) || $invocant;
#	my $self     = {@_};
#    my $prompt ='/'.$Web::Terminal::Settings::prompt.'/';
#    $self->{'prompt'}=$prompt;
#    $self->{'error'}=0;
#    $self->{'recent'}=[];
#    print "Starting pugs session for ",$self->{'id'},"\n" if $v; 
#	## Start pugs
#    my $app=$self->{'app'};
#    my $command=$Web::Terminal::Settings::commands[$app];
#    if (not $Web::Terminal::Settings::test) {
#    if ($self->{'ia'}==0) {
#        #1. Create a file with the content of $cmd using $id.p6 for name, store in data
#        my $id=$self->{'id'};
#        open(P6,">$Web::Terminal::Settings::tmp_path/$id.p6") or ($self->{'error'}=1);
#        if($self->{'error'}==0) {
#        print P6 $self->{'cmds'};
#        close P6;
#            $command.=" $Web::Terminal::Settings::tmp_path/$id.p6";
#        }
#    }
#	( $self->{'pty'},$self->{'pid'} ) = &spawn($command);    # spawn() defined below
#    if ( $self->{'pty'}==-1 and  $self->{'pid'}==0) {
#        print "There was a problem starting pugs. Please try again later.\n"
#        if $v;
#        $self->{'output'}= "\nThere was a problem starting pugs. Please try again later.";
#        $self->{'error'}=1;
#        if ($self->{'ia'}==0) {
#            my $id=$self->{'id'};
#            unlink "$Web::Terminal::Settings::tmp_path/$id.p6";
#        }
#    } else {
#	## Create a Net::Telnet object to perform I/O on pugs's tty.
#	print "Pugs started successfully. Creating Net::Telnet object as session handler...\n" if $v;
#	use Net::Telnet;
#	$self->{'pugs'} = new Net::Telnet(
#		-fhopen => $self->{'pty'},
#		-timeout => $Web::Terminal::Settings::timeout_call,
#		-prompt => $prompt,
#		-telnetmode      => 0,
#		-cmd_remove_mode => 0,
#	);
#    }
#    } else {
#    	$self->{'pid'}=42;
#    	$self->{'pugs'} ="Test flag is set -- no session started for $command.";
#    }
#	bless($self,$class);
#	print "Reading Pugs startup message...\n" if $v;
#    my $m=$self->readlines();
#    #my $m=$self->write('');
#      if ($self->{'error'}==1 and not $Web::Terminal::Settings::test) {
#          # should close the TTY
#          $self->{'pty'}->close() unless ($self->{'pty'}==-1);
#          $self->{'pugs'}->close();
#      }
#        if ($self->{'ia'}==0 and not $Web::Terminal::Settings::test) {
#        my $id=$self->{'id'};
#        unlink "$Web::Terminal::Settings::tmp_path/$id.p6";
#          # should close the TTY
#          $self->{'pty'}->close() unless ($self->{'pty'}==-1);
#          $self->{'pugs'}->close();
#        }
#	$self->{'output'}= $m;
#	print "\nStatus is ",($self->{'error'}==1)?'ERROR':'OK',"\n" if $v; 
#print "Returning with output:\n",$m,"\n\n" if $v;
#	return $self;
#} # END of new() constructor method
#------------------------------------------------------------------------------
sub DESTROY {
    my $obj = shift;
    if ( $Web::Terminal::Settings::test!=1) {
    	if (defined $obj->{'pty'}) {
    $obj->{'pty'}->close();
     $obj->{'pty'}->close_slave();
    	}
    	if (defined $obj->{'pugs'}) {
      $obj->{'pugs'}->close();
    	}
      if(defined $obj->{'pid'} and $obj->{'pid'}>10){
          kill 9,$obj->{'pid'};
      }
    }
} # END of DESTROY()
#------------------------------------------------------------------------------
sub write {
	my $obj = shift;
	my $cmd  = shift;
	print "CMD: $cmd\n" if $v;
#    $cmd=pack("U0C*", unpack("C*",$cmd));    
    chomp $cmd;
	my $ps = '';
		my $i     = 1;
	my $lline = '';
if ( $Web::Terminal::Settings::test!=1) {
	if ( $cmd eq $Web::Terminal::Settings::quit_command ) {
        $obj->{pugs}->close();
		kill 9, $obj->{'pid'};		
		return "\n$Web::Terminal::Settings::quit_message\n";
	}	

    my $pugs=$obj->{'pugs'};
    $pugs->errmode(sub {kill 9,$obj->{'pid'};});

	$pugs->print($cmd) unless $cmd eq '';
	while ($i<$Web::Terminal::Settings::nlines) {
		my $line = $pugs->getline;
	        my $msg=$pugs->errmsg;
		    print "L:{",$line,"}\n",$msg if $v;
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
# print "\nTEST",$Web::Terminal::Settings::prompt_pattern,"\n"; 
#print ('(^(pugs|\.\.\.\.)>\s+)' eq $Web::Terminal::Settings::prompt_pattern);
#if ($line=~/pugs/) {
#print "OK!\n";
#}
        	if ( ($line =~ /$Web::Terminal::Settings::prompt_pattern/ or
	        ($line=~/$Web::Terminal::Settings::quit_pattern/)) and $i > 1 ) { 
			$ps = $1;
		       my $ps1=$ps;	
			my $charnum=ord(chop $ps1);
			if ($charnum<32) {
				$ps=$ps1;
			}
			last;
	       	}
		$lline .= $line unless $line =~
        	/$Web::Terminal::Settings::prompt_pattern/;
		$i++;
	}

   if ($i>=$Web::Terminal::Settings::nlines-1) {
       $obj->{pugs}->close();
        kill 9, $obj->{'pid'};
        $lline.="Generated output is limited to $Web::Terminal::Settings::nlines lines. Aborted.\n"; 
        $obj->{'error'}=1;
    }
} else {
	if ($cmd ne ':A') {
		$lline="Test flag is set -- write() called with $cmd.";
	} else {
		$obj->{'error'}=1;
		$lline="Test flag is set -- write() called with $cmd! Aborted.";
	}
	}
    chomp $ps; # a hack!
	$obj->{prompt}=$ps;
	return $lline;
} # END of write() method
#------------------------------------------------------------------------------
# Could be considered private;
# Only used by the constructor
# The tacit assumption is that the MOTD is never too long
sub readlines {
	my $obj = shift;
	my $ps = '';
#	my $i     = 1;
	my $lline = '';
	if ( $Web::Terminal::Settings::test!=1) {
    my $pugs=$obj->{'pugs'};
    $pugs->errmode(sub {kill 9,$obj->{'pid'}; });
#        my $prev=0;
#	while ($i<$Web::Terminal::Settings::nlines) {
#    my $char='';
    my $line='';
#    my $j=0;

#    while ($char ne "\n" and ($j<$Web::Terminal::Settings::nchars)) {
#    #$char=$pugs->get();#
#    $char=getc $obj->{'pty'};
#    #if ($prev>0) {
#    #print ord($char),"\t:char\n" if $v;
#    #}
#    #if (ord($char)!=27) {    	    	
#    $j++;
#    last if $char eq '';
#    $line.=$char;
#   # print "{$line}\n";
#    last if $line =~/${Web::Terminal::Settings::prompt}$/;
#    #} else {die;$prev=1;}
#    }
# print "TEST",$Web::Terminal::Settings::prompt,"\n"; 
	($line, $ps)=$pugs->waitfor(String=>$Web::Terminal::Settings::prompt);
	  $pugs->buffer_empty(); # maybe redundant, but play safe   
#	my @chars=split('',$line);
#	for my $char(@chars) {
#		print ord($char),"\n";
#	}
	  
	#$line=~s/....$//;
       #print "L:<",$line,">" if $v;
        #print "P:<",$ps,">" if $v;
     #   if ($line=~/Prelude/i){$prev++};
     #die if (($line=~/${Web::Terminal::Settings::prompt}/) and ($prev>1));
    
#        if ($j>=$Web::Terminal::Settings::nchars-1) {
#        $line.="Generated output is limited to $Web::Terminal::Settings::nchars characters. Aborted.\n";
#       $obj->{pugs}->close();
#        kill 9, $obj->{'pid'};
#        $obj->{'error'}=1;
#        $lline .= $line;
#        last;
#        }
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
#        if ( ($line =~ /$Web::Terminal::Settings::prompt_pattern/ or
#        ($line=~/$Web::Terminal::Settings::quit_pattern/)) and $i > 1 ) { $ps = $1; last }
		$lline .= $line unless $line =~
        /$Web::Terminal::Settings::prompt_pattern/;
#		$i++;
        last if $line eq '';
#	}
#   if ($i>=$Web::Terminal::Settings::nlines-1) {
#       $obj->{pugs}->close();
#        kill 9, $obj->{'pid'};
#        $lline.="Generated output is limited to $Web::Terminal::Settings::nlines lines. Aborted.\n";
#        $obj->{'error'}=1;
#    }
    
	} else {
		$lline="Test flag is set -- readlines() called.";
	}
    chomp $ps; # a hack!
	$obj->{prompt}=$ps;
	return $lline;
} # END of readlines() method

#------------------------------------------------------------------------------
# Borrowed from Net::Telnet docs and adapted for use in server environment 
sub spawn {
	my (@cmd) = @_;
	my ( $pid, $pty, $tty, $tty_fd );
    my $error=0;
	## Create a new pseudo terminal.
	use IO::Pty ();
	$pty = new IO::Pty
	  or do {
	  	die $!;
          return ( -1, 0 );
      };
      #
    #binmode $pty, ":utf8"; 
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
        #binmode $tty, ":utf8";
		$tty_fd = $tty->fileno;
		close $pty;

		## Make stdio use the new controlling terminal.
		open STDIN,  "<&$tty_fd" or ($error=1);#die $!;
		open STDOUT, ">&$tty_fd" or ($error=1);#die $!;
		open STDERR, ">&STDOUT"  or ($error=1);#die $!;
        #binmode STDIN, ":utf8";
        #binmode STDOUT, ":utf8";
        #binmode STDERR, ":utf8";
		close $tty;

		## Execute requested program.
		exec @cmd
			or die "problem executing $cmd[0]\n";
#		  or  ($error=1);
          }
	}    # end child process

    if($error==1) {
        $pty=-1;
        $pid=0;
    }

	return ( $pty, $pid );
}    # end sub spawn

1;
__END__

=head1 NAME

Web::Terminal::Server::Session -- Session object to encapsulate terminal
application.
Requires Net::Telnet, Moose.

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
The object's constructor takes following arguments:
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

Copyright (c) 2006,2007 Wim Vanderbauwhede. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
