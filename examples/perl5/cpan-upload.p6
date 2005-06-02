#!/usr/bin/pugs
#
# cpan-upload - upload one or more file to CPAN (via PAUSE)
#
# $Id: cpan-upload,v 2.2 2002/07/02 21:44:10 neilb Exp $
#

use AppConfig::Std--perl5;
use Net::FTP--perl5;
use HTTP::Request::Common--perl5;
use LWP::UserAgent--perl5;
use HTTP::Status--perl5;
use File::Basename--perl5;

#-----------------------------------------------------------------------
#       Configuration constants and globals
#-----------------------------------------------------------------------
my $PROGRAM;
my $SITE          = 'pause.perl.org';
my $UPLOAD_DIR    = 'incoming';
my $PAUSE_ADD_URI = 'http://pause.perl.org/pause/authenquery';
my $config;
my @uploaded_files;
my &POST := eval_perl5 'sub { my $url = shift; &HTTP::Request::Common::POST($url, {@_}) }';
my &basename := eval_perl5 '\&File::Basename::basename';

#-----------------------------------------------------------------------
#       MAIN BODY
#-----------------------------------------------------------------------


my $VERSION = "Pugs $?PUGS_VERSION";

initialise();

@uploaded_files = ftp_upload_files(@ARGS);
pause_add_files(@uploaded_files) if @uploaded_files > 0;
_verbose(int(@ARGS), int(@ARGS) == 1 ?? " file " :: " files ",
         "uploaded successfully.\n");

exit 0;

#=======================================================================
#
# initialise()
#
# Create AppConfig instance, parse config file if there is one,
# and command-line options.
#
#=======================================================================
sub initialise () {
    my $config_file;
    my $HOME;
    my $password;


    ($PROGRAM = $*PROGRAM_NAME) ~~ s:P5{^.*/}{};

    #-------------------------------------------------------------------
    # Create an AppConfig::Std object, and define our interface
    # The EXPAND flag on password tells AppConfig not to try and
    # expand any embedded variables - eg if you have a $ sign
    # in your password.
    #-------------------------------------------------------------------
    $HOME = %ENV<HOME>;
    $config_file = "$HOME/.pause";
    $config = eval_perl5(q!
        my $config = AppConfig::Std->new();
        $config->define('user');
        $config->define('directory', {ARGCOUNT => 1, ALIAS => 'dir'});
        $config->define('password', { EXPAND   => 0 });
        $config->define('mailto');
        $config->define('ftp_gateway');
        $config->define('ftp_proxy');
        $config->define('http_proxy');
        $config->define('non_interactive', { ALIAS => 'ni', ARGCOUNT => 0 });
        $config;
    !);

    #-------------------------------------------------------------------
    # Read the user's config file, if they have one,
    # then parse the command-line.
    #-------------------------------------------------------------------
    if (-f $config_file)
    {
        $config.file($config_file) || exit 1;
    }
    $config.args(\@ARGS)
        || die "run \"$PROGRAM -help\" to see valid options\n";

    #-------------------------------------------------------------------
    # Check we have the information we need
    #-------------------------------------------------------------------

    die "No files specified for upload\n" unless @ARGS > 0;

    die "No email address (mailto) specified\n" unless $config.mailto;
    die "No PAUSE user specified\n"             unless $config.user;

    $config.verbose(1) if $config.debug && !$config.verbose;

    #-------------------------------------------------------------------
    # Display banner at the start of the run
    #-------------------------------------------------------------------
    _verbose("$PROGRAM v$VERSION\n");
}

#=======================================================================
#
# ftp_upload_files()
#
# upload the one or more files to PAUSE ftp server.
# return a list of the files that were successfully uploaded.
#
#=======================================================================
sub ftp_upload_files (*@files) {
    my @uploaded = ();            # list of files actually uploaded
    my $ftp;                      # Net::FTP instance
    my @new_args;                 # arg list to pass to constructor
    my ($user, $password);        # user and password for login method
    my $file;

    _verbose("Using FTP to upload files to PAUSE\n");

    #-------------------------------------------------------------------
    # Make the connection to the PAUSE ftp server:
    # First we determine how we're going to make the connection ...
    #-------------------------------------------------------------------
    if $config.ftp_gateway {
        _debug("  establishing connection via an FTP gateway\n");
        @new_args = ($config.ftp_gateway);
	($user, $password) = ("ftp\@$SITE", $config.mailto);
    }
    else {
        ($user, $password) = ('ftp', $config.mailto);
        @new_args = ($SITE);
	if $config.ftp_proxy {
	    _debug("  establishing connection via proxy",
                     $config.ftp_proxy, "\n");
            push(@new_args, 'Firewall' => $config.ftp_proxy);
	}
	else {
	    _debug("  establishing connection\n");
	}
    }

    #-------------------------------------------------------------------
    # ... and then we actually make the connection and log in
    #-------------------------------------------------------------------
    $ftp = Net::FTP.new(@new_args);
    if (!$ftp) {
        die "failed to connect to remote server: $!\n";
    }

    if (!$ftp.login($user, $password)) {
        $ftp.quit();
        die "    failed to login as user 'ftp', password $password - ",
            $ftp.message(), "[", $ftp.code(), "]\n";
    }

    #-------------------------------------------------------------------
    # Change to the right directory, and set binary mode
    #-------------------------------------------------------------------
    _debug("  changing to \"$UPLOAD_DIR\" directory...\n");
    if (!$ftp.cwd($UPLOAD_DIR))
    {
        $ftp.quit();
	die "failed to change directory to $UPLOAD_DIR!\n";
    }

    _debug("  setting binary mode.\n");
    unless $ftp.binary() {
        $ftp.quit();
        die "  failed to change type to 'binary' - ", $ftp.message(),
            "[", $ftp.code(), "]\n";
    }

    #-------------------------------------------------------------------
    # Put the file(s)
    #-------------------------------------------------------------------
    for @files -> $file {

        _verbose("  uploading file \"$file\"\n");
        if $ftp.put($file) {
	    push(@uploaded, $file);
	}
	else {
            warn "failed to upload $file - ", $ftp.message(), "\n";
	    if (@files > 0 and !$config.non_interactive) {
		my $continue;

		loop {
		    print "Do you want to continue? [y] ";
		    $continue = =$*IN;
		    $continue = 'y' if $continue ~~ m:P5/^$/;
		} while ($continue !~ m:P5<i>/^[yn]/);
		exit(0) if $continue ~~ m:P5<i>/^n/;
	    }
        }
    }

    #-------------------------------------------------------------------
    # Close the connection with the server.
    #-------------------------------------------------------------------
    _debug("  closing connection with FTP server\n");
    $ftp.quit;

    return @uploaded;
}

#=======================================================================
#
# pause_add_files()
#
# make an HTTP request to the add_uri form
#
#=======================================================================
sub pause_add_files (*@files) {
    my $file;
    my $basename;
    my $request;
    my $response;
    my $agent;
    my $argref;


    _verbose("registering upload with PAUSE web server\n");

    #-------------------------------------------------------------------
    # Create the agent we'll use to make the web requests
    #-------------------------------------------------------------------
    _debug("  creating instance of LWP::UserAgent\n");
    $agent = LWP::UserAgent.new() err die "Failed to create UserAgent: $!\n";
    $agent.agent("$PROGRAM/$VERSION");
    $agent.from($config.mailto);
    if (defined $config.http_proxy)
    {
        $agent.proxy(['http'], $config.http_proxy);
    }

    #-------------------------------------------------------------------
    # Post an upload message to the PAUSE web site for each file
    #-------------------------------------------------------------------
    for @files -> $file {
	$basename = basename($file);

        #---------------------------------------------------------------
        # Create the request to add the file
        #---------------------------------------------------------------
	$argref = [
                    'HIDDENNAME'                    , "$config.user()",
                    'pause99_add_uri_upload'        , "$basename",
                    'SUBMIT_pause99_add_uri_upload' , " Upload the checked file "
                   ];
	if ($config.directory)
	{
	    $argref.{'pause99_add_uri_subdirtext'} = $config.directory;
	}

        $request = POST($PAUSE_ADD_URI, @$argref);
        $request.authorization_basic("$config.user()", "$config.password()");

        _debug("----- REQUEST BEGIN -----\n",
               $request.as_string(),
               "----- REQUEST END -------\n");

        #---------------------------------------------------------------
        # Make the request to the PAUSE web server
        #---------------------------------------------------------------
        _verbose("  POSTing upload for $file\n");
        $response = $agent.request($request);

        #---------------------------------------------------------------
        # So, how'd we do?
        #---------------------------------------------------------------
        if (not defined $response)
        {
            die "Request completely failed - we got undef back: $!\n";
        }
        if ($response.is_error)
        {
            if ($response.code == 404)
            {
                die "PAUSE's CGI for handling messages seems to have moved!\n",
                    "(HTTP response code of 404 from the PAUSE web server)\n",
                        "It used to be:\n\n\t", $PAUSE_ADD_URI, "\n\n",
                            "Please inform the maintainer of this script\n";
            }
            else
            {
                die "request failed\n  Error code: ", $response.code,
                    "\n  Message: ", $response.message, "\n";
            }
        }
        else
        {
            _debug("Looks OK!\n",
                   "----- RESPONSE BEGIN -----\n",
                   $response.as_string(),
                   "----- RESPONSE END -------\n");
            _verbose("    PAUSE add message sent ok [",
                     $response.code, "]\n");
        }
    }
}


#=======================================================================
#
# _verbose()
#
# displays the message strings passed if in verbose mode.
#
#=======================================================================
sub _verbose
{
    return unless $config.verbose;
    print join('', @_);
}


#=======================================================================
#
# _debug()
#
# displays the message strings passed if in debug mode.
#
#=======================================================================
sub _debug
{
    return unless $config.debug;
    print join('', @_);
}


__END__

#-----------------------------------------------------------------------
