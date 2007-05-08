package Web::Terminal::Settings;

# Various settings for Web::Terminal

use vars qw( $VERSION );
$VERSION = '0.4.0';
use utf8;
use strict;

use Exporter;

our @ISA = qw(Exporter);
our @EXPORT = qw(
test
appname
commands
prompt
init_pattern
prompt_pattern
quit_command
abort_command
quit_pattern
quit_message
reset_command
filter
filter_pattern
port
host
nsessions
npreloaded_sessions
nsessions_ip
timeout_idle
timeout_call
check_interval
create_interval
nlines
nchars
daemon
perl
server
lib_path
bin_path
cgi_path
data_path
tmp_path
log_path
nrecent
n_inactive_min
n_inactive_max
n_max
);

#ghci
##GHCi
#our $command='/usr/local/bin/ghci';
#our $prompt='Prelude> ';
#our $prompt_pattern='(^(Prelude)>\s+)';
#our $quit_pattern='^Leaving\ GHCi\.';
#our $quit_message='Leaving GHCi.';
#cut

#Pugs
my $ulimit='ulimit -m 64000; ulimit -v 64000;';
my $nice='/usr/bin/nice';
my $pugs='/usr/bin/pugs';
my $rel_root='/home/andara/pugs-rel';
my $dev_root='/home/andara/pugs-dev';
my $rel_lib="-I$rel_root/blib6/lib";
my $dev_lib="-I$dev_root/blib6/lib";

#$ulimit='';
#$nice='';#/bin/nice';
#$pugs='/usr/bin/pugs';
#$rel_root='/usr/bin';
#$dev_root='/usr/bin';
#$rel_lib='';
#$dev_lib='';

our @commands=(
"$ulimit $nice $rel_root/pugs $rel_lib",
"$ulimit $nice $dev_root/pugs $dev_lib",
#"perl ./perl_repl.pl"
);

our $test=0;
our $appname='runpugs';
our $prompt='pugs> ';
#$prompt='perl> ';
our $prompt_pattern='(^(pugs|\.\.\.\.)>\s+)';
our $quit_pattern='^Leaving\ pugs\.';
our $quit_message='Leaving pugs.';

our $init_pattern='(\>\s+)';
our $quit_command=':q';
our $reset_command=':r';
our $abort_command=':A'; # To simulate abort in test mode

our $filter=0;
our $filter_pattern='';
my $root='/home/andara/apache';

our $cgi_path="$root/cgi-bin/";
our $lib_path="$root/lib/";
our $bin_path="$root/bin/";
our $data_path="$root/data/";
our $tmp_path="$root/data/tmp/";
our $log_path="$root/data/log";
our $daemon=1;
our $port=2057;
our $host='localhost';

our $restart_parent=0; # restart parent server from child if 1
our $server="$bin_path/termserv.pl"; # used to restart parent server from child

our $nsessions=50; # obsolete
our @n_max=(20,50);#,5);
our $nsessions_ip=10; 
our @npreloaded_sessions=(5,10);#,2); 
our @n_inactive_max=@npreloaded_sessions;
our @n_inactive_min=(2,5);#,0);

our $timeout_idle=90; # was 600
our $timeout_call=30; #  was 30
our $check_interval=60; 
our $create_interval=5; # should be 30 I guess
our $nlines=250;
our $nchars=250;
our $nrecent=10;
our $perl='/usr/bin/perl';

__END__

=head1 NAME

Web::Terminal::Settings -- Configuration variables for Web::Terminal

=head1 SYNOPSIS

    use Web::Terminal::Settings;

=head1 DESCRIPTION

This is the configuration used to run Pugs:

=head2 PATHS AND NAMES

    our $appname='runpugs';
    our $server='termserv.pl';
    our $cgi_path='/home/andara/apache/cgi-bin/';
    our $data_path='/home/andara/apache/data/';
    our $tmp_path='/home/andara/apache/data/tmp/';

=head2 TERMINAL CONFIGURATION

    our @commands=(
'/usr/bin/nice /home/andara/pugs-rel/pugs -I/home/andara/pugs-rel/blib6/lib',
'/usr/bin/nice /usr/bin/pugs'
);
    our $prompt='pugs> ';
    our $prompt_pattern='(^(pugs|\.\.\.\.)>\s+)';
    our $quit_pattern='^Leaving\ pugs\.';
    our $quit_message='Leaving pugs.';
    our $init_pattern='(\>\s+)';
    our $quit_command=':q';

=head2 NETWORKING SETTINGS

    our $daemon=1;
    our $port=2057;
    our $host='localhost';

=head2 SESSION CONTROL

    our $filter=0;
    our $filter_pattern='';
    our $nsessions=50;
    our $nsessions_ip=10;
    our $timeout_idle=600;
    our $timeout_call=10;
    our $check_interval=300;
    our $nlines=250;

=head1 SEE ALSO

L<Web::Terminal::Dispatcher>,
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
