package Web::Terminal::Settings;

# Various settings for Web::Terminal

use vars qw( $VERSION );
$VERSION = '0.2.0';
use utf8;
use strict;

use Exporter;

our @ISA = qw(Exporter);
our @EXPORT = qw(
appname
commands
prompt
init_pattern
prompt_pattern
quit_command
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
nlines
nchars
daemon
perl
server
cgi_path
data_path
tmp_path
log_path
nrecent
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
#Rel
our @commands=(
'/usr/bin/nice /home/andara/pugs-rel/pugs -I/home/andara/pugs-rel/blib6/lib',
'/usr/bin/nice /usr/bin/pugs'
);
#Dev
#our $command='/usr/bin/nice /usr/bin/pugs';
our $appname='runpugs2';
our $prompt='pugs> ';
our $prompt_pattern='(^(pugs|\.\.\.\.)>\s+)';
our $quit_pattern='^Leaving\ pugs\.';
our $quit_message='Leaving pugs.';

our $init_pattern='(\>\s+)';
our $quit_command=':q';
our $reset_command=':r';
our $server='termserv2.pl';

our $filter=0;
our $filter_pattern='';
our $cgi_path='/home/andara/apache/cgi-bin/';
our $data_path='/home/andara/apache/data/';
our $tmp_path='/home/andara/apache/data/tmp/';
our $log_path='/home/andara/apache/data/log/';
our $daemon=1;
our $port=2057;
our $host='localhost';

our $nsessions=50;
our $nsessions_ip=10;
our @npreloaded_sessions=(2,5);

our $timeout_idle=600; # was 600
our $timeout_call=30; #  was 10
our $check_interval=60;
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
