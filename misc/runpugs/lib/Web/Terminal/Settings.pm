package Web::Terminal::Settings;

# Various settings for Web::Terminal

use vars qw( $VERSION );
$VERSION = '0.2.0';
use utf8;
use strict;

use Exporter;

our @ISA = qw(Exporter);
our @EXPORT = qw(
commands
prompt
init_pattern
prompt_pattern
quit_command
quit_pattern
quit_message
filter
filter_pattern
port
host
nsessions
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
nrecent
);

=ghci
#GHCi
our $command='/usr/local/bin/ghci';
our $prompt='Prelude> ';
our $prompt_pattern='(^(Prelude)>\s+)';
our $quit_pattern='^Leaving\ GHCi\.';
our $quit_message='Leaving GHCi.';
=cut

#Pugs
#Rel
our @commands=(
'/usr/bin/nice /home/andara/pugs-rel/pugs -I/home/andara/pugs-rel/blib6/lib',
'/usr/bin/nice /usr/bin/pugs'
);
#Dev
#our $command='/usr/bin/nice /usr/bin/pugs';
our $prompt='pugs> ';
our $prompt_pattern='(^(pugs|\.\.\.\.)>\s+)';
our $quit_pattern='^Leaving\ pugs\.';
our $quit_message='Leaving pugs.';

our $init_pattern='(\>\s+)';
our $quit_command=':q';
our $server='termserv2.pl';

our $filter=0;
our $filter_pattern='';
our $cgi_path='/home/andara/apache/cgi-bin/';
our $data_path='/home/andara/apache/data/';
our $tmp_path='/home/andara/apache/data/tmp/';
our $daemon=0;
our $port=2057;
our $host='localhost';

our $nsessions=50;
our $nsessions_ip=10;

our $timeout_idle=600;
our $timeout_call=10;
our $check_interval=300;
our $nlines=250;
our $nchars=250;
our $nrecent=10;
our $perl='/usr/bin/perl';

