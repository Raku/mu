package Web::Terminal::Settings;

# Various settings for Web::Terminal

use vars qw( $VERSION );
$VERSION = '0.2.0';
use utf8;
use strict;

use Exporter;

our @ISA = qw(Exporter);
our @EXPORT = qw(
command
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
);

our $command='/usr/bin/pugs';

our $prompt='pugs> ';
our $init_pattern='(\>\s+)';
our $prompt_pattern='(^(pugs|\.\.\.\.)>\s+)';
our $quit_command=':q';
our $quit_pattern='^Leaving\ pugs\.';
our $quit_message='Leaving pugs.';

our $filter=0;
our $filter_pattern='';

our $port=2057;
our $host='localhost';

our $nsessions=50;
our $nsessions_ip=10;

our $timeout_idle=600;
our $timeout_call=10;
our $check_interval=300;
