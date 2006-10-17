package Web::Terminal::Config;

our $prompt='pugs> ';
our $prompt_pattern='^(pugs|\.\.\.\.)> ';
our $quit_pattern='^Leaving pugs.';

our $port=2057;
our $host='127.0.0.1';

our $nsessions=50;
our $nsessions_ip=10;

our $timeout_idle=600;
our $timeout_call=10;
our $check_interval=300;
