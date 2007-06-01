package Web::Terminal::Settings;

# Various settings for Web::Terminal

use vars qw( $VERSION );
$VERSION = '0.4.0';
#use utf8
use strict;

use Config::General;

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

#my %config = Config::General::ParseConfig("./wtrc");
 my $conf = new Config::General(
         -ConfigFile => $ENV{'HOME'}.'/.webtermrc',
         -InterPolateVars=>1,
         -InterPolateEnv=>1
 		);
my %config=$conf->getall();

no strict 'refs';

for my $key (keys %config) {
	my $fkey="Web::Terminal::Settings::$key";
	if ($config{$key}=~/^\(\d+(,\d+)*\)/) {
		my $vals=$config{$key};
		$vals=~s/[\(\)]//g;
		@{$fkey}=split(/,/,$vals);
	} else {
		${$fkey}=$config{$key};
	}
}

our @commands=(
"$Web::Terminal::Settings::ulimit $Web::Terminal::Settings::nice $Web::Terminal::Settings::rel_root/pugs $Web::Terminal::Settings::rel_lib",
"$Web::Terminal::Settings::ulimit $Web::Terminal::Settings::nice $Web::Terminal::Settings::dev_root/pugs $Web::Terminal::Settings::dev_lib",
);

if ($Web::Terminal::Settings::test==2) {
	push @commands,"perl ./perl_repl.pl";
	push @Web::Terminal::Settings::n_max,1;
	push @Web::Terminal::Settings::npreloaded_sessions,1;
	push @Web::Terminal::Settings::n_inactive_min,1;
}

our @n_inactive_max=@Web::Terminal::Settings::npreloaded_sessions;

1;

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
