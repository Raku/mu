use strict;

use lib qw(/home/andara/apache/lib/);
$ENV{MOD_PERL} or die "not running under mod_perl!";

use Apache::Registry ( );

use Carp ( );
$SIG{__WARN__} = \&Carp::cluck;

use CGI ( );
CGI->compile(':all');

use Web::Terminal::Settings ( );
#use Web::Terminal::Dispatcher ( );
use Web::Terminal::Dispatcher3 ( );
#use Web::Terminal::Msg ( );
#use YAML::Syck ( );

use HTML::Entities ( );


1;
