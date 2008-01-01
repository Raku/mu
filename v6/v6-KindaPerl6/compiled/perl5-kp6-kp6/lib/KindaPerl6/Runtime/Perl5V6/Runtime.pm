package KindaPerl6::Runtime::Perl5V6::Runtime;
use Exporter 'import';
our @EXPORT=qw($Code_say);
our $Code_say = sub {
    print(@_,"\n");
}
