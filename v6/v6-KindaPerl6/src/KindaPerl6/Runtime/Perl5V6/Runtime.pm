package KindaPerl6::Runtime::Perl5V6::Runtime;
use Exporter 'import';
our @EXPORT=qw(say);
sub say {
    print(@_,"\n");
}
