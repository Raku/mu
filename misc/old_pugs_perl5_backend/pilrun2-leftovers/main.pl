
require Perl6::Run::OnPerl5::X1::PilUtil;
require Perl6::Run::OnPerl5::X1::Api;
require Perl6::Run::OnPerl5::X1::Compile;
require Perl6::Run::OnPerl5::X1::Prelude;

sub initialize {
    Perl6::Run::OnPerl5::X1::Api::p6_initialize_package("",'package');
    Perl6::Run::OnPerl5::X1::Api::p6_initialize_package("main",'package');
    Perl6::Run::OnPerl5::X1::Prelude::initialize();
#    END { p6_apply(p6_var('&*END')); }
}
initialize();

1;
__END__
