
package PIL::Run::Main;

require Perl6::Value;
require Perl6::Container::Scalar;
require Perl6::Container::Array;
require Perl6::Container::Hash;
require Perl6::Code;
require PIL::Run::Type::Object;
require PIL::Run::Type::Macro;

local $SIG{__WARN__} = sub {};
require PIL::Run::PrimP5;

1;
__END__
