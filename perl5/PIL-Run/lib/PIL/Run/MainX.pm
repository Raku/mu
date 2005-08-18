
package PIL::Run::Main;

require PIL::Run::Type::Object;
require Perl6::Value;
require Perl6::Container::Scalar;
require Perl6::Container::Array;
require Perl6::Container::Hash;
require PIL::Run::Type::Sub;

local $SIG{__WARN__} = sub {};
require PIL::Run::PrimP5;

1;
__END__
