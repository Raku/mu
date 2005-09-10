
package PIL::Run::Main;

require Error;
require Perl6::Value;

#require Math::BigInt;
sub Math::BigInt::binf { Perl6::Value::Num::Inf() };
sub Math::BigInt::bnan { Perl6::Value::Num::NaN() };

require Perl6::Container::Scalar;
require Perl6::Container::Array;
require Perl6::Container::Hash;
require Perl6::Code;
require PIL::Run::Type::Object;
require PIL::Run::Type::Macro;
require PIL::Run::Match;

local $SIG{__WARN__} = sub {};
require PIL::Run::PrimP5;

1;
__END__
