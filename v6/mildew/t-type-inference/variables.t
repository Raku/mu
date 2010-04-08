use Test;
INFERRED-TYPE-TEST(1,'$type->isa("Type::IntegerConstant")');
my $foo = 1;
INFERRED-TYPE-TEST($foo,'$type->isa("Type::Lexical")');
INFERRED-TYPE-TEST($foo.FETCH,'$type->isa("Type::IntegerConstant")');
my $bar = $foo;
INFERRED-TYPE-TEST($bar.FETCH,'$type->isa("Type::IntegerConstant")');
done_testing;

