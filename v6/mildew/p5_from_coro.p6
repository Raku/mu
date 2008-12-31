my $p5 = ::P5Interpreter.new();
$p5.eval('use lib "../smop/SMOP/blib/arch/","../smop/SMOP/blib/lib/"');
$p5.eval('use SMOP;');
$p5.eval('print $Coro::main,"\n";');
$OUT.print("in smop land\n");
knowhow Foo {
    method foo {
        $OUT.print(">   in smop callback # foo\n");
    }
    method bar {
        $OUT.print(">   in smop callback # bar\n");
    }
}
$p5.coro_from_string('
use strict;
my $new;
$SMOP::current_coro = Coro::State->new(sub {
    print ">    in p5 land\n";
    my $coro = $SMOP::main_coro;
    $::smop_object->foo;
    $::smop_object->foo;
    $::smop_object->bar;
    $::smop_object->foo;
    print ">    back in p5 land\n";
    $SMOP::current_coro->transfer($coro);
    print ">    and back yet again in p5 land\n";
});
',::Foo.FETCH);
$OUT.print(">    back in smop land\n");
