use v6-alpha;

use Test;

plan 8;

{
    # L<S02/Literals/filehandle "named as" %=POD{'DATA'}>
    my $fh;
    eval_ok q(

=begin DATA

hello, world!

=end DATA

        $fh = $=POD{'DATA'};
    ), '=begin DATA works';

    ok $fh, q/$=POD{'DATA'} defined/;
    my $line = =$fh if $fh;
    is $line, "hello, world!\n", q/$=POD{'DATA'} can be read/;

    # L<S02/"pod stream" "as a scalar" via $=DATA>
    my $str;
    eval_ok '$str = $=DATA', '$=DATA is parsed';
    is $str, "hello, world!\n", '$=DATA contains the right string';

    # L<S02/"pod stream" "as an array" via @=DATA>
    $str = undef;
    my ($count);
    eval_ok '$str = @=DATA[0]; $count = @=DATA[0].elems';
    is $count, 1, '@=DATA contains a single elem';
    is $str, "hello, world!\n", '@=DATA[0] contains the right value';
}

# The following commented-out tests are currnetly unspecified:
# others will be added later, or you can do it.

#eval_ok '
#=begin DATA LABEL1
#LABEL1.1
#LABEL1.2
#LABEL1.3
#=end DATA

#=begin DATA LABEL2
#LABEL2.1
#LABEL2.2
#=end DATA
#', "=begin DATA works", :todo;

#eval_is('%=DATA<LABEL1>[0]', 'LABEL1.1', '@=DATA<LABEL1>[0] is correct', :todo);
#eval_is('%=DATA<LABEL1>[2]', 'LABEL1.3', '@=DATA<LABEL1>[2] is correct', :todo);
#eval_is('~ %=DATA<LABEL1>', 'LABEL1.1LABEL1.2LABEL1.3', '~ %=DATA<LABEL1> is correct', :todo);

#eval_is('~ $=LABEL2', 'LABEL2.1LABEL2.2', '~ $=LABEL2 is correct', :todo);
#eval_is('$=LABEL2[1]', 'LABEL2.2', '$=LABEL2[1] is correct', :todo);
