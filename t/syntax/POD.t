use v6-alpha;

use Test;

plan 5;

# L<S02/Literals/filehandle "named as" %=POD{'DATA'}>
{
    eval_ok q(

=begin DATA

hello, world!

=end DATA

        %=POD{'DATA'};
    ), '=begin DATA works and %=POD<DATA> defined';

    eval_is q{
        my $line = =%=POD<DATA>;
    }, "hello, world!\n", q/%=POD{'DATA'} can be read/;
}

# L<S02/Literals/"pod stream" "as a scalar" via $=DATA>
{
    eval_is '$=DATA', "hello, world!\n",
        '$=DATA contains the right string';
}

# L<S02/Literals/"pod stream" "as an array" via @=DATA>
{
    eval_is '@=DATA.elems', 1, '@=DATA contains a single elem';
    eval_is '@=DATA[0]', "hello, world!\n",
        '@=DATA[0] contains the right value';
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
