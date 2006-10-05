use v6-alpha;

use Test;

plan 5;

# L<S02/Literals/filehandle "named as" %=POD{'DATA'}>
{
    ok eval(q{

=begin DATA

hello, world!

=end DATA

        %=POD{'DATA'};
    }), '=begin DATA works and %=POD<DATA> defined', :todo<feature>;

    is eval(q{
        my $line = =%=POD<DATA>;
    }), "hello, world!\n", q/%=POD{'DATA'} can be read/, :todo<feature>;
}

# L<S02/Literals/"pod stream" "as a scalar" via $=DATA>
{
    is eval('$=DATA'), "hello, world!\n",
        '$=DATA contains the right string', :todo<feature>;
}

# L<S02/Literals/"pod stream" "as an array" via @=DATA>
{
    is eval('@=DATA.elems'), 1, '@=DATA contains a single elem', :todo<feature>;
    is eval('@=DATA[0]'), "hello, world!\n",
        '@=DATA[0] contains the right value', :todo<feature>;
}

# The following commented-out tests are currnetly unspecified:
# others will be added later, or you can do it.

#ok eval('
#=begin DATA LABEL1
#LABEL1.1
#LABEL1.2
#LABEL1.3
#=end DATA

#=begin DATA LABEL2
#LABEL2.1
#LABEL2.2
#=end DATA
#'), "=begin DATA works", :todo;

#is(eval('%=DATA<LABEL1>[0]'), 'LABEL1.1', '@=DATA<LABEL1>[0] is correct', :todo);
#is(eval('%=DATA<LABEL1>[2]'), 'LABEL1.3', '@=DATA<LABEL1>[2] is correct', :todo);
#is(eval('~ %=DATA<LABEL1>'), 'LABEL1.1LABEL1.2LABEL1.3', '~ %=DATA<LABEL1> is correct', :todo);

#is(eval('~ $=LABEL2'), 'LABEL2.1LABEL2.2', '~ $=LABEL2 is correct', :todo);
#is(eval('$=LABEL2[1]'), 'LABEL2.2', '$=LABEL2[1] is correct', :todo);
