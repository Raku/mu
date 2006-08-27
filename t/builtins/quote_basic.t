use v6-alpha;

use Test;

plan 24;

# L<S06/"Generalized quotes may now take adverb:" /for user-defined quotes/>

# q:x
{
    is q:x/echo hello/, "hello\n", "Testing for q:x operator.";
}

# utf8
{
    # 一 means "One" in Chinese.
    is q:x/echo 一/, "一\n", "Testing for q:x operator. (utf8)";
}

# q:w
{
    my @a = ('1st', '2nd', '3rd');
    is q:w /1st 2nd 3rd/, @a, "Testing for q:w operator";
    is qw /1st 2nd 3rd/, @a, "Testing for qw operator";

    my @a_utf8 = ('一', '二', '三');
    is q:w /一 二 三/, @a_utf8, "Testing for q:w operator. (utf8)";
    is qw /一 二 三/, @a_utf8, "Testing for qw operator. (utf8)";
}

# q:ww
{
    # XXX
    # quote protection needs to be tested.
    my @a = ('1st', '2nd', '3rd');
    is q:ww /1st 2nd 3rd/, @a, "Testing for q:ww operator";

    my @a_utf8 = ('一', '二', '三');
    is q:ww /一 二 三/, @a_utf8, "Testing for q:ww operator. (utf8)";
}

# q:t
{
# XXX
# Pugs has problem for parsing heredoc stream.
# The one works is:
# my $t = q:t /STREAM/
# Hello, world
# STREAM;
# But this one doesn't conform to the Synopsis...

    my $t;
    eval_ok q{$t = q:t /STREAM/;
Hello, world
STREAM
    }, :todo<parsefail>;

    is $t, "Hello, World\n", "Testing for q:t operator.";

    eval_ok qq{
$t = q:t /结束/;
Hello, World
结束
    }, :todo<parsefail>;

    is $t, "Hello, World\n", "Testing for q:t operator. (utf8)";
}

# q:n
{
    my $s1 = "hello";
    my $t1 = q:n /$s1, world/;
    is $t1, '$s1, world', "Testing for q:n operator.";

    my $s2 = "你好";
    my $t2 = q:n /$s2, 世界/;
    is $t2, '$s2, 世界', "Testing for q:n operator. (utf8)";
}

# q:q
{
    my $s1 = "//";
    my $t1 = q:q /\/\//;

    is $t1, $s1, "Testing for q:q operator.";
}

# q:qq 
{
    # Skiped, Since this can work with :s :a :h :f :c :b
    # If all of the above works, then, we'll likely have another more complex one.
}

# q:s
{
    my $s = "someone is laughing";
    my $t = q:s /$s/;
    is $t, $s, "Testing for q:s operator.";

    my $s = "有人在笑";
    my $t = q:s /$s/;
    is $t, $s, "Testing for q:s operator. (utf8)";
}

# q:a
{
    my @t = qw/a b c/;
    my $s = q:a /@t[]/;
    is $s, ~@t, "Testing for q:a operator.";
}

# q:h
{
    # Pugs can't parse q:h currently.
    my %t = (a => "perl", b => "rocks");
    my $s;
    eval_ok "$s = q;h /%t<>/", :todo<parsefail>;
    is $s, ~%t, "Testing for q:h operator.";
}

# q:f
{
    sub f { "hello" };
    my $t = q:f /&f(), world/;
    is $t, f() ~ ", world", "Testing for q:f operator.";

    sub f_utf8 { "你好" };
    $t = q:f /&f_utf8(), 世界/;
    is $t, f_utf8() ~ ", 世界", "Testing for q:f operator. (utf8)";
}

# q:c
{
    sub f { "hello" };
    my $t = q:c /{f}, world/;
    is $t, f() ~ ", world", "Testing for q:c operator.";
}

# q:b
{
    my $t = q:b /\n\n\n/;
    is $t, "\n\n\n", "Testing for q:b operator.";
}
