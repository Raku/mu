#!/usr/bin/pugs

use v6;
use Test;

=pod

 Hyper operators L<S03/"Hyper operators">

=cut

plan 39;

{ # binary infix
        my @r;
        eval '@r = (1, 2, 3) »+« (2, 4, 6)';
        my @e = (3, 6, 9);
        is(~@r, ~@e, "hyper-sum two arrays");

        eval '@r = (1, 2, 3) »-« (2, 4, 6)';
        @e = (-1, -2, -3);
        is(~@r, ~@e, "hyper-subtract two arrays");

        eval '@r = (1, 2, 3) >>+<< (2, 4, 6)';
        @e = (3, 6, 9);
        is(~@r, ~@e, "hyper-sum two arrays ASCII notation");

        eval '@r = (1, 2, 3) >>-<< (2, 4, 6)';
        @e = (-1, -2, -3);
        is(~@r, ~@e, "hyper-subtract two arrays");

        @r = (1, 2, 3) »*« (2, 4, 6);
        @e = (2, 8, 18);
        is(~@r, ~@e, "hyper-multiply two arrays");

        @r = (1, 2, 3) >>*<< (2, 4, 6);
        @e = (2, 8, 18);
        is(~@r, ~@e, "hyper-multiply two arrays ASCII notation");

        @r = (1, 2, 3) »x« (3, 2, 1);
        @e = ('111', '22', '3');
        is(~@r, ~@e, "hyper-x two arrays");

        @r = (1, 2, 3) >>x<< (3, 2, 1);
        @e = ('111', '22', '3');
        is(~@r, ~@e, "hyper-x two arrays ASCII notation");

        @r = (1, 2, 3) »xx« (3, 2, 1);
        @e = ((1,1,1), (2,2), (3));
        is(~@r, ~@e, "hyper-xx two arrays");

        @r = (1, 2, 3) >>xx<< (3, 2, 1);
        @e = ((1,1,1), (2,2), (3));
        is(~@r, ~@e, "hyper-xx two arrays ASCII notation");

        @r = (20, 40, 60) »/« (2, 5, 10);
        @e = (10, 8, 6);
        is(~@r, ~@e, "hyper-divide two arrays");

        @r = (20, 40, 60) >>/<< (2, 5, 10);
        @e = (10, 8, 6);
        is(~@r, ~@e, "hyper-divide two arrays ASCII notation");

        @r = (1, 2, 3) »+« (10, 20, 30) »*« (2, 3, 4);
        @e = (21, 62, 123);
        is(~@r, ~@e, "precedence - »+« vs »*«");

        @r = (1, 2, 3) >>+<< (10, 20, 30) >>*<< (2, 3, 4);
        @e = (21, 62, 123);
        is(~@r, ~@e, "precedence - >>+<< vs >>*<< ASCII notation");
};

{ # unary postfix
        my @r = (1, 2, 3);
        eval '@r »++';
        my @e = (2, 3, 4);
        is(~@r, ~@e, "hyper auto increment an array", :todo);

        @r = (1, 2, 3);
        eval '@r >>++';
        @e = (2, 3, 4);
        is(~@r, ~@e, "hyper auto increment an array ASCII notation", :todo);
};

{ # unary prefix
        my @r;
        eval '@r = -« (3, 2, 1)';
        my @e = (-3, -2, -1);
        is(~@r, ~@e, "hyper op on assignment/pipeline");

        eval '@r = -<< (3, 2, 1)';
        @e = (-3, -2, -1);
        is(~@r, ~@e, "hyper op on assignment/pipeline ASCII notation");
};

{ # dimension upgrade
        my @r;
        eval '@r = (1, 2, 3) »+« 1';
        my @e = (2, 3, 4);
        is(~@r, ~@e, "auto dimension upgrade on rhs");

        eval '@r = (1, 2, 3) >>+<< 1';
        @e = (2, 3, 4);
        is(~@r, ~@e, "auto dimension upgrade on rhs ASCII notation");

        @r = 2 »*« (10, 20, 30);
        @e = (20, 40, 60);
        is(~@r, ~@e, "auto dimension upgrade on lhs");

        @r = 2 >>*<< (10, 20, 30);
        @e = (20, 40, 60);
        is(~@r, ~@e, "auto dimension upgrade on lhs ASCII notation");

        @r = (1,2,3,4) »+« (1,2);
        @e = (2,4,3,4);
        is(~@r, ~@e, "list-level element extension on rhs");
        
        @r = (1,2,3,4) >>+<< (1,2);
        @e = (2,4,3,4);
        is(~@r, ~@e, "list-level element extension on rhs ASCII notation");
        
        @r = (1,2) »+« (1,2,3,4);
        @e = (2,4,3,4);
        is(~@r, ~@e, "list-level element extension on lhs");
        
        @r = (1,2) >>+<< (1,2,3,4);
        @e = (2,4,3,4);
        is(~@r, ~@e, "list-level element extension on lhs ASCII notation");
  
        @r = (1,2,3,4) »+« (1,);
        @e = (2,2,3,4);
        is(~@r, ~@e, "list-level element extension on rhs");
        
        @r = (1,2,3,4) >>+<< (1,);
        @e = (2,2,3,4);
        is(~@r, ~@e, "list-level element extension on rhs ASCII notation");
        
        @r = (1,) »+« (1,2,3,4);
        @e = (2,2,3,4);
        is(~@r, ~@e, "list-level element extension on lhs");
        
        @r = (1,) >>+<< (1,2,3,4);
        @e = (2,2,3,4);
        is(~@r, ~@e, "list-level element extension on lhs ASCII notation");
};

{ # unary postfix again, but with a twist
        my @r;
        eval '@r = ("f", "oo", "bar")».length';
        my @e = (1, 2, 3);
        is(~@r, ~@e, :todo);

        eval '@r = ("f", "oo", "bar")>>.length';
        @e = (1, 2, 3);
        is(~@r, ~@e, :todo);
};

{ # regression test, ensure that hyper works on arrays
        my @r1;
        my @r2;
        my @e1 = (2, 4, 6);
        my @e2 = (2, 3, 4);
        my @a = (1, 2, 3);
        @r1 = @a >>+<< @a;
        @r2 = @a >>+<< 1;
        is(~@r1, ~@e1, "hyper op works on variables, too.");
        is(~@r2, ~@e2, "hyper op and correctly promotes scalars");
};


{ # mixed hyper and reduce metaops
    is ~([+]<< ([1,2,3], [4,5,6])), "6 15", "mixed hyper and reduce metaop ([+]<<) works";

    # XXX: Test for [+]<<<<
    is ~([+]<<<< ([[1,2],[3,4]],[[5,6],[7,8]])), "3 7 11 15",
      "mixed double hyper and reduce metaop ([+]<<<<) works";

    is eval('~[+]« [1,2,3], [4,5,6]'), "6 15",
      "mixed Unicode hyper and reduce metaop ([+]«) works";
}

{ # hyper dereferencing
    my @array = (
        { key => 'val' },
        { key => 'val' },
        { key => 'val' }
    );

    my $full = join '', eval '@array>>.<key>';
    is($full, 'valvalval', 'hyper-dereference an array',:todo);

    my $part = join '', eval '@array[0,1]>>.<key>';
    is($part, 'valval', 'hyper-dereference an array slice',:todo);
}
