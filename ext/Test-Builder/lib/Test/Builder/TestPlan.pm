class Test::Builder::TestPlan
{
    has Int $.expect;

    submethod BUILD ( $expect = 0 )
    {
        fail "Invalid or missing plan" unless $expect;
        $.expect = $expect;
    }

    method header returns Str
    {
        return "1..$.expect";
    }

    method footer returns Str (Int $run)
    {
        return '' if $run == $.expect;
        return "Expected $self.expect but ran $run";
    }
}

class Test::Builder::NullPlan is Test::Builder::TestPlan
{
    method header returns Str
    {
        return '';
    }

    method footer returns Str (Int $run)
    {
        return "1..$run";
    }
}
