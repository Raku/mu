class Test::Builder:Test
{
    method new(
        $number,     
        $passed       = 1,
        ?$skip        = 0,
        ?$todo        = 0,
        ?$reason      = '', 
        ?$description = '',
    )
    {
        return Test::Builder::Test::TODO.new(
            description => $description, reason => $reason, passed => $passed
        ) if $todo;

        return Test::Builder::Test::Skip.new(
            description => $description, reason => $reason, passed => 1
        ) if $skip;

        return Test::Builder::Test::Pass.new(
            description => $description, passed => 1
        ) if $passed;

        return Test::Builder::Test::Fail.new(
            description => $description, passed => 0
        );
    }
}

role Test::Builder::Test::Base {
    has Bool $.passed;
    has Int  $.number;
    has Str  $.diagnostic;
    has Str  $.description;

    submethod BUILD (
        $.description,
        $.passed,
        ?$.number     =     0,
        ?$.diagnostic = '???',
    ) {}

    method status returns Hash
    {
        return
        {
            passed      => $.passed,
            description => $.description,
        };
    }

    method report returns Str
    {
        my $ok          = $.passed ?? 'ok ' :: 'not ok ';
        my $description = " - $.description";
        return join( ' ', $ok, $.number, $description );
    }

}

class Test::Builder::Test::Pass does Test::Builder::Test::Base {}
class Test::Builder::Test::Fail does Test::Builder::Test::Base {}

role Test::Builder::Test::WithReason does Test::Builder::Test::Base
{
    has Str $.reason;

    submethod BUILD ( $.reason ) {}

    method status returns Hash
    {
        my $status        = $self.*WALK[:super];
        $status{"skip"}   = 1;
        $status{"reason"} = $.reason;
        return $status;
    }
}

class Test::Builder::Test::Skip does Test::Builder::Test::WithReason
{
    method report returns Str
    {
        return "not ok $.number #skip $.reason";
    }

    method status returns Hash
    {
        my $status      = $self.*WALK[:super];
        $status{"skip"} = 1;
        return $status;
    }

}

class Test::Builder::Test::TODO does Test::Builder::Test::WithReason
{
    method report returns Str
    {
        my $ok          = $.really_passed ?? 'ok ' :: 'not ok ';
        my $description = " # TODO $.description";
        return join( ' ', $ok, $.number, $description );
    }

    method status returns Hash
    {
        my $status               = $self.*WALK[:super];
        $status{"TODO"}          = 1;
        $status{"passed"}        = 1;
        $status{"really_passed"} = $.passed;
        return $status;
    }
}
