use Test::More 'no_plan'; 
use Perl6::Perldoc::Parser;

# Bad filename...
{
    my $filename = '^#@*';

    ok !defined eval { Perl6::Perldoc::Parser->parse($filename) }
        => 'Failed to find file';

    like $@, qr/\Qparse() can't open file $filename\E/
        => "....error message was correct";
}

# Bad =use URI...
{
    my $uri = 'http://dev.null';

    open my $fh, '<', \"=use $uri\n";

    my $errors_ref =  Perl6::Perldoc::Parser->parse($fh)->{errors};
    ok @{$errors_ref} == 1
        => 'Unacceptable URI';

    like $errors_ref->[0], qr/\QUnable to open URI in '=use $uri'\E/
        => "....error message was correct";
}


# Bad M<> code (no scheme)...
{
    open my $fh, '<', \"=para M<no scheme>\n";

    my $errors_ref =  Perl6::Perldoc::Parser->parse($fh)->{errors};
    ok @{$errors_ref} == 1
            => 'Schemeless M<>';

    like $errors_ref->[0],
         qr/\QMissing scheme specifier in M<> formatting code\E/
            => "....error message was correct";
}


# Orphaned item...
{
    open my $fh, '<', \"=item2 Missing parent item\n";

    my $warnings_ref =  Perl6::Perldoc::Parser->parse($fh)->{warnings};
    ok @{$warnings_ref} == 1
            => 'Orphaned item';

    like $warnings_ref->[0],
         qr/\QNo =item1 before =item2\E/
            => "....warning message was correct";
}


# Useless use of =end END
{
    open my $fh, '<', \"=begin END\n=end END\n";

    my $warnings_ref =  Perl6::Perldoc::Parser->parse($fh)->{warnings};
    ok @{$warnings_ref} == 1
            => 'Useless =end END';

    like $warnings_ref->[0],
         qr/\QIgnored explicit '=end END'\E/
            => "....warning message was correct";
}


# Unexpected =end...
{
    open my $fh, '<', \"=begin para\n=end code\n=end para\n";

    my $errors_ref =  Perl6::Perldoc::Parser->parse($fh)->{errors};
    ok @{$errors_ref} == 1
            => 'Unexpected =end';

    like $errors_ref->[0],
         qr/\QInvalid '=end code' (not in 'code' block)\E/
            => "....error message was correct";
}


# Late option...
{
    open my $fh, '<', \"=for para\ntext here\n= :numbered\n";

    my $warnings_ref =  Perl6::Perldoc::Parser->parse($fh)->{warnings};
    ok @{$warnings_ref} == 1
            => 'Late option';

    like $warnings_ref->[0],
         qr/\QPossible attempt to specify extra options too late in 'para' block\E/
            => "....warning message was correct";
}


# Unknown reserved type...
{
    open my $fh, '<', \"=foo bar\n";

    my $errors_ref =  Perl6::Perldoc::Parser->parse($fh)->{errors};
    ok @{$errors_ref} == 1
            => 'Unknown reserved type';

    like $errors_ref->[0],
         qr/\QUnknown reserved block type ('foo')\E/
            => "....error message was correct";
}


# Trailing junk...
{
    open my $fh, '<', \"=for para :numbered and sorted\ntext\n";

    my $errors_ref =  Perl6::Perldoc::Parser->parse($fh)->{errors};
    ok @{$errors_ref} == 1
            => 'Trailing junk';

    like $errors_ref->[0],
         qr/\QTrailing junk after 'para' block specifier\E.*\Q: and sorted\E/
            => "....error message was correct";
}


# No closing delimiter...
{
    open my $fh, '<', \"=begin para\ntext\n";

    my $errors_ref =  Perl6::Perldoc::Parser->parse($fh)->{errors};
    ok @{$errors_ref} == 1
            => 'No closing delimiter';

    like $errors_ref->[0],
         qr/\QNo closing delimiter for 'para' block\E/
            => "....error message was correct";
}


# Multivalued accessor called in scalar context...
{
    open my $fh, '<', \"=para\ntext\n=para\ntext\n";

    my $tree = Perl6::Perldoc::Parser->parse($fh)->{tree};

    my $warning;
    local $SIG{__WARN__} = sub {
        $warning = join q{}, @_;
    };

    ok defined scalar $tree->content,
            => 'Multivalued accessor called in scalar context';

    like $warning,
         qr/\QMultivalued accessor content() called in scalar context\E/
            => "....warning message was correct";
}
