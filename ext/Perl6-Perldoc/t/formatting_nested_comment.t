my $perldoc_data = <<'END_PERLDOC';
B<I shall say this loudly
=begin comment
and repeatedly
=end comment
and with emphasis.>
C<I shall say this loudly
=begin comment
and repeatedly
=end comment
and with emphasis.>

END_PERLDOC

my $expected_structure = <<'END_EXPECTED';
errors: []

tree: !!perl/hash:Perl6::Perldoc::Document 
  content: 
    - !!perl/hash:Perl6::Perldoc::Block::pod 
      content: 
        - !!perl/hash:Perl6::Perldoc::Block::para 
          content: 
            - !!perl/hash:Perl6::Perldoc::FormattingCode::B 
              content: 
                - "I shall say this loudly\n"
                - !!perl/hash:Perl6::Perldoc::Block::comment 
                  content: 
                    - "and repeatedly\n"
                - and with emphasis.
            - "\n"
            - !!perl/hash:Perl6::Perldoc::FormattingCode::C 
              content: 
                - "I shall say this loudly\n"
                - !!perl/hash:Perl6::Perldoc::Block::comment 
                  content: 
                    - "and repeatedly\n"
                - and with emphasis.
      typename: pod
  typename: (document)
warnings: []


END_EXPECTED

use Perl6::Perldoc::Parser;
use Test::More 'no_plan';

sub is_subset {
    my ($found, $expected) = @_;
    my @found    = split /\n/, $found;
    my @expected = split /\n/, $expected;

    while (@found && @expected) {
        if ($found[0] eq $expected[0]) {
            is $found[0], $expected[0], $expected[0];
            shift @found;
            shift @expected;
        }
        else {
            shift @found;
        }
    }
    
    for my $expected (@expected) {
        ok 0, "Missing '$expected'";
    }
}

open my $fh, '<', \$perldoc_data
    or die "Could not open file on test data";

my $representation = Perl6::Perldoc::Parser->parse($fh ,{all_pod=>1});

use YAML::Syck 'Dump';
is_subset Dump($representation), $expected_structure;
