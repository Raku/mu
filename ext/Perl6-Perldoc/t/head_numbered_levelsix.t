my $perldoc_data = <<'END_PERLDOC';
=head1 Head 1
=head2 Head 2
=head3 Head 3
=head4 Head 4
=head5 Head 5
=head6 # The Rescue of the Kobayashi Maru

END_PERLDOC

my $expected_structure = <<'END_EXPECTED';
errors: []

tree: !!perl/hash:Perl6::Perldoc::Document 
  content: 
    - !!perl/hash:Perl6::Perldoc::Block::pod 
      content: 
        - !!perl/hash:Perl6::Perldoc::Block::head1 
        - !!perl/hash:Perl6::Perldoc::Block::head2 
        - !!perl/hash:Perl6::Perldoc::Block::head3 
        - !!perl/hash:Perl6::Perldoc::Block::head4 
        - !!perl/hash:Perl6::Perldoc::Block::head5 
        - !!perl/hash:Perl6::Perldoc::Block::head6 
          content: 
            - "The Rescue of the Kobayashi Maru\n"
          number: 1.1.1.1.1.1
          style: abbreviated
          typename: head6
      style: implicit
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
