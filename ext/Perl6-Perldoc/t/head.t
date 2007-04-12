my $perldoc_data = <<'END_PERLDOC';
=head1 A Top Level Heading

=head2 A Second Level Heading

=head3 A third level heading

=head86 A "Missed it by I<that> much!" heading

END_PERLDOC

my $expected_structure = <<'END_EXPECTED';
errors: []

tree: !!perl/hash:Perl6::Perldoc::Document 
  content: 
    - !!perl/hash:Perl6::Perldoc::Block::pod 
      content: 
        - !!perl/hash:Perl6::Perldoc::Block::head1 
          content: 
            - "A Top Level Heading\n"
          level: 1
          style: abbreviated
          typename: head1
        - !!perl/hash:Perl6::Perldoc::Block::head2 
          content: 
            - "A Second Level Heading\n"
          level: 2
          style: abbreviated
          typename: head2
        - !!perl/hash:Perl6::Perldoc::Block::head3 
          content: 
            - "A third level heading\n"
          level: 3
          style: abbreviated
          typename: head3
        - !!perl/hash:Perl6::Perldoc::Block::head86 
          content: 
            - "A \"Missed it by "
            - !!perl/hash:Perl6::Perldoc::FormattingCode::I 
              content: 
                - that
              style: formatting
              typename: I
            - " much!\" heading\n"
          style: abbreviated
          typename: head86
      style: implicit
      typename: pod
  terminator: (?!)
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
