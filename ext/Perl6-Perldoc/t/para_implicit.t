my $perldoc_data = <<'END_PERLDOC';
=head1 This is a heading block

This is an ordinary paragraph.
Its text  will   be     squeezed     and
short lines filled. It is terminated by
the first blank line.

This is another ordinary paragraph.
Its     text    will  also be squeezed and
short lines filled. It is terminated by
the trailing directive on the next line.
=head2 This is another heading block

END_PERLDOC

my $expected_structure = <<'END_EXPECTED';
errors: []

tree: !!perl/hash:Perl6::Perldoc::Document 
  content: 
    - !!perl/hash:Perl6::Perldoc::Block::pod 
      content: 
        - !!perl/hash:Perl6::Perldoc::Block::head1 
          content: 
            - "This is a heading block\n"
        - !!perl/hash:Perl6::Perldoc::Block::para 
          content: 
            - "This is an ordinary paragraph.\n\
              Its text  will   be     squeezed     and\n\
              short lines filled. It is terminated by\n\
              the first blank line.\n"
          style: implicit
          typename: para
        - !!perl/hash:Perl6::Perldoc::Block::para 
          content: 
            - "This is another ordinary paragraph.\n\
              Its     text    will  also be squeezed and\n\
              short lines filled. It is terminated by\n\
              the trailing directive on the next line.\n"
          style: implicit
          typename: para
        - !!perl/hash:Perl6::Perldoc::Block::head2 
          content: 
            - "This is another heading block\n"
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
