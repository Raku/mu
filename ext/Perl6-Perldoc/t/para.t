my $perldoc_data = <<'END_PERLDOC';
=for table  :caption<Table of Contents>
    Constants           1
    Variables           10
    Subroutines         33
    Everything else     57

=for Name  :required
=          :width(50)
The applicant's full name


END_PERLDOC

my $expected_structure = <<'END_EXPECTED';
errors: []

tree: !!perl/hash:Perl6::Perldoc::Document 
  content: 
    - !!perl/hash:Perl6::Perldoc::Block::pod 
      content: 
        - !!perl/hash:Perl6::Perldoc::Block::table 
          content: 
            - "    Constants           1\n    Variables           10\n    Subroutines         33\n    Everything else     57\n"
          options: 
            caption: 
              - Table
              - of
              - Contents
          rows: 
            - !!perl/hash:Perl6::Perldoc::Block::table::Row 
              cells: 
                - !!perl/hash:Perl6::Perldoc::Block::table::Cell 
                  bottom: ''
                  content: 
                    - !!perl/hash:Perl6::Perldoc::Block::pod 
                      content: 
                        - !!perl/hash:Perl6::Perldoc::Block::para 
                          content: 
                            - Constants
                          style: implicit
                          typename: para
                      is_implicit: 1
                      style: implicit
                      typename: pod
                  header: ''
                  left: ''
                  right: ''
                  top: ''
                - !!perl/hash:Perl6::Perldoc::Block::table::Cell 
                  bottom: ''
                  content: 
                    - !!perl/hash:Perl6::Perldoc::Block::pod 
                      content: 
                        - !!perl/hash:Perl6::Perldoc::Block::para 
                          content: 
                            - 1
                          style: implicit
                          typename: para
                      is_implicit: 1
                      style: implicit
                      typename: pod
                  header: ''
                  left: ''
                  right: ''
                  top: ''
            - !!perl/hash:Perl6::Perldoc::Block::table::Row 
              cells: 
                - !!perl/hash:Perl6::Perldoc::Block::table::Cell 
                  bottom: ''
                  content: 
                    - !!perl/hash:Perl6::Perldoc::Block::pod 
                      content: 
                        - !!perl/hash:Perl6::Perldoc::Block::para 
                          content: 
                            - Variables
                          style: implicit
                          typename: para
                      style: implicit
                      typename: pod
                  header: ''
                  left: ''
                  right: ''
                  top: ''
                - !!perl/hash:Perl6::Perldoc::Block::table::Cell 
                  bottom: ''
                  content: 
                    - !!perl/hash:Perl6::Perldoc::Block::pod 
                      content: 
                        - !!perl/hash:Perl6::Perldoc::Block::para 
                          content: 
                            - 10
                          style: implicit
                          typename: para
                      style: implicit
                      typename: pod
                  header: ''
                  left: ''
                  right: ''
                  top: ''
            - !!perl/hash:Perl6::Perldoc::Block::table::Row 
              cells: 
                - !!perl/hash:Perl6::Perldoc::Block::table::Cell 
                  bottom: ''
                  content: 
                    - !!perl/hash:Perl6::Perldoc::Block::pod 
                      content: 
                        - !!perl/hash:Perl6::Perldoc::Block::para 
                          content: 
                            - Subroutines
                          style: implicit
                          typename: para
                      style: implicit
                      typename: pod
                  header: ''
                  left: ''
                  right: ''
                  top: ''
                - !!perl/hash:Perl6::Perldoc::Block::table::Cell 
                  bottom: ''
                  content: 
                    - !!perl/hash:Perl6::Perldoc::Block::pod 
                      content: 
                        - !!perl/hash:Perl6::Perldoc::Block::para 
                          content: 
                            - 33
                          style: implicit
                          typename: para
                      style: implicit
                      typename: pod
                  header: ''
                  left: ''
                  right: ''
                  top: ''
            - !!perl/hash:Perl6::Perldoc::Block::table::Row 
              cells: 
                - !!perl/hash:Perl6::Perldoc::Block::table::Cell 
                  bottom: ''
                  content: 
                    - !!perl/hash:Perl6::Perldoc::Block::pod 
                      content: 
                        - !!perl/hash:Perl6::Perldoc::Block::para 
                          content: 
                            - Everything else
                          style: implicit
                          typename: para
                      style: implicit
                      typename: pod
                  header: ''
                  left: ''
                  right: ''
                  top: ''
                - !!perl/hash:Perl6::Perldoc::Block::table::Cell 
                  bottom: ''
                  content: 
                    - !!perl/hash:Perl6::Perldoc::Block::pod 
                      content: 
                        - !!perl/hash:Perl6::Perldoc::Block::para 
                          content: 
                            - 57
                          style: implicit
                          typename: para
                      style: implicit
                      typename: pod
                  header: ''
                  left: ''
                  right: ''
                  top: ''
          style: paragraph
          typename: table
        - !!perl/hash:Perl6::Perldoc::Block::Named::Name 
          content: 
            - "The applicant's full name\n"
          options: 
            required: 1
            width: 50
          style: paragraph
          typename: Name
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
