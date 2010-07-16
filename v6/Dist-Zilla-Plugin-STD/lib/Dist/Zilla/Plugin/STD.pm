package Dist::Zilla::Plugin::STD;
# ABSTRACT: build a dist out of the STD
use Moose;
use Moose::Autobox;

use Dist::Zilla::File::OnDisk;
with 'Dist::Zilla::Role::FileGatherer';


sub renamed_file {
    my ($self,$source_name,$target_name) = @_;
    open my $fh, '<', $source_name or die "can't open $source_name for reading: $!";

    # This is needed or \r\n is filtered to be just \n on win32.
    # Maybe :raw:utf8, not sure.
    #     -- Kentnl - 2010-06-10
    # Cargo Culted from Dist::Zilla::File::OnDisk
    #     -- Pawel Murias
    binmode $fh, ':raw';

    $self->add_file(Dist::Zilla::File::OnDisk->new({
        name    => $target_name,
        content => do { local $/; <$fh> },
    }));

}
sub gather_files {
  my ($self, $arg) = @_;

  # handwritten .pmc files
  for (qw(CursorBase DEBUG NAME RE_ast Stash)) {
    $self->renamed_file("$_.pmc","lib/$_.pm")
  }

  # .pm files
  for (qw(Actions DumpMatch LazyMap)) {
    $self->renamed_file("$_.pm","lib/$_.pm")
  }

}

__PACKAGE__->meta->make_immutable;
no Moose;
1;
