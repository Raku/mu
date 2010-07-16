package Dist::Zilla::Plugin::STD;
# ABSTRACT: build a dist out of the STD
use Moose;
use Moose::Autobox;

use Dist::Zilla::File::OnDisk;
with 'Dist::Zilla::Role::FileGatherer';


sub gather_files {
  my ($self, $arg) = @_;


  my $file = Dist::Zilla::File::OnDisk->new({
    name    => 'CursorBase.pmc',
    _orginal_name => 'CursorBase.pmc'
  });

  $self->add_file($file);
  return;
}

__PACKAGE__->meta->make_immutable;
no Moose;
1;
