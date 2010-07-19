package Dist::Zilla::Plugin::STD;
# ABSTRACT: build a dist out of the STD
use Moose;
use Moose::Autobox;

use Dist::Zilla::File::OnDisk;
with 'Dist::Zilla::Role::FileGatherer';


sub renamed_file {
    my ($self,$source_name,$target_name,$filter) = @_;
    open my $fh, '<', $source_name or die "can't open $source_name for reading: $!";

    # This is needed or \r\n is filtered to be just \n on win32.
    # Maybe :raw:utf8, not sure.
    #     -- Kentnl - 2010-06-10
    # Cargo Culted from Dist::Zilla::File::OnDisk
    #     -- Pawel Murias
    binmode $fh, ':raw';

    my $content = do { local $/; <$fh> };
    $content = $filter->($content) if $filter;
    $self->add_file(Dist::Zilla::File::OnDisk->new({
        name    => $target_name,
        content => $content,
    }));


}
sub gather_files {
  my ($self, $arg) = @_;

  # handwritten .pmc files
  for (qw(CursorBase DEBUG NAME RE_ast Stash)) {
    $self->renamed_file("$_.pmc","lib/$_.pm")
  }

  # .pm files
  for (qw(Actions LazyMap)) {
      $self->renamed_file("$_.pm","lib/$_.pm");
  }

  system('make');

  # generated files
  for (qw(STD Cursor)) {
      my $version = $self->zilla->version;
      $self->renamed_file("$_.pmc","lib/$_.pm",sub {my $content = shift;$content =~ s/package STD;/package STD;BEGIN {\$STD::VERSION = $version}/;$content});
  }

  # adhoc stuff
  $self->renamed_file('viv','lib/viv');
  $self->renamed_file('uniprops','data/uniprops');
  $self->renamed_file('NULL.lex','lib/NULL.lex');
  $self->renamed_file('mangle.pl','lib/mangle.pl');
  $self->renamed_file('inc/MyBuilder.pm','inc/MyBuilder.pm');

}

__PACKAGE__->meta->make_immutable;
no Moose;
1;
