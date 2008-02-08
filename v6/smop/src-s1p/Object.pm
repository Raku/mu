=head1 NAME

Object - This is the base object for the standard Perl 6 libraries

=head1 DESCRIPTION

See http://www.perlfoundation.org/perl6/index.cgi?smop_oo_api

=head1 Methods

This are the methods implemented by Object

=cut


class Object {

=over

=item method new($prototype: *@protoobjects, *%initialize)

This method creates a new object with the default represenation and
initializes it with the given named parameters and protoobjects.

=back

=cut

  method new($prototype: *@protoobjects, *%initialize) {
      return $prototype.bless($prototype.CREATE(),
                            |@protoobjects,
                            |%initialize);
  };


=over

=item method bless($prototype: $candidate?, *@protoobjects, *%initialize)

This method sets the invocant as the prototype of the given object and
call the initialization code from least-derived to most-derived.

=back

=cut

  method bless($prototype: $candidate?, *@protoobjects, *%initialize) {
      $candidate //= $prototype.CREATE();
      my $object = $prototype.^^bless($candidate);
      $object.BUILDALL(|@protoobjects, |%initialize);
      return $object;
  }

=over

=item method CREATE($prototype: :$repr)

This method will create a new object instance using the given
representation or the default one.

=back

=cut

  method CREATE($prototype: :$repr) {
      # TODO: we don't really support creating alternative
      # representations right now.
      return $prototype.^^CREATE()
  }

=over

=item method BUILDALL(*@protoobjects, *%initialize)

This will traverse the hierarchy calling BUILD in each class.

=back

=cut


  method BUILDALL($object: *@protoobjects, *%initialize) {
      return $object!buildall_recurse($object, @protoobjects, %initialize);
  }

  my method buildall_recurse($object: $prototype, *@protoobjects, *%initialize) {
      if (my $super = $prototype.^^get_direct_prototype()) {
          $object!buildall_recurse($super, |@protoobjects, |%initalize);
      } else {
          if (my $count = $prototype.^^isa_cout()) {
              my $i = 0;
              while ($i < $count) {
                  $object!buildall_recurse($prototype.^^isa_at($count), |@protoobjects, |%initialize)
                  $count++;
              }
          }
          if (my $count = $prototype.^^role_cout()) {
              my $i = 0;
              while ($i < $count) {
                  $object!buildall_recurse($prototype.^^role_at($count), |@protoobjects, |%initialize)
                  $count++;
              }
          }
      }
      $prototype.^^initialize_instance_storage($object);
      # TODO: test if any of the protoobjects are of the same type of
      # the current prototype, and if that's the case, translate it into
      # named arguments.
      $prototype.?BUILD($object: |%initialize);
  }

=over

=item method DESTROYALL()

This will traverse the hierarchy calling DESTROY in each class.

=back

=cut

  method DESTROYALL($object:) {
      $object!destroyall_recurse($object);
      $object.^^DESTROY();
  }

  my method destroyall_recurse($object: $prototype) {
      $prototype.?DESTROY($object: );
      $prototype.^^destroy_instance_storage($object);
      if (my $super = $prototype.^^get_direct_prototype()) {
          $object!destroyall_recurse($super);
      } else {
          if (my $count = $prototype.^^isa_cout()) {
              while ($count >= 0) {
                  $count--;
                  $object!destroyall_recurse($prototype.^^isa_at($count))
              }
          }
          if (my $count = $prototype.^^role_cout()) {
              while ($count >= 0) {
                  $count--;
                  $object!destroyall_recurse($prototype.^^role_at($count))
              }
          }
      }
  }

=over

=item submethod BUILD(*%initialize)

The default build initializes all public attributes defined in the
named arguments.

=back

=cut

  submethod BUILD($object: *%initialize) {
      for (%initialize.keys) -> $key {
          $object.?"$key" = $initialize($value);
      }
  }

}


=back

=cut
