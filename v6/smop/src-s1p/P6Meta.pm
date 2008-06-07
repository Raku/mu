class P6Meta {

=begin

=head1 NAME

P6Meta - Default Perl 6 Metaclass

=head1 DESCRIPTION

See http://www.perlfoundation.org/perl6/index.cgi?smop_oo_api

=head1 METHODS

This class implements the HOW API.

=over

=item method ^CREATE($prototype: :$repr --> Object )

This method will alloc an object of the given representation.

=end

  method ^CREATE($prototype: :$repr --> Object) {
      return ___EMPTY_REPR___($repr).^!CREATE();
  }

=begin

=item method ^bless($prototype: $candidate, *@protoobjects, *%initialize --> Object)

This method will initialize the candidate object.

=end

  method ^bless($prototype: $candidate, *@protoobjects, *%initialize --> Object) {
      $candidate.^!bless($prototype);
      $candidate.BUILDALL(|@protoobjects, |%initialize);
      return $candidate;
  }

=begin

=item method ^BUILDALL($object: *@protoobjects, *%initialize)

This method is called from bless, to actually initialize the values of the object.

=end

  method ^BUILDALL($object: *@protoobjects, *%initialize) {
      fail if not $object.^!instance;
      return $object!^buildall_recurse($object, @protoobjects, %initialize);
  }

  my method ^buildall_recurse($object: $prototype, *@protoobjects, *%initialize) {
      for ($prototype.^!isa()) -> $isa {
          $object!buildall_recurse($isa, |@protoobjects, |%initialize)
      }
      for ($prototype.^!does()) -> $does {
          $object!buildall_recurse($does, |@protoobjects, |%initialize)
      }

      my $package = $prototype.^!package();
      $object.^!initialize_instance_storage($package);

      for ($prototype.^!attributes()) -> $att {
          $object.^!initialize_instance_storage_slot
            ($package, $att.private_name(), $att.create_container());
      }

      # TODO: test if any of the protoobjects are of the same type of
      # the current prototype, and if that's the case, translate it into
      # named arguments.
      $prototype.?BUILD($object: |%initialize);
  }

=begin

=item method ^DESTROYALL($object: )

This method is called when the object is being destroyed.

=end

  method DESTROYALL($object:) {
      $object!^destroyall_recurse($object);
  }

  my method ^destroyall_recurse($object: $prototype) {
      $prototype.?DESTROY($object: );
      $object.^!destroy_instance_storage($prototype.^!package());

      for ($prototype.^!does()) -> $does {
          $object!destroyall_recurse($does)
      }
      for ($prototype.^!isa()) -> $isa {
          $object!destroyall_recurse($isa)
      }
  }

=begin

=item method ^clone($object: )

In this metaclass, clone is a direct call on the REPR.

=end

  method ^clone($object: ) {
      return $object.^!clone();
  }

=begin

=item method ^defined($object: )

In this metaclass, defined is a direct call on the REPR.

=end

  method ^defined($object: ) {
      return $object.^!defined();
  }



}
