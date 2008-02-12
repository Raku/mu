class Object {

=begin

=head1 NAME

Object - This is the base object for the standard Perl 6 libraries

=head1 DESCRIPTION

See http://www.perlfoundation.org/perl6/index.cgi?smop_oo_api

=head1 Methods

This are the methods implemented by Object

=over

=item our Object method new(*@protoobjects, *%initialize)

This method creates a new object with the default represenation and
initializes it with the given named parameters and protoobjects.

=end

  our Object method new($prototype: *@protoobjects, *%initialize) {
      return $prototype.bless($prototype.CREATE(),
                            |@protoobjects,
                            |%initialize);
  };


=begin

=item our Object method bless($candidate?, *@protoobjects, *%initialize)

This method sets the invocant as the prototype of the given object and
call the initialization code from least-derived to most-derived.

=end

  our Object method bless($prototype: $candidate?, *@protoobjects, *%initialize) {
      $candidate //= $prototype.CREATE();
      $candidate.^!isa().push($prototype);
      $candidate.BUILDALL(|@protoobjects, |%initialize);
      return $candidate;
  }


=item our Object method clone()

Creates a clone of the current object.

=end

  our Object method clone($object: ) {
      return $object.^!clone();
  }

=begin

=item our bool method defined()

Is this object defined?

=end

  our bool method defined($object: ) {
      return $object.^!defined();
  }

=begin

=item our bool method isa($prototype)

Is $prototype part of this object's hierarchy?

=end

  our bool method isa($object: $prototype) {
      return $object.^isa($prototype);
  }

=begin

=item our bool method does($prototype)

Does this object implement $prototype?

=end

  our bool method does($object: $prototype) {
      return $object.^does($prototype);
  }


=begin

=item our List of Method method can($name, $capture?)

Asks the metaclass if this object can do this method.

=end

  our List of Method method can($name, $capture?) {
      return $object.^can($name, $capture?);
  }

=begin

=item our Object method CREATE(:$repr)

This method will create a new object instance using the given
representation or the default one.

=end

  our Object method CREATE($prototype: :$repr) {
      # TODO: we don't really support creating alternative
      # representations right now. But one can always call CREATE on
      # another representation by himself and call bless with it.
      return $prototype.^!CREATE()
  }

=begin

=item method BUILDALL(*@protoobjects, *%initialize)

This will traverse the hierarchy calling BUILD in each class.

=end


  method BUILDALL($object: *@protoobjects, *%initialize) {
      return $object!buildall_recurse($object, @protoobjects, %initialize);
  }

  my method !buildall_recurse($object: $prototype, *@protoobjects, *%initialize) {
      for ($prototype.^!isa()) -> $isa {
          $object!buildall_recurse($isa, |@protoobjects, |%initialize)
      }
      for ($prototype.^!does()) -> $does {
          $object!buildall_recurse($does, |@protoobjects, |%initialize)
      }

      $object.^!initialize_instance_storage($prototype.^!package());

      # TODO: test if any of the protoobjects are of the same type of
      # the current prototype, and if that's the case, translate it into
      # named arguments.
      $prototype.?BUILD($object: |%initialize);
  }

=begin

=item method DESTROYALL()

This will traverse the hierarchy calling DESTROY in each class.

=end

  method DESTROYALL($object:) {
      $object!destroyall_recurse($object);
  }

  my method !destroyall_recurse($object: $prototype) {
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

=item submethod BUILD(*%initialize)

The default build initializes all public attributes defined in the
named arguments.

=end

  submethod BUILD($object: *%initialize) {
      for (%initialize.keys) -> $key {
          $object.?"$key" = %initialize{$key};
      }
  }

=begin

=back

=end

}
