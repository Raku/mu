class P6Meta {

=begin

=head1 NAME

P6Meta - Default Perl 6 Metaclass

=head1 DESCRIPTION

See http://www.perlfoundation.org/perl6/index.cgi?smop_oo_api

=head1 METHODS

This class implements the HOW API.

=over

=item method CREATE($prototype: :$repr --> Object )

This method will alloc an object of the given representation.

=end

  method CREATE($prototype: :$repr --> Object) {
      my $obj = ___EMPTY_REPR___($repr).^!CREATE();
      $obj.^!how($prototype.^!how);
  }

=begin

=item method bless($prototype: $candidate, *@protoobjects, *%initialize --> Object)

This method will initialize the candidate object.

=end

  method bless($prototype: $candidate, *@protoobjects, *%initialize --> Object) {
      $candidate.^!bless($prototype);
      @protoobjects.unshift($candidate.^!whence) if
        $candidate.^!whence;
      $candidate.BUILDALL(|@protoobjects, |%initialize);
      return $candidate;
  }

=begin

=item method BUILDALL($object: *@protoobjects, *%initialize)

This method is called from bless, to actually initialize the values of the object.

=end

  method BUILDALL($object: *@protoobjects, *%initialize) {
      my sub buildall_recurse($object: $prototype, *@protoobjects, *%initialize) {
          for ($prototype.^!isa()) -> $isa {
              buildall_recurse($object: $isa, |@protoobjects, |%initialize)
          }
          for ($prototype.^!does()) -> $does {
              buildall_recurse($object: $does, |@protoobjects, |%initialize)
          }

          my $package = $prototype.^!package();
          $object.^!initialize_instance_storage($package);

          for ($prototype.^!attributes()) -> $att {
              $object.^!initialize_instance_storage_slot
                ($package, $att.private_name(), $att.create_container());
          }

          my %protoargs = @protoobjects.grep { $_.WHAT === $prototype };
          $prototype.?BUILD($object: |%protoargs, |%initialize);
      }
      fail if not $object.^!instance;
      return buildall_recurse($object: $object, @protoobjects, %initialize);
  }


=begin

=item method DESTROYALL($object: )

This method is called when the object is being destroyed.

=end

  method DESTROYALL($object:) {
      my sub destroyall_recurse($object: $prototype) {
          $prototype.?DESTROY($object: );
          $object.^!destroy_instance_storage($prototype.^!package());

          for ($prototype.^!does()) -> $does {
              destroyall_recurse($object: $does)
          }
          for ($prototype.^!isa()) -> $isa {
              destroyall_recurse($object: $isa)
          }
      }
      destroyall_recurse($object: $object);
  }


=begin

=item method clone($object: )

In this metaclass, clone is a direct call on the REPR.

=end

  method clone($object: ) {
      return $object.^!clone();
  }

=begin

=item method defined($object: )

In this metaclass, defined is a direct call on the REPR.

=end

  method defined($object: ) {
      $object.^!defined();
  }

=begin

=item method methods($object: --> List of Method)

Returns a lazy list with all the methods implemented by this object.

=end

  method methods($object: --> List of Method) {
      my sub list_methods_recurse($obj) {
          for ($obj.^!methods) --> $selfdef {
              take $selfdef;
          }
          for ($obj.^!isa) --> $isa {
              list_methods_recurse($isa);
          }
          for ($obj.^!role) --> $role {
              list_methods_recurse($role);
          }
      }

      List of Method @methods = gather {
          list_methods_recurse($object);
          for ($object.^!submethods) --> $submethod {
             take $submethod;
          }
      };
      return @methods;
  }


=begin

=item method attributes($object: --> List of Attribute)

Returns a lazy list with all the attributes of this object.

=end

  method attributes($object: --> List of Attribute) {
      my sub list_attributes_recurse($obj) {
          for ($obj.^!attributes) --> $attr {
              take $attr;
          }
          for ($obj.^!isa) --> $isa {
              list_attributes_recurse($isa);
          }
          for ($obj.^!role) --> $role {
              list_attributes_recurse($role);
          }
      }

      List of Attribute @attributes = gather {
          list_attributes_recurse($object);
      };
      return @attributes;
  }


=begin

=item method isa($object: $superclass --> bool)

Is this a subclass of the given class?

=end

  method isa($object: $superclass --> bool) {
      return true if $object === $superclass;
      for ($object.^!isa) --> $isa {
          return true if $isa === $superclass;
          my $res = $isa.^isa($superclass);
          return true if $res;
      }
      return false;
  }

=begin

=item method does($object: $superclass --> bool)

Does this object matches the given class?

=end

  method does($object: $superclass --> bool) {
      return true if $object === $superclass;
      for ($object.^!does) --> $isa {
          return true if $isa === $superclass;
          my $res = $isa.^does($superclass);
          return true if $res;
      }
      for ($object.^!does) --> $does {
          return true if $does === $superclass;
          my $res = $does.^does($superclass);
          return true if $res;
      }
      return false;
  }

=begin

=item method can($object: $name, $capture? --> List of Method)

Returns a lazy list of methods that match to this name/capture.

=end

  method can($object: $name, $capture? --> List of Method) {
      my List of Method @methods = gather {
          take
            <=== grep { .name eq $name &&
                        $capture ?? .signature.ACCEPTS($capture) !! 1 }
            <=== $object.^methods(); #>
      };
      return @methods;
  }

}
