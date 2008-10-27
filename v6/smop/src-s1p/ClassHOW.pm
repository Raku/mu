knowhow ClassHOW {

=begin

=head1 NAME

Class - Default Perl 6 Metaclass

=head1 DESCRIPTION

See http://www.perlfoundation.org/perl6/index.cgi?smop_oo_api

=head1 METHODS

This class implements the HOW API.

=over

=item method CREATE($how: $prototype, :$repr --> Object )

This method will alloc an object of the given representation.

=end

  method CREATE($how: $prototype, :$repr) {
      $repr //= 'p6opaque';
      my $obj = ::($repr).^!CREATE();
      $obj.^!how = $prototype.^!how;
  }

=begin

=item method bless($how: $prototype, $candidate, *@protoobjects, *%initialize)

This method will initialize the candidate object.

=end

  method bless($how: $prototype, $candidate, *@protoobjects, *%initialize) {
      $candidate.^!isa = ($prototype);
      $candidate.^!does = ();
      my %whence_eval = $candidate.^!whence.() if
        $candidate.^!whence;
      $candidate.BUILDALL(|@protoobjects, |%whence_eval, |%initialize);
      return $candidate;
  }

=begin

=item method BUILDALL($how: $object, *@protoobjects, *%initialize)

This method is called from bless, to actually initialize the values of the object.

=end

  method BUILDALL($how: $object, *@protoobjects, *%initialize) {
      my sub buildall_recurse($object, $prototype, *@protoobjects, *%initialize) {
          for ($prototype.^!isa()) -> $isa {
              buildall_recurse($object, $isa, |@protoobjects, |%initialize)
          }

          my $package = $prototype.^!package();
          $object.^!initialize_instance_storage($package);

          for ($prototype.^!attributes()) -> $att {
              $object.^!initialize_instance_storage_slot($package, $att.private_name(), $att.create_container());
          }

          my %protoargs = map { $_.^!whence.() if $_.^!whence },
            grep { $_.WHAT === $prototype }, @protoobjects;
          $prototype.?BUILD($object: |%protoargs, |%initialize);
      }
      return buildall_recurse($object, $object, |@protoobjects, |%initialize);
  }


=begin

=item method DESTROYALL($how: $object)

This method is called when the object is being destroyed.my sub list_hierarchy 

=end

  method DESTROYALL($how: $object) {
      my sub destroyall_recurse($object, $prototype) {
          $prototype.?DESTROY($object: );
          $object.^!destroy_instance_storage($prototype.^!package());

          for ($prototype.^!isa()) -> $isa {
              destroyall_recurse($object, $isa)
          }
      }
      destroyall_recurse($object, $object);
  }


=begin

=item method clone($how: $object)

In this metaclass, clone is a direct call on the REPR.

=end

  method clone($how: $object) {
      return $object.^!clone();
  }

=begin

=item method defined($how: $object)

In this metaclass, defined is a direct call on the REPR.

=end

  method defined($how: $object) {
      $object.^!defined();
  }

=begin

=item method methods($how: $object --> List of Method)

Returns a lazy list with all the methods implemented by this object.

=end

  method methods($how: $object) {
      my @methods;
      my sub list_methods_recurse($obj) {
          for ($obj.^!methods.keys) -> $selfdef {
              @methods.push($selfdef);
          }
          for ($obj.^!isa) -> $isa {
              list_methods_recurse($isa);
          }
      }
      list_methods_recurse($object);
      for ($object.^!submethods) -> $submethod {
          @method.push($submethod);
      }
      return @methods;
  }


=begin

=item method attributes($how: $object --> List of Attribute)

Returns a lazy list with all the attributes of this object.

=end

  method attributes($how: $object) {
      my @attributes;
      my sub list_attributes_recurse($obj) {
          for ($obj.^!attributes) -> $attr {
              @attributes.push($attr);
          }
          for ($obj.^!isa) -> $isa {
              list_attributes_recurse($isa);
          }
      }

      list_attributes_recurse($object);
      return @attributes;
  }


=begin

=item method isa($how: $object, $superclass --> bool)

Is this a subclass of the given class?

=end

  method isa($how: $object, $superclass) {
      return true if $object === $superclass;
      for ($object.^!isa) -> $isa {
          return true if $isa === $superclass;
          my $res = $isa.^isa($superclass);
          return true if $res;
      }
      return False;
  }

=begin

=item method does($how: $object, $superclass --> bool)

Does this object matches the given class?

=end

  method does($how: $object, $superclass) {
      return true if $object === $superclass;
      for ($object.^!isa) -> $isa {
          return true if $isa === $superclass;
          my $res = $isa.^does($superclass);
          return true if $res;
      }
      for ($object.^!does) -> $does {
          return true if $does === $superclass;
          my $res = $does.^does($superclass);
          return true if $res;
      }
      return False;
  }

=begin

=item method can($how: $object, $name, $capture? --> List of Method)

Returns a lazy list of methods that match to this name/capture.

=end

  method can($how: $object, $name, $capture?) {
      my @variants;
      if ($object.^!instanceof.^!submethods.exists($name)) {
          # check if there's a submethod of the same name.
          @variants = $object.^!instanceof.^!submethods.{$name}.variants();
      } else {
          my @everyone;
          my sub list_hierarchy {
              @everyone.push($_);
              for $_.^!isa -> $isa {
                  list_hierarchy($isa);
              }
          }
          list_hierarchy($object.^!instanceof);
          for @everyone {
              if .^!methods.exists($name) {
                  @variants.push(.^!methods.{$name}.variants());
                  if (! .^!methods.{$name}.multi) {
                      last;
                  }
              }
          }
      }
      return grep { $capture ?? .signature.ACCEPTS($capture) !! 1 },
        @variants;
  }

=begin

=item method dispatch($how: $responder, $identifier, $capture)

Gets all the possible candidates for this method and invoke one.

=end

  method dispatch($how: $responder, $identifier, $capture) {
      my $invocant = $$capture;
      my @candidates = $invocant.^can($identifier, $capture);
      if (@candidates.elems > 1) {
          fail 'Ambiguous dispatch!'; # call disambiguator.
      } elsif (@candidates.elems < 1) {
          fail 'No method ' ~ $identifier;
      } else {
          @candidates[0].($capture);
      }
  }

}
