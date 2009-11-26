knowhow ClassHOW {

=begin

=head1 NAME

ClassHOW - Default Perl 6 Metaclass

=head1 DESCRIPTION

See http://www.perlfoundation.org/perl6/index.cgi?smop_oo_api

=head1 METHODS

This class implements the HOW API.

=over

=item method CREATE($how: $prototype, :$repr --> Object )

This method will alloc an object of the given representation.

=end

##   method CREATE($how: $prototype, :$repr = 'p6opaque') {
##       my $obj = ::($repr).^!CREATE();
##       $obj.^!how = $prototype.^!how;
##   }

=begin

=item method bless($how: $prototype, $candidate, *@protoobjects, *%initialize)

This method will initialize the candidate object.

=end

##   method bless($how: $prototype, $candidate, *@protoobjects, *%initialize) {
##       $candidate.^!isa = $prototype;
##       $candidate.^!does = ();
##       $candidate.^!attributes = ();
##       $candidate.^!methods = ();
##       $candidate.^!submethods = ();
##       my %whence_eval = $candidate.^!whence.() if
##         $candidate.^!whence;
##       $candidate.BUILDALL(|@protoobjects, |%whence_eval, |%initialize);
##       return $candidate;
##   }

=begin

=item method BUILDALL($how: $object, *@protoobjects, *%initialize)

This method is called from bless, to actually initialize the values of the object.

=end

##   method BUILDALL($how: $object, *@protoobjects, *%initialize) {
##       my sub buildall_recurse($object, $prototype, *@protoobjects, *%initialize) {
##           my $CLASS is context = $prototype;
##           for ($prototype.^!isa()) -> $isa {
##               buildall_recurse($object, $isa, |@protoobjects, |%initialize)
##           }
## 
##           my $package = $prototype.^!package();
##           $object.^!initialize_instance_storage($package);
## 
##           for ($prototype.^!attributes()) -> $att {
##               $object.^!initialize_instance_storage_slot($package, $att.private_name(), $att.create_container());
##           }
## 
##           my %protoargs = map { $_.^!whence.() if $_.^!whence },
##             grep { $_.^!instanceof === $prototype }, @protoobjects;
##           $prototype.?BUILD($object, |%protoargs, |%initialize);
##       }
##       return buildall_recurse($object, $object, |@protoobjects, |%initialize);
##   }


=begin

=item method DESTROYALL($how: $object)

This method is called when the object is being destroyed.my sub list_hierarchy 

=end

##   method DESTROYALL($how: $object) {
##       my sub destroyall_recurse($object, $prototype) {
##           $prototype.?DESTROY($object);
##           $object.^!destroy_instance_storage($prototype.^!package());
## 
##           for ($prototype.^!isa()) -> $isa {
##               destroyall_recurse($object, $isa)
##           }
##       }
##       destroyall_recurse($object, $object);
##   }


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

=item method methods($how: $object, :$local --> List of Method)

Returns a lazy list with all the methods implemented by this object.

The :$local option specifies that no traversal in the ISA should be
made, returning only the methods locally defined in this class.

=end

##   method methods($how: $object, :$local) {
##       my @methods;
##       my sub list_methods_recurse($obj) {
##           for ($obj.^!methods.keys) -> $selfdef {
##               @methods.push($selfdef);
##           }
##           return if $local;
##           for ($obj.^!isa) -> $isa {
##               list_methods_recurse($isa);
##           }
##       }
##       list_methods_recurse($object);
##       for ($object.^!submethods) -> $submethod {
##           @method.push($submethod);
##       }
##       return @methods;
##   }


=begin

=item method attributes($how: $object, :$local --> List of Attribute)

Returns a lazy list with all the attributes of this object.

The :$local option specifies that no traversal in the ISA should be
made, returning only the attributes locally defined in this class.

=end

##   method attributes($how: $object, :$local) {
##       my @attributes;
##       my sub list_attributes_recurse($obj) {
##           for ($obj.^!attributes) -> $attr {
##               @attributes.push($attr);
##           }
##           return if $local;
##           for ($obj.^!isa) -> $isa {
##               list_attributes_recurse($isa);
##           }
##       }
## 
##       list_attributes_recurse($object);
##       return @attributes;
##   }


=begin

=item method isa($how: $object, $superclass --> bool)

Is this a subclass of the given class?

=end

##   method isa($how: $object, $superclass) {
##       return true if $object === $superclass;
##       for ($object.^!isa) -> $isa {
##           return true if $isa === $superclass;
##           my $res = $isa.^isa($superclass);
##           return true if $res;
##       }
##       return False;
##   }

=begin

=item method does($how: $object, $superclass --> bool)

Does this object matches the given class?

=end

  method does($how: $object, $superclass) {
##       return true if $object === $superclass;
##       for ($object.^!isa) -> $isa {
##           return true if $isa === $superclass;
##           my $res = $isa.^does($superclass);
##           return true if $res;
##       }
##       for ($object.^!does) -> $does {
##           return true if $does === $superclass;
##           my $res = $does.^does($superclass);
##           return true if $res;
##       }
##       return False;
  }

=begin

=item method roles($how: $object, :$local)

Returns the list of roles composed into this type.

Use :local to avoid searching the roles composed into the superclasses
of this type.

=end

  method roles($how: $object, :$local) {
##      if $local {
##          return $object.^!does;
##      } else {
##          return $object.^!does, $object.^!isa.map: { .^roles };
##      }
  }

=begin

=item method parents($how: $object, :$local)

Returns the list of roles composed into this type.

Use :local to get only the direct parents.

=end

  method parents($how: $object, :$local) {
##      if $local {
##          return $object.^!isa;
##      } else {
##          return $object.^!isa, $object.^!isa.map: { .^parents };
##      }
  }

=begin

=item method can($how: $object, $name, $capture? --> List of Method)

Returns a lazy list of methods that match to this name/capture.

=end

  method can($how: $object, $name, $capture?) {
##       my @variants;
##       if ($object.^!instanceof.^!submethods.exists($name)) {
##           # check if there's a submethod of the same name.
##           @variants = $object.^!instanceof.^!submethods.{$name}.variants();
##       } else {
##           my @everyone;
##           my sub list_hierarchy {
##               @everyone.push($_);
##               for $_.^!isa -> $isa {
##                   list_hierarchy($isa);
##               }
##           }
##           list_hierarchy($object.^!instanceof);
##           for @everyone {
##               if .^!methods.exists($name) {
##                   @variants.push(.^!methods.{$name}.variants());
##                   if (! .^!methods.{$name}.multi) {
##                       last;
##                   }
##               }
##           }
##       }
##       return grep { $capture ?? .signature.ACCEPTS($capture) !! 1 },
##         @variants;
  }

=begin

=item method dispatch($how: $responder, $identifier, $capture)

Gets all the possible candidates for this method and invoke one.

=end

  method dispatch($how: $responder, $identifier, $capture) {
##       my $invocant = $$capture;
##       my @candidates = $invocant.^can($identifier, $capture);
##       if @candidates.elems > 1 {
##           fail 'Ambiguous dispatch!'; # call disambiguator.
##       } elsif @candidates.elems < 1 {
##           fail 'No method ' ~ $identifier;
##       } else {
##           @candidates[0].($capture);
##       }
  }

=begin

=item method add_method($object, $name, $code)

Add this method to this prototype.

=end

  method add_method($how: $object, $name, $code) {
      say 'adding method';
      if $object.^!methods.exists($name.FETCH) {
          warn 'Method ', $name, ' redefined.';
      }
      $object.^!methods.{$name} = $code;
  }

=begin

=item method add_attribute($how: $object, $privname, $attribute)

Add this attribute to this prototype, it does not create the
accessors, it only registers the attribute, the accessor should be
additionally registered via add_method.

The name passed here should always be the private name, otherwise it
will not be found by regular syntax. The $attribute object should
store the declared name anyway, to allow proper introspection.

=end

  method add_attribute($how: $object, $privname, $attribute) {
##       if $object.^!attributes.exists($privname) {
##           warn 'Attribute ', $privname, ' redefined.';
##       }
##       $object.^!attributes.{$privname} = $attribute;
  }

=begin

=item method compose_role($how: $object, $role)

This method composes the given role into this object. This will mean
copying the following definitions from the role:

  * isa
  * does (recurse compose_role)
  * attributes
  * methods

In that order.

=end

  method compose_role($how: $object, $role) {
##       for $role.^!isa -> $isa {
##           $object.^!isa.push($isa);
##       }
##       for $role.^!does -> $does {
##           $object.^compose_role($does);
##       }
##       for $role.^!attritutes -> $att {
##           $object.^add_attribute($att.private_name, $att);
##       }
##       for $role.^!methods -> $meth {
##           if $meth.yada {
##               if ! $object.^can($meth.name) {
##                   $object.^add_method($meth.name, $meth);
##               }
##           } else {
##               $object.^add_method($meth.name, $meth);
##           }
##       }
  }



}
$LexicalPrelude{'ClassHOW'} := ::ClassHOW;
## vim: expandtab sw=4 ft=perl6
