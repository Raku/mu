
module Meta::Util;

# a simple eval-based accessor generator... based on some of the best
# parts of Perl 5 Class::Tangram

use Set;

our $DEBUG = 0;

our %defaults
    = (
       # collection types
        Set => { default => sub { set() },
		 coll => 1,
	       },
        Hash => { default => sub { my %hash },
		  coll => 1,
		  to_set => ".values",
	        },
        Array => { default => sub { [ ] },
		   coll => 1,
		   to_set => ".values",
		 },
       'ref' => { coll => 1,
		  to_set => "",
		},

        # scalar types.. this is a STOPGAP :)
        "Any" => { "_dummy" => 1 },
        "Str" => { "_dummy" => 1 },
        "Bool" => { "_dummy" => 1 },
        "Int" => { "_dummy" => 1 },
      );

sub Has(Str $class, Str $name, Str ?$type="Any", Hash ?$options)
    returns Void is export
{

    my %o;
    if ($options) {
	$options.keys.map:{ %o{$_} = $options{$_} };
    }

    # in Class::Tangram, I insert accessors into a seperate package so
    # that they may be freely defined in the parent package.. but that
    # needs a sound inheritance model to work.
    #my $target_pkg = $class ~ "::mu";
    my $target_pkg = $class; # ~ "::mu";

    my $_type = $type;

    my $defaults = %defaults{$type} || do {
	$_type = "ref";
	%o<class> = $_type;
	%defaults<ref>;
    };

    for <default to_set coll required companion base_type> -> $opt {
	if !%o.exists($opt) and $defaults.exists($opt) {
	    #say "got here! |$name,$type,$opt,$defaults{$opt},$o|";
	    %o{$opt} = $defaults{$opt};
	}
    }

    #my ($default, $required, $companion, $base_type);
    my $accessor_code = "# line 1 'accessorcode'\n";

    #if ($target_pkg ne $class and $class.isa($target_pkg)) {
    #	say "$class is already a $target_pkg, not bothering\n";
        #} else {
	    #say "$class ne $target_pkg, adding isa\n";
	    #$accessor_code ~= "class $class isa $target_pkg;\n";
        #}

    $accessor_code ~= "class $target_pkg;\n";

    # yes, template code generation sucks.  But, it works :)
    $_type = "Any" if $_type eq "ref";
    my $hasa = "has $_type \$:$name;\n";
    my $getter;
    my $setter;
    my $switch;
    my $other = "";
    my $method_type;

    # 'collection' types get special treatment..

    if %o<coll> {
        $setter = 
"
multi method set_{$name}(\$self: $_type ?\$$name) returns {$_type} \{
";

	if %o<companion> {
	    my $tmp = "    my \$new = ";
	    $setter ~= "    my \$old = ";
	    if %o.exists("to_set") {
		$setter ~= "set(\$:$name%o<to_set>)";
		$tmp ~= "set(\$$name%o<to_set>)";
	    } else {
	        $setter =~ "\$:$name";
		$tmp ~= "\$$name";
	    }
	    $setter ~= ";\n$tmp;\n";
        }

        $setter ~= "    \$:$name = \$$name;\n";

	if %o<companion> {
	    $setter ~= "
    # propagate set membership..
    (\$new - \$old).grep:\{ !(.{$companion}_includes(\$self)) }
	.map\{ .{$companion}_insert($self) };
    (\$old - \$new).grep:\{ .{$companion}_includes(\$self) }
	.map\{ .{$companion}_remove($self) };
";
	}

	$setter ~= "    \$:$name\n}\n";

	$other = "
multi method {$name}_includes(\$self: *\@items) returns Bool \{
    ";
	if %o.exists("to_set") {
	    $other ~= "set(\$self.get_{$name}%o<to_set>)";
	} else {
	    $other ~= "\$self.get_{$name}";
	}
	$other ~= ".includes(\@items);\n}\n";

	$other ~= "

multi method {$name}_insert(\$self: *\@items) returns Int \{
    my \$pre_size = \$self.{$name}_size;
    \$self.set_{$name}(Meta::Util::insert_{$_type}(\$self.get_{$name}, \@items));
    return \$self.{$name}_size - \$pre_size;
}
multi method {$name}_remove(\$self: *\@items) returns Int \{
    my \$pre_size = \$self.{$name}_size;
    \$self.set_{$name}(Meta::Util::remove_{$_type}(\$self.get_{$name}, \@items));
    return \$self.{$name}_size - \$pre_size;
}
multi method {$name}_size(\$self:) returns Int \{
    Meta::Util::size_{$_type}(\$self.get_{$name});
}
    ";

	# later;
        #  - indexed array lookup
        #  - replace
        #  - pairs
        #  - clear
	#  - push/pop/shift/unshift/splice

    }

	else
    {
	# non-containers, use something simple
	$setter = "
method set_{$name}(\$self: $_type ?\$$name) returns $_type \{
    \$:$name = \$$name;
}
";

    }

    $getter = "
multi method get_{$name}(" ~ '$' ~ 'self' ~ ":) returns $_type \{
    \$:$name;
}
";

    # only a getter for now
    $switch = "
our &{$target_pkg}::$name ::= &{$target_pkg}::get_$name;
";

    $accessor_code ~=
"
# HAS A:
$hasa
# GETTER:
$getter
# SETTER:
$setter
# OTHER STUFF:
$other
# SWITCH:
$switch
";

    say "Going to eval: >-\n$accessor_code\n...\n" if $DEBUG;

    eval $accessor_code;
    die $! if $!;


}

sub insert_Hash(Hash $coll, *@items) returns Hash {
    # XXX we can't autodetect these keys for now.. without .can()
    ???;
    my $n = 0;
    my $newcoll = { $coll.pairs };
    for @items -> $item {
	while $newcoll.exists($n) {
	    $n++;
	}
	$newcoll{$n} = $item;
    }
    return $newcoll;
}
sub insert_Array(Array $coll, *@items) returns Array {
    # XXX we can't autodetect these keys for now.. without .can()
    [ $coll, @items ]
}
sub insert_Set(Set $coll, *@items) {
    set($coll.members, @items);
}
sub insert_Ref(Any $coll, *@items) {
    @items[0] || $coll;
}

sub remove_Hash(Hash $coll, *@items) returns Hash {
    # hmm, remove by value ;)
    my $dontwant = set(@items);
    my %newcoll;
    for $coll.pairs -> $k, $v {
	%newcoll{$k} = $v unless $dontwant.includes($v);
    }
    %newcoll;
}
sub remove_Array(Array $coll, *@items) returns Array {
    my $dontwant = set(@items);
    my @newlist = $coll.grep:{ !$dontwant.has($_) };
    return @newlist;
}
sub remove_Set(Set $coll, *@items) returns Set {
    $coll - set(@items);
}
sub remove_Ref(Any $coll, *@items) returns Any {
    set(@items).member($coll);
}

sub size_Hash(Hash $coll, *@items) returns Int {
    $coll.elems;
}
sub size_Array(Array $coll, *@items) returns Int {
    $coll.elems;
}
sub size_Set(Set $coll, *@items) returns Int {
    $coll.elems;
}
sub size_Ref(Any $coll, *@items) returns Int {
    if ($coll.defined) {
	1
    } else {
	0
    }
}

