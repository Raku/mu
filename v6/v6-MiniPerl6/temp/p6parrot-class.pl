use v6-alpha;

my @a = 5..10;

# for-loop

{
my $i = 0;
my $elems = (@a.elems) - 1;
(
    my &s = sub {
      say @a[$i];
      $i++;
      $i <= $elems and &s(1);
    }
)(1);
}

# class

my %obj; # = {};
%obj{'vtable'}{'meth'} = sub ($x) { say "here: $x "; $x };
%obj{'data'} = 123;
say (%obj{'vtable'}{'meth'})( 123 );
say (%obj{'vtable'}{'meth'})( %obj{'data'} );

%obj{'vtable'}{'meth'} = sub (%x) { say "here: %x{'data'} "; %x };
say (%obj{'vtable'}{'meth'})( %obj );

#my $obj2;
#$obj2 = %obj;

