use v6-alpha;

use Test;

# Basic &return tests
# L<S06/"The C<return> function">

plan 3;

{
  my sub userdefinedcontrol (&block) { &block(); return 24 }
  my sub official {
    userdefinedcontrol { return 42 };
  }
  is official(), 42, "bare blocks are invisible to return";
}

{
  my sub userdefinedcontrol (&block) { &block(); return 24 }
  my sub official {
    {
	{
	    userdefinedcontrol { return 42 };
	}
    }
  }
  is official(), 42, "nested bare blocks are invisible to return";
}

{
  my sub userdefinedcontrol ($value, &block) { &block($value); return 24 }
  my sub official($value) {
    {
	userdefinedcontrol $value, -> $x { return $x };
    }
  }
  is official(42), 42, "pointy blocks are invisible to return";
}
