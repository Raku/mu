#!/usr/bin/pugs
use v6;

##########################
# OO solution by eric256 #
##########################

my @loops = ([1..3], ['a'..'e'], ['foo', 'bar']);

class MultiCounter {
      has @.positions is rw;
      has @.lengths   is rw;
      submethod BUILD() { @.positions = (0) xx (@.lengths.end + 1); }
      method inc () returns Bool {
        @.positions[-1]++;
        for reverse (0..@.lengths.end - 1) -> $i {
         if (@.positions[$i + 1] > @.lengths[$i + 1]) {
            @.positions[$i]++;
            @.positions[$i + 1] = 0;
         }
        }
        return (@.positions[0] > @.lengths[0]) ?? 0 !! 1;             
      }                
}

class NestedLoops {
      has @.loops is rw;
      has MultiCounter $.counter is r;
      submethod BUILD($self:) {
          $self.reset;
      }
      method reset () {
          $.counter = MultiCounter.new( :lengths( @.loops.map:{.end } ) );
      }
      method iter () returns Bool { $.counter.inc;  }
      method iter ($self: Code $code) {
             while ($self.iter) { $code($self.data) };
             $self.reset;
      }
      method data () {
          return map -> $i { @.loops[$i][ $.counter.positions[$i] ] } 0 .. @.loops.end;
      };
}

my $counter = NestedLoops.new( :loops(@loops) );

$counter.iter( sub { say "CODE BLOCK " ~ @_ } );
while ($counter.iter) { say "WHILE LOOP " ~ $counter.data.join(" "); }

