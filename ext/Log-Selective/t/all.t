#!/usr/bin/pugs

use Test;

plan 4;
use_ok ::Log::Selective;
lives_ok { Log::Selective::select(:foo, :all) }, "Selected tags ':foo' and ':all'";

ok (note "This should be output", :foo), 'tag in list';
ok (note "This should also be output", :bar), 'tag not in list (but caught by :all)';