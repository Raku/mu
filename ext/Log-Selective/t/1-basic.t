#!/usr/bin/pugs

use Test;

plan 4;
use_ok ::Log::Selective;
lives_ok { Log::Selective::select(:foo) }, "Selected tag ':foo'";

ok (note "This should be output", :foo), 'tag in list';
ok not(note "This should not be output", :bar), 'tag not in list';