#!/usr/bin/pugs

use Test;

plan 4;
lives_ok { %ENV<SELECT_LOGS>='foo' }, "Selected tag 'foo' via environment";
use_ok ::Log::Selective;

ok (note "This should be output", :foo), 'tag in list';
ok not(note "This should not be output", :bar), 'tag not in list';