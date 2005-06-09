#!/usr/bin/pugs

use v6;
use Test;

plan 12;

use_ok('CGI::Util');

is make_attribute({ foo => "bar" }), <foo="bar">, 'single attribute';
is make_attribute({ baz => "quux", foo => "bar" }), <baz="quux" foo="bar">, 'two attributes';
is make_attribute({ foo => "bar", baz => "quux" }), <baz="quux" foo="bar">, 'two attributes (in alphabetical order)';

is make_attribute({ foo => q/bar"bar/ }), <foo="bar"bar">, 'no escaping';
is make_attribute({ foo => q/bar<bar/ }), eval('<foo="bar<bar">'), 'no escaping';
is make_attribute({ foo => q/bar>bar/ }), <foo="bar\>bar">, 'no escaping';
is make_attribute({ foo => q/bar&bar/ }), <foo="bar&bar">, 'no escaping';

is make_attribute({ foo => q/bar"bar/ }, escape => 1), <foo="bar&quot;bar">, 'escaping';
is make_attribute({ foo => q/bar<bar/ }, escape => 1), <foo="bar&lt;bar">, 'escaping';
is make_attribute({ foo => q/bar>bar/ }, escape => 1), <foo="bar&gt;bar">, 'escaping';
is make_attribute({ foo => q/bar&bar/ }, escape => 1), <foo="bar&amp;bar">, 'escaping';
