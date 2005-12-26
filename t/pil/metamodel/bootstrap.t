#!./pugs

use v6;
use Test::PIL::Bootstrap;

# check that we have our components

pil_is_eq('::Class`not_nil()',   'true', '... ::Class is defined');
pil_is_eq('::Object`not_nil()',  'true', '... ::Object is defined');
pil_is_eq('::Package`not_nil()', 'true', '... ::Package is defined');
pil_is_eq('::Module`not_nil()',  'true', '... ::Module is defined');
pil_is_eq('::Role`not_nil()',    'true', '... ::Role is defined');

# check the names, versions and authoritiies are correctly assigned

pil_is_eq('::Class.name()',   '"Class"',   '... ::Class.name.eq(Class)');
pil_is_eq('::Object.name()',  '"Object"',  '... ::Object.name.eq(Object)');
pil_is_eq('::Package.name()', '"Package"', '... ::Package.name.eq(Package)');
pil_is_eq('::Module.name()',  '"Module"',  '... ::Module.name.eq(Module)');
pil_is_eq('::Role.name()',    '"Role"',    '... ::Role.name.eq(Role)');

pil_is_eq('::Class.version()',   '"0.0.1"', '... ::Class.version.eq(0.0.1)');
pil_is_eq('::Object.version()',  '"0.0.1"', '... ::Object.version.eq(0.0.1)');
pil_is_eq('::Package.version()', '"0.0.1"', '... ::Package.version.eq(0.0.1)');
pil_is_eq('::Module.version()',  '"0.0.1"', '... ::Module.version.eq(0.0.1)');
pil_is_eq('::Role.version()',    '"0.0.1"', '... ::Role.version.eq(0.0.1)');

pil_is_eq('::Class.authority()',   '"url:pugscode.org"', '... ::Class.authority.eq(url:pugscode.org)');
pil_is_eq('::Object.authority()',  '"url:pugscode.org"', '... ::Object.authority.eq(url:pugscode.org)');
pil_is_eq('::Package.authority()', '"url:pugscode.org"', '... ::Package.authority.eq(url:pugscode.org)');
pil_is_eq('::Module.authority()',  '"url:pugscode.org"', '... ::Module.authority.eq(url:pugscode.org)');
pil_is_eq('::Role.authority()',    '"url:pugscode.org"', '... ::Role.authority.eq(url:pugscode.org)');

pil_is_eq('::Class.identifier()',   '"Class-0.0.1-url:pugscode.org"',   '... ::Class.identifier');
pil_is_eq('::Object.identifier()',  '"Object-0.0.1-url:pugscode.org"',  '... ::Object.identifier');
pil_is_eq('::Package.identifier()', '"Package-0.0.1-url:pugscode.org"', '... ::Package.identifier');
pil_is_eq('::Module.identifier()',  '"Module-0.0.1-url:pugscode.org"',  '... ::Module.identifier');
pil_is_eq('::Role.identifier()',    '"Role-0.0.1-url:pugscode.org"',    '... ::Role.identifier');

# check that they all have ::Class as thier $?CLASS

pil_is_eq('::Class.class()`eq(::Class)',   'true', '... ::Class.class.eq(Class)');
pil_is_eq('::Object.class()`eq(::Class)',  'true', '... ::Object.class.eq(Class)');
pil_is_eq('::Package.class()`eq(::Class)', 'true', '... ::Package.class.eq(Class)');
pil_is_eq('::Module.class()`eq(::Class)',  'true', '... ::Module.class.eq(Class)');
pil_is_eq('::Role.class()`eq(::Class)',    'true', '... ::Role.class.eq(Class)');

# check some of our is-a relationships

# Class

pil_is_eq('::Class.is_a(::Class)',   'true', '... ::Class.is_a(::Class)');
pil_is_eq('::Class.is_a(::Object)',  'true', '... ::Class.is_a(::Object)');
pil_is_eq('::Class.is_a(::Package)', 'true', '... ::Class.is_a(::Package)');
pil_is_eq('::Class.is_a(::Module)',  'true', '... ::Class.is_a(::Module)');

pil_is_eq('::Class.superclasses()`length()', '1', '... ::Class.superclasses().length() == 1');
pil_is_eq('::Class.superclasses()`fetch(0)`eq(::Module)', 'true', '... ::Class.superclasses().eq(Module)');

pil_is_eq('::Class.subclasses()', '[]', '... ::Class.subclasses() == nil');

# Module

pil_is_eq('::Module.is_a(::Module)',  'true', '... ::Module.is_a(::Module)');
pil_is_eq('::Module.is_a(::Package)', 'true', '... ::Module.is_a(::Package)');
pil_is_eq('::Module.is_a(::Object)',  'true', '... ::Module.is_a(::Object)');

pil_is_eq('::Module.superclasses()`length()', '1', '... ::Module.superclasses().length() == 1');
pil_is_eq('::Module.superclasses()`fetch(0)`eq(::Package)', 'true', '... ::Module.superclasses().eq(Package)');

pil_is_eq('::Module.subclasses()`length()', '2', '... ::Module.subclasses().length() == 1');
pil_is_eq('::Module.subclasses()`fetch(0)`eq(::Class)', 'true', '... ::Module.subclasses()[0].eq(Class)');
pil_is_eq('::Module.subclasses()`fetch(1)`eq(::Role)',  'true', '... ::Module.subclasses()[0].eq(Role)');

# Package

pil_is_eq('::Package.is_a(::Package)', 'true', '... ::Package.is_a(::Package)');
pil_is_eq('::Package.is_a(::Object)',  'true', '... ::Package.is_a(::Object)');

pil_is_eq('::Package.superclasses()`length()', '1', '... ::Package.superclasses().length() == 1');
pil_is_eq('::Package.superclasses()`fetch(0)`eq(::Object)', 'true', '... ::Package.superclasses()[0].eq(Object)');

pil_is_eq('::Package.subclasses()`length()', '1', '... ::Package.subclasses().length() == 1');
pil_is_eq('::Package.subclasses()`fetch(0)`eq(::Module)', 'true', '... ::Package.subclasses()[0].eq(Module)');

# Object

pil_is_eq('::Object.is_a(::Object)',  'true', '... ::Object.is_a(::Object)');

pil_is_eq('::Object.superclasses()', '[]', '... ::Object.superclasses() == []');

pil_is_eq('::Object.subclasses()`length()', '1', '... ::Object.subclasses().length() == 1');
pil_is_eq('::Object.subclasses()`fetch(0)`eq(::Package)', 'true', '... ::Object.subclasses()[0].eq(Package)');

# Role

pil_is_eq('::Role.is_a(::Role)',  'true', '... ::Role.is_a(::Role)');

pil_is_eq('::Role.superclasses()`length()', '1', '... ::Role.superclasses().length() == 1');
pil_is_eq('::Role.superclasses()`fetch(0)`eq(::Module)', 'true', '... ::Role.superclasses()[0].eq(Module)');

pil_is_eq('::Role.subclasses()`length()', '0', '... ::Role.subclasses().length() == o');

# check that our Role model bootstrapped properly

pil_is_eq('::Class.does("Role")', 'true', '... ::Class.does(Role)');
pil_is_eq('::Role.does("Role")', 'true', '... ::Role.does(Role)');

pil_is_eq('::Object.does("Role")', 'false', '... ! ::Object.does(Role)');
pil_is_eq('::Package.does("Role")', 'false', '... ! ::Package.does(Role)');
pil_is_eq('::Module.does("Role")', 'false', '... ! ::Module.does(Role)');




