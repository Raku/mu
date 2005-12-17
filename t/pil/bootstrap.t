#!/usr/bin/pugs

use v6;
use Test::PIL::Bootstrap;

# check that we have our components

pil_is_ok('::Class',   '... ::Class is defined');
pil_is_ok('::Object',  '... ::Object is defined');
pil_is_ok('::Package', '... ::Package is defined');
pil_is_ok('::Module',  '... ::Module is defined');

# check the names, versions and authoritiies are correctly assigned

pil_is_eq('::Class.name()', '"Class"', '... ::Class.name.eq(Class)');
pil_is_eq('::Object.name()', '"Object"', '... ::Object.name.eq(Object)');
pil_is_eq('::Package.name()', '"Package"', '... ::Package.name.eq(Package)');
pil_is_eq('::Module.name()', '"Module"', '... ::Module.name.eq(Module)');

pil_is_eq('::Class.version()', '"0.0.1"', '... ::Class.version.eq(0.0.1)');
pil_is_eq('::Object.version()', '"0.0.1"', '... ::Object.version.eq(0.0.1)');
pil_is_eq('::Package.version()', '"0.0.1"', '... ::Package.version.eq(0.0.1)');
pil_is_eq('::Module.version()', '"0.0.1"', '... ::Module.version.eq(0.0.1)');

pil_is_eq('::Class.authority()', '"url:pugscode.org"', '... ::Class.authority.eq(url:pugscode.org)');
pil_is_eq('::Object.authority()', '"url:pugscode.org"', '... ::Object.authority.eq(url:pugscode.org)');
pil_is_eq('::Package.authority()', '"url:pugscode.org"', '... ::Package.authority.eq(url:pugscode.org)');
pil_is_eq('::Module.authority()', '"url:pugscode.org"', '... ::Module.authority.eq(url:pugscode.org)');

pil_is_eq('::Class.identifier()', '"Class-0.0.1-url:pugscode.org"', '... ::Class.identifier');
pil_is_eq('::Object.identifier()', '"Object-0.0.1-url:pugscode.org"', '... ::Object.identifier');
pil_is_eq('::Package.identifier()', '"Package-0.0.1-url:pugscode.org"', '... ::Package.identifier');
pil_is_eq('::Module.identifier()', '"Module-0.0.1-url:pugscode.org"', '... ::Module.identifier');

# check that they all have ::Class as thier $?CLASS

pil_is_eq('::Class.class().name()', '"Class"', '... ::Class.class.name.eq(Class)');
pil_is_eq('::Object.class().name()', '"Class"', '... ::Object.class.name.eq(Class)');
pil_is_eq('::Package.class().name()', '"Class"', '... ::Package.class.name.eq(Class)');
pil_is_eq('::Module.class().name()', '"Class"', '... ::Module.class.name.eq(Class)');

# check some of our is-a relationships

pil_is_eq('::Class.superclasses().length()', '1', '... ::Class.superclasses().length() == 1');
pil_is_eq('::Class.superclasses().fetch(0).name', '"Module"', '... ::Class.superclasses()[0].name == Module');

pil_is_eq('::Class.subclasses()', '[]', '... ::Class.subclasses() == nil');

pil_is_eq('::Module.superclasses().length()', '1', '... ::Module.superclasses().length() == 1');
pil_is_eq('::Module.superclasses().fetch(0).name', '"Package"', '... ::Module.superclasses()[0].name == Package');

pil_is_eq('::Module.subclasses().length()', '1', '... ::Module.subclasses().length() == 1');
pil_is_eq('::Module.subclasses().fetch(0).name', '"Class"', '... ::Module.subclasses()[0].name == Class');

pil_is_eq('::Package.superclasses().length()', '1', '... ::Package.superclasses().length() == 1');
pil_is_eq('::Package.superclasses().fetch(0).name', '"Object"', '... ::Package.superclasses()[0].name == Object');

pil_is_eq('::Package.subclasses().length()', '1', '... ::Package.subclasses().length() == 1');
pil_is_eq('::Package.subclasses().fetch(0).name', '"Module"', '... ::Package.subclasses()[0].name == Module');

pil_is_eq('::Object.superclasses()', 'nil', '... ::Object.superclasses() == nil');

pil_is_eq('::Object.subclasses().length()', '1', '... ::Object.subclasses().length() == 1');
pil_is_eq('::Object.subclasses().fetch(0).name', '"Package"', '... ::Object.subclasses()[0].name == Package');

# check some of the MROs

pil_is_eq('::Class.MRO().length()', '4', '... ::Class.MRO().length() == 4');
pil_is_eq('::Class.MRO().fetch(0).name', '"Class"', '... ::Class.MRO()[0].name == Class');
pil_is_eq('::Class.MRO().fetch(1).name', '"Module"', '... ::Class.MRO()[1].name == Module');
pil_is_eq('::Class.MRO().fetch(2).name', '"Package"', '... ::Class.MRO()[2].name == Package');
pil_is_eq('::Class.MRO().fetch(3).name', '"Object"', '... ::Class.MRO()[3].name == Object');

pil_is_eq('::Module.MRO().length()', '3', '... ::Module.MRO().length() == 3');
pil_is_eq('::Module.MRO().fetch(0).name', '"Module"', '... ::Module.MRO()[0].name == Module');
pil_is_eq('::Module.MRO().fetch(1).name', '"Package"', '... ::Module.MRO()[1].name == Package');
pil_is_eq('::Module.MRO().fetch(2).name', '"Object"', '... ::Module.MRO()[2].name == Object');

pil_is_eq('::Package.MRO().length()', '2', '... ::Package.MRO().length() == 2');
pil_is_eq('::Package.MRO().fetch(0).name', '"Package"', '... ::Package.MRO()[0].name == Package');
pil_is_eq('::Package.MRO().fetch(1).name', '"Object"', '... ::Package.MRO()[1].name == Object');

pil_is_eq('::Object.MRO().length()', '1', '... ::Object.MRO().length() == 1');
pil_is_eq('::Object.MRO().fetch(0).name', '"Object"', '... ::Object.MRO()[0].name == Object');






