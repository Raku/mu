/*
Copyright (C) 2007, Flavio S. Glock.

=head1 NAME

mp6.c - An MP6 Runtime

=head1 DESCRIPTION

This file implements a runtime for the MP6 language.

=head2 Functions

=over 4

=cut

*/

#include <stdio.h>

// TODO
// create_instance
// destroy
// garbage collector
// fetch_by_key - ok
// fetch_by_name - ok
// add_method
// alias_method - ok, with store_by_key()
// HOW - ok
// new, build, ...
// lexical pads - ok
//   - OUTER needs some work
//   - pads are objects - is this a problem?
// namespaces
// accessors
// captures
// MMD
// Scalar, Array, Hash
// split the source code into several files
// mp6/kp6 code generator
// @NAMESPACES
// @ISA, @DOES, ID, SUPER
// unicode
// threads

// TODO 
// method, classes  are objects
//   - method boxing is unfinished
// string is object - ok
// int, num are objects
// closure object
// metaclass - ok
// 'Object' class
// rename 'Str' to 'Buf'

typedef code (void (void));

typedef struct {
	int  key;
	char * name;
	code * method;
} method_key;

typedef struct {
    method_key * class;
    char * data;
} instance;

typedef enum {
    METH_no_such_method,   // key number zero, used for error messages
	METH_HOW,
	METH_name,
	METH_name_internal,
	METH_apply,  // TODO
	METH_FETCH,  // TODO
	METH_STORE,  // TODO
	METH_BIND,   // TODO - I think this should be a 'pad' method
	METH_index,  // TODO - Array
	METH_lookup, // TODO - Hash
} method_name;

// method protos
instance * 
my_method ( instance * self, char * data );

instance * 
meth_how  ( instance * self, char * data );

instance * 
meth_name ( instance * self, char * data );

instance * 
meth_name_metaclass ( instance * self, char * data );

instance * 
meth_name_method ( instance * self, char * data );

instance * 
meth_apply ( instance * self, char * data );

instance * 
meth_name_str ( instance * self, char * data );

instance * 
meth_name_myclass ( instance * self, char * data );

// default method (error)

instance * 
error_undefined_method ( instance * self, char * data )
{
	printf( "Error: called an undefined method\n" );
	return self;
}


// class tables, indexed by hash key
// TODO - move names to main dictionary

method_key Class_Metaclass[] = {
	{ METH_HOW,				"HOW",			(code *)&meth_how },
	{ METH_name,			"name",			(code *)&meth_name },
	{ METH_name_internal,	"__name",		(code *)&meth_name_metaclass },
	{ METH_no_such_method,	"",				(code *)&error_undefined_method },
};

method_key Class_Method[] = {
	{ METH_HOW,				"HOW",			(code *)&meth_how },
	{ METH_name,			"name",			(code *)&meth_name },
	{ METH_name_internal,	"__name",		(code *)&meth_name_method },
	{ METH_apply,			"apply",		(code *)&meth_apply },
	{ METH_no_such_method,	"",				(code *)&error_undefined_method },
};

method_key Class_Str[] = {
	{ METH_HOW,				"HOW",			(code *)&meth_how },
	{ METH_name_internal,	"__name",		(code *)&meth_name_str },
	{ METH_no_such_method,	"",				(code *)&error_undefined_method },
};

method_key Class_MyClass[] = {
	{ METH_HOW,				"HOW",			(code *)&meth_how },
	{ METH_name_internal, 	"__name",		(code *)&meth_name_myclass },
	{ 123,					"my_method",	(code *)&my_method },
	{ METH_no_such_method,	"",				(code *)&error_undefined_method },
};

method_key Pad_MyPad[] = {
	{ 1234,					"my_var",       (code *)0 },
	{ METH_no_such_method,	"",				(code *)&error_undefined_method },
};

/*

=item C<code * fetch_by_key ( instance * self, int key )>

Returns the method entry point, given a method hash key. 
Method hash keys are calculated at compile time.

XXX - for methods, the returned value needs to be cast with:

  ((instance * (*) ( instance *, char * )) 

=cut

*/

	// TODO - indexed lookup for larger sets
	// TODO - lookup and cache method
	// TODO - exception handler
	// TODO - 'internal' methods like .HOW can be positional; wouldn't need to be looked up

code *
fetch_by_key ( instance * self, int key )
{
    method_key * meth;
    for ( meth = (*self).class; ; meth++ ) {
		//printf( "lookup: key=%d\n", (*meth).key );
		if ( (*meth).key == key ) {
			return (*meth).method;
		}
		if ( ! (*meth).key ) {
			printf( "Error: lookup: no such method, key=%d\n", key );
			return (*meth).method;
		}
	}
}

/*

=item C<code * store_by_key ( instance * self, int key, code * method )>

Replace an existing method, given a method hash key and a method pointer. 
Method hash keys are calculated at compile time.

=cut

*/

code *
store_by_key ( instance * self, int key, code * method )
{
    method_key * meth;
    for ( meth = (*self).class; ; meth++ ) {
		//printf( "lookup: key=%d\n", (*meth).key );
		if ( (*meth).key == key ) {
            (*meth).method = method;
			return method;
		}
		if ( ! (*meth).key ) {
			printf( "Error: store: no such method, key=%d\n", key );
			return (*meth).method;
		}
	}
}

/*

=item C<code * fetch_by_name ( instance * self, char * name )>

Returns the method entry point, given a method mangled name. 
Method mangled names are calculated at compile time.

XXX - The returned value needs to be cast with:

  ((instance * (*) ( instance *, char * )) 

=cut

*/

	// TODO - lookup in main dictionary, then lookup by key
	// TODO - indexed lookup for larger sets
	// TODO - lookup and cache method
	// TODO - exception handler

code *
fetch_by_name ( instance * self, char * name )
{
    method_key * meth;
    for ( meth = (*self).class; ; meth++ ) {
		if ( strcmp( (*meth).name, name ) == 0 ) {
			return (*meth).method;
		}
		if ( ! (*meth).name ) {
			printf( "Error: lookup: no such method, name=%s\n", name );
			return (*meth).method;
		}
	}
}

// metaclass methods

instance * 
meth_how ( instance * self, char * data )
{
	// TODO - malloc
	// TODO - metaclass should be a singleton
    static instance return_obj;
	// returns the metaclass 
	return_obj.class = (method_key *)&Class_Metaclass;
	return_obj.data = (char *)self;
	return &return_obj;
}

instance * 
meth_name ( instance * self, char * data )
{
	instance * class;
	class = (instance *)( (*self).data );
	return
	((instance * (*) ( instance *, char * ))
	 ( fetch_by_key( class, METH_name_internal ) ))( class, "" );
}

instance * 
meth_name_metaclass ( instance * self, char * data )
{
	// "name" is a singleton
    static instance return_obj;
	return_obj.class = (method_key *)&Class_Str;
	return_obj.data = "MetaClass";
	return &return_obj;
}

// TODO - box methods
//instance *
//initialize_metaclass ()
//{
//    static instance MetaClass;
//	MetaClass.class = (method_key *)&Class_Method;
//	MetaClass.data = (char *)&meth_how;
//
//}


// 'Method' methods

instance * 
meth_name_method ( instance * self, char * data )
{
	// "name" is a singleton
    static instance return_obj;
	return_obj.class = (method_key *)&Class_Str;
	return_obj.data = "Method";
	return &return_obj;
}

instance * 
meth_apply ( instance * self, char * data )
{
	// TODO
	printf( "Error: Method.apply not implemented yet\n" );
	return self;
}


// Str methods

instance * 
meth_name_str ( instance * self, char * data )
{
	// "name" is a singleton
    static instance return_obj;
	return_obj.class = (method_key *)&Class_Str;
	return_obj.data = "Str";
	return &return_obj;
}

// user methods

// internal method - class name
instance * 
meth_name_myclass ( instance * self, char * data )
{
	// "name" is a singleton
    static instance return_obj;
	return_obj.class = (method_key *)&Class_Str;
	return_obj.data = "MyClass";
	return &return_obj;
}

instance * 
my_method ( instance * self, char * data )
{
	return self;
}

int 
main ( int argc, char* argv[] ) 
{
    instance my_obj;
    my_obj.class = (method_key *)&Class_MyClass;
    my_obj.data = "object_data";
    printf( "created object\n" );
    instance * return_obj;

		return_obj = 
			((instance * (*) ( instance *, char * ))
			( fetch_by_key( &my_obj, 123 ) ))( &my_obj, "" );

    printf( "obj: %s\n", (*return_obj).data );

	// $obj.HOW
		return_obj = 
			((instance * (*) ( instance *, char * ))
			( fetch_by_key( &my_obj, METH_HOW ) ))( &my_obj, "" );
	// $obj.HOW.name
		return_obj = 
			((instance * (*) ( instance *, char * ))
			( fetch_by_key( return_obj, METH_name ) ))( return_obj, "" );

    printf( "obj.HOW.name: %s\n", (*return_obj).data );

	// $obj.HOW
		return_obj = 
			((instance * (*) ( instance *, char * ))
			( fetch_by_key( &my_obj, METH_HOW ) ))( &my_obj, "" );
	// $obj.HOW.HOW
		return_obj = 
			((instance * (*) ( instance *, char * ))
			( fetch_by_key( return_obj, METH_HOW ) ))( return_obj, "" );
	// $obj.HOW.HOW.name
		return_obj = 
			((instance * (*) ( instance *, char * ))
			( fetch_by_key( return_obj, METH_name ) ))( return_obj, "" );

    printf( "obj.HOW.HOW.name: %s\n", (*return_obj).data );


    printf( "variable binding and lookup:\n" );
    // XXX - unnecessary boxing? - call fetch_by_key() using a vtable pointer instead
    instance my_pad;
    my_pad.class = (method_key *)&Pad_MyPad;
    my_pad.data = "";   // XXX - unused field
    // Note: store_by_key( fetch_by_key() ) makes an alias (but not a binding, because it's not a container operation)

    // $my_var := "Hello";
    // Note: no runtime type-checking
    instance my_str;
    my_str.class = (method_key *)&Pad_MyPad;
    my_str.data = "Hello";
	store_by_key( &my_pad, 1234, (code *)&my_str );    // bind the string to the pad

    instance * my_str2;
    my_str2 =
			(instance *)
			( fetch_by_key( &my_pad, 1234 ) );
    printf( "got value: %s\n", (*my_str2).data );
    my_str2 =
			(instance *)
			( fetch_by_name( &my_pad, "my_var" ) );
    printf( "got value: %s\n", (*my_str2).data );

/*
	printf( "benchmarking method lookup and call\n" );

	long int i;
	code * meth;
	
	for ( i = 0; i < 55000000; i++ ) {
		//return_obj = 
		//	((instance * (*) ( instance *, char * ))
		//	( fetch_method( &my_obj, 123 ) ))( &my_obj, "" );
		meth = fetch_by_key( &my_obj, 123 );
		return_obj = 
		( (instance * (*) ( instance *, char * ))
		  (meth) )( &my_obj, "" );
		
		// reuse the cached method
		//return_obj = 
		//( (instance * (*) ( instance *, char * ))
		//  (meth) )( &my_obj, "" );
	}

    printf( "obj: %s\n", (*return_obj).data );
*/

    return 0;

}

