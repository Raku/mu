
package org.perl6.metamodel.tests;

import java.util.*;

import junit.framework.*;

import org.perl6.metamodel.Class;
import org.perl6.metamodel.Method;

public class ClassTest extends TestCase {

	private HashMap options;
	private Class foo;
	
	public static void main (String[] args) {
		junit.textui.TestRunner.run (suite());
	}

	public static Test suite() {
		return new TestSuite(ClassTest.class);
	}

	public void setUp () {
		options = new HashMap();
		
		// Set up the class data
		HashMap cls = new HashMap();
		HashMap cmeths = new HashMap();
		Method m1 = new Method () {
			public java.lang.Object code (java.lang.Object inv, ArrayList args) {
				return "Foo.bar";
			} 
		};	
		cmeths.put("bar", m1);
		cls.put("methods", cmeths);

		ArrayList cattrs = new ArrayList();
		cattrs.add("$.foo");
		cls.put("attrs", cattrs);
		
		options.put("class", cls);

		foo = new Class("Foo", options);
	}

	public void testConstructor () {
		assertEquals( foo.getClass().getName(), "org.perl6.metamodel.Class" );
		assertEquals( foo.meta().getClass().getName(), "org.perl6.metamodel.MetaClass" );
	}

	public void testIsA () {
		assertTrue(foo.isa("Foo"));
		assertFalse(foo.isa("Bar"));
	}

	public void testCan () {
		Method bar = foo.can("bar");
		assertEquals(bar.call(foo, new ArrayList()), "Foo.bar");
		
		assertNull(foo.can("wysiwyg"));
	}
}