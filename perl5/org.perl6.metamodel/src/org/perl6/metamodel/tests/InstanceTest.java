
package org.perl6.metamodel.tests;

import java.util.*;

import junit.framework.*;

import org.perl6.metamodel.Class;
import org.perl6.metamodel.Instance;
import org.perl6.metamodel.Method;

public class InstanceTest extends TestCase {

	private HashMap options;
	private Instance foo;
	
	public static void main (String[] args) {
		junit.textui.TestRunner.run (suite());
	}

	public static Test suite() {
		return new TestSuite(InstanceTest.class);
	}

	public void setUp () {
		options = new HashMap();
	
		// Set up the instance data
		HashMap inst = new HashMap();
		HashMap imeths = new HashMap();
		Method m1 = new Method () {
			public java.lang.Object code (java.lang.Object inv, ArrayList args) {
				return "Foo.bar";
			} 
		};
		imeths.put("baz", m1);
		inst.put("methods", imeths);
				
		options.put("instance", inst);
		Class Foo = new Class("Foo", options);
		foo = new Instance(Foo);
	}

	public void testConstructor () {
		assertEquals( foo.getClass().getName(), "org.perl6.metamodel.Instance" );
	}

	public void testIsA () {
		assertTrue(foo.isa("Foo"));
		assertFalse(foo.isa("Bar"));
	}

	public void testCan () {
		
		Method m = foo.can("baz");
		assertEquals(m.call(foo, new ArrayList()),"Foo.bar");
		
		assertNull(foo.can("sglerbadck"));
	}

}