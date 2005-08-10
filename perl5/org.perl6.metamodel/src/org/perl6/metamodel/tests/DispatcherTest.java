
package org.perl6.metamodel.tests;

import java.util.*;
import junit.framework.*;

import org.perl6.metamodel.MetaClass;
import org.perl6.metamodel.metaclass.Dispatcher;

public class DispatcherTest extends TestCase {

		
	public static void main (String[] args) {
		junit.textui.TestRunner.run(suite());
	}

	public static Test suite() {
		return new TestSuite(DispatcherTest.class);
	}

	
	public void testDispatcher () {
		MetaClass a = new MetaClass("A");
		MetaClass b = new MetaClass("B");
		ArrayList bsup = new ArrayList();
		bsup.add(a);
		b.superclasses(bsup);
		
		MetaClass c = new MetaClass("C");
		ArrayList csup = new ArrayList();
		csup.add(a);
		c.superclasses(csup);
		MetaClass d = new MetaClass("D");
		ArrayList dsup = new ArrayList();
		dsup.add(b);
		dsup.add(c);
		d.superclasses(dsup);

		Dispatcher dispatcher = d.dispatcher(":canonical");
		
		assertEquals(dispatcher.getClass().getName(), "org.perl6.metamodel.metaclass.Dispatcher");
		
		assertEquals(dispatcher.next(), d);
		assertEquals(dispatcher.next(), b);
		assertEquals(dispatcher.next(), c);
		assertEquals(dispatcher.next(), a);
	
	}
}
