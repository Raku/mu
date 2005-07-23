package org.perl6.metamodel.tests;

import java.util.*;
import junit.framework.*;

import org.perl6.metamodel.*;

public class MethodTest extends TestCase {

	public static void main (String[] args) {
		junit.textui.TestRunner.run (suite());
	}

	public static Test suite() {
		return new TestSuite(MethodTest.class);
	}
	
	public void testMethod () {
	    MetaClass mc = new MetaClass("Foo");
	    
    	Method m = new Method (mc) {
    	    public Object code (Object inv, ArrayList args) {
    	        return "MyMethod";
    	    }
    	};
	
    	assertEquals(m.call("inv", new ArrayList()), "MyMethod");
	    assertEquals(m.associated_with(), mc);
	}
}