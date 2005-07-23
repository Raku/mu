
package org.perl6.metamodel.tests;

import junit.framework.*;

public class AllTests extends TestCase {

	public static void main (String[] args) {
		junit.textui.TestRunner.run (suite());
	}

    public static Test suite () {
        TestSuite suite = new TestSuite("org.perl6.metamodel.tests.AllTests");        
        suite.addTest(MetaClassTest.suite());
        suite.addTest(MROTest.suite()); 
        suite.addTest(MethodTest.suite());                           
        return suite;
    }

}