
package org.perl6.metamodel.tests;

import java.util.*;
import junit.framework.*;

import org.perl6.metamodel.*;

public class MetaClassTest extends TestCase {

	public static void main (String[] args) {
		junit.textui.TestRunner.run (suite());
	}
    
	public static Test suite() {
		return new TestSuite(MetaClassTest.class);
	}
    
    public void testNameOnlyConstructor () {
        MetaClass m = new MetaClass("Foo");
        assertEquals(m.name(), "Foo");
        assertEquals(m.version(), "0.0.0");        
        assertNull(m.authority());                
    }      
    
    public void testNameVersionConstructor () {
        MetaClass m = new MetaClass("Foo", "0.0.1");
        assertEquals(m.name(), "Foo");
        assertEquals(m.version(), "0.0.1");        
        assertNull(m.authority());                
    }       
    
    public void testNameVersionAuthorityConstructor () {
        MetaClass m = new MetaClass("Foo", "0.0.1", "javasan:STEVAN");
        assertEquals(m.name(), "Foo");
        assertEquals(m.version(), "0.0.1");        
        assertEquals(m.authority(), "javasan:STEVAN");                
    }                                                                                                
    
    public void testNameMutator () {
        MetaClass m = new MetaClass("Foo");
        assertEquals(m.name(), "Foo");
        m.name("Bar");      
        assertEquals(m.name(), "Bar");        
    }       
    
    public void testVersionMutator () {
        MetaClass m = new MetaClass("Foo");
        assertEquals(m.version(), "0.0.0");
        m.version("0.1.0");      
        assertEquals(m.version(), "0.1.0");        
    }     
    
    public void testAuthorityMutator () {
        MetaClass m = new MetaClass("Foo");
        assertNull(m.authority());
        m.authority("javasan:STEVAN");      
        assertEquals(m.authority(), "javasan:STEVAN");        
    }     
    
    public void testIdentifier () {
        MetaClass m = new MetaClass("Foo", "0.0.1", "javasan:STEVAN");
        assertEquals(m.identifier(), "Foo-0.0.1-javasan:STEVAN");  
        
        MetaClass m2 = new MetaClass("Foo", "0.0.1");
        assertEquals(m2.identifier(), "Foo-0.0.1");
        
        MetaClass m3 = new MetaClass("Foo");
        assertEquals(m3.identifier(), "Foo-0.0.0");                                
    }     
    
}




