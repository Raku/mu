
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
    
    public void testSuperclasses () {
        MetaClass m1 = new MetaClass("Foo");
        MetaClass m2 = new MetaClass("Bar");
        MetaClass m3 = new MetaClass("Baz");
        
        ArrayList supers = new ArrayList();
        supers.add(m2);
        supers.add(m3);        
        m1.superclasses(supers);
        
        ArrayList _supers = m1.superclasses();
        assertEquals(_supers.get(0), m2);
        assertEquals(_supers.get(1), m3);        
    }     
    
    public void testIsA () {
        MetaClass m1 = new MetaClass("Foo");
        MetaClass m2 = new MetaClass("Bar");
        MetaClass m3 = new MetaClass("Baz");
        MetaClass m4 = new MetaClass("Blah");        

        ArrayList supers2 = new ArrayList();
        supers2.add(m3);
        supers2.add(m4);
        m2.superclasses(supers2);         

        ArrayList supers1 = new ArrayList();
        supers1.add(m2);        
        m1.superclasses(supers1);

        assertTrue(m1.is_a("Foo"));
        assertTrue(m1.is_a("Bar"));
        assertTrue(m1.is_a("Baz"));
        assertTrue(m1.is_a("Blah"));
                                
        assertTrue(m2.is_a("Bar"));        
        assertTrue(m2.is_a("Baz"));                
        assertTrue(m2.is_a("Blah")); 
        
        assertTrue(m3.is_a("Baz"));                        
        
        assertTrue(m4.is_a("Blah"));         
    }        
  
    public void testInstanceMethod () {
        MetaClass m = new MetaClass("Foo");
        
        Method method = new Method (m) {
            public Object code (Object inv, ArrayList args) {
                return "Foo.bar";
            }
        };
        
        m.add_method("bar", method);
        
        assertTrue(m.has_method("bar"));
        
        Method method2 = m.get_method("bar");
        assertEquals(method, method2);
        
        assertEquals(method2.call("inv", new ArrayList()), "Foo.bar");
    }  
  
    public void testExplicitInstanceMethod () {
        MetaClass m = new MetaClass("Foo");

        Method method = new Method (m) {
            public Object code (Object inv, ArrayList args) {
                return "Foo.bar";
            }
        };

        boolean exception_thrown = false;
        try {
            m.add_method("bar", method, "INSTANCE");
        } catch (Exception e) {
            exception_thrown = true;
        }
        assertFalse(exception_thrown);
        
        exception_thrown = false;
        try {
            assertTrue(m.has_method("bar", "INSTANCE"));
        } catch (Exception e) {
            exception_thrown = true;
        }
        assertFalse(exception_thrown);

        Method method2 = null;
        try {
            method2 = m.get_method("bar", "INSTANCE");
        } catch (Exception e) {}
            
        assertEquals(method, method2);
        assertEquals(method2.call("inv", new ArrayList()), "Foo.bar");
    }    
    
    public void testClassMethod () {
        MetaClass m = new MetaClass("Foo");

        Method method = new Method (m) {
            public Object code (Object inv, ArrayList args) {
                return "Foo.bar";
            }
        };

        boolean exception_thrown = false;
        try {
            m.add_method("bar", method, "CLASS");
        } catch (Exception e) {
            exception_thrown = true;
        }
        assertFalse(exception_thrown);

        exception_thrown = false;
        try {
            assertTrue(m.has_method("bar", "CLASS"));
        } catch (Exception e) {
            exception_thrown = true;
        }
        assertFalse(exception_thrown);

        Method method2 = null;
        try {
            method2 = m.get_method("bar", "CLASS");
        } catch (Exception e) {}

        assertEquals(method, method2);
        assertEquals(method2.call("inv", new ArrayList()), "Foo.bar");
    }     
}




