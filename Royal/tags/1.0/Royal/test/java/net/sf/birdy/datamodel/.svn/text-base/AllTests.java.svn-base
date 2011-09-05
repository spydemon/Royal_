package net.sf.birdy.datamodel;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

    public static Test suite() {
        TestSuite suite = new TestSuite("Test for net.sf.birdy.datamodel");
		try
		{
			HibernateUtil.initSessionFactory();
		} catch (Exception e) {
			e.printStackTrace();
		}
        //$JUnit-BEGIN$
        suite.addTestSuite(InsertionTest.class);
        //$JUnit-END$
        return suite;
    }

}
