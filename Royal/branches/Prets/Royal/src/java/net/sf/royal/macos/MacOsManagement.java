package net.sf.royal.macos;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

import net.sf.royal.Royal;


/**
 * Manage MacOS integration for BirDy
*/
public class MacOsManagement {

    public MacOsManagement(final Royal theApp) {
        try {
            Class<?> MacOsAppClass = Class.forName("com.apple.eawt.Application");

            Constructor<?> constructor = MacOsAppClass.getConstructor();
            
            Object MacOsAppInstance = constructor.newInstance();
            
            Class<?>[] booleanTypeParams = {boolean.class};
            Object[] booleantrueValueParams = {true};
            
            //Invoke addAboutMenuItem() method
            Method addAboutMenuItem = MacOsAppClass.getMethod("addAboutMenuItem");
            addAboutMenuItem.invoke(MacOsAppInstance);
            
            //Invoke addPreferencesMenuItem() method
            Method addPreferencesMenuItem = MacOsAppClass.getMethod("addPreferencesMenuItem");
            addPreferencesMenuItem.invoke(MacOsAppInstance);
            
            //Invoke setEnabledAboutMenu(true) method
            Method setEnabledAboutMenu = MacOsAppClass.getMethod("setEnabledAboutMenu", booleanTypeParams);
            setEnabledAboutMenu.invoke(MacOsAppInstance, booleantrueValueParams);
            
            //Invoke setEnabledPreferencesMenu(true) method
            Method setEnabledPreferencesMenu = MacOsAppClass.getMethod("setEnabledPreferencesMenu", booleanTypeParams);
            setEnabledPreferencesMenu.invoke(MacOsAppInstance, booleantrueValueParams);
            
            //ApplicationListener management
            
            Object proxyListener = Proxy.newProxyInstance(ClassLoader.getSystemClassLoader(),
                                                          new Class[] { Class.forName("com.apple.eawt.ApplicationListener")},
                                                          new ApplicationAdapterHandler() );
            
            Class<?>[] listenerTypeParam = {Class.forName("com.apple.eawt.ApplicationListener")};
            Method addApplicationListener = MacOsAppClass.getMethod("addApplicationListener", listenerTypeParam);
            
            Object[] listenerObjectParam = {proxyListener};
            addApplicationListener.invoke(MacOsAppInstance, listenerObjectParam);
        
        } catch (Exception e) {
            e.printStackTrace();
        }
    } 
}
