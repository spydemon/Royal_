package net.sf.royal.macos;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

import net.sf.royal.Royal;
import net.sf.royal.gui.manager.MessagePaneManager;

public class ApplicationAdapterHandler implements InvocationHandler {

    private final String APPLICATION_EVENT = "com.apple.eawt.ApplicationEvent";
    
    public Object invoke(Object proxy, Method method, Object[] args)
            throws Throwable {
        Object lReturn = null;

        if (method.getDeclaringClass().equals(Object.class)) {
            lReturn = method.invoke(this, args);
        } else {
            if (method.getName().equals("handleAbout")){
                this.handleAbout(args);
            } else if (method.getName().equals("handleOpenApplication")) {
                this.handleOpenApplication(args);
            } else if (method.getName().equals("handleOpenFile")) {
                this.handleOpenFile(args);
            } else if (method.getName().equals("handleOpenApplication")) {
                this.handleOpenApplication(args);
            } else if (method.getName().equals("handlePreferences")) {
                this.handlePreferences(args);
            } else if (method.getName().equals("handlePrintFile")) {
                this.handlePrintFile(args);
            } else if (method.getName().equals("handleQuit")) {
                this.handleQuit(args);
            } else if (method.getName().equals("handleReOpenApplication")) {
                this.handleReOpenApplication(args);
            }
        }
        return lReturn;
    }

    private void handleAbout(Object[] args) {
        MessagePaneManager.showAboutDialog();
        //Do : evt.setHandled(true);
        try {
            Object instance = args[0];
            Class<?> appEvent = Class.forName(APPLICATION_EVENT);
            Class<?>[] parameterTypes = {boolean.class};
            Method setHandled = appEvent.getMethod("setHandled", parameterTypes);	/* Import the method corresponding to the class */
            
            Object[] paramaterObjects = {Boolean.TRUE};
            setHandled.invoke(instance, paramaterObjects);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void handleOpenApplication(Object[] args) {
        //Do nothing
    }

    private void handleOpenFile(Object[] args) {
        //Do nothing
    }

    private void handlePreferences(Object[] args) {
        //TODO: change to perspective ?
    }

    private void handlePrintFile(Object[] args) {
        //Do nothing
    }

    private void handleQuit(Object[] args) {
        Royal.close(false);
    }

    private void handleReOpenApplication(Object[] args){
        //Do nothing
    }
}
