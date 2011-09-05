package net.sf.royal.exception;

import net.sf.royal.Royal;
import net.sf.royal.gui.manager.MessagePaneManager;

import org.apache.log4j.Logger;


/**
 * Define the kind of exception which could occures in BirDy
 * @author bibounde
 */
public abstract class RoyalException extends Exception {

	/**
	 * Constant if the exception is fatal, and if the
	 * application must be closed.
	 */
    public static boolean FATAL = Boolean.TRUE.booleanValue();
    /**
     * Constant if the application can continue to run after the
     * exception management .
     */
    public static boolean CONTINUE = Boolean.FALSE.booleanValue(); 
    
    private Throwable throwable;
    private boolean fatal;
    
    /**
     * Instance to use log4j and log the error(s)
     */
    private final static Logger logger = 
		Logger.getLogger(RoyalException.class);
    
    /**
     * Constructor of the exception
     * @param throwable
     * @param fatal
     * @see RoyalException#FATAL
     * @see RoyalException#CONTINUE
     */
    public RoyalException(Throwable throwable, boolean fatal){
        this.throwable = throwable;
        this.fatal = fatal;
    }
    
	/**
	 * Print the error and its backtrace
	 */
    public StackTraceElement[] getStackTrace() {
        return this.throwable.getStackTrace();
    }

	/**
	 * Print the error and its backtrace
	 */
    public void printStackTrace() {
        this.throwable.printStackTrace();
    }
    
    
	/**
	 * Open a dialog to signal the exception and if fatal, exit BirDy
	 * @see MessagePaneManager
	 */
    public void manageException(){
        this.throwable.printStackTrace();
        boolean close = MessagePaneManager.showExceptionPane(this.throwable, this.fatal);
        logger.error(this.throwable.getMessage());
        if (close){
            Royal.pm.removeFile();
            System.exit(0);
        }
    }

    /**
     * @return Returns the fatal.
     */
    public boolean isFatal() {
        return fatal;
    }
}
