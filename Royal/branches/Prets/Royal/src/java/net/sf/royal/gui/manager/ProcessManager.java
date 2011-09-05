package net.sf.royal.gui.manager;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;

import net.sf.royal.exception.RoyalException;
import net.sf.royal.exception.DefaultException;


/**
  * Manage the lock when the app is opened
  * @author bibounde
  */
public class ProcessManager {

    private File file = null; 
    
	/**
	  * ProcessManager constructor, check and create the lock
	  */
    public ProcessManager(){
        file = new File("royal.lock");
		// If the file exist, stop BirDy's start
        if (file.canRead()){
            MessagePaneManager.showProcessPane();
            System.exit(0);
		// Else, create the lock file (continue if there is an error)
        } else {
            try {
                OutputStreamWriter writer = new OutputStreamWriter(new FileOutputStream(file));
                writer.write("Royal is running");
                writer.close();
            } catch (Exception e) {
                RoyalException be = new DefaultException(e, RoyalException.CONTINUE);
                be.manageException();
            }
        }
    }
    
	/**
	  * When the program is quitting, remove lock file.
	  */
    public void removeFile(){
        file.delete();
    }
}
