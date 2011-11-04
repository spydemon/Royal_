package net.sf.royal;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.sql.SQLException;
import java.util.Locale;

import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.JPanel;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;

import net.sf.royal.datamodel.HibernateUtil;
import net.sf.royal.exception.DefaultException;
import net.sf.royal.exception.RoyalException;
import net.sf.royal.gui.manager.ArchiManager;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.MenuBarManager;
import net.sf.royal.gui.manager.ProcessManager;
import net.sf.royal.gui.manager.PropertyManager;
import net.sf.royal.gui.pane.AlbumPane;
import net.sf.royal.gui.pane.BottomBarPane;
import net.sf.royal.gui.pane.MainPane;
import net.sf.royal.gui.pane.MainToolBar;
import net.sf.royal.macos.MacOsManagement;

import org.apache.log4j.Logger;
import org.hibernate.HibernateException;

/**
    * Creation of the main window of Royal
    * @author bibounde
    * @author Soulou
    */
public class Royal extends JFrame
{
	private final String sLookAndFeel = "Nimbus";

	private MainToolBar north = null;
	private MainPane center = null;
	private JPanel bottom = null;
	private JMenuBar menu = null;

    public static ProcessManager pm = null;
    public static Container contentPane = null;
    public static Royal applicationInstance = null;
    
    private final static Logger logger = Logger.getLogger(Royal.class);
 
	/**
	  * Main constructor
      */	  
    private Royal(){
	        try {
				for(LookAndFeelInfo info : UIManager.getInstalledLookAndFeels())
				{
					if(this.sLookAndFeel.equals(info.getName()))
					{
						UIManager.setLookAndFeel(info.getClassName());
						break;
					}
				}
            } catch (Exception e) {
                e.printStackTrace();
            }

            logger.debug("Loading locales");
			Locale.getDefault();
			// Set up the LocaleManager (for the const Strings)
			Locale.setDefault(LocaleManager.getInstance().loadLocale());

			// Set up application lock	
			logger.debug("Setup application lock");
            pm = new ProcessManager();

			// Set up the file architecture ~/.royal/...
            logger.debug("Setup user files");
            ArchiManager.getInstance().initArchitecture();
            
			// Set up Hibernate for databases access
            logger.debug("Loading database");
            try {
                HibernateUtil.initSessionFactory();
            } catch (FileNotFoundException e1) 
            {
                e1.printStackTrace();
            } catch (IOException e1) 
            {
                e1.printStackTrace();
            } catch (HibernateException e) 
            {
                e.printStackTrace();
            } catch (SQLException e) 
            {
                e.printStackTrace();
            }
            
            logger.debug("Creating main window");
            this.setTitle("Royal MAIL DEV 01");
			this.north = MainToolBar.getInstance();
            this.bottom = BottomBarPane.getInstance();
			this.center = MainPane.getInstance();
			this.menu = MenuBarManager.getInstance().getMenuBar();
			this.getContentPane().add(this.north, BorderLayout.NORTH);
            this.getContentPane().add(this.bottom, BorderLayout.SOUTH);
			this.getContentPane().add(this.center, BorderLayout.CENTER);
            this.setJMenuBar(this.menu);
           
            this.addWindowListener(new WindowListener(){
            	public void windowActivated(WindowEvent arg0) {}
                public void windowClosed(WindowEvent arg0) {}

                public void windowClosing(WindowEvent arg0) {
                    Royal.close(false);
                }

                public void windowDeactivated(WindowEvent arg0) {}
                public void windowDeiconified(WindowEvent arg0) {}
                public void windowIconified(WindowEvent arg0) {}
                public void windowOpened(WindowEvent arg0) {}
                
            });
            
            this.setMinimumSize(new Dimension(650, 525));
            this.pack();
            this.setLocationRelativeTo(null);
            
            this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            
            this.setVisible(Boolean.TRUE.booleanValue());
            if (PropertyManager.getInstance().getProperty("show_log_err").equals("true")){
                try {
                    System.setErr(new PrintStream(new FileOutputStream(new File(PropertyManager.getInstance().getProperty("fileErr")))));
                } catch (FileNotFoundException e) {
                    e.printStackTrace();
                }
            }
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        if (System.getProperty("os.name").startsWith("Mac OS")){
        	applicationInstance = new Royal();
            new MacOsManagement(applicationInstance);
        } else {
            applicationInstance = new Royal();
        }
        
    }
    
	/**
	  * Function called we the user wants to quit
	  * @param restart : true if we want to restart the application 
	  */
    public static void close(boolean restart){
		//Remove lock file
        Royal.pm.removeFile();
        //Remove temporary cover pictures
        ArchiManager.getInstance().cleanTmp();
        
        //Save the sorting type
        PropertyManager.getInstance().changeProperty("sort", AlbumPane.getInstance().getSort());

        /* Destroy the GUI, because if we rerun the program with another language for exemple, like class
         * are static, the language isn't changed
         */
        MainToolBar.setInstance(null);
        MainPane.setInstance(null);
        BottomBarPane.setInstance(null);
        AlbumPane.setInstance(null);
        MenuBarManager.setInstance(null);
        
        LocaleManager.shutdown();
		//Stop HSQLDB hibernate instance
        try {
            HibernateUtil.shutdown();
        } catch (HibernateException e) {
            new DefaultException(e, RoyalException.FATAL).manageException();
        } catch (SQLException e) {
            new DefaultException(e, RoyalException.FATAL).manageException();
        }
        
        if (PropertyManager.getInstance() != null){
            PropertyManager.getInstance().save();
        }
        if(restart)
        {
        	applicationInstance.dispose();
        	Runtime.getRuntime().gc();
        	applicationInstance = new Royal();
        }
        else
        {
        	System.exit(0);
        }
    }
    
    public MainToolBar getToolBar()
    {
    	return this.north;
    }
}
