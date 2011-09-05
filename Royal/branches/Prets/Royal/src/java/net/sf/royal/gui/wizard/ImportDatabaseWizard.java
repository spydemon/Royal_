package net.sf.royal.gui.wizard;

import java.awt.Color;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import net.sf.royal.datamodel.HibernateUtil;
import net.sf.royal.gui.guimodel.property.DatabaseGuiObject;
import net.sf.royal.gui.manager.FileManager;
import net.sf.royal.gui.manager.IconManager;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.MessagePaneManager;
import net.sf.royal.gui.manager.PropertyManager;
import net.sf.royal.gui.manager.ShortcutManager;

import org.apache.log4j.Logger;

/**
 * Dialog to import the old BirDy database
 * @author Soulou
 */
public class ImportDatabaseWizard extends JDialog
{
	/* Fields */
	
	/**
	 * Static logger
	 */
	private static Logger logger = Logger.getLogger(ImportDatabaseWizard.class);
	/**
	 * Label which asks the user to select the old installation path
	 */
	private JLabel jlChoice;
	/**
	 * Icon of the next label
	 */
	private JLabel jlIconIncorrect;
	/**
	 * Hidden first, tell the user if the path is a valid on
	 */
	private JLabel jlIncorrect;
	/**
	 * Label to display current file transfer
	 */
	private JLabel jlFile;
	/**
	 * Textfield where will appear the installation path chosen by the user
	 */
	private JTextField jtfBrowse;
	/**
	 * Button which will display the browsing dialog
	 */
	private JButton jbBrowse;
	/**
	 * Validating button
	 */
	private JButton jbOk;
	/**
	 * Canceling button
	 */
	private JButton jbCancel;
	/**
	 * Advancement of the importation
	 */
	private JProgressBar jpbImport;
	/**
	 * Constraints to draw the GUI
	 */
	private GridBagConstraints gbc;
	
	/* Constructor */
	/**
	 * Build the dialog, create the different fields
	 */
	public ImportDatabaseWizard(Window parent) 
	{
		super(parent, LocaleManager.getInstance().getString("import"), 
				Dialog.ModalityType.DOCUMENT_MODAL);
		
		// We modify the position of the JDialog on the parent JFrame
		Rectangle r = parent.getBounds();
        int x = r.x + (r.width/4 - this.getSize().width)/2;
        int y = r.y + (r.height - this.getSize().height)/2;
        this.setLocation(x, y);
        
		this.jlChoice = new JLabel(LocaleManager.getInstance().getString("old_location"));
		this.jlChoice.setFont(this.jlChoice.getFont().deriveFont(Font.BOLD));
		this.jlIconIncorrect = new JLabel(IconManager.getIcon("incorrect.png"));
		this.jlIncorrect = new JLabel("   " + LocaleManager.getInstance().getString("bad_location"));
		this.jlIncorrect.setForeground(Color.RED);
		this.jlFile = new JLabel(LocaleManager.getInstance().getString("file"));
		this.jtfBrowse = new JTextField();
		this.jbBrowse = new JButton(LocaleManager.getInstance().getString("browse"),
									IconManager.getIcon("browse.gif"));
		this.jbOk = new JButton(LocaleManager.getInstance().getString("ok"));
		this.jbOk.setMnemonic(ShortcutManager.OK);
		this.jbOk.setEnabled(false);
		this.jbCancel = new JButton(LocaleManager.getInstance().getString("cancel"));
		this.jbCancel.setMnemonic(ShortcutManager.CANCEL);
		this.jpbImport = new JProgressBar();
		
		// In Swing event thread, we draw the different components, and initialize the listeners 
		SwingUtilities.invokeLater(new Runnable()
		{	
			@Override
			public void run()
			{	
				ImportDatabaseWizard.this.drawDialog();
				ImportDatabaseWizard.this.initListeners();
			}
		});
	}
	
	/**
	 * Setup the layout
	 */
	private void drawDialog()
	{
		this.setLayout(new GridBagLayout());
		gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.BOTH;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 10, 5);
		
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 4;
		this.add(this.jlChoice, gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.gridwidth = 4;
		gbc.weightx = 1.0;
		this.add(this.jtfBrowse, gbc);
		
		gbc.gridx = 4;
		gbc.gridy = 1;
		gbc.gridwidth = 1;
		gbc.weightx = 0.0;
		this.add(this.jbBrowse, gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.gridwidth = 4;
		jlIconIncorrect.setVisible(false);
		jlIncorrect.setVisible(false);
		this.add(jlIconIncorrect,gbc);
		this.add(jlIncorrect,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 3;
		gbc.gridwidth = 4;
		gbc.weightx = 1.0;
		jpbImport.setVisible(false);
		this.add(jpbImport, gbc);

		gbc.gridx = 0;
		gbc.gridy = 4;
		gbc.gridwidth = 2;
		gbc.weightx = 1.0;
		jlFile.setVisible(false);
		this.add(jlFile,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 5;
		gbc.gridwidth = 1;
		gbc.weightx = 0.0;
		JPanel jpButton = new JPanel();
		jpButton.add(this.jbOk);
		jpButton.add(this.jbCancel);
		this.add(jpButton,gbc);
		
		
		this.pack();
		this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);
	}
	
	private void initListeners()
	{
		/**
		 * Whene there is a modification of the text in the jtextfield, we check if it's a good dir
		 */
		this.jtfBrowse.getDocument().addDocumentListener(new DocumentListener() 
		{	
			@Override
			public void removeUpdate(DocumentEvent e)
			{
				ImportDatabaseWizard.this.checkInstallationPath(jtfBrowse.getText());
			}
			@Override
			public void insertUpdate(DocumentEvent e)
			{
				ImportDatabaseWizard.this.checkInstallationPath(jtfBrowse.getText());
			}			
			@Override
			public void changedUpdate(DocumentEvent e) 
			{
				ImportDatabaseWizard.this.checkInstallationPath(jtfBrowse.getText());
			}
		});
	
		/**
		 * When the user click on OK
		 */
		this.jbOk.addActionListener(new ActionListener() 
		{	
			@Override
			public void actionPerformed(ActionEvent arg0) 
			{	
				/* 
				 * If there was an error, the progress bar is still visible
				 * so we check it, in order to have still a correct window size
				 */
				if(!jpbImport.isVisible())
				{
					jpbImport.setVisible(true);
					Dimension d = ImportDatabaseWizard.this.getSize();
					d.height += jpbImport.getPreferredSize().height*2;
					ImportDatabaseWizard.this.setSize(d);
				
					jlFile.setVisible(true);
					d = ImportDatabaseWizard.this.getSize();
					d.height += jlFile.getPreferredSize().height*2;
					ImportDatabaseWizard.this.setSize(d);
				
					ImportDatabaseWizard.this.invalidate();
				}
				
				// If the user has click on OK button, the path is correct
				ImportDatabaseWizard.this.importDatabase();
			}
		});
		
		this.jbBrowse.addActionListener(new ActionListener() 
		{	
			@Override
			public void actionPerformed(ActionEvent arg0) 
			{
				JFileChooser jfc = new JFileChooser(new File("~"));
				jfc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
				int ret = jfc.showOpenDialog(ImportDatabaseWizard.this);
				if(ret == JFileChooser.APPROVE_OPTION)
				{
					String installPath = jfc.getSelectedFile().getAbsolutePath();
					jtfBrowse.setText(installPath);
					ImportDatabaseWizard.this.checkInstallationPath(installPath);
				}
			}
		});
		
		this.jbCancel.addActionListener(new ActionListener()
		{	
			@Override
			public void actionPerformed(ActionEvent arg0) 
			{
				ImportDatabaseWizard.this.dispose();	
			}
		});
	}
	
	/**
	 * We check if the installPath is a correct BirDy 0.2.0 installpath
	 * @param installPath directory given by the user
	 */
	private void checkInstallationPath(String installPath)
	{		
        if (System.getProperty("os.name").startsWith("Mac OS"))
        {
        	installPath += "/BirDy.app/Contents/Resources/Java";
        }
		File databaseProp = new File(installPath + PropertyManager.sep + "database.properties");
		File databaseScript = new File(installPath + PropertyManager.sep + "database.script");
		
		// IF there is something in the text field, and we can find the database files
		if(!this.jtfBrowse.getText().isEmpty() && (!databaseProp.exists() || !databaseScript.exists()))
		{
			if(!this.jlIncorrect.isVisible())
			{
				this.jlIconIncorrect.setVisible(true);
				this.jlIncorrect.setVisible(true);
				Dimension d = this.getSize();
				d.height += this.jlIncorrect.getPreferredSize().height*2;
				this.setSize(d);
			}
			this.jbOk.setEnabled(false);
		}
		else
		{
			if(this.jlIncorrect.isVisible())
			{
				Dimension d = this.getSize();
				d.height -= this.jlIncorrect.getPreferredSize().height*2;
				this.setSize(d);
				this.jlIconIncorrect.setVisible(false);
				this.jlIncorrect.setVisible(false);
			}
			if(!this.jtfBrowse.getText().isEmpty())
			{
				this.jbOk.setEnabled(true);
			}
		}
		this.invalidate();
	}
	
	private void importDatabase()
	{
		/*
		 * To name the future database, we get the current date
		 */
		Calendar c = GregorianCalendar.getInstance();
		String strDate;
		if(LocaleManager.getInstance().getCurrentLocale().equals(LocaleManager.ENGLISH))
		{
			strDate = "" + c.get(Calendar.YEAR) + c.get(Calendar.MONTH) + c.get(Calendar.DAY_OF_MONTH)
						+ c.get(Calendar.SECOND);
		}
		else
		{
			strDate = "" + c.get(Calendar.DAY_OF_MONTH) + c.get(Calendar.MONTH) + c.get(Calendar.YEAR)
						+ c.get(Calendar.SECOND);
		}
		String databaseName = "database" + strDate;
		
		// We copy in another thread (SwingWorker subclass) the different files
		CopyProgressTask cpt = new CopyProgressTask(databaseName);

		// We create the database
        DatabaseGuiObject dgo = new DatabaseGuiObject();
        dgo.setDefaultDB(false);
        dgo.setName("database" + strDate);
        String dialect = HibernateUtil.getTypefromLabel((String)"HSQLDB"); 
        dgo.setDialect(dialect);
        dgo.setUsername("sa");
        dgo.setUrl("jdbc:hsqldb:file:" + PropertyManager.getInstance().getPathProperty("path_resources")
        		+ PropertyManager.sep + databaseName);
        dgo.setPassword("");
        
        try
        {
        	HibernateUtil.createDatabase(dgo);
        } catch (IOException e) {
			MessagePaneManager.showExceptionPane(e, false);
			e.printStackTrace();
			return;
        }

		try
		{
			cpt.doInBackground();
			while(cpt.isDone())
			{
				Thread.sleep(300);
			}
		} catch (Exception e)
		{
			MessagePaneManager.showExceptionPane(e, false);
			e.printStackTrace();
		}
		
        // If the user want to use it now, we ask for that 
        SwingUtilities.invokeLater(new Runnable() 
        {
			@Override
			public void run() 
			{
				ImportDatabaseWizard.this.jlFile.setText(LocaleManager.getInstance().getString("importation_end"));
				ImportDatabaseWizard.this.jpbImport.setValue(ImportDatabaseWizard.this.jpbImport.getMaximum());
			}
		});
		if(MessagePaneManager.showConfirmation(LocaleManager.getInstance().getString("change_current_database")))
		{
			HibernateUtil.changeDatabase(databaseName);
		}
		this.invalidate();
		
		// We close the dialog
		this.dispose();
	}
	
	/**
	 * Task to copy the different fiels in background
	 * @author Soulou
	 */
	public class CopyProgressTask extends SwingWorker<List<Object>, Object> 
	{
		public ImportDatabaseWizard parent;
		public String databaseName;
		
		public CopyProgressTask(String dbname)
		{
			this.databaseName = dbname;
			this.parent = ImportDatabaseWizard.this;
		}
		
		@Override
		protected List<Object> doInBackground() throws Exception 
		{
			int nbFilesToCopy = 2;
			
			String installPath = parent.jtfBrowse.getText();
		
			// We get the different covers of the BirDy installation
			File coverDir = new File(installPath + PropertyManager.sep + "resources" + PropertyManager.sep
									+ "images" + PropertyManager.sep + "cover");
			File[] coverFiles = coverDir.listFiles();
		
			// We define the length of the progressbar
			nbFilesToCopy += coverFiles.length;
			parent.jpbImport.setMaximum(nbFilesToCopy);
		
			// We'll firstly copy the database files
			File databaseProp = new File(installPath + PropertyManager.sep + "database.properties");
			File databaseScript = new File(installPath + PropertyManager.sep + "database.script");
			
			File newdatabaseProp = new File(PropertyManager.getInstance().getPathProperty("path_resources")
					+ PropertyManager.sep + databaseName + ".properties");
			File newdatabaseScript = new File(PropertyManager.getInstance().getPathProperty("path_resources")
					+ PropertyManager.sep + databaseName + ".script");
			
			String newCover;
			
			try
			{
				logger.debug(databaseProp + " --> " + newdatabaseProp);
				parent.jlFile.setText(databaseProp + " --> " + newdatabaseProp);
				FileManager.copyFile(databaseProp, newdatabaseProp);
				publish(1);
				
				logger.debug(databaseScript + " --> " + newdatabaseScript);
				parent.jlFile.setText(databaseScript + " --> " + newdatabaseScript);
				FileManager.copyFile(databaseScript, newdatabaseScript);
				publish(2);
				
				coverDir = new File(PropertyManager.getInstance().getPathProperty("path_cover") + 
						PropertyManager.sep + this.databaseName);
				
				// And then the different covers
				for(int i = 0; i < coverFiles.length; i++)
				{
					newCover = coverDir.getAbsolutePath() + PropertyManager.sep
								+ coverFiles[i].getName();
					logger.debug(coverFiles[i].getCanonicalPath() + " --> " + newCover);
					publish(coverFiles[i].getCanonicalPath() + " --> " + newCover);
					
					FileManager.copyFile(coverFiles[i], new File(newCover));
					publish(i+2);
				}
			} catch (Exception e)
			{
				MessagePaneManager.showExceptionPane(e, false);
				e.printStackTrace();
				return null;
			}
			
			return null;
		}
		
		private List<Object> thingsToDo;
		public void process(List<Object> chunks)
		{
			this.thingsToDo = chunks;
			SwingUtilities.invokeLater(new Runnable(){
				public void run()
				{
					/*
					 *  We publish object, if it is a String, this is for jlFile
					 *  and if this is an Integer, this is to update the progressbar
					 */
					
					for(Object o : thingsToDo)
					{
						if(o instanceof String)
						{
							ImportDatabaseWizard.this.jlFile.setText((String)o);
							ImportDatabaseWizard.this.jlFile.revalidate();
						}
						else if(o instanceof Integer)
						{
							ImportDatabaseWizard.this.jpbImport.setValue((Integer)o);
							ImportDatabaseWizard.this.jpbImport.revalidate();
						}
					}
				}
			});
		}
	}
}
