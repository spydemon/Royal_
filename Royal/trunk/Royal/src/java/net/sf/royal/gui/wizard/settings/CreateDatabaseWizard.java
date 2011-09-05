package net.sf.royal.gui.wizard.settings;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;

import org.apache.log4j.Logger;

import net.sf.royal.datamodel.Database;
import net.sf.royal.datamodel.HibernateUtil;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.MessagePaneManager;
import net.sf.royal.gui.manager.PropertyManager;
import net.sf.royal.gui.manager.ShortcutManager;
import net.sf.royal.gui.util.MandatoryTextField;
import net.sf.royal.persistency.PersistencyManager;

public class CreateDatabaseWizard extends JDialog 
{    
	private static Logger logger = Logger.getLogger(CreateDatabaseWizard.class);
	
    public static final String DEFAULT = "default";

    private MandatoryTextField password;
    private MandatoryTextField login;
    private MandatoryTextField url;
    private JComboBox types;
    private MandatoryTextField dbName;
    private JButton jbAdd;
    private JButton jbCancel;
    private JLabel urlLabel;
    private JLabel passwordLabel;
    private JLabel loginLabel;
    private Database editDB;
    


    public CreateDatabaseWizard(Window parent) 
    {
        super(parent, LocaleManager.getInstance().getString("title_database") , ModalityType.APPLICATION_MODAL);
        this.setLocationRelativeTo(parent);
        this.setSize(new Dimension(400,200));
        this.createPanel();
        this.pack();
    }
    
    public CreateDatabaseWizard(Window parent, String currentDatabase) 
    {
        super(parent, LocaleManager.getInstance().getString("title_database") , ModalityType.APPLICATION_MODAL);
        this.setLocationRelativeTo(parent);
        this.setSize(new Dimension(400,200));
        this.createPanel();
        this.pack();
        try
        {
        	Database db = Database.loadFromFile(
        			new File(PropertyManager.getInstance().getPathProperty("path_database") + PropertyManager.sep
        					+ currentDatabase + ".properties"));
        	this.setDatabase(db);
        	this.editDB = db;
        }
        catch (IOException ioe)
        {
        	ioe.printStackTrace();
        	MessagePaneManager.showCheckErrorPane(LocaleManager.getInstance().getString("error_edit_database"));
        	this.dispose();
        }
        this.jbAdd.setText(LocaleManager.getInstance().getString("edit"));
        this.jbAdd.setMnemonic(ShortcutManager.EDIT);
        ActionListener[] als = this.jbAdd.getActionListeners();
        if(als.length > 0)
        {
        	this.jbAdd.removeActionListener(als[als.length-1]);
        }
        this.jbAdd.addActionListener(new ActionListener() 
        {	
			@Override
			public void actionPerformed(ActionEvent arg0) 
			{
				if(CreateDatabaseWizard.this.check())
				{
					CreateDatabaseWizard.this.saveDatabase();
				}
			}
		});
    }
    
    private void createPanel()
    {
        this.setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.insets = new Insets(5, 5, 5, 5);
        
        this.add(new JLabel(LocaleManager.getInstance().getString("name") + " :"), gbc);
        
        gbc.gridx++;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        
        this.dbName = new MandatoryTextField(15);
        
        this.add(this.dbName, gbc);
                
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        
        this.add(new JLabel(LocaleManager.getInstance().getString("type") + " :"), gbc);
        
        gbc.gridx++;
        gbc.weightx = 1;
        gbc.gridwidth = 2;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        
        this.types = new JComboBox(HibernateUtil.getTypeListForCombo());
        
        this.types.addActionListener(new ActionListener(){

            public void actionPerformed(ActionEvent e) 
            {
                String selectedDB = (String) types.getSelectedItem();
                if (selectedDB.equals(LocaleManager.getInstance().getString("db_hsqldb")))
                {
                    setEnabledPanel(false);
                }
                else 
                {
                    setEnabledPanel(true);
                    if(selectedDB.equals(LocaleManager.getInstance().getString("db_mysql")))
                    {
                    	CreateDatabaseWizard.this.url.setText("jdbc:mysql://");
                    }
                    else if(selectedDB.equals(LocaleManager.getInstance().getString("db_postgresql")))
                    {
                    	CreateDatabaseWizard.this.url.setText("jdbc:postegresql://");
                    }
                }
            }
            
        });
        
        this.add(this.types, gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.NONE;
        
        this.urlLabel = new JLabel(LocaleManager.getInstance().getString("url") + " :"); 
        
        this.add(this.urlLabel, gbc);
        
        gbc.gridx++;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridwidth = 2;
        
        this.url = new MandatoryTextField(15);
        
        this.add(this.url, gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.NONE;
        
        this.loginLabel = new JLabel(LocaleManager.getInstance().getString("login") + " :"); 
        
        this.add(this.loginLabel, gbc);
        
        gbc.gridx++;
        gbc.weightx = 1;
        gbc.gridwidth = 2;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        this.login = new MandatoryTextField(15);
        
        this.add(this.login, gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.NONE;
        
        this.passwordLabel = new JLabel(LocaleManager.getInstance().getString("password") + " :"); 
        
        this.add(this.passwordLabel, gbc);
        
        gbc.gridx++;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridwidth = 2;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        
        this.password = new MandatoryTextField(15);        
        this.add(this.password, gbc);
        
        gbc.gridy++;
        gbc.gridx = 1;
        gbc.gridwidth = 1;
        
        this.jbAdd = new JButton(LocaleManager.getInstance().getString("add"));
        this.jbAdd.setMnemonic(ShortcutManager.ADD);
        this.jbAdd.addActionListener(new ActionListener()
        {	
			@Override
			public void actionPerformed(ActionEvent e) 
			{
				if(CreateDatabaseWizard.this.check())
				{
					CreateDatabaseWizard.this.createDatabase();
				}
			}
		});
        this.add(this.jbAdd, gbc);
        
        gbc.gridx++;
        
        this.jbCancel = new JButton(LocaleManager.getInstance().getString("cancel"));
        this.jbCancel.setMnemonic(ShortcutManager.CANCEL_);
        this.jbCancel.addActionListener(new ActionListener()
        {	
			@Override
			public void actionPerformed(ActionEvent arg0) 
			{
				CreateDatabaseWizard.this.dispose();	
			}
		});
        this.add(this.jbCancel, gbc);
        
        this.types.setSelectedItem(LocaleManager.getInstance().getString("db_hsqldb"));
    }
    
    private void setEnabledPanel(boolean enabled){
        this.urlLabel.setEnabled(enabled);
        this.url.setEnabled(enabled);
        this.loginLabel.setEnabled(enabled);
        this.login.setEnabled(enabled);
        this.passwordLabel.setEnabled(enabled);
        this.password.setEnabled(enabled);
    }
    
    public boolean check(){
        boolean res = this.dbName.check();
        if (!res || this.nameAlreadyExist(this.dbName.getText())){
            res &= false;
            this.dbName.setIncorrect();
        } else {
            this.dbName.setCorrect();
        }
        String selectedDB = (String) types.getSelectedItem();
        if (!selectedDB.equals(LocaleManager.getInstance().getString("db_hsqldb"))){
            res &= this.url.check();
            res &= this.login.check();
            res &= this.password.check();
        }
        return res;
    }
    
    private boolean nameAlreadyExist(String name)
    {
    	// If we are editing a database, we don't need to check that
    	if(this.editDB != null)
    	{
    		return false;
    	}
    	List<String> dbList;
    	try
    	{
    		dbList = HibernateUtil.getListForCombo(new String[1]);
    	} catch(IOException ioe) {
    		ioe.printStackTrace();
    		dbList = new ArrayList<String>();
    	}
    	
        for (int i = 0; i < dbList.size(); i++) {
            if (dbList.get(i).equals(name)){
                return true;
            }
        }
        return false;
    }

    private void createDatabase()
    {
    	Database db = new Database();
    	db.setName(this.dbName.getText());
    	db.setDefaultDB(false);
    	if(this.types.getSelectedItem().toString().equals
    			(LocaleManager.getInstance().getString("db_hsqldb")))
    	{
    		db.setDialect(HibernateUtil.HSQLDB);
    		db.setDriver(HibernateUtil.getDriverClass(HibernateUtil.HSQLDB));
    		db.setUrl("jdbc:hsqldb:file:" + 
        			PropertyManager.getInstance().getPathProperty("path_resources") + 
        			PropertyManager.sep + this.dbName.getText());
    		db.setUsername("sa");
    		db.setPassword("");
    	}
    	else if(this.types.getSelectedItem().toString().equals
    			(LocaleManager.getInstance().getString("db_mysql")))
    	{
    		db.setDialect(HibernateUtil.MYSQL);
    		db.setDriver(HibernateUtil.getDriverClass(HibernateUtil.MYSQL));
    		db.setUrl(this.url.getText());
    		db.setUsername(this.login.getText());
    		db.setPassword(this.password.getText());
    	}
    	else if(this.types.getSelectedItem().toString().equals
    			(LocaleManager.getInstance().getString("db_postgresql")))
    	{
    		db.setDialect(HibernateUtil.POSTGRESQL);
    		db.setDriver(HibernateUtil.getDriverClass(HibernateUtil.POSTGRESQL));
    		db.setUrl(this.url.getText());
    		db.setUsername(this.login.getText());
    		db.setPassword(this.password.getText());
    	}
    	try
    	{
    		db.saveToFile(new File(PropertyManager.getInstance().getPathProperty("path_database") + 
    			PropertyManager.sep + this.dbName.getText() + ".properties"));
    		File coverDir = new File(PropertyManager.getInstance().getPathProperty("path_cover") +
    			PropertyManager.sep + this.dbName.getText());
    		if(!coverDir.mkdir())
    		{
    			MessagePaneManager.showCheckErrorPane(LocaleManager.getInstance().getString("error_cover_dir"));
    			logger.error("Error creating " + coverDir.getAbsoluteFile());
    		}
    	} catch (IOException ioe)
    	{
    		ioe.printStackTrace();
    		MessagePaneManager.showExceptionPane(ioe, false);
    	}
    	this.dispose();
    }

    private void setDatabase(Database db)
    {
    	String item;
    	this.dbName.setText(db.getName());
    	if(!db.getDialect().equals(HibernateUtil.HSQLDB))
    	{
    		if(db.getDialect().equals(HibernateUtil.MYSQL))
    		{
    			item = LocaleManager.getInstance().getString("db_mysql");
    		}
    		else
    		{
    			item = LocaleManager.getInstance().getString("db_postgresql");
    		}
        	for(int i = 0; i < this.types.getItemCount(); i++)
    		{
    			if(this.types.getItemAt(i).toString().
    					equals(item))
    			{
    				this.types.setSelectedIndex(i);
    			}
    		}
    		this.url.setText(db.getUrl());
    		this.login.setText(db.getUsername());
    		this.password.setText(db.getPassword());
    	}
    	else
    	{
    		item = LocaleManager.getInstance().getString("db_hsqldb");
        	for(int i = 0; i < this.types.getItemCount(); i++)
    		{
    			if(this.types.getItemAt(i).toString().
    					equals(item))
    			{
    				this.types.setSelectedIndex(i);
    			}
    		}
    	}
    }
    
    private void saveDatabase()
    {
    	// If the name has changed, we need to modify the name of the files
    	File newProp;
    	if(!this.editDB.getName().equals(this.dbName.getText()))
    	{
    		File oldProp, oldScript, newScript, oldCover, newCover;
    		oldProp = new File(PropertyManager.getInstance().getPathProperty("path_database") +
    				PropertyManager.sep + editDB.getName() + ".properties");
    		if(!oldProp.delete())
    		{
            	MessagePaneManager.showCheckErrorPane(LocaleManager.getInstance().getString("error_edit_database"));
            	logger.error("Error deleting " + oldProp.getAbsolutePath());
            	return;
    		}
    		
        	if(this.types.getSelectedItem().toString().equals
        			(LocaleManager.getInstance().getString("db_hsqldb")))
        	{
        		oldProp = new File(PropertyManager.getInstance().getPathProperty("path_resources") +
        				PropertyManager.sep + editDB.getName() + ".properties");
        		newProp = new File(PropertyManager.getInstance().getPathProperty("path_resources") +
        				PropertyManager.sep + this.dbName.getText() + ".properties");
        		if(!oldProp.renameTo(newProp))
        		{
        			MessagePaneManager.showCheckErrorPane(LocaleManager.getInstance().getString("error_edit_database"));
        			logger.error("Error while moving " + oldProp.getAbsolutePath() + " to " + newProp.getAbsolutePath());
        			return;
        		}
        		
        		oldScript = new File(PropertyManager.getInstance().getPathProperty("path_resources") +
        				PropertyManager.sep + editDB.getName() + ".script");
        		newScript = new File(PropertyManager.getInstance().getPathProperty("path_resources") +
        				PropertyManager.sep + this.dbName.getText() + ".script");
        		if(!oldScript.renameTo(newScript))
        		{
        			MessagePaneManager.showCheckErrorPane(LocaleManager.getInstance().getString("error_edit_database"));
        			logger.error("Error while moving " + oldScript.getAbsolutePath() + " to " + newScript.getAbsolutePath());
        			return;
        		}
        	}
    		
    		oldCover = new File(PropertyManager.getInstance().getPathProperty("path_cover") +
    				PropertyManager.sep + editDB.getName());
    		newCover = new File(PropertyManager.getInstance().getPathProperty("path_cover") +
    				PropertyManager.sep + this.dbName.getText());
    		
    		if(!oldCover.renameTo(newCover))
    		{
    			MessagePaneManager.showCheckErrorPane(LocaleManager.getInstance().getString("error_edit_database"));
    			logger.error("Error while moving " + oldCover.getAbsolutePath() + " to " + newCover.getAbsolutePath());
            	return;
    		}
    		
    		editDB.setName(this.dbName.getText());
    	}
    	
    	String dialect = "";
    	if(this.types.getSelectedItem().toString().equals
    			(LocaleManager.getInstance().getString("db_hsqldb")))
    		dialect = HibernateUtil.HSQLDB;
    	
    	else if(this.types.getSelectedItem().toString().equals
    			(LocaleManager.getInstance().getString("db_mysql")))
    		dialect = HibernateUtil.MYSQL;
    	
    	else if(this.types.getSelectedItem().toString().equals
    			(LocaleManager.getInstance().getString("db_postgresql")))
    		dialect = HibernateUtil.POSTGRESQL;
    	
    	
		if(dialect.equals(HibernateUtil.HSQLDB))
		{
    		editDB.setUrl("jdbc:hsqldb:file:" + 
        			PropertyManager.getInstance().getPathProperty("path_resources") + 
        			PropertyManager.sep + this.dbName.getText());
    		editDB.setUsername("sa");
    		editDB.setPassword("");
		}
    	
    	if(!dialect.equals(editDB.getDialect()))
    	{
    		editDB.setDialect(dialect);
    		editDB.setDriver(HibernateUtil.getDriverClass(dialect));
    	}
    	else if(!dialect.equals(HibernateUtil.HSQLDB))
    	{
    		if(!this.url.getText().equals(editDB.getUrl()))
    		{
    			this.editDB.setUrl(this.url.getText());
    		}
    		if(!this.login.getText().equals(this.editDB.getUsername()))
    		{
    			this.editDB.setUsername(this.login.getText());
    		}
    		if(!this.password.getText().equals(this.editDB.getPassword()))
    		{
    			this.editDB.setPassword(this.password.getText());
    		}
    	}
    	
    	newProp = new File(PropertyManager.getInstance().getPathProperty("path_database") +
				PropertyManager.sep + editDB.getName() + ".properties");
    	try
    	{
    		editDB.saveToFile(newProp);
    	} catch (IOException ioe)
    	{
    		ioe.printStackTrace();
    		logger.error("Error while saving " + newProp.getAbsolutePath(), ioe);
    		return;
    	}
    	this.dispose();
    }
}
