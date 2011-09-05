package net.sf.royal.gui.manager;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.net.URL;

import javax.help.HelpSet;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import net.sf.royal.Royal;
import net.sf.royal.gui.wizard.ImportDatabaseWizard;
import net.sf.royal.gui.wizard.settings.SettingsTabbedDialog;

/**
  * @author bibounde, Soulou, Maxx
  * MenuBar of the main window
  */
public class MenuBarManager {

	/**
	 * Instance of the MenuBarManager
	 */
    public static MenuBarManager instance = new MenuBarManager();
    
    /**
     * JMenuBar of the main window
     */
    private JMenuBar menuBar;
    
    /* Different menus */
    private JMenu file;
    private JMenuItem exit;
    
    private JMenu help;
    private JMenuItem helpContents;
    private JMenuItem about;
    private JMenuItem log;
	
    private JMenu tools;
    private JMenuItem settings;
    private JMenuItem importDatabase;
    
    /* Constructor */
    private MenuBarManager(){
        menuBar = new JMenuBar();
        file = new JMenu(LocaleManager.getInstance().getString("file"));
		file.setMnemonic(ShortcutManager.MENU_FILE);
        exit = new JMenuItem(LocaleManager.getInstance().getString("exit"));
		exit.setAccelerator(KeyStroke.getKeyStroke(ShortcutManager.QUIT, InputEvent.CTRL_DOWN_MASK));
        file.add(exit);
        menuBar.add(file);
        
		tools = new JMenu(LocaleManager.getInstance().getString("tools_settings"));
		tools.setMnemonic(ShortcutManager.MENU_TOOLS);
		
		importDatabase = new JMenuItem(LocaleManager.getInstance().getString("import"));
		importDatabase.setIcon(IconManager.getIcon("import.gif"));
		importDatabase.setAccelerator(KeyStroke.getKeyStroke(ShortcutManager.IMPORT, InputEvent.CTRL_DOWN_MASK));
		
		tools.add(importDatabase);
		
		settings = new JMenuItem(LocaleManager.getInstance().getString("settings"));
		settings.setIcon(IconManager.getIcon("settings.png"));
		settings.setAccelerator(KeyStroke.getKeyStroke(ShortcutManager.SETTINGS, InputEvent.CTRL_DOWN_MASK));
		
		tools.add(settings);
		
        menuBar.add(tools);
        
        help = new JMenu(LocaleManager.getInstance().getString("help"));
		help.setMnemonic(ShortcutManager.MENU_HELP);

        helpContents = new JMenuItem(LocaleManager.getInstance().getString("help_contents"));
        helpContents.setIcon(IconManager.getIcon("help.gif"));
		helpContents.setAccelerator(KeyStroke.getKeyStroke(ShortcutManager.HELP,0));

        help.add(helpContents);

        log = new JMenuItem(LocaleManager.getInstance().getString("see_log"));
        log.setIcon(IconManager.getIcon("log.gif"));
		log.setAccelerator(KeyStroke.getKeyStroke(ShortcutManager.LOG, InputEvent.CTRL_DOWN_MASK));

        help.add(log);

        about = new JMenuItem(LocaleManager.getInstance().getString("about"));
        about.setIcon(IconManager.getIcon("about.gif"));
		about.setAccelerator(KeyStroke.getKeyStroke(ShortcutManager.ABOUT, InputEvent.CTRL_DOWN_MASK));

        help.add(about);

        menuBar.add(help);
        
        this.initListeners();
    }

    /**
     * Initialize the listeners of the different items of the menu.
     */
    private void initListeners() {
        
        exit.addActionListener(new ActionListener(){

            public void actionPerformed(ActionEvent arg0) {
                Royal.close(false);
            }
            
        });
        
        
        helpContents.addActionListener(new ActionListener(){

            public void actionPerformed(ActionEvent e) {
                HelpManager.getIntance().showHelp(null);
            }
            
        });
        
        about.addActionListener(new ActionListener(){

            public void actionPerformed(ActionEvent e) {
                MessagePaneManager.showAboutDialog();
            }
            
        });
        
        log.addActionListener(new ActionListener(){

            public void actionPerformed(ActionEvent e) {
                MessagePaneManager.showLogDialog();
            }
            
        });

        settings.addActionListener(new ActionListener(){
        	public void actionPerformed(ActionEvent e) {
        		SettingsTabbedDialog std = new SettingsTabbedDialog(Royal.applicationInstance);
        		std.setModal(true);
        		std.setVisible(true);
        	}
        });
        
        importDatabase.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				new ImportDatabaseWizard(Royal.applicationInstance).setVisible(true);
			}
		});
    }

    /**
     * @return Returns the menuBar.
     */
    public JMenuBar getMenuBar() {
        return menuBar;
    }
    
    
    public HelpSet getHelpSet(String helpsetfile) { 
        HelpSet hs = null; 
        ClassLoader cl = this.getClass().getClassLoader(); 
        try{ 
        URL hsURL = HelpSet.findHelpSet(cl, helpsetfile); 
        hs= new HelpSet(null, hsURL); 
        } catch(Exception ee) { 
            ee.printStackTrace();
        } 
        return hs; 
    }

	public static MenuBarManager getInstance() {
		if(instance == null)
		{
			instance = new MenuBarManager();
		}
		return instance;
	}
	
	public static void setInstance(MenuBarManager in)
	{
		instance = in;
	}
}
