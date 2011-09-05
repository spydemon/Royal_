/**
 * This is the help frame.<br/>
 * It will load the help content when created
 */
package net.sf.birdy.help.gui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.JDialog;
import javax.swing.JToolBar;

import net.sf.birdy.help.core.model.Help;
import net.sf.birdy.help.core.model.Model;
import net.sf.birdy.help.core.model.ModelImpl;
import net.sf.birdy.help.core.model.Populator;

public class Helper extends JDialog {

    //public static final String ICON_PATH = "net/sf/birdy/help/gui/icons";
	/**
	 * The path for the directory which contains the icons
	 */
	public static final String ICON_PATH = "resources/icons";
    
	/**
	 * Get an instance of the Helper JDialog frame.
	 * Will also load the help content.
	 *
	 * @param locale The locale to be used
	 * @see net.sf.birdy.help.core.locale.LocaleManager
	 * @param title The title of the dialog frame
	 * @param searchDirectory The directory where the program will search the help content
	 */
    public Helper(String locale, String title, String searchDirectory){
        this.initLayout(locale, title, searchDirectory);
    }
    
	/**
	 * Do the same as Helper(String locale, String title, String searchDirectory)
	 * excepted that you have to specify the JDialog which will own the help frame.
	 *
	 * @param locale The locale to be used
	 * @see net.sf.birdy.help.core.locale.LocaleManager
	 * @param title The title of the dialog frame
	 * @param searchDirectory The directory where the program will search the help content
	 * @param owner The owner of this frame
	 */
    public Helper(String locale, String title, String searchDirectory, JDialog owner){
        super(owner);
        this.initLayout(locale, title, searchDirectory);
    }
    
	/**
	 * Initialise the layout of the Helper frame.<br/>
	 * The mainpanel of the frame is an MainPanel
	 * @see net.sf.birdy.help.gui.MainPanel
	 *
	 * @param locale The locale to be used
	 * @see net.sf.birdy.help.core.locale.LocaleManager
	 * @param title The title of the frame
	 * @param searchDirectory The directory where the program will search the help content
	 */
    private void initLayout(String locale, String title, String searchDirectory){
        this.setTitle(title);
        Populator populator = new Populator("help.xml");
        Help help = null;
        try {
            help = populator.populate();
        } catch (Exception e) {
            e.printStackTrace();
        }
        HelperManager.getInstance().setLocale(locale);
        HelperManager.getInstance().setHelp(help);
        HelperManager.getInstance().setDirectory(searchDirectory);
        MainPanel main = new MainPanel();
        HelperManager.getInstance().setMainPanel(main);
        JToolBar bar = new JToolBar();
        bar.setFloatable(Boolean.FALSE.booleanValue());
       
        bar.add(main.getPrevious());
        bar.add(main.getNext());
        bar.add(main.getHome());

        //bar.add(main.getPrint(), gbc);
        //bar.add(main.getSearchLabel(), gbc);
        //bar.add(main.getSearchField(), gbc);
        //bar.add(main.getBar(), gbc);
        
        this.getContentPane().add(bar, BorderLayout.PAGE_START);
        this.getContentPane().add(main, BorderLayout.CENTER);
        this.setPreferredSize(new Dimension(1024, 768));
        //this.setIconImage(new ImageIcon(this.getClass().getClassLoader().getResource("net/sf/birdy/help/gui/help.gif")).getImage());
    }
    
	
    public void showHelp(String pageName){
        this.pack();
        this.setLocationRelativeTo(null);
        this.setVisible(Boolean.TRUE.booleanValue());
        if (pageName != null){
            Model model = HelperManager.getInstance().getHelp().findModel(pageName); 
            HelperManager.getInstance().displayPage((ModelImpl) model);
        } else {
            HelperManager.getInstance().displayPage(HelperManager.getInstance().getHelp());
        }
    }
    
    public static void main(String[] args){
        Helper help = null;
        String locale = "fr";
        String page = "index";
        if (args.length == 1){
            locale = args[0];
        } else if (args.length == 2 ) {
            locale = args[0];
            if (!args[1].equals("${page}")){
                page = args[1];
            }
            
        }
        help = new Helper(locale, "", "index");
        help.pack();
        help.setVisible(true);
        HelperManager.getInstance().displayPage((ModelImpl) HelperManager.getInstance().getHelp().findModel(page));
        //HelperManager.getInstance().displayPage(HelperManager.getInstance().getHelp());
        help.addWindowListener(new WindowListener(){

            public void windowOpened(WindowEvent e) { }

            public void windowClosing(WindowEvent e) {
                System.exit(0);
            }

            public void windowClosed(WindowEvent e) {}
            public void windowIconified(WindowEvent e) {}
            public void windowDeiconified(WindowEvent e) {}
            public void windowActivated(WindowEvent e) {}
            public void windowDeactivated(WindowEvent e) {}
            
        });
    }
}
