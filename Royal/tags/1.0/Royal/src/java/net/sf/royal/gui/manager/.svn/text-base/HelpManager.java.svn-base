package net.sf.royal.gui.manager;

import java.awt.Dimension;

import javax.swing.JDialog;

import net.sf.birdy.help.gui.Helper;

public class HelpManager {

    private static HelpManager intance = new HelpManager();
    
    private Helper helper = null;
    private Dimension dim = new Dimension(1024, 768);
    
    private HelpManager(){
        
    }
    
    public void showHelp(String key){
        if (this.helper == null || this.helper.getOwner() != null){
            this.helper = new Helper(LocaleManager.getInstance().getHelpLocale(),
                                     LocaleManager.getInstance().getString("help_contents"),
                                     PropertyManager.getInstance().getPathProperty("path_help"));
            
            this.helper.setPreferredSize(dim);
            this.helper.setResizable(Boolean.TRUE.booleanValue());
            this.helper.setFocusable(Boolean.TRUE.booleanValue());
        }
        this.helper.showHelp(key);
    }
    
    public void showHelp(JDialog owner, String key){
        System.out.println(this.helper + "/" + owner);
        if (this.helper == null || this.helper.getOwner() != owner){
            this.helper = new Helper(LocaleManager.getInstance().getHelpLocale(),
                    LocaleManager.getInstance().getString("help_contents"),
                    PropertyManager.getInstance().getPathProperty("path_help"),
                    owner);
            this.helper.setPreferredSize(dim);
            this.helper.setResizable(Boolean.TRUE.booleanValue());
            this.helper.setFocusable(Boolean.TRUE.booleanValue());
        }
            
        this.helper.showHelp(key);
    }

    public static HelpManager getIntance() {
        return intance;
    }

    public Helper getHelper() {
        return helper;
    }
}
