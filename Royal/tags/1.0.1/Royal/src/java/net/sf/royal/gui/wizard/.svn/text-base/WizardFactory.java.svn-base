package net.sf.royal.gui.wizard;

import net.sf.royal.gui.manager.LocaleManager;

public class WizardFactory {

    public WizardFactory instance = new WizardFactory();
    
    private WizardFactory(){
        
    }
    
    /**
     * Create a dedication wizard. The user will choose a file for dedication
     * @param pathChooser Default path for the file chooser
     * @return
     */
    public static ImageWizard createDedicationWizard(String pathChooser, String img){
        ImageWizard res = new ImageWizard(LocaleManager.getInstance().getString("choose_dedication"), LocaleManager.getInstance().getString("title_info_personal"), LocaleManager.getInstance().getString("subtitle_dedication"));
        res.customize(pathChooser, img);
        return res;
    }
    
    /**
     * Create an 
     * @param pathChooser
     * @param img
     * @return
     */
    public static ImageWizard createImageWizard(String pathChooser, String img){
        ImageWizard res = new ImageWizard(LocaleManager.getInstance().getString("title_img"), LocaleManager.getInstance().getString("title_img"), LocaleManager.getInstance().getString("subtitle_img"));
        res.customize(pathChooser, img);
        return res;
    }
    
    public static EsTexteEditorWizard createEsTexteWizard(String source){
        EsTexteEditorWizard res = new EsTexteEditorWizard(LocaleManager.getInstance().getString("title_estexte"),
                                          LocaleManager.getInstance().getString("title_estexte"),
                                          LocaleManager.getInstance().getString("subtitle_estexte"),
                                          source);
        return res;
    }
}
