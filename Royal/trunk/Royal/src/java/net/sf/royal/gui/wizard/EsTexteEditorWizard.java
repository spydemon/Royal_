package net.sf.royal.gui.wizard;

import java.awt.Dimension;

import net.sf.royal.gui.util.EsTexteEditorPanel;

public class EsTexteEditorWizard extends Wizard {

    public static final String EDITOR = "editor";
    
    private EsTexteEditorPanel panel;
    
    public EsTexteEditorWizard(String frameTitle, String title, String subtitle, String source) {
        super(title);
        this.setDimension(new Dimension(800,600));
        EstexteEditorWizardModel model = new EstexteEditorWizardModel();
        model.setWizardView(new WizardView(title, subtitle));
        this.setWizardModel(model);
        
        this.panel = new EsTexteEditorPanel(source);
        
        WizardPage wizardPage = new WizardPage(EDITOR, panel);
        this.getWizardModel().getWizardView().addWizardPage(wizardPage);
    }

    public EsTexteEditorPanel getPanel() {
        return panel;
    }

}
