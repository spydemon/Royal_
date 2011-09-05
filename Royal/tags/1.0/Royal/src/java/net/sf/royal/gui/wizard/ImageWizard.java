package net.sf.royal.gui.wizard;

import java.awt.Dimension;

import javax.swing.JPanel;

import net.sf.royal.gui.filechooser.ImageChooser;

public class ImageWizard extends Wizard {

    public static final String KEY_FC = "imageChooser"; 
    
    public ImageWizard(String frameTitle, String title, String subtitle) {
        super(frameTitle);
        this.setDimension(new Dimension(600,350));
        ImageWizardModel model = new ImageWizardModel();
        WizardView wizardView = new WizardView(title, subtitle);
        model.setWizardView(wizardView);
        this.setWizardModel(model);
    }
    
    public void customize(String fileChooserPath, String img){
        JPanel panel = new JPanel();
        ImageChooser imgChooser = new ImageChooser(fileChooserPath, img);
        imgChooser.setControlButtonsAreShown(Boolean.FALSE.booleanValue());
        panel.add(imgChooser);
        WizardPage wizardPage = new WizardPage(KEY_FC,panel);
        
        this.getWizardModel().getWizardView().addWizardPage(wizardPage);
    }
}
