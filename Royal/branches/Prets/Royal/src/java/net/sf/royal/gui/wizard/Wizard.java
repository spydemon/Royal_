package net.sf.royal.gui.wizard;

import java.awt.Dimension;

import javax.swing.JDialog;
import javax.swing.JFrame;

public abstract class Wizard extends JDialog {

    private WizardModel wizardModel;
    private Dimension dimension = new Dimension(600,400);
    private boolean canceled = Boolean.TRUE.booleanValue();
    
    public Wizard(String title){
        super(new JFrame());
        this.setTitle(title);
        this.setModal(Boolean.TRUE.booleanValue());
    }
    
    /**
     * @return Returns the wizardModel.
     */
    public WizardModel getWizardModel() {
        return wizardModel;
    }
    /**
     * @param wizardModel The wizardModel to set.
     */
    public void setWizardModel(WizardModel wizardModel) {
        this.wizardModel = wizardModel;
        this.wizardModel.setWizard(this);
        this.getContentPane().removeAll();
        this.getContentPane().add(this.wizardModel.getWizardView());
    }
    
    public void showWizard(){
        this.wizardModel.getWizardView().next();
        this.pack();
        this.setLocationRelativeTo(null);
        this.setVisible(Boolean.TRUE.booleanValue());
    }

    /**
     * @return Returns the dimension.
     */
    public Dimension getDimension() {
        return dimension;
    }

    /**
     * @param dimension The dimension to set.
     */
    public void setDimension(Dimension dimension) {
        this.dimension = dimension;
    }

    /**
     * @return Returns the canceled.
     */
    public boolean isCanceled() {
        return canceled;
    }

    /**
     * @param canceled The canceled to set.
     */
    public void setCanceled(boolean canceled) {
        this.canceled = canceled;
    }
}
