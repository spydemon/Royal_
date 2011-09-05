package net.sf.royal.gui.wizard;


public abstract class WizardModel {

    private Wizard wizard;
    private WizardView wizardView;
    
    protected String currentPage = null;
    
    public WizardModel(){
    }
    /**
     * @return Returns the wizard.
     */
    public Wizard getWizard() {
        return wizard;
    }
    /**
     * @param wizard The wizard to set.
     */
    public void setWizard(Wizard wizard) {
        this.wizard = wizard;
    }
    /**
     * @return Returns the wizardView.
     */
    public WizardView getWizardView() {
        return wizardView;
    }
    /**
     * @param wizardView The wizardView to set.
     */
    public void setWizardView(WizardView wizardView) {
        this.wizardView = wizardView;
        this.wizardView.setWizardModel(this);
    }
    
    /**
     * Get the key of the next WizardPage to display
     * @return
     */
    public abstract String getNextPage();
    
    /**
     * Get the key of the previous WizardPage to display
     * @return
     */
    public abstract String getPreviousPage();
    
    /**
     * Method called by finish method of WizardView
     * @return if true the wizard is closed
     *
     */
    public abstract boolean finish();
    
    public String getCurrentPage(){
        return this.currentPage;
    }
}
