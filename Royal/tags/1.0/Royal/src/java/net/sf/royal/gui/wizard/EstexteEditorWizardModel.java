package net.sf.royal.gui.wizard;

public class EstexteEditorWizardModel extends WizardModel {
    
    private String source;
    
    @Override
    public String getNextPage() {
        this.getWizardView().setButtonState(WizardView.FINISH);
        return EsTexteEditorWizard.EDITOR;
    }

    @Override
    public String getPreviousPage() {
        return null;
    }

    @Override
    public boolean finish() {
        this.source = ((EsTexteEditorWizard) this.getWizard()).getPanel().getEsTexteView().getText();
        return true;
    }
    
    public String getSource(){
        return this.source;
    }

}
