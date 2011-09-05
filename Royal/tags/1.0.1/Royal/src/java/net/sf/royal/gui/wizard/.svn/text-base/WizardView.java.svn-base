package net.sf.royal.gui.wizard;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.border.Border;
import javax.swing.border.EtchedBorder;

import net.sf.royal.gui.manager.HelpManager;

public class WizardView extends JPanel{

    public static final int NONE = 0;
    public static final int FINISH = 7;
    public static final int NEXT = 11;
    public static final int PREVIOUS = 13;
    private static final Border border = BorderFactory.createEtchedBorder(EtchedBorder.LOWERED);
    
    private List<WizardPage> wizardPages = new ArrayList<WizardPage>();
    private WizardModel wizardModel;
    private BarPanel barPanel;
    private ButtonPanel buttonPanel;
    private JPanel currentPanel;
    private GridBagConstraints gbc = new GridBagConstraints();
    
    public WizardView(String title, String subtitle){
        this.setLayout(new GridBagLayout());
        barPanel = new BarPanel(title, subtitle);
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        
        this.add(barPanel, gbc);
        
        this.buttonPanel = new ButtonPanel(this);
        gbc.gridy = 2;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        
        this.add(buttonPanel, gbc);
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
    }
    /**
     * @return Returns the wizardPages.
     */
    public List<WizardPage> getWizardPages() {
        return wizardPages;
    }
    /**
     * @param wizardPages The wizardPages to set.
     */
    public void setWizardPages(List<WizardPage> wizardPages) {
        this.wizardPages = wizardPages;
    }
    
    public void addWizardPage(WizardPage wizardPage){
        this.wizardPages.add(wizardPage);
        wizardPage.setWizardView(this);
        wizardPage.getPanel().setPreferredSize(this.getWizardModel().getWizard().getDimension());
    }
    
    public void removeWizardPage(WizardPage wizardPage){
        this.wizardPages.remove(wizardPage);
        wizardPage.setWizardView(null);
    }
    
    /**
     * @return Returns the barPanel.
     */
    public BarPanel getBarPanel() {
        return barPanel;
    }
    /**
     * @param barPanel The barPanel to set.
     */
    public void setBarPanel(BarPanel barPanel) {
        this.barPanel = barPanel;
    }
    /**
     * @return Returns the buttonPanel.
     */
    public ButtonPanel getButtonPanel() {
        return buttonPanel;
    }
    /**
     * @param buttonPanel The buttonPanel to set.
     */
    public void setButtonPanel(ButtonPanel buttonPanel) {
        this.buttonPanel = buttonPanel;
    }
    
    /**
     * Find WizardPage by its key 
     * @param key
     * @return WizardPage
     */
    public WizardPage getWizardPage(String key){
        for(int i=0;i<this.wizardPages.size();i++){
            if (((WizardPage) this.wizardPages.get(i)).getKey().equals(key)){
                return (WizardPage) this.wizardPages.get(i);
            }
        }
        return null;
    }
    
    /**
     * Method called by Wizard to show the first wizard page and by next button
     *
     */
    public void next(){
        WizardPage page = this.getWizardPage(this.wizardModel.getNextPage());
        if (page.getHelpKey() != null){
            this.buttonPanel.getHelp().setEnabled(Boolean.TRUE.booleanValue());
        } else {
            this.buttonPanel.getHelp().setEnabled(Boolean.FALSE.booleanValue());
        }
        
        
        JPanel panel = page.getPanel();
        if (!panel.equals(this.currentPanel)){
            if (this.currentPanel != null){
                this.remove(currentPanel);
            }
            this.currentPanel = panel;
            this.currentPanel.setBorder(border);
            gbc.gridy = 1;
            gbc.weighty = 1;
            gbc.fill = GridBagConstraints.BOTH;
            this.add(this.currentPanel,gbc);
            
            this.validate();
            this.repaint();
        }
    }
    
    /**
     * Method called by previous button
     *
     */
    public void previous(){
        WizardPage page = this.getWizardPage(this.wizardModel.getPreviousPage());
        if (page.getHelpKey() != null){
            this.buttonPanel.getHelp().setEnabled(Boolean.TRUE.booleanValue());
        } else {
            this.buttonPanel.getHelp().setEnabled(Boolean.FALSE.booleanValue());
        }
        JPanel panel = page.getPanel();
        if (!panel.equals(this.currentPanel)){
            if (this.currentPanel != null){
                this.remove(currentPanel);
            }
            this.currentPanel = panel; 
            this.currentPanel.setBorder(border);
            gbc.gridy = 1;
            gbc.weighty = 1;
            gbc.fill = GridBagConstraints.BOTH;
            this.add(this.currentPanel,gbc);
            this.validate();
            this.repaint();
        }
    }
    
    /**
     * Method called by finish button
     *
     */
    public void finish(){
        boolean res = this.getWizardModel().finish();
        if (res){
            this.getWizardModel().getWizard().setCanceled(Boolean.FALSE.booleanValue());
            this.getWizardModel().getWizard().dispose();
        }
    }
    
    /**
     * Method called by cancel button
     *
     */
    public void cancel(){
        this.getWizardModel().getWizard().setCanceled(Boolean.TRUE.booleanValue());
        this.getWizardModel().getWizard().dispose();
    }
    
    /**
     * Set the buttonState which enables/disables button
     * @param state
     */
    public void setButtonState(int state){
        this.getButtonPanel().getFinish().setEnabled(Boolean.FALSE.booleanValue());
        this.getButtonPanel().getNext().setEnabled(Boolean.FALSE.booleanValue());
        this.getButtonPanel().getPrevious().setEnabled(Boolean.FALSE.booleanValue());
        if (state == FINISH){
            this.getButtonPanel().getFinish().setEnabled(Boolean.TRUE.booleanValue());
        } else if (state == NEXT){
            this.getButtonPanel().getNext().setEnabled(Boolean.TRUE.booleanValue());
        } else if (state == PREVIOUS){
            this.getButtonPanel().getPrevious().setEnabled(Boolean.TRUE.booleanValue());
        } else if (state == FINISH + NEXT){
            this.getButtonPanel().getFinish().setEnabled(Boolean.TRUE.booleanValue());
            this.getButtonPanel().getNext().setEnabled(Boolean.TRUE.booleanValue());
        } else if ( state == FINISH + PREVIOUS){
            this.getButtonPanel().getFinish().setEnabled(Boolean.TRUE.booleanValue());
            this.getButtonPanel().getPrevious().setEnabled(Boolean.TRUE.booleanValue());
        } else if ( state == NEXT + PREVIOUS){
            this.getButtonPanel().getNext().setEnabled(Boolean.TRUE.booleanValue());
            this.getButtonPanel().getPrevious().setEnabled(Boolean.TRUE.booleanValue());
        } else if (state == (FINISH + NEXT + PREVIOUS)) {
            this.getButtonPanel().getFinish().setEnabled(Boolean.TRUE.booleanValue());
            this.getButtonPanel().getNext().setEnabled(Boolean.TRUE.booleanValue());
            this.getButtonPanel().getPrevious().setEnabled(Boolean.TRUE.booleanValue());
        }
    }

    /**
     * @return Returns the currentPanel.
     */
    public JPanel getCurrentPanel() {
        return currentPanel;
    }
    
    public void showHelp(){
        String currentPage = this.getWizardModel().getCurrentPage();
        
        if (currentPage != null){
            String helpKey = this.getWizardPage(currentPage).getHelpKey();
            HelpManager.getIntance().showHelp(this.getWizardModel().getWizard(), helpKey);
        } else {
            throw new IllegalArgumentException("The method getCurrentPage() return null");
        }
    }
}
