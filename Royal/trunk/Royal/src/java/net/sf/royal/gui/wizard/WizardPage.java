package net.sf.royal.gui.wizard;

import javax.swing.JPanel;

public class WizardPage{

    private WizardView wizardView;
    
    private JPanel panel;
    
    private String key;
    
    private String helpKey;
    
    public WizardPage(String key, JPanel panel){
        super();
        this.key = key;
        this.panel = panel;
    }
    
    public WizardPage(String key, JPanel panel, String helpKey){
        this(key, panel);
        this.helpKey = helpKey;
    }
    
    
    /**
     * @return Returns the panel.
     */
    public JPanel getPanel() {
        return panel;
    }
    /**
     * @param panel The panel to set.
     */
    public void setPanel(JPanel panel) {
        this.panel = panel;
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
    }

    /**
     * @return Returns the key.
     */
    public String getKey() {
        return key;
    }

    /**
     * @param key The key to set.
     */
    public void setKey(String key) {
        this.key = key;
    }

    public String getHelpKey() {
        return helpKey;
    }

    public void setHelpKey(String helpKey) {
        this.helpKey = helpKey;
    }
}
