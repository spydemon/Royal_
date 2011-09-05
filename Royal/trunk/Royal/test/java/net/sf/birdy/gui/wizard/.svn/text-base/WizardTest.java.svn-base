package net.sf.birdy.gui.wizard;

import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;

public class WizardTest extends Wizard{
    
    public WizardTest(String title){
        super(title);
        WizardModel model = new WizardModelTest();
        model.setWizardView(new WizardView("title", "SubTitle"));
        this.setWizardModel(model);
        
        //WizardPage creation
        JPanel panel1 = new JPanel();
        panel1.add(new JLabel("label1"));
        panel1.add(new JTextField(15));
        panel1.add(new JCheckBox());
        WizardPage wizardPage1 = new WizardPage("panel1", panel1);
        
        JPanel panel2 = new JPanel();
        panel2.add(new JLabel("label2"));
        panel2.add(new JTextArea(10,20));
        WizardPage wizardPage2 = new WizardPage("panel2", panel2);
        
        JPanel panel3 = new JPanel();
        panel3.add(new JLabel("label3 : This is the end"));
        WizardPage wizardPage3 = new WizardPage("panel3", panel3);
        
        model.getWizardView().addWizardPage(wizardPage1);
        model.getWizardView().addWizardPage(wizardPage2);
        model.getWizardView().addWizardPage(wizardPage3);
  
    }
    /**
     * @param args
     */
    public static void main(String[] args) {
        Wizard wizard = new WizardTest("Test");
        wizard.setDefaultCloseOperation(JDialog.EXIT_ON_CLOSE);
        wizard.showWizard();
    }

}
