package net.sf.birdy.gui.wizard;

import javax.print.attribute.standard.Finishings;
import javax.swing.JCheckBox;

public class WizardModelTest extends WizardModel {

    private static int FIRST = 0;
    private static int SECOND = 1;
    private static int THIRD = 2;
    
    private int state = -1;
    
    private boolean skipSecond = Boolean.FALSE.booleanValue();
    
    public WizardModelTest(){
        super();
    }

    public String getNextPage() {
        if (state == -1){
            this.getWizardView().setButtonState(WizardView.NEXT);
            this.state = FIRST;
            return "panel1";
        } else if (state == FIRST){
            skipSecond = ((JCheckBox) this.getWizardView().getCurrentPanel().getComponent(2)).isSelected();
            if (skipSecond){
                this.getWizardView().setButtonState(WizardView.PREVIOUS + WizardView.FINISH);
                this.state = THIRD;
                return "panel3";
            } else {
                this.getWizardView().setButtonState(WizardView.NEXT + WizardView.PREVIOUS);
                this.state = SECOND;
                return "panel2";
            }
        } else if (state == SECOND){
            this.getWizardView().setButtonState(WizardView.PREVIOUS + WizardView.FINISH);
            this.state = THIRD;
            return "panel3";
        }
        return null;
    }
    
    public String getPreviousPage(){
        if (state == SECOND){
            this.getWizardView().setButtonState(WizardView.NEXT);
            this.state = FIRST;
            return "panel1";
        } if (state == THIRD){
            if (skipSecond){
                this.getWizardView().setButtonState(WizardView.NEXT);
                this.state = FIRST;
                return "panel1";
            } else {
                this.getWizardView().setButtonState(WizardView.NEXT + WizardView.PREVIOUS);
                this.state = SECOND;
                return "panel2";
            }
        }
        return null;
    }

    public boolean finish() {
        return Boolean.TRUE.booleanValue();
    }
    
}
