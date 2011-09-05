package net.sf.royal.gui.util;

import java.awt.Color;

import net.sf.royal.gui.datepicker.JDatePicker;

public class MandatoryDatePicker extends JDatePicker implements Checkable {

    public boolean check() {
        if (this.getDate() == null){
            this.setIncorrect();
            return Boolean.FALSE.booleanValue();
        }
        this.setCorrect();
        return Boolean.TRUE.booleanValue();
    }

    public void setCorrect() {
        this.getTextfield().setBackground(Color.white);
        this.getTextfield().setForeground(Color.black);
    }

    public void setIncorrect() {
        this.getTextfield().setBackground(Checkable.errorBackGroundColor);
        this.getTextfield().setForeground(Color.white);
    }

}
