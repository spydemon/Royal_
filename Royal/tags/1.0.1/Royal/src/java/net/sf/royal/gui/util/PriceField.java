package net.sf.royal.gui.util;

import java.awt.Color;
import java.text.ParseException;

import javax.swing.JTextField;

import net.sf.royal.gui.manager.LocaleManager;

public class PriceField extends JTextField implements Checkable {

    private boolean mandatory;

    public PriceField(){
        
    }
    
    public PriceField(boolean mandatory){
        this.mandatory = mandatory;
    }
    
    public boolean check() {
        try {
            if (mandatory && this.getText().equals("")){
                this.setIncorrect();
                return false;
            }
            LocaleManager.getInstance().getPriceFloat(this.getText());
        } catch (Exception e) {
            this.setIncorrect();
            return Boolean.FALSE.booleanValue();
        }
        this.setCorrect();
        return Boolean.TRUE.booleanValue();
    }

    public void setCorrect() {
        this.setForeground(Color.black);
        this.setBackground(Color.white);
    }

    public void setIncorrect() {
        this.setForeground(Color.white);
        this.setBackground(Checkable.errorBackGroundColor);
    }

    public void setText(float price) {
        super.setText(LocaleManager.getInstance().getPriceText(price));
    }
    
    public float getPrice() throws ParseException{
        return LocaleManager.getInstance().getPriceFloat(this.getText());
    }
}
