package net.sf.royal.gui.util;

import java.awt.Color;

import javax.swing.JTextField;

public class MandatoryTextField extends JTextField implements Checkable{
    
    private boolean special = Boolean.FALSE.booleanValue();

    public MandatoryTextField(){
        super();
        this.setOpaque(Boolean.TRUE.booleanValue());
    }
    
    public MandatoryTextField(boolean special){
        this();
        this.special = special;
    }
    
    public MandatoryTextField(int row){
        super(row);
        this.setOpaque(Boolean.TRUE.booleanValue());
    }
    
    
    public boolean check() {
        if (this.special){
            return Boolean.TRUE.booleanValue();
        }
        if (this.getText() == null || this.getText().equals("")){
            this.setIncorrect();
            return Boolean.FALSE.booleanValue();
        } else {
            this.setCorrect();
            return Boolean.TRUE.booleanValue();
        }
    }

    public void setCorrect() {
        this.setForeground(Color.BLACK);
        this.setBackground(Color.WHITE);
    }

    public void setIncorrect() {
        this.setForeground(Color.WHITE);
        this.setBackground(Checkable.errorBackGroundColor);
    }

}
