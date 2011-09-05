package net.sf.royal.gui.util;

import java.awt.Color;

import javax.swing.JTextArea;

public class MandatoryTextArea extends JTextArea implements Checkable{

    public MandatoryTextArea(){
        super();
        this.setOpaque(Boolean.TRUE.booleanValue());
    }
    
    
    public boolean check() {
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
