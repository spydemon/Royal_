package net.sf.royal.gui.util;

import java.awt.Color;

public abstract class MandatoryEsTexteField extends EsTexteField implements Checkable {

    public MandatoryEsTexteField(){
        super();
        this.getEsTexteView().setOpaque(true);
    }
    
    public boolean check() {
        if (this.getSource() == null || this.getSource().equals("")){
            this.setIncorrect();
            return Boolean.FALSE.booleanValue();
        } else {
            this.setCorrect();
            return Boolean.TRUE.booleanValue();
        }
    }

    public void setCorrect() {
        this.getEsTexteView().setForeground(Color.BLACK);
        this.getEsTexteView().setBackground(Color.WHITE);
    }

    public void setIncorrect() {
        this.getEsTexteView().setForeground(Color.WHITE);
        this.getEsTexteView().setBackground(Checkable.errorBackGroundColor);
    }
}
