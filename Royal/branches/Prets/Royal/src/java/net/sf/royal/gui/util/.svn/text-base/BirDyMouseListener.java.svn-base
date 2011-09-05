package net.sf.royal.gui.util;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

public abstract class BirDyMouseListener implements MouseListener {

    public void mouseClicked(MouseEvent e) {
    }

    public void mousePressed(MouseEvent e) {
        if (e.isPopupTrigger()){
            rightMouseClicked(e);
        }
    }

    public void mouseReleased(MouseEvent e) {
        if (e.isPopupTrigger()){
            rightMouseClicked(e);
        } else {
            leftMouseClicked(e);
        }
    }

    public void mouseEntered(MouseEvent e) {
    }

    public void mouseExited(MouseEvent e) {
    }
    
    public abstract void rightMouseClicked(MouseEvent e);
    
    public abstract void leftMouseClicked(MouseEvent e);

}
