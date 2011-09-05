package net.sf.royal.gui.datepicker;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.Icon;
import javax.swing.JLabel;

public abstract class DatePickerButton extends JLabel {

    protected Icon enteredIcon;
    protected Icon exitedIcon;
    
    protected void initListener(){
        this.addMouseListener(new MouseAdapter(){
            public void mouseEntered(MouseEvent arg0) {
                DatePickerButton.this.setIcon(enteredIcon);
                
            }

            public void mouseExited(MouseEvent arg0) {
                DatePickerButton.this.setIcon(exitedIcon);
            }
        });
        
    }
}
