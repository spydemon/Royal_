package net.sf.royal.gui.datepicker;

import net.sf.royal.gui.manager.IconManager;

public class NextButton extends DatePickerButton {

    public NextButton(){
        super();
        this.enteredIcon = IconManager.getIcon("pick_next_entered.gif");
        this.exitedIcon = IconManager.getIcon("pick_next.gif");
        this.setIcon(this.exitedIcon);
        this.initListener();
    }
}
