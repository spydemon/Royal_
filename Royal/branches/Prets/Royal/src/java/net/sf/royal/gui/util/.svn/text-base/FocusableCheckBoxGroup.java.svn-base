package net.sf.royal.gui.util;

import java.util.ArrayList;
import java.util.List;

public abstract class FocusableCheckBoxGroup {

    private List<FocusableCheckBox> list = new ArrayList<FocusableCheckBox>();
    private FocusableCheckBox selectedcbx = null;
    
    public void addCheckBox(FocusableCheckBox cbx){
        this.list.add(cbx);
        cbx.setGroup(this);
    }
    
    public void removeCheckBox(FocusableCheckBox cbx){
        this.list.remove(cbx);
    }
    
    public void removeAll(){
        this.list.clear();
        selectedcbx = null;
    }
    
    public void select(FocusableCheckBox cbx){
        if (selectedcbx != null){
            selectedcbx.setSelected(Boolean.FALSE.booleanValue());
            cbx.setSelected(Boolean.TRUE.booleanValue());
            selectedcbx = cbx;
        } else {
            selectedcbx = cbx;
        }
        this.doSelect();
    }
    
    public void unselect(FocusableCheckBox cbx){
        if (selectedcbx != null){
            if (selectedcbx == cbx){
                selectedcbx.setSelected(Boolean.FALSE.booleanValue());
            }
        }
        this.doUnselect();
    }

    public FocusableCheckBox getSelectedcbx() {
        return selectedcbx;
    }
    
    public abstract void doSelect();
    
    public abstract void doUnselect(); 
}
