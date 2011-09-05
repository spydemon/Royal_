package net.sf.royal.gui.util;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.Icon;
import javax.swing.JLabel;

import net.sf.royal.gui.manager.IconManager;

public class FocusableCheckBox extends JLabel {

	private boolean selected = Boolean.FALSE.booleanValue();
	private boolean focusState = Boolean.TRUE.booleanValue();
    private boolean forcedIcon = Boolean.FALSE.booleanValue();
	
	private Icon iconSelected = IconManager.getIcon("selected_true_enabled.gif");
	private Icon iconUnSelected = IconManager.getIcon("selected_false_enabled.gif");
    private Icon iconSelectedDisabled = IconManager.getIcon("selected_true_disabled.gif");
    private Icon iconUnSelectedDisabled = IconManager.getIcon("selected_false_disabled.gif");
    
    private FocusableCheckBoxGroup group = null;
	
	public FocusableCheckBox(String text){
		this.setText(text);
		this.setIcon(iconUnSelected);
		this.initListener();
	}
    
    public FocusableCheckBox(Icon iconSelected, Icon iconUnSelected){
        this.iconSelected = iconSelected;
        this.iconUnSelected = iconUnSelected;
        this.setText("");
        this.setIcon(iconUnSelected);
        this.initListener();
    }
	
	private void initListener(){
		this.addMouseListener(new MouseListener(){

			public void mouseClicked(MouseEvent arg0) {
				if (focusState){
					setSelected(!selected);
                    if (group != null){
                        if (isSelected()){
                            group.select(FocusableCheckBox.this);
                        } else {
                            group.unselect(FocusableCheckBox.this);
                        }
                    }
				}
			}

			public void mousePressed(MouseEvent arg0) {}

			public void mouseReleased(MouseEvent arg0) {}

			public void mouseEntered(MouseEvent arg0) {}

			public void mouseExited(MouseEvent arg0) {}
			
		});
	}

	/**
	 * @return Returns the focusState.
	 */
	public boolean isFocusState() {
		return focusState;
	}

	/**
	 * @param focusState The focusState to set.
	 */
	public void setFocusState(boolean focusState) {
		this.focusState = focusState;
	}

	/**
	 * @return Returns the selected.
	 */
	public boolean isSelected() {
		return selected;
	}

	/**
	 * @param selected The selected to set.
	 */
	public void setSelected(boolean selected) {
		this.selected = selected;
        if (!forcedIcon){
            if (this.selected){
                if (this.isEnabled()){
                    this.setIcon(iconSelected);
                } else {
                    this.setDisabledIcon(iconSelectedDisabled);
                }
            } else {
                if (this.isEnabled()){
                    this.setIcon(iconUnSelected);
                } else {
                    this.setDisabledIcon(iconUnSelectedDisabled);
                }
            }
        }
		
	}

    public FocusableCheckBoxGroup getGroup() {
        return group;
    }

    public void setGroup(FocusableCheckBoxGroup group) {
        this.group = group;
    }
    
    public void setForcedIcon(Icon icon){
        this.setIcon(icon);
        this.forcedIcon = Boolean.TRUE.booleanValue();
    }

    public void setForcedIcon(boolean forcedIcon) {
        this.forcedIcon = forcedIcon;
    }

    @Override
    public void setEnabled(boolean enabled) {
        super.setEnabled(enabled);
        if (!forcedIcon){
            if (this.selected){
                if (enabled){
                    this.setIcon(iconSelected);
                } else {
                    this.setDisabledIcon(iconSelectedDisabled);
                }
            } else {
                if (enabled){
                    this.setIcon(iconUnSelected);
                } else {
                    this.setDisabledIcon(iconUnSelectedDisabled);
                }
            }
        }
    }
}
