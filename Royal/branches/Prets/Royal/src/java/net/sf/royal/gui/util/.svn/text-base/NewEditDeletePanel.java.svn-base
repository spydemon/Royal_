package net.sf.royal.gui.util;

import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.EtchedBorder;

import net.sf.royal.gui.manager.IconManager;

public class NewEditDeletePanel extends JPanel {

    private boolean editAvailable = Boolean.TRUE.booleanValue();
    
    private Button neew;
    private Button edit;
    private Button delete;
    private NewEditDeleteListener newEditDeleteListener;
    
    public NewEditDeletePanel(){
        this.initLayout();
        this.initListener();
    }

    public NewEditDeletePanel(boolean editAvailable) {
        this.editAvailable = editAvailable;
        this.initLayout();
        this.initListener();
    }
    
    private void initListener() {

        neew.addMouseListener(new MouseAdapter(){
            public void mouseClicked(MouseEvent arg0) {
                if (newEditDeleteListener != null && neew.isFocusableState()){
                    newEditDeleteListener.neew();
                }
            }
            
        });

        
        if (editAvailable){
            edit.addMouseListener(new MouseAdapter(){
                public void mouseClicked(MouseEvent arg0) {
                    if (newEditDeleteListener != null && edit.isFocusableState()){
                        newEditDeleteListener.edit();
                    }
                }
                
            });
        }
        
        delete.addMouseListener(new MouseAdapter(){
            public void mouseClicked(MouseEvent arg0) {
                if (newEditDeleteListener != null && delete.isFocusableState()){
                    newEditDeleteListener.delete();
                }
            }
            
        });
        
    }

    private void initLayout() {
        this.setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.insets = new Insets(1,1,1,1);
        
        
        neew = new Button(IconManager.getIcon("new_enabled.png"), IconManager.getIcon("new_disabled.png"));
        
        this.add(neew);
        
        if (editAvailable){
            gbc.gridy++;
            
            edit = new Button(IconManager.getIcon("edit_object_enabled.png"), IconManager.getIcon("edit_object_disabled.png"));
            
            this.add(edit);
        }
        
        
        gbc.gridy++;
        gbc.weighty = 1;
        
        delete = new Button(IconManager.getIcon("delete_enabled.png"), IconManager.getIcon("delete_disabled.png"));
        
        this.add(delete);
        
    }
    
    public void setFocusState(boolean state){
        delete.setFocusableState(state);
        neew.setFocusableState(state);
        if (edit != null){
            edit.setFocusableState(state);
        }
    }
    
    public void setDeleteFocusState(boolean state){
        delete.setFocusableState(state);
    }
    
    public void setNewFocusState(boolean state){
        neew.setFocusableState(state);
    }
    
    public void setEditFocusState(boolean state){
        edit.setFocusableState(state);
    }
    
    private class Button extends JPanel{
        
        private Icon enabled = null;
        private Icon disabled = null;
        private boolean focusableState = Boolean.TRUE.booleanValue();
        private JLabel label = null;
        
        public Button(Icon enabled, Icon disabled){
            this.enabled = enabled;
            this.disabled = disabled;
            this.label = new JLabel();
            this.label.setIcon(enabled);
            this.label.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
            this.label.setPreferredSize(new Dimension(24,24));
            ((FlowLayout) this.getLayout()).setVgap(3);
            this.add(this.label);
            this.addMouseListener(new MouseListener(){

                public void mouseClicked(MouseEvent arg0) {}

                public void mousePressed(MouseEvent arg0) {}

                public void mouseReleased(MouseEvent arg0) {}

                public void mouseEntered(MouseEvent e) {
                    if (focusableState){
                        e.getComponent().setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
                    }
                }

                public void mouseExited(MouseEvent e) {
                    e.getComponent().setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
                }
            });
        }
        
        public void setButtonIconEnabled(){
            this.label.setIcon(enabled);
            this.focusableState = Boolean.TRUE.booleanValue();
        }
        
        public void setButtonIconDisabled(){
            this.label.setIcon(disabled);   
            this.focusableState = Boolean.FALSE.booleanValue();
        }

        /**
         * @return Returns the focusableState.
         */
        public boolean isFocusableState() {
            return focusableState;
        }

        /**
         * @param focusableState The focusableState to set.
         */
        public void setFocusableState(boolean focusableState) {
            this.focusableState = focusableState;
            if (this.focusableState){
                this.setButtonIconEnabled();
            } else {
                this.setButtonIconDisabled();
            }
        }
    }
    
    public static void main(String[] args){
        JFrame frame = new JFrame();
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.getContentPane().add(new NewEditDeletePanel());
        frame.pack();
        frame.setVisible(Boolean.TRUE.booleanValue());
    }

    /**
     * @param newEditDeleteListener The newEditDeleteListener to set.
     */
    public void setNewEditDeleteListener(NewEditDeleteListener newEditDeleteListener) {
        this.newEditDeleteListener = newEditDeleteListener;
    }

    /**
     * @return Returns the delete.
     */
    public Button getDelete() {
        return delete;
    }

    /**
     * @return Returns the edit.
     */
    public Button getEdit() {
        return edit;
    }

    /**
     * @return Returns the neew.
     */
    public Button getNeew() {
        return neew;
    }

}
