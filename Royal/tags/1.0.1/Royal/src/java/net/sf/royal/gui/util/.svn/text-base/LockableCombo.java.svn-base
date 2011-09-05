package net.sf.royal.gui.util;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class LockableCombo extends JPanel{

    private boolean focusState = Boolean.TRUE.booleanValue();
    private JComboBox comboBox;
    private JTextField textfield;
    private GridBagConstraints gbc;
    private String[] items;
    private String itemToSelect;
    private LockableComboListener listener;
    private boolean blockListener = Boolean.FALSE.booleanValue();
    
    
    public LockableCombo(){
        this.setLayout(new GridBagLayout());
        gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(0,0,0,0);
        
        items = new String[1];
        items[0] = "";
        itemToSelect = "";
        
        this.changeView();
    }
    
    private void changeView(){
        this.removeAll();
        JComponent cpt = null;
        if (focusState){
            comboBox = new JComboBox();
            comboBox.setEditable(Boolean.TRUE.booleanValue());
            if (listener != null){
                this.addLockableListener(listener);
            }
            cpt = comboBox;
        } else {
            textfield = new JTextField();
            textfield.setFocusable(Boolean.FALSE.booleanValue());
            cpt = textfield;
            
        }
        this.add(cpt, gbc);
        this.revalidate();
        this.update();
    }
    
    private void update(){
        this.blockListener = Boolean.TRUE.booleanValue();
        if (focusState){
            comboBox.removeAllItems();
            for(int i=0;i<items.length;i++){
                ((DefaultComboBoxModel) comboBox.getModel()).addElement(items[i]);
            }
            comboBox.setSelectedItem(itemToSelect);
        } else {
            textfield.setText(itemToSelect);
        }
        this.blockListener = Boolean.FALSE.booleanValue();
    }
    
    public void setItems(String[] items, String itemToSelect){
        this.items = items;
        this.itemToSelect = itemToSelect;
        this.update();
    }
    
    public String getSelectedItem(){
        if (focusState){
            return (String) comboBox.getSelectedItem();
        } else {
            return textfield.getText();
        }
    }
    
    public boolean isSelectedItemInItemList(){
        for(int i=0;i<this.items.length;i++){
            if (items[i].equals(getSelectedItem())){
                return Boolean.TRUE.booleanValue();
            }
        }
        return Boolean.FALSE.booleanValue();
    }
   
    public boolean isFocusState() {
        return focusState;
    }

    public void setFocusState(boolean focusState) {
        this.focusState = focusState;
        this.changeView();
    }
    
    public void addLockableListener(LockableComboListener cl){
        listener = cl;
        this.comboBox.addItemListener(new ItemListener(){

            public void itemStateChanged(ItemEvent e) {
                if (focusState 
                        && e.getStateChange() == ItemEvent.SELECTED 
                        && !e.getItem().equals(itemToSelect)
                        && !blockListener){
                    listener.actionPerformed();
                }
            }
            
        });
    }

}
