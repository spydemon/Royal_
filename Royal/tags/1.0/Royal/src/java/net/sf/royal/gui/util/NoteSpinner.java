package net.sf.royal.gui.util;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerListModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import net.sf.royal.gui.manager.IconManager;

public class NoteSpinner extends JSpinner {
    
    public static final String PROPERTY = "property";
    
    private boolean isFocusable = Boolean.TRUE.booleanValue();
    private String note = null;
    private int maxNote = -1;
    
    public NoteSpinner(int maxNote){
        super();
        this.maxNote = maxNote + 1;
        String[] notes = new String[this.maxNote];
        for(int i=0;i<this.maxNote;i++){
            notes[i] = String.valueOf(i);
        }
        setModel(new SpinnerListModel(notes));
        setEditor(new Editor(this));
    }
    
    public int getNote(){
        return new Integer((String) this.getValue()).intValue();
    }
    
    public void setNote(String note){
        this.note = note;
        ((Editor) this.getEditor()).changeValue(note);
    }
    
    /**
     * @return Returns the isFocusable.
     */
    public boolean isFocusable() {
        return isFocusable;
    }

    /**
     * @param isFocusable The isFocusable to set.
     */
    public void setFocusable(boolean isFocusable) {
        this.isFocusable = isFocusable;
        if (note != null){
            this.setValue(note);
        }
    }
    
    public void firePropertyChangeEvent(){
        this.firePropertyChange(PROPERTY, Boolean.TRUE.booleanValue(), Boolean.FALSE.booleanValue());
    }
    
    private class Editor extends JPanel implements ChangeListener{

        private int preferredWidth = 100;
        private int preferredHeight = 20;
        private List<JLabel> labels = new ArrayList<JLabel>();
        
        Editor(JSpinner spinner) {
            GridBagConstraints gbc = new GridBagConstraints();
            this.setLayout(new GridBagLayout());
            this.setBackground(Color.white);
            gbc.gridx = -1;
            gbc.anchor = GridBagConstraints.WEST;
            gbc.weightx = 0;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.insets = new Insets(2,2,2,2);
            for(int i=0;i<maxNote/2;i++){
                JLabel label = new JLabel();
                gbc.gridx++;
                if (i == maxNote/2 - 1){
                    gbc.weightx = 1; 
                }
                this.add(label, gbc);
                labels.add(label);
            }
            spinner.addChangeListener(this);
            setPreferredSize(new Dimension(preferredWidth, preferredHeight));
        }

        private void changeValue(String value){
                int iValue = (new Integer(value)).intValue();
                if((iValue % 2) == 0){
                    for(int i=0;i<maxNote/2;i++){
                        if(i<iValue/2){
                            ((JLabel)labels.get(i)).setIcon(IconManager.getIcon("note2.gif"));
                        } else {
                            ((JLabel)labels.get(i)).setIcon(null);
                        }
                    }
                } else {
                    int currentIndex = 0;
                    for(int i=0;i<iValue/2;i++){
                        ((JLabel)labels.get(i)).setIcon(IconManager.getIcon("note2.gif"));
                        currentIndex++;
                    }
                    ((JLabel)labels.get(currentIndex)).setIcon(IconManager.getIcon("note1.gif"));
                    currentIndex++;
                    for(int i=currentIndex;i<maxNote/2;i++){
                        ((JLabel)labels.get(i)).setIcon(null);
                    }
                }
            }

        public void stateChanged(ChangeEvent e) {
            if (isFocusable){
                JSpinner spinner = (JSpinner)e.getSource();
                if (!note.equals((String) spinner.getValue())){
                    note = (String) spinner.getValue();
                    changeValue((String) spinner.getValue());
                    firePropertyChangeEvent();
                }
                
            }
        }
    }
}
