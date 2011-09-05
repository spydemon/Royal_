package net.sf.royal.gui.datepicker;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.text.ParseException;
import java.util.Date;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JTextField;
import javax.swing.border.EtchedBorder;

import net.sf.royal.gui.manager.IconManager;
import net.sf.royal.gui.manager.LocaleManager;

public class JDatePicker extends JPanel {

    private JTextField textfield;
    private JLabel button;
    private Calendar calendar;
    private JPopupMenu calendarPopUp = new JPopupMenu();
    private boolean focusState = Boolean.TRUE.booleanValue();
    private boolean completeDate = Boolean.TRUE.booleanValue();
    
    private JPopupMenu menu;
    private JMenuItem delete;
    
    public JDatePicker(boolean completeDate){
        this();
        this.completeDate = completeDate;
    }

    public JDatePicker() {
        super();
        this.textfield = new JTextField();
        this.textfield.setColumns(15);
        this.setOpaque(Boolean.TRUE.booleanValue());
        this.textfield.setFocusable(Boolean.FALSE.booleanValue());
        
        this.menu = new JPopupMenu();
        this.delete = new JMenuItem(LocaleManager.getInstance().getString("delete_object"));
        this.delete.setIcon(IconManager.getIcon("delete_enabled.png"));
        this.delete.setDisabledIcon(IconManager.getIcon("delete_disabled.png"));
        this.delete.setMnemonic(KeyEvent.VK_D);
        if (this.textfield.getText().equals("")){
            this.delete.setEnabled(Boolean.FALSE.booleanValue());
        }
        
        this.menu.add(this.delete);
        
        this.delete.addActionListener(new ActionListener(){

            public void actionPerformed(ActionEvent arg0) {
                textfield.setText("");
            }
            
        });
        
        this.button = new JLabel(IconManager.getIcon("error.png"));
        this.button.setDisabledIcon(IconManager.getIcon("error.png"));
        this.calendar = new Calendar(this);
        this.calendarPopUp.add(calendar);

        this.button.addMouseListener(new MouseAdapter() {

            public void mouseClicked(MouseEvent arg0) {
                if (focusState) {
                    JDatePicker.this.textfield.setText("");
                }
            }

            public void mouseEntered(MouseEvent arg0) {
                if (focusState){
                    button.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
                }
            }
            
        });
        this.textfield.addMouseListener(new MouseAdapter() {

            public void mouseClicked(MouseEvent arg0) {
                if (focusState) {
                    calendarPopUp.show(textfield, 0, textfield.getHeight());
                }
            }

            public void mouseEntered(MouseEvent arg0) {
                if (focusState){
                    button.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
                }
            }
            
        });
        this.initLayout();
    }

    private void initLayout() {
        this.setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        // gbc.insets = new Insets(0,2,0,2);
        gbc.fill = GridBagConstraints.HORIZONTAL;

        this.add(textfield, gbc);

        gbc.gridx++;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.BOTH;
        button.setBorder(BorderFactory
                        .createEtchedBorder(EtchedBorder.LOWERED));
        this.add(button, gbc);
    }

    public Component getJDatePicker() {
        return this;
    }

    /**
     * @return Returns the date.
     */
    public Date getDate() {
        try {
            return LocaleManager.getInstance().getDate(this.textfield.getText(), this.completeDate);
        } catch (ParseException e) {
            return null;
        }
    }

    /**
     * @param date
     *            The date to set.
     */
    public void setDate(Date date) {
        if (date == null){
            delete.setEnabled(Boolean.FALSE.booleanValue());
        } else {
            delete.setEnabled(Boolean.TRUE.booleanValue());
        }
        this.textfield.setText(LocaleManager.getInstance().getTextDate(date,
                completeDate));
    }

    public void setFocusState(boolean focusState) {
        this.focusState = focusState;
        this.button.setEnabled(focusState);
    }

    public JTextField getTextfield() {
        return textfield;
    }
    
    public void closePopUp(){
        this.calendarPopUp.setVisible(Boolean.FALSE.booleanValue()); 
    }
}
