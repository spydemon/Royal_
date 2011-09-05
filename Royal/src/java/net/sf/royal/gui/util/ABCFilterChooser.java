package net.sf.royal.gui.util;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JTextField;
import javax.swing.border.EtchedBorder;

import net.sf.royal.gui.manager.IconManager;

public class ABCFilterChooser extends JPanel {

    private JTextField text;
    private JLabel button;
    private JPopupMenu menu;
    private ABCPanel panel;
    
    public ABCFilterChooser() {
        text = new JTextField(15);
        button = new JLabel(IconManager.getIcon("abcfilter.gif"));
        this.button.addMouseListener(new MouseAdapter() {

            public void mouseClicked(MouseEvent e) {
                panel.reset();
                menu.show(text, 0, text.getHeight());
            }

            public void mouseEntered(MouseEvent e) {
                button.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
            }
            
        });
        menu = new JPopupMenu();
        panel = new ABCPanel(menu, text);
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

        this.add(text, gbc);

        gbc.gridx++;
        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.BOTH;
        button.setBorder(BorderFactory
                        .createEtchedBorder(EtchedBorder.LOWERED));
        this.add(button, gbc);
        
        this.menu.add(panel);
    }
    
    public String getText(){
        return panel.getText();
    }
    
    public JTextField getTextField(){
        return text;
    }
    
    public void addActionListener(ActionListener listener){
        this.text.addActionListener(listener);
    }
    
    private class ABCPanel extends JPanel{
        
        private final Color selectedColor = new Color(248,212,200);
        
        private LetterPanel selectedPanel;
        private JTextField text;
        private JPopupMenu menu;
        
        public ABCPanel(JPopupMenu menu, JTextField text){
            this.menu = menu;
            this.text = text;
            this.setOpaque(Boolean.TRUE.booleanValue());
            this.setBackground(Color.WHITE);
            this.setLayout(new GridBagLayout());
            GridBagConstraints gbc = new GridBagConstraints();
            
            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.weightx = 0;
            gbc.weighty = 0;
            gbc.fill = GridBagConstraints.NONE;
            gbc.anchor = GridBagConstraints.CENTER;
            
            this.add(new LetterPanel("#", this), gbc);
            
            gbc.gridx++;
            this.add(new LetterPanel("A", this), gbc);
            
            gbc.gridx++;
            this.add(new LetterPanel("B", this), gbc);
            
            gbc.gridx++;
            this.add(new LetterPanel("C", this), gbc);
            
            gbc.gridx++;
            this.add(new LetterPanel("D", this), gbc);
            
            gbc.gridx++;
            this.add(new LetterPanel("E", this), gbc);
            
            gbc.gridx++;
            this.add(new LetterPanel("F", this), gbc);
            
            gbc.gridx++;
            this.add(new LetterPanel("G", this), gbc);
            
            gbc.gridx++;
            gbc.weightx = 1;
            this.add(new LetterPanel("H", this), gbc);
            
            gbc.gridx = 0;
            gbc.gridy++;
            gbc.weightx = 0;
            gbc.weighty = 1;
            this.add(new LetterPanel("I", this), gbc);
            
            gbc.gridx++;
            this.add(new LetterPanel("J", this), gbc);
            
            gbc.gridx++;
            this.add(new LetterPanel("K", this), gbc);
            
            gbc.gridx++;
            this.add(new LetterPanel("L", this), gbc);
            
            gbc.gridx++;
            this.add(new LetterPanel("M", this), gbc);
            
            gbc.gridx++;
            this.add(new LetterPanel("N", this), gbc);
            
            gbc.gridx++;
            this.add(new LetterPanel("O", this), gbc);
            
            gbc.gridx++;
            this.add(new LetterPanel("P", this), gbc);
            
            gbc.gridx++;
            gbc.weightx = 1;
            this.add(new LetterPanel("Q", this), gbc);
            
            gbc.gridx = 0;
            gbc.gridy++;
            gbc.weightx = 0;
            gbc.weighty = 1;
            this.add(new LetterPanel("R", this), gbc);
            
            gbc.gridx++;
            this.add(new LetterPanel("S", this), gbc);
            
            gbc.gridx++;
            this.add(new LetterPanel("T", this), gbc);
            
            gbc.gridx++;
            this.add(new LetterPanel("U", this), gbc);
            
            gbc.gridx++;
            this.add(new LetterPanel("V", this), gbc);
            
            gbc.gridx++;
            this.add(new LetterPanel("W", this), gbc);
            
            gbc.gridx++;
            this.add(new LetterPanel("X", this), gbc);
            
            gbc.gridx++;
            this.add(new LetterPanel("Y", this), gbc);
            
            gbc.gridx++;
            gbc.weightx = 1;
            this.add(new LetterPanel("Z", this), gbc);
        }
        
        public void select(LetterPanel panel, boolean doubleClick){
            for(int i=0;i<this.getComponentCount();i++){
                if (this.getComponent(i) instanceof LetterPanel && !this.getComponent(i).equals(panel)){
                    this.getComponent(i).setBackground(Color.WHITE);
                }
            }
            panel.setBackground(this.selectedColor);
            this.selectedPanel = panel;
            this.text.setText(this.selectedPanel.getText());
            if (doubleClick){
                menu.setVisible(Boolean.FALSE.booleanValue());
            }
        }
        
        public void reset(){
            if (this.selectedPanel != null){
                this.selectedPanel.setBackground(Color.WHITE);
            }
        }
       
        public String getText(){
            return text.getText();
        }
    }
    
    private class LetterPanel extends JLabel{

        private ABCPanel panel;
        
        public LetterPanel(String text, ABCPanel abcpanel){
            super(text);
            this.panel = abcpanel;
            this.setOpaque(Boolean.TRUE.booleanValue());
            this.setBackground(Color.WHITE);
            this.setPreferredSize(new Dimension(20,20));
            this.setHorizontalAlignment(JLabel.CENTER);
            this.setVerticalAlignment(JLabel.CENTER);
            this.addMouseListener(new BirDyMouseListener(){
                public void rightMouseClicked(MouseEvent e) {
                }

                @Override
                public void leftMouseClicked(MouseEvent e) {
                    if (e.getClickCount() == 1){
                        panel.select(LetterPanel.this, Boolean.FALSE.booleanValue());
                    } else if (e.getClickCount() == 2){
                        panel.select(LetterPanel.this, Boolean.TRUE.booleanValue());
                    }
                }
                
            });
        }
    }
}
