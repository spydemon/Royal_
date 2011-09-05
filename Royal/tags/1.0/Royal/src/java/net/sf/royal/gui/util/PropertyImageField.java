package net.sf.royal.gui.util;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Locale;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.UIManager;

import net.sf.royal.gui.manager.IconManager;
import net.sf.royal.gui.manager.LocaleManager;

import com.jgoodies.plaf.plastic.Plastic3DLookAndFeel;
import com.jgoodies.plaf.plastic.theme.ExperienceBlue;

public class PropertyImageField extends JPanel implements Checkable{

    private RegexpTextField height;
    private RegexpTextField width;
    private horizontalArrow horizontalArrow;
    private VerticalArrow verticalArrow;
    private JLabel image;
    private boolean same;
    
    
    public PropertyImageField(){
        this.initLayout();
    }

    public PropertyImageField(boolean same){
        this.same = same;
        this.initLayout();
    }


    public void initLayout(){
        
        this.setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.insets = new Insets(5, 5, 5, 5);
        
        this.add(new JLabel(LocaleManager.getInstance().getString("width") + " :"), gbc);
        
        gbc.gridx++;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        
        this.width = new RegexpTextField(5, RegexpTextField.NUMBER, true);
        
        this.width.getAccessibleContext().addPropertyChangeListener(new PropertyChangeListener(){

            public void propertyChange(PropertyChangeEvent e) {
                if (e.getPropertyName().equals("AccessibleText")){
                    if (width.check()){
                        horizontalArrow.setContent(width.getText());
                    } else {
                        horizontalArrow.setContent("0");
                    }
                }
            }
            
        });
        
        this.width.addActionListener(new ActionListener(){

            public void actionPerformed(ActionEvent e) {
                if (same){
                    height.setText(width.getText());
                }
            }
            
        });
        
        this.width.addFocusListener(new FocusListener(){

            public void focusGained(FocusEvent e) {
                //DO nothing
            }

            public void focusLost(FocusEvent e) {
                if (same){
                    height.setText(width.getText());
                }
            }
            
        });
        
        this.add(this.width, gbc);
        
        gbc.gridx++;
        gbc.weightx = 0;
        gbc.gridheight = 2;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.insets = new Insets(5, 5, 5, 5);
        
        this.image = new JLabel(IconManager.getIcon("gf.png"));
        this.add(this.image, gbc);
        
        gbc.gridx++; 
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        
        this.verticalArrow = new VerticalArrow();
        
        this.add(this.verticalArrow, gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.insets = new Insets(5, 5, 5, 5);
        
        this.add(new JLabel(LocaleManager.getInstance().getString("height") + " :"), gbc);
        
        gbc.gridx++;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        
        this.height = new RegexpTextField(5, RegexpTextField.NUMBER, true);
        
        this.height.getAccessibleContext().addPropertyChangeListener(new PropertyChangeListener(){

            public void propertyChange(PropertyChangeEvent e) {
                if (e.getPropertyName().equals("AccessibleText")){
                    if (height.check()){
                        verticalArrow.setContent(height.getText());
                    } else {
                        verticalArrow.setContent("0");
                    }
                }
            }
            
        });
        
        this.height.addActionListener(new ActionListener(){

            public void actionPerformed(ActionEvent e) {
                if (same){
                    width.setText(height.getText());
                }
            }
            
        });
        
        this.height.addFocusListener(new FocusListener(){

            public void focusGained(FocusEvent e) {
                //DO nothing
            }

            public void focusLost(FocusEvent e) {
                if (same){
                    width.setText(height.getText());
                }
            }
            
        });
        
        this.add(this.height, gbc);
        
        gbc.gridx = 2;
        gbc.gridy++;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.weighty = 1;
        gbc.weightx = 0;
        
        this.horizontalArrow = new horizontalArrow();
        
        this.add(horizontalArrow, gbc);
        
    }
    
    public class horizontalArrow extends JPanel{
        
        private String content;
        
        public horizontalArrow(){
            this.setPreferredSize(new Dimension(64, 12));
        }
        

        @Override
        public void paint(Graphics g) {
            super.paint(g);
            int w = 0;
            int toCompare = 0;
            if (content != null && !content.equals("")){
                toCompare = Integer.valueOf(content);
            }
            
            if (toCompare >= 1000){
                w = 15;
            } else if (toCompare >= 100){
                w = 20;
            } else if (toCompare >= 10){
                w = 25;
            } else {
                w = 30;
            }
            g.drawString(String.valueOf(toCompare), w, 10);
        }

        public void setContent(String content) {
            this.content = content;
            this.repaint();
        }
        
        
    }
    
    public class VerticalArrow extends JPanel{
        
        private String content;
        
        public VerticalArrow(){
            this.setPreferredSize(new Dimension(40, 64));
        }
        

        @Override
        public void paint(Graphics g) {
            super.paint(g);
            int toCompare = 0;
            if (content != null && !content.equals("")){
                toCompare = Integer.valueOf(content);
            }
            g.drawString(String.valueOf(toCompare), 0, 40);
        }

        public void setContent(String content) {
            this.content = content;
            this.repaint();
        }
        
        
    }
    
    public static void main(String[] args) {
        Locale.setDefault(LocaleManager.getInstance().loadLocale());
        Plastic3DLookAndFeel.setCurrentTheme(new ExperienceBlue());
        try {
            UIManager.setLookAndFeel(new Plastic3DLookAndFeel());
        } catch (Exception e) {
            
        }
        JFrame frame = new JFrame();
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.getContentPane().add(new PropertyImageField(true));
        frame.pack();
        frame.setVisible(true);
    }

    public boolean check() {
        return this.width.check() && this.height.check();
    }

    public void setCorrect() {
        //Do nothing
    }

    public void setIncorrect() {
        //Do nothing
    }

    public int getHeightSize() {
        return Integer.valueOf(height.getText());
    }

    public int getWidthSize() {
        return Integer.valueOf(width.getText());
    }

    public RegexpTextField getHeightField() {
        return height;
    }

    public RegexpTextField getWidthField() {
        return width;
    }
}
