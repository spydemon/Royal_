package net.sf.royal.gui.util;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.JCheckBox;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;

import net.sf.royal.gui.manager.IconManager;

public class CheckBoxBorder implements Border{

    private JCheckBox box;
    private Border border;
    private Container container;
    private Rectangle rect;
    
    private int offset = 5;
    
    private CheckBoxBorderListener checkBoxBorderListener;
    
    public CheckBoxBorder(String title, Border border, Container container){
        box = new JCheckBox(title);
        box.setSelectedIcon(IconManager.getIcon("selected_true_enabled.gif"));
        box.setIcon(IconManager.getIcon("selected_false_enabled.gif"));
        this.container = container;
        box.addMouseListener(new MouseAdapter(){
            public void mouseClicked(MouseEvent e) {
                box.setSelected(!box.isSelected());
                setComponentEnabled(box.isSelected());
                if (checkBoxBorderListener != null){
                    checkBoxBorderListener.selectionChanged();
                }
            }
        });
        
        container.addMouseListener(new MouseAdapter(){
            public void mouseClicked(MouseEvent e) {
                dispatchEvent(e);
            }
        });
        this.border = border;
    }
    
    public void paintBorder(Component c, Graphics g, int x, int y, int width, int height){ 
        Insets borderInsets = border.getBorderInsets(c); 
        Insets insets = getBorderInsets(c); 
        int temp = (insets.top-borderInsets.top)/2; 
        border.paintBorder(c, g, x, y+temp, width, height-temp); 
        Dimension size = box.getPreferredSize(); 
        rect = new Rectangle(offset, 0, size.width, size.height);
        SwingUtilities.paintComponent(g, box, (Container)c, rect);
    } 

    public Insets getBorderInsets(Component c) {
        Dimension size = box.getPreferredSize(); 
        Insets insets = border.getBorderInsets(c); 
        insets.top = Math.max(insets.top, size.height); 
        return insets; 
    }

    public boolean isBorderOpaque() {
        return false;
    }
    
    private void dispatchEvent(MouseEvent me) {
        if (rect != null && rect.contains(me.getX(), me.getY())) {
            Point pt = me.getPoint();
            pt.translate(-offset, 0);
            box.setBounds(rect);
            box.dispatchEvent(new MouseEvent(box, me.getID(), me.getWhen(),
                    me.getModifiers(), pt.x, pt.y, me.getClickCount(), me
                            .isPopupTrigger(), me.getButton()));
            if (!box.isValid()){
                container.repaint();
            }
        }
    } 
    
    private void setComponentEnabled(boolean enable){
        Object[] components = container.getComponents();
        for (int i = 0; i < components.length; i++) {
            ((Component) components[i]).setEnabled(enable);
        }
    }
    
    public void setEnabled(boolean enable){
        this.box.setSelected(enable);
        this.setComponentEnabled(enable);
        if (!box.isValid()){
            container.repaint();
        }
    }
    
    /**
     * @return Returns the box.
     */
    public JCheckBox getBox() {
        return box;
    }

    /**
     * @param checkBoxBorderListener The checkBoxBorderListener to set.
     */
    public void setCheckBoxBorderListener(CheckBoxBorderListener checkBoxBorderListener) {
        this.checkBoxBorderListener = checkBoxBorderListener;
    }
}
