package net.sf.royal.gui.datepicker;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;

import javax.swing.JLabel;
import javax.swing.border.AbstractBorder;

public class DateLabel extends JLabel {

    
    public static final int INVISIBLE = 0;
    public static final int SELECTED = 1;
    public static final int CURRENT = 2;
    public static final int ONLYVISIBLE = 3;
    public static final int TITLE = 4;
    public static final int SOLD = 5;
    
    public static Color titleColor = new Color(236,233,216);
    public static Color currentColor = new Color(200,212,247);
    public static Color selectedColor = new Color(248,212,200);
    private Color soldColor = new Color(232,242,254);
    
    private int state = ONLYVISIBLE;
    private int labelLocation = -1;
    private GridDay gridDay = null;
    
    public DateLabel(String text, int labelLocation, GridDay gridDay){
        super(text);
        this.labelLocation = labelLocation;
        this.gridDay = gridDay;
        this.setOpaque(Boolean.TRUE.booleanValue());
        this.setHorizontalAlignment(JLabel.CENTER);
        this.setVerticalAlignment(JLabel.CENTER);
        this.setPreferredSize(new Dimension(30,20));
        //this.setBorder(new DateLabelBorder());
    }

    /**
     * @return Returns the state.
     */
    public int getState() {
        return state;
    }

    /**
     * @param state The state to set.
     */
    public void setState(int state) {
        this.state = state;
        switch (state) {
        case ONLYVISIBLE:
            this.setForeground(Color.black);
            this.setBackground(Color.white);
            break;
        case INVISIBLE:
            this.setForeground(Color.white);
            this.setBackground(Color.white);
            break;
        case SELECTED:
            this.setForeground(Color.black);
            this.setBackground(selectedColor);
            break;
        case CURRENT:
            this.setForeground(Color.white);
            this.setBackground(currentColor);
            break;
        case TITLE:
            this.setForeground(Color.black);
            this.setBackground(titleColor);
            break;
        case SOLD:
            this.setForeground(Color.black);
            this.setBackground(soldColor);
            break;
        default:
            break;
        }
    }
    
    @SuppressWarnings("unused")
	private class DateLabelBorder extends AbstractBorder{
        
        public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
            DateLabel label = (DateLabel) c;
            if (label.getState() == DateLabel.CURRENT){
                this.drawLines(Color.blue, g, x, y, width, height);
            } else if (label.getState() == DateLabel.SELECTED){
                this.drawLines(Color.yellow, g, x, y, width, height);
            }
            
        }
        
        private void drawLines(Color color, Graphics g, int x, int y, int width, int height){
            g.setColor(color);
            g.drawLine(x + 3, y + 1, x + width - 3, y + 1);
            g.drawLine(x + 2, y + 2, x + 2, y + 2);
            
            g.drawLine(x + 1, y + 3, x + 1, y + height - 4);
            g.drawLine(x + 2, y + height - 3, x + 2, y + height - 3);
            
            g.drawLine(x + 3, y + height - 2, x + width - 3, y + height - 2);
            g.drawLine(x + width - 2, y + height - 3, x + width - 2, y + height - 3);
            
            g.drawLine(x + width - 1, y + height - 4, x + width - 1, y + 3);
            g.drawLine(x + width - 2, y + 2, x + width - 2, y + 2);
            
            g.setColor(Color.white);
            g.drawRect(0,0,width, height - 1);
            
            g.drawLine(x + 1, y + 1, x + 2, y + 1);
            g.drawLine(x, y + 2 , x + 1, y + 2);
            g.drawLine(x, y + 3 , x, y + 3);
            
            g.drawLine(x + 1, y + height - 3, x + 1, y + height - 3);
            g.drawLine(x + 1, y + height - 2, x + 2, y + height - 2);
            g.drawLine(x + 1, y + height -2, x + 3, y + height - 1);
            
            g.drawLine(x + width - 2, y + height - 2, x + width, y + height - 2);
            g.drawLine(x + width - 1, y + height - 3, x + width, y + height - 3);
            
            g.drawLine(x + width - 3, y, x + width, y);
            g.drawLine(x + width - 2, y + 1, x + width, y + 1);
            g.drawLine(x + width - 1, y + 2, x + width - 1, y + 2);
        }
    }

    /**
     * @return Returns the labelLocation.
     */
    public int getLabelLocation() {
        return labelLocation;
    }

    /**
     * @return Returns the gridDay.
     */
    @SuppressWarnings("unused")
	private GridDay getGridDay() {
        return gridDay;
    }    
}
