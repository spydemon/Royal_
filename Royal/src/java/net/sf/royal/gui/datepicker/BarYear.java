package net.sf.royal.gui.datepicker;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.Date;

import javax.swing.JTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.border.AbstractBorder;

import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.util.DigitDocument;

public class BarYear extends JPanel implements DatePlayer {
    
    private DatePickerButton next = new NextButton();
    private DatePickerButton previous = new PreviousButton();
    private JTextField year = new JTextField(4);
    
    private Date displayedDate; 
    
    public BarYear(Date currentDate, Date displayedDate){
        this.displayedDate = displayedDate;
        this.year.setHorizontalAlignment(JLabel.CENTER);
        DigitDocument dd = new DigitDocument();
        dd.setMaxLength(4);
        this.year.setDocument(dd);
        this.setBorder(new BarYearBorder());
        this.setBackground(new Color(200,212,247));
        this.initLayout();
    }
    
    private void initLayout(){
        this.setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 0;
        gbc.weighty = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.insets = new Insets(0,10,0,0);
        gbc.fill = GridBagConstraints.NONE;
        
        this.add(previous, gbc);
        
        gbc.gridx++;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(0,0,0,0);
        gbc.anchor = GridBagConstraints.CENTER;
        
        this.add(year,gbc);
        
        gbc.gridx++;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.insets = new Insets(0,0,0,10);
        gbc.anchor = GridBagConstraints.CENTER;
        
        this.add(next,gbc);
    }

    /**
     * @return Returns the year.
     */
    public JTextField getYear() {
        return year;
    }

    /**
     * @return Returns the next.
     */
    public JLabel getNext() {
        return next;
    }

    /**
     * @return Returns the previous.
     */
    public JLabel getPrevious() {
        return previous;
    }

    public void displayDate(Date date)
    {
    	displayDate(date, true);
    }
    
    public void displayDate(Date date, boolean updateLabel) {
        this.displayedDate = date;
        if(updateLabel)
        {
        	SwingUtilities.invokeLater(new Runnable() {
        		
        		@Override
        		public void run() {
        			BarYear.this.year.setText(LocaleManager.getInstance().getYearDate(displayedDate));				
        		}
        	});
        }
    }
    
    private class BarYearBorder extends AbstractBorder {
        public void paintBorder(Component c, Graphics g, int x, int y, int width, int height){
            g.setColor(DateLabel.titleColor);
            
            g.drawLine(x, y + width - 1, x + 1, y + width - 1);
            g.drawLine(x, y + width - 2, x, y + width - 2);
            
            g.drawLine(x + width - 1, y + width - 1, x + width - 2, y + width - 1);
            g.drawLine(x + width - 1, y + width - 2, x + width - 1, y + width - 2);
        }
    }

    public int getValue() {
        return DateHelper.getYear(displayedDate);
    }

    public void setSelectedDate(Date date) {
    }

}
