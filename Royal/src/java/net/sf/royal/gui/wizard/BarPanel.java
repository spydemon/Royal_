package net.sf.royal.gui.wizard;

import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.JLabel;
import javax.swing.JPanel;

import net.sf.royal.gui.manager.IconManager;

public class BarPanel extends JPanel {

    private JLabel title = new JLabel();
    private JLabel subTitle = new JLabel();
    
    public BarPanel(String ttitle, String tsubtitle){
        super();
        this.setBackground(Color.white);
        this.setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 0;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(0,5,0,0);
        
        title.setText(ttitle);
        title.setFont(new Font("Arial", Font.BOLD, 13));
        this.add(title, gbc);
        
        gbc.gridy++;
        gbc.insets = new Insets(-2,15,0,0);
        subTitle.setFont(new Font("Helvetica", Font.PLAIN, 10));
        subTitle.setText(tsubtitle);
        this.add(subTitle, gbc);
        
        gbc.gridy = 0;
        gbc.gridx++;
        gbc.gridheight = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.insets = new Insets(0,0,0,0);
        
        JLabel label = new JLabel(IconManager.getIcon("banner.png"));
        label.setHorizontalAlignment(JLabel.RIGHT);
        
        this.add(label, gbc);
    }

    /**
     * @return Returns the subTitle.
     */
    public JLabel getSubTitle() {
        return subTitle;
    }

    /**
     * @param subTitle The subTitle to set.
     */
    public void setSubTitle(JLabel subTitle) {
        this.subTitle = subTitle;
    }

    /**
     * @return Returns the title.
     */
    public JLabel getTitle() {
        return title;
    }

    /**
     * @param title The title to set.
     */
    public void setTitle(JLabel title) {
        this.title = title;
    }
    
    /**
     * Set the subtitle text
     * @param text
     */
    public void setSubTitleText(String text){
        this.subTitle.setText(text);
    }
}
