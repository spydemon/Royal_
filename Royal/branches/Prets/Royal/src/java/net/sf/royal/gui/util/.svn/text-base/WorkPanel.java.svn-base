package net.sf.royal.gui.util;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;

import net.sf.royal.gui.manager.IconManager;

public class WorkPanel extends JPanel {

    public static WorkPanel instance = new WorkPanel();
    
    private JLabel label;
    private JProgressBar progressBar;
    private JLabel icon;
    
    private WorkPanel(){
        this.initLayout();
    }

    private void initLayout() {
        this.setPreferredSize(new Dimension(800, 600));
        this.setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.fill = GridBagConstraints.NONE;
        gbc.insets = new Insets(2,2,2,2);
        
        label = new JLabel("");
        label.setToolTipText("");
        label.setHorizontalAlignment(JLabel.RIGHT);
        
        this.add(this.label, gbc);
        
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        
        progressBar = new JProgressBar();
        progressBar.setPreferredSize(new Dimension(100, 15));
        progressBar.setEnabled(Boolean.FALSE.booleanValue());
        
        this.add(this.progressBar,gbc);
        
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        
        icon = new JLabel(IconManager.getIcon("progress_ok.gif"));
        
        this.add(this.icon, gbc);
    }
    
    public void setProgressBarMax(int total){
        this.progressBar.setMaximum(total);
    }
    
    public void setProgressBarValue(int value, String text, boolean enabled){
        this.progressBar.setEnabled(enabled);
        this.progressBar.setValue(value);
        if (text == null){
            this.label.setText("");
            label.setToolTipText("");
            this.icon.setIcon(IconManager.getIcon("progress_ok.gif"));
        } else {
            this.icon.setIcon(IconManager.getIcon("progress.gif"));
            this.label.setText(text);
            label.setToolTipText(text);
        }   
    }
    
    public void setError(){
        this.icon.setIcon(IconManager.getIcon("progress_error.gif"));
    }
}
