package net.sf.royal.gui.wizard;

import java.awt.Dimension;

import javax.swing.JLabel;
import javax.swing.JPanel;

public class DownloadActionPanel extends JPanel {

    private DownloadAction[] downloadActions;
    
    public DownloadActionPanel(DownloadAction[] downloadActions){
        this.downloadActions = downloadActions;
        for (int i = 0; i < this.downloadActions.length; i++) {
            this.add(new OneActionPanel(downloadActions[i].getTitle(), downloadActions[i].getKey()));
        }         
    }
    
    private class OneActionPanel extends JPanel {
                
        public OneActionPanel(String title, String key){
            this.setPreferredSize(new Dimension(100, 50));
            this.add(new JLabel(title));
        }
    }
}
