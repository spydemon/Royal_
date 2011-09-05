package net.sf.royal.gui.util;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;

import net.sf.royal.gui.manager.LocaleManager;

public class LogDialog extends JDialog 
{
    private String fileLog;
    
    public LogDialog(String fileLog, String title){
        this.fileLog = fileLog;
        this.initLayout();
        this.setModal(Boolean.TRUE.booleanValue());
    }
    
    
    private void initLayout() {
        JPanel panel = new JPanel();
        panel.setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.insets = new Insets(5,5,5,5);
        
        JTabbedPane tabbedPane = new JTabbedPane();
        tabbedPane.addTab(LocaleManager.getInstance().getString("log_debug"), new PanePanel(fileLog));
        
        panel.add(tabbedPane, gbc);
        
        gbc.gridy++;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.CENTER;
        
        JButton button = new JButton(LocaleManager.getInstance().getString("ok"));
        button.addActionListener(new ActionListener(){

            public void actionPerformed(ActionEvent arg0) {
                dispose();
            }
            
        });
        
        panel.add(button, gbc);
        
        this.getContentPane().add(panel);
    }
    
    
    private class PanePanel extends JPanel{
        
        public PanePanel(String file){
            JEditorPane pane = null;
            try {
                pane = new JEditorPane(new File(file).toURI().toURL());
            } catch (Exception e) {
                e.printStackTrace();
            }
            pane.setEditable(Boolean.FALSE.booleanValue());
            pane.setOpaque(Boolean.TRUE.booleanValue());
            
            this.setLayout(new GridBagLayout());
            GridBagConstraints gbc = new GridBagConstraints();

            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.gridwidth = 1;
            gbc.gridheight = 1;
            gbc.weightx = 1;
            gbc.weighty = 1;
            gbc.fill = GridBagConstraints.BOTH;
            gbc.insets = new Insets(5,5,5,5);
            
            JScrollPane sp = new JScrollPane(pane);
            sp.setPreferredSize(new Dimension(900, 400));
            sp.setMinimumSize(new Dimension(400, 350));
            this.add(sp, gbc);
        }
    }

}
