package net.sf.royal.gui.util;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.StyleSheet;

import net.sf.royal.gui.manager.LocaleManager;

import org.apache.log4j.Logger;

public class FileDialog extends JDialog {
    
    private final static Logger logger = Logger.getLogger(FileDialog.class);

    private JEditorPane pane;

    private String paneCss = "pane.css";
    
    public FileDialog(String file, String title, boolean sysResource, boolean html){
        this.setTitle(title);
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
        
        pane = new JEditorPane();
        if (html){
            pane.setContentType("text/html;charset=utf-8");
            StyleSheet css = ((HTMLEditorKit) pane.getEditorKit()).getStyleSheet();
            css.importStyleSheet(this.getClass().getClassLoader().getResource(paneCss ));
        }
        try {
            if (sysResource){
                pane = new JEditorPane(ClassLoader.getSystemResource(file));
            } else {
                pane = new JEditorPane(new File(file).toURI().toURL());
            }
            
            pane.setEditable(Boolean.FALSE.booleanValue());
            pane.setOpaque(Boolean.TRUE.booleanValue());
        } catch (MalformedURLException e) {
            e.printStackTrace();
            logger.error(e.getMessage());
        } catch (IOException e) {
            e.printStackTrace();
            logger.error(e.getMessage());
        }
        
        JScrollPane sp = new JScrollPane(pane);
        sp.setPreferredSize(new Dimension(400, 350));
        panel.add(sp, gbc);
        
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
        
        this.setModal(Boolean.TRUE.booleanValue());
        //this.setResizable(Boolean.FALSE.booleanValue());
    }
}
