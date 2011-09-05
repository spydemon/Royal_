package net.sf.royal.gui.util;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.JToggleButton;

import net.sf.royal.gui.manager.IconManager;

public class EsTexteEditorPanel extends JPanel {
    
    private JEditorPane htmlView;
    private JTextPane esTexteView;
    private JToggleButton synchronize;
    private EstexteTransformer transformer;
    
    public EsTexteEditorPanel(String source){
        this.initLayout(source);
    }

    private void initLayout(String source) {
        this.transformer = new EstexteTransformer();
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

        this.add(this.createLeftPanel(source), gbc);
        
        gbc.gridx++;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        
        synchronize = new JToggleButton(IconManager.getIcon("sync.gif"));
        synchronize.setSelected(true);
        this.add(synchronize, gbc);
        
        gbc.gridx++;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.BOTH;
        
        this.add(this.createRightPanel(source), gbc);
     
        this.initListener();
    }
    
    private JPanel createLeftPanel(String source){
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
        
        esTexteView = new JTextPane();
        esTexteView.setText(source);
        
        panel.add(new JScrollPane(esTexteView), gbc);
        
        return panel;
    }
    
    private JPanel createRightPanel(String source){
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
        
        htmlView = new JEditorPane();
        htmlView.setContentType("text/html");
        htmlView.setEditable(false);
        htmlView.setText(EsTexteUtil.getHTML(source));
        
        panel.add(new JScrollPane(htmlView), gbc);
        
        return panel;
    }
    
    private void initListener(){
        this.esTexteView.getAccessibleContext().addPropertyChangeListener(new PropertyChangeListener(){

            public void propertyChange(PropertyChangeEvent e) {
                if (e.getPropertyName().equals("AccessibleText") && synchronize.isSelected()){
                    transformer.setSourceToTransform(esTexteView.getText());
                    transformer.setMustRun(true);
                    if (!transformer.isRunning){
                        new Thread(transformer).start();   
                    }
                }
            }
            
        });

    }
    
    private class EstexteTransformer implements Runnable {
        
        private String sourceToTransform;
        private boolean isRunning = false;
        private boolean mustRun = false;

        public void setMustRun(boolean mustRun) {
            this.mustRun = mustRun;
        }

        public void setSourceToTransform(String sourceToTransform) {
            this.sourceToTransform = sourceToTransform;
        }

        public void run() {
            isRunning = true;
            while(mustRun){
                mustRun = false;
                String html = EsTexteUtil.getHTML(sourceToTransform);
                htmlView.setText(html);
            }
            
            isRunning = false;
        }
   
    }

    public JTextPane getEsTexteView() {
        return esTexteView;
    }

}
