package net.sf.royal.gui.util;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import net.sf.royal.gui.manager.IconManager;
import net.sf.royal.gui.wizard.EsTexteEditorWizard;
import net.sf.royal.gui.wizard.EstexteEditorWizardModel;
import net.sf.royal.gui.wizard.WizardFactory;

public abstract class EsTexteField extends JLabel {
    
    public static final String HTML = "html";
    public static final String EDIT = "edit";

    private JEditorPane htmlView;
    private JTextArea esTexteView;
    private JButton esTextEditor;
    private String source;
    
    public EsTexteField(){
        htmlView = new JEditorPane();
        htmlView.setContentType("text/html");
        htmlView.setEditable(false);
        esTexteView = new JTextArea();
        esTexteView.setLineWrap(true);
        esTextEditor = new JButton(IconManager.getIcon("estexte.gif"));
        esTextEditor.addActionListener(new ActionListener(){

            public void actionPerformed(ActionEvent e) {
                EsTexteEditorWizard wizard = WizardFactory.createEsTexteWizard(esTexteView.getText());
                wizard.showWizard();
                if (!wizard.isCanceled()){
                    String res = ((EstexteEditorWizardModel) wizard.getWizardModel()).getSource();
                    esTexteView.setText(res);
                    doOnModification();
                }
            }
            
        });
        
        this.setFocusable(false);
    }
    
    private JPanel createHTMLView(){
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
        
        panel.add(new JScrollPane(htmlView), gbc);
        return panel;
    }
    
    private JPanel createEsTexteView(){
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
        
        panel.add(new JScrollPane(esTexteView), gbc);
        
        gbc.gridx++;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.NORTH;
        
        panel.add(esTextEditor, gbc);
        return panel;
    }
    
    private void changePanel(String type){
        this.setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.BOTH;
        
        JPanel panel = null;
        if (type.equals(EDIT)){
            panel = this.createEsTexteView();
        } else {
            panel = this.createHTMLView();
        }
        this.removeAll();
        this.add(panel, gbc);
        this.revalidate();
        this.repaint();
    }

    @Override
    public void setFocusable(boolean focusable) {
        super.setFocusable(focusable);
        if (focusable){
            changePanel(EDIT);
        } else {
            if (this.esTexteView != null){
                this.source = this.esTexteView.getText();
            }
            changePanel(HTML);
        }
        if (source != null){
            this.changeContent(focusable);
        }
    }

    public JTextArea getEsTexteView() {
        return esTexteView;
    }

    public void setEsTexteView(JTextArea esTexteView) {
        this.esTexteView = esTexteView;
    }

    public JEditorPane getHtmlView() {
        return htmlView;
    }

    public void setHtmlView(JEditorPane htmlView) {
        this.htmlView = htmlView;
    }

    public String getSource() {
        if (isFocusable()){
            return this.esTexteView.getText();
        } else {
            return this.source;
        }
    }

    public void setSource(String source) {
        this.source = source;
        this.changeContent(isFocusable());
    }
    
    private void changeContent(boolean focusable){
        if (focusable){
            this.esTexteView.setText(source);
        } else {
            this.htmlView.setText(EsTexteUtil.getHTML(source));
        }
    }
    
    public abstract void doOnModification();

    public JButton getEsTextEditor() {
        return esTextEditor;
    }
    
    
    
}
