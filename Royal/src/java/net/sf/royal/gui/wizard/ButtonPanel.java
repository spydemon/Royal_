package net.sf.royal.gui.wizard;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

import net.sf.royal.gui.manager.IconManager;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.util.BirDyMouseListener;

public class ButtonPanel extends JPanel {

    private WizardView wizardView;
    private JButton previous;
    private JButton next;
    private JButton finish;
    private JButton cancel;
    private JLabel help;
    
    public ButtonPanel(WizardView wizardView){
        this.wizardView = wizardView;
        previous = new JButton(LocaleManager.getInstance().getString("back"));
        previous.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent arg0) {
                ButtonPanel.this.wizardView.previous();
            }
        });
        next = new JButton(LocaleManager.getInstance().getString("next"));
        next.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent arg0) {
                ButtonPanel.this.wizardView.next();
            }
        });
        finish = new JButton(LocaleManager.getInstance().getString("finish"));
        finish.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent arg0) {
                ButtonPanel.this.wizardView.finish();
            }
        });
        cancel = new JButton(LocaleManager.getInstance().getString("cancel"));
        cancel.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent arg0) {
                ButtonPanel.this.wizardView.cancel();
            }
        });
        help = new JLabel(IconManager.getIcon("help.gif"));
        help.setDisabledIcon(IconManager.getIcon("blank.gif"));
        //help.setToolTipText(LocaleManager.getInstance().getString("help_contents"));
        help.setEnabled(Boolean.FALSE.booleanValue());
        help.addMouseListener(new BirDyMouseListener(){

            @Override
            public void rightMouseClicked(MouseEvent e) {
                //DO nothing
            }

            @Override
            public void leftMouseClicked(MouseEvent e) {
                getWizardView().showHelp();
            }
            
        });
        this.initLayout();
    }

    /**
     * @return Returns the wizardView.
     */
    public WizardView getWizardView() {
        return wizardView;
    }

    /**
     * @param wizardView The wizardView to set.
     */
    public void setWizardView(WizardView wizardView) {
        this.wizardView = wizardView;
    }
    
    private void initLayout(){
        this.setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5,5,5,5);
        
        this.add(help, gbc);
        
        gbc.gridx++;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.EAST;
        
        this.add(previous, gbc);
        
        gbc.gridx++;
        gbc.weightx = 0;
        
        this.add(next, gbc);

        gbc.gridx++;
        gbc.weightx = 0;
        
        this.add(finish, gbc);

        gbc.gridx++;
        gbc.weightx = 0;
        
        this.add(cancel, gbc);
    }

    /**
     * @return Returns the cancel.
     */
    public JButton getCancel() {
        return cancel;
    }

    /**
     * @return Returns the finish.
     */
    public JButton getFinish() {
        return finish;
    }

    /**
     * @return Returns the next.
     */
    public JButton getNext() {
        return next;
    }

    /**
     * @return Returns the previous.
     */
    public JButton getPrevious() {
        return previous;
    }

    public JLabel getHelp() {
        return help;
    }
}
