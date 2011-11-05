package net.sf.royal.gui.wizard.settings;

import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.Hashtable;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JTextField;

import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.PropertyManager;
import net.sf.royal.gui.wizard.settings.ChangeLanguagePanel.ItemChangeListener;

/**
 * Tab of the {@SettingsTabbedDialog} containing the stuff to switch the languages.
 * @author Maxime Kientz
 */
public class ChangeEmailPanel extends JPanel implements SettingsTab
{
	private String[] prt = {"IMAP","POP3"};
	private JLabel login;
	private JLabel mdp;
	private JLabel proto;
	private JLabel serv;
	private JTextField jtflogin = new JTextField(20);
	private JComboBox jtfproto = new JComboBox(prt);
	private JTextField jtfserv = new JTextField(20);
	private JPasswordField jpfmdp = new JPasswordField(20);

	// the boolean which will be returned by the methods
	// of the SettingsTab interface
	private boolean isChanged = false;

	public ChangeEmailPanel()
	{
		this.login = new JLabel(LocaleManager.getInstance().getString("login")+" : ");
		this.mdp = new JLabel(LocaleManager.getInstance().getString("password")+" : ");
		this.proto = new JLabel(LocaleManager.getInstance().getString("protocol")+" : ");
		this.serv = new JLabel(LocaleManager.getInstance().getString("mail_server")+" : ");
		this.jpfmdp.setEchoChar('*');
		this.setLayout(new GridBagLayout());
		this.login.setHorizontalAlignment(JLabel.RIGHT);
		this.mdp.setHorizontalAlignment(JLabel.RIGHT);
		this.proto.setHorizontalAlignment(JLabel.RIGHT);
		this.serv.setHorizontalAlignment(JLabel.RIGHT);
		this.jtflogin.setText(PropertyManager.getInstance().getProperty("mail_login"));
		this.jtfproto.setSelectedItem(PropertyManager.getInstance().getProperty("mail_protocol"));
		this.jpfmdp.setText(PropertyManager.getInstance().getProperty("mail_password"));
		this.jtfserv.setText(PropertyManager.getInstance().getProperty("mail_server"));
		this.jtflogin.addKeyListener(new ItemChangeListener());
		this.jpfmdp.addKeyListener(new ItemChangeListener());
		this.jtfproto.addActionListener(new ItemChangeListener());
		this.jtfserv.addKeyListener(new ItemChangeListener());
		GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.insets = new Insets(2,2,2,2);
		c.gridx=0;
		c.gridy=0;
		this.add(login,c);
		c.gridx +=1;
		this.add(jtflogin,c);
		c.gridy +=1;
		c.gridx =0;
		this.add(mdp, c);
		c.gridx +=1;
		this.add(jpfmdp,c);
		c.gridy +=1;
		c.gridx =0;
		this.add(proto, c);
		c.gridx +=1;
		this.add(jtfproto,c);
		c.gridy +=1;
		c.gridx =0;
		this.add(serv, c);
		c.gridx +=1;
		this.add(jtfserv,c);
		c.gridy +=1;
		c.gridx =0;
	}

	public boolean apply()
	{
		if(isChanged)
		{
			PropertyManager.getInstance().changeProperty("mail_login",this.jtflogin.getText());
			PropertyManager.getInstance().changeProperty("mail_password",new String(this.jpfmdp.getPassword()));
			PropertyManager.getInstance().changeProperty("mail_protocol",this.jtfproto.getSelectedItem().toString());
			PropertyManager.getInstance().changeProperty("mail_server",this.jtfserv.getText());
			this.isChanged = false;
			return false;
		}
		else
		{
			return false;
		}
	}

	public class ItemChangeListener implements KeyListener, ActionListener
	{
		@Override
		public void keyPressed(KeyEvent arg0) {
			// TODO Auto-generated method stub
			if(PropertyManager.getInstance().getProperty("mail_login").equals(ChangeEmailPanel.this.jtflogin.getText()) 
					&& PropertyManager.getInstance().getProperty("mail_password").equals(new String(ChangeEmailPanel.this.jpfmdp.getPassword()))
					&& PropertyManager.getInstance().getProperty("mail_protocol").equals(ChangeEmailPanel.this.jtfproto.getSelectedItem().toString())
					&& PropertyManager.getInstance().getProperty("mail_server").equals(ChangeEmailPanel.this.jtfserv.getText()))
			{
				ChangeEmailPanel.this.isChanged = false;
			}
			else
			{
				ChangeEmailPanel.this.isChanged = true;
			}
		}

		@Override
		public void keyReleased(KeyEvent arg0) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void keyTyped(KeyEvent arg0) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			// TODO Auto-generated method stub
			if(PropertyManager.getInstance().getProperty("mail_login").equals(ChangeEmailPanel.this.jtflogin.getText()) 
					&& PropertyManager.getInstance().getProperty("mail_password").equals(new String(ChangeEmailPanel.this.jpfmdp.getPassword()))
					&& PropertyManager.getInstance().getProperty("mail_protocol").equals(ChangeEmailPanel.this.jtfproto.getSelectedItem().toString())
					&& PropertyManager.getInstance().getProperty("mail_server").equals(ChangeEmailPanel.this.jtfserv.getText()))
			{
				ChangeEmailPanel.this.isChanged = false;
			}
			else
			{
				ChangeEmailPanel.this.isChanged = true;
			}
		}
	}
	
	@Override
	public boolean hasBeenChanged()
	{
		return this.isChanged;
	}
}
