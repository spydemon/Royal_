package net.sf.royal.gui.wizard.borrowsystem;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JTextField;

import net.sf.royal.gui.manager.LocaleManager;

@SuppressWarnings("serial")
public class BorrowAddDialog extends JDialog {
	private JTextField jtfName;
	private JTextField jtfFirstname;
	private JButton jbOK;
	private JButton jbCancel;
	private GridBagConstraints gbc;
	private BorrowDialog parent;
	
	private static String name = LocaleManager.getInstance().getString("name") + " : ";
	private static String firstname = LocaleManager.getInstance().getString("firstname") + " : ";
	private static String title = LocaleManager.getInstance().getString("borrower_title");
	
	public BorrowAddDialog(BorrowDialog parent)
	{
		super(parent, title, ModalityType.TOOLKIT_MODAL);
		this.parent = parent;
		this.setLocationRelativeTo(null);
		this.init();
		this.initListener();
		this.pack();
		this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		this.setVisible(true);
	}
	
	public BorrowAddDialog(BorrowDialog parent, long id)
	{
		this(parent);
	}
	
	private void init()
	{
		this.setLayout(new GridBagLayout());
		this.gbc = new GridBagConstraints();
		
		
		this.gbc.fill = GridBagConstraints.HORIZONTAL;
		this.gbc.gridx = 0;
		this.gbc.gridy = 0;
		this.gbc.insets.set(0,5,5,5);
		
		/**** Name ****/
		this.gbc.weightx = 0;
		this.gbc.gridwidth = 1;
		this.add(new JLabel(name), this.gbc);
		
		this.gbc.gridx ++; 
		this.gbc.weightx = 1.0;
		this.gbc.gridwidth = GridBagConstraints.REMAINDER;
		this.jtfName = new JTextField();
		this.add(this.jtfName, this.gbc);
		
		/**** Firstname ****/
		this.gbc.gridx = 0;
		this.gbc.gridy ++;
		this.gbc.weightx = 0;
		this.gbc.gridwidth = 1;
		this.add(new JLabel(firstname), this.gbc);
		
		this.gbc.gridx ++;
		this.gbc.weightx = 1.0;
		this.gbc.gridwidth = GridBagConstraints.REMAINDER;
		this.jtfFirstname = new JTextField();
		this.add(this.jtfFirstname, this.gbc);
		
		/**** Button ok ****/
		this.gbc.gridx = 1;
		this.gbc.gridy ++;
		this.gbc.gridwidth = 1;
		this.jbOK = new JButton(BorrowDialog.ok);
		this.add(this.jbOK, this.gbc);
		
		/**** Button cancel ****/
		this.gbc.gridx ++;
		this.jbCancel = new JButton(BorrowDialog.cancel);
		this.add(this.jbCancel, this.gbc);
	}
	
	private void initListener()
	{
		this.jbCancel.addActionListener(new ActionListener() 
		{
			@Override
			public void actionPerformed(ActionEvent e) {
				BorrowAddDialog.this.dispose();
			}
		});
	}
}
