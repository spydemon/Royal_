package net.sf.royal.gui.wizard.borrowsystem;

import java.awt.Dialog;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import java.util.ArrayList;
import java.util.Set;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;

import net.sf.royal.datamodel.Borrower;
import net.sf.royal.datamodel.Loan;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.MessagePaneManager;
import net.sf.royal.gui.wizard.add_dialog.JEntryPane;
import net.sf.royal.gui.wizard.add_dialog.EntryPaneObject;
import net.sf.royal.gui.wizard.add_dialog.BorrowerEntryObject;
import net.sf.royal.persistency.PersistencyManager;

@SuppressWarnings("serial")
public class BorrowDialog extends JDialog {
	private JEntryPane jepBorrow;
	private JButton jbOk;
	private JButton jbCreate;
	private JButton jbDelete;
	private JButton jbCancel;
	private GridBagConstraints gbc;
	private ActionListener al;
	
	private static final String title = LocaleManager.getInstance().getString("lend_to");
	private static final String label = LocaleManager.getInstance().getString("select_a_person");
	private static final String delete = LocaleManager.getInstance().getString("delete");
	public static final String ok = LocaleManager.getInstance().getString("ok");
	public static final String cancel = LocaleManager.getInstance().getString("cancel");
	private static final String create = "-->";
	private static final String must_select = LocaleManager.getInstance().getString("choose_error_selection");
	private static final String confirm_delete = LocaleManager.getInstance().getString("confirm_delete_borrower");
	private static final String borrower_has_loan = LocaleManager.getInstance().getString("borrower_has_loans");
	
	public BorrowDialog()
	{
		super(null, title, Dialog.ModalityType.DOCUMENT_MODAL);
		this.init();
	}
	
	/** 
	 * Init the components and place them in the Dialog
	 */
	private void init()
	{
		this.setLayout(new GridBagLayout());
		this.gbc = new GridBagConstraints();
		this.gbc.anchor = GridBagConstraints.WEST;
		this.gbc.fill = GridBagConstraints.HORIZONTAL;
		this.gbc.weightx = 0.0;
		this.gbc.weighty = 0.0;
		this.gbc.insets.set(0, 5, 5, 5);
		
		this.gbc.gridx = 0;
		this.gbc.gridy = 0;
		this.add(new JLabel(label), this.gbc);
		
		/**** JEntryPane ****/
		this.gbc.gridx=0;
		this.gbc.gridy ++;
		this.gbc.weightx = 1.0;
		this.jepBorrow = new JEntryPane(JEntryPane.CUSTOM);
		this.jepBorrow.fill(getData());
		this.add(this.jepBorrow, this.gbc);
		
		/**** Button delete ****/
		/*this.gbc.gridx ++;
		this.gbc.weightx = 0;
		this.jbDelete = new JButton(delete);
		this.add(this.jbDelete, this.gbc);*/
		
		/**** Button create ****/
		this.gbc.gridx ++;
		this.gbc.weightx = 0;
		this.gbc.fill = GridBagConstraints.NONE;
		this.jbCreate = new JButton(create);
		this.add(this.jbCreate, this.gbc);
		
		/**** Button ok ****/
		this.gbc.gridx = 0;
		this.gbc.gridy ++;
		this.gbc.anchor = GridBagConstraints.EAST;
		this.jbOk = new JButton(ok);
		this.add(this.jbOk, this.gbc);
		
		/**** Button cancel ****/
		this.gbc.gridx ++;
		this.jbCancel = new JButton(cancel);
		this.add(this.jbCancel, this.gbc);
		
		
		this.initListener();
	}
	
	private void initListener()
	{
		this.jbCancel.addActionListener(new ActionListener() 
		{
			@Override
			public void actionPerformed(ActionEvent e) {
				BorrowDialog.this.finish();
			}
		});
		
		this.jbOk.addActionListener(new ActionListener() 
		{
			@Override
			public void actionPerformed(ActionEvent e) {
				if(BorrowDialog.this.check())
				{
					BorrowDialog.this.fireActionPerformed();
				}
			}
		});
		
		/*this.jbDelete.addActionListener(new ActionListener() 
		{			
			@Override
			public void actionPerformed(ActionEvent e) {
				BorrowDialog.this.processDelete();
			}
		});*/
		this.jbCreate.addActionListener(new ActionListener() 
		{			
			@Override
			public void actionPerformed(ActionEvent e) {
				new BorrowAddDialog(BorrowDialog.this);
			}
		});
	}
	
	private static List<EntryPaneObject> getData()
	{
		ArrayList<EntryPaneObject> res = new ArrayList<EntryPaneObject>();
		List<Borrower> list = PersistencyManager.findBorrowers();
		
		for(Borrower b : list)
		{
			res.add(new BorrowerEntryObject(b));
		}
		
		return res;
	}

	
	/**
	 * Displays the Dialog
	 */
	public void display()
	{
		this.pack();
		this.setDefaultCloseOperation(HIDE_ON_CLOSE);
		this.setVisible(true);
	}
	
	/**
	 * Hide the dialog and remove actionListener
	 */
	private void finish()
	{
		this.dispose();
		Runtime.getRuntime().gc();
	}
	
	/**
	 * Add an ActionListener which will be notified when
	 * the user selects a Borrower
	 * @param al The ActionListener
	 */
	public void addActionListener(ActionListener al)
	{
		this.al = al;
	}
	
	/**
	 * Fire actionPerformed
	 */
	private void fireActionPerformed()
	{
		if(this.al != null)
		{
			this.al.actionPerformed(new ActionEvent(this, 0, ""));
		}
	}
	
	/**
	 * Get the id of a borrower, null if canceled or no selection
	 * @return The id of the selected Borrower
	 */
	public long getID()
	{
		return this.jepBorrow.getID();
	}
	
	/**
	 * Delete the selected Borrower
	 */
	private void processDelete()
	{
		if(!this.check())
		{
			return;
		}
		
		if(MessagePaneManager.showConfirmDeletion(confirm_delete))
		{
			long id = this.getID();
			// Check if there are no loan from the Borrower to delete
			Set<Loan> loans = PersistencyManager.findBorrowerByID(id).getLoans();
			
			if(loans.size() != 0)
			{
				if(MessagePaneManager.showConfirmation(borrower_has_loan))
				{
					
				}
			}
		}
	}
	
	/**
	 * Reset the BorrowDialog : <br/>
	 *  - no text in the JEntryPane, 
	 */
	private void reset()
	{
		//this.jepBorrow.setText("salut");
		
	}
	
	public static BorrowDialog getInstance(JComponent parent)
	{
		BorrowDialog instance = new BorrowDialog();
		instance.setLocationRelativeTo(null);
		instance.reset();
		return instance;
	}
	
	private boolean check()
	{
		if(this.jepBorrow.getID() != null)
		{
			return true;
		}
		else
		{
			MessagePaneManager.showInfoPane(must_select);
			return false;
		}
	}
}
