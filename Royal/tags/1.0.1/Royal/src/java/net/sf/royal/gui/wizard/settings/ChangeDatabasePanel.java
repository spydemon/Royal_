package net.sf.royal.gui.wizard.settings;

import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;

import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import net.sf.royal.datamodel.HibernateUtil;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.MessagePaneManager;
import net.sf.royal.gui.manager.ShortcutManager;

/**
 * Tab of the {@SettingsTabbedDialog} containing the stuff to switch the database.
 * @author Maxime Kientz
 */
public class ChangeDatabasePanel extends JPanel implements SettingsTab
{
	private JLabel DB_choice;
	private JList jliDataBase;
	private JButton jbNewDB;
	private JButton jbDeleteDB;
	private JButton jbEditDB;
	
	private List<String> DBFiles;

	private Window parent;
	
	/** The boolean which will be returned by the methods
	 *  of the SettingsTab interface
	 */
	private boolean isChanged=false;
  	
	/**
	 * Main constructor
	 */
	public ChangeDatabasePanel(Window parent)
	{
		this.parent = parent;
		// setup of the layouts
		this.setLayout(new BorderLayout());
		JPanel jp = new JPanel(new GridBagLayout());
		GridBagConstraints gbc = new GridBagConstraints();

		gbc.insets = new Insets(5, 5, 5, 5);
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 3;
		
		// Choice of the database
		DB_choice = new JLabel(LocaleManager.getInstance().getString("database") + " : ");
		jp.add(DB_choice, gbc);
		
		gbc.gridwidth = 1;
		gbc.gridy++;
		gbc.gridwidth = 3;
		gbc.gridheight = 2;
		// Fill the list with the name of the databases
		jliDataBase = new JList();
		jliDataBase.setModel(new DefaultListModel());
		jliDataBase.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		JScrollPane jsp = new JScrollPane(jliDataBase);
		jp.add(jsp, gbc);
		
		gbc.gridheight = 1;
		gbc.gridwidth = 1;
		gbc.gridy+= 2;
		gbc.gridx = 0;
		this.jbNewDB = new JButton(LocaleManager.getInstance().getString("add"));
		this.jbNewDB.setMnemonic(ShortcutManager.ADD);
		jp.add(this.jbNewDB, gbc);
		
		gbc.gridx++;
		this.jbEditDB = new JButton(LocaleManager.getInstance().getString("edit"));
		this.jbEditDB.setMnemonic(ShortcutManager.EDIT);
		this.jbEditDB.setEnabled(false);
		jp.add(this.jbEditDB, gbc);
		
		gbc.gridx++;
		this.jbDeleteDB = new JButton(LocaleManager.getInstance().getString("delete"));
		this.jbDeleteDB.setMnemonic(ShortcutManager.DELETE);
		this.jbDeleteDB.setEnabled(false);
		jp.add(this.jbDeleteDB, gbc);
		this.refreshDatabaseList();
		
		this.add(jp, BorderLayout.NORTH);
		
		this.initListeners();
	}

	public void initListeners()
	{
		jliDataBase.addListSelectionListener(new ItemChangeListener());
		
		jbNewDB.addActionListener(new ActionListener()
		{	
			@Override
			public void actionPerformed(ActionEvent arg0) 
			{
				CreateDatabaseWizard cdw = new CreateDatabaseWizard(ChangeDatabasePanel.this.parent);
				cdw.setModal(true);
				cdw.setVisible(true);
				ChangeDatabasePanel.this.refreshDatabaseList();
			}
		});
		
		jbDeleteDB.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent arg0) 
			{
				if(MessagePaneManager.showConfirmation(LocaleManager.getInstance().getString("confirm_database_delete")))
				{
					HibernateUtil.removeDatabase(ChangeDatabasePanel.this.jliDataBase.getSelectedValue().toString());
				}
			}
		});
		
		jbEditDB.addActionListener(new ActionListener()
		{	
			@Override
			public void actionPerformed(ActionEvent arg0) 
			{
				CreateDatabaseWizard cdw = new CreateDatabaseWizard(ChangeDatabasePanel.this.parent, 
						ChangeDatabasePanel.this.jliDataBase.getSelectedValue().toString());
				cdw.setModal(true);
				cdw.setVisible(true);
				ChangeDatabasePanel.this.refreshDatabaseList();
			}
		});
	}
	
	public void refreshDatabaseList()
	{
		String[] defaultDatabase = new String[1];
		DefaultListModel model = (DefaultListModel)this.jliDataBase.getModel();
		model.removeAllElements();
		try {
			DBFiles = HibernateUtil.getListForCombo(defaultDatabase);
			String currentDB = HibernateUtil.getCurrentDatabase();
			for(int i = 0; i < DBFiles.size(); i++)
			{
				if(DBFiles.get(i).equals(currentDB))
				{
					DBFiles.remove(i);
					break;
				}
			}
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		for(String str : DBFiles)
		{
			model.addElement(str);
		}
	}

	public class ItemChangeListener implements ListSelectionListener
	{
		@Override
		public void valueChanged(ListSelectionEvent e) 
		{
			if(ChangeDatabasePanel.this.jliDataBase.getSelectedValue() != null)
			{
				ChangeDatabasePanel.this.jbDeleteDB.setEnabled(true);
				ChangeDatabasePanel.this.jbEditDB.setEnabled(true);
				ChangeDatabasePanel.this.isChanged = true;
			}
			else
			{
				ChangeDatabasePanel.this.jbDeleteDB.setEnabled(false);
				ChangeDatabasePanel.this.jbEditDB.setEnabled(false);
				ChangeDatabasePanel.this.isChanged = false;	
			}
		}
	}

	@Override
	public boolean apply()
	{
		if(jliDataBase.getSelectedValue() != null)
		{
			// Si le changement se passe bien
			if(HibernateUtil.changeDatabase(jliDataBase.getSelectedValue().toString()))
			{
				this.isChanged = false;
				this.refreshDatabaseList();
			}
		}
		/* Do not restart */
		return false;
	}
	
	@Override
	public boolean hasBeenChanged()
	{
		return this.isChanged;
	}
}

