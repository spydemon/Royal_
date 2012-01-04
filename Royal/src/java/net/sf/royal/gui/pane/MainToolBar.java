package net.sf.royal.gui.pane;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JToolBar;
import javax.swing.tree.DefaultMutableTreeNode;

import net.sf.royal.gui.manager.IconManager;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.ShortcutManager;
import net.sf.royal.gui.tree.AlbumTree;
import net.sf.royal.gui.tree.DeleteActionListener;
import net.sf.royal.gui.tree.EditActionListener;
import net.sf.royal.gui.wizard.add_dialog.AlbumAddDialog;
import net.sf.royal.gui.wizard.add_dialog.LibraryAddDialog;

/** 
  * @author Soulou
  * Toolbar of the main window
  */
public class MainToolBar extends JToolBar
{
	/* Fields */
	private JButton jbAdd;
	private JButton jbEdit;
	private JButton jbDelete;
	private Object userObject;
	private EditActionListener editListener;
	private DeleteActionListener deleteListener;
	
	private static MainToolBar instance = null;
	
	/* Constructor */
	private MainToolBar()
	{
		super(LocaleManager.getInstance().getString("basic_actions"));

		this.jbAdd = this.makeButton(LocaleManager.getInstance().getString("add"),
									 IconManager.getIcon("add.png"),
									 LocaleManager.getInstance().getString("fulladd"),
									 ShortcutManager.ADD);
		
		this.jbAdd.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e)
			{
				new AlbumAddDialog();
			}
		});

		this.jbDelete = this.makeButton(LocaleManager.getInstance().getString("delete"), 
										IconManager.getIcon("delete.png"), 
										LocaleManager.getInstance().getString("fulldelete"), 
										ShortcutManager.DELETE);
		this.jbDelete.setEnabled(false);
		this.deleteListener = new DeleteActionListener(this.userObject);
		this.jbDelete.addActionListener(this.deleteListener);
		
		this.jbEdit = this.makeButton(LocaleManager.getInstance().getString("edit"),
									  IconManager.getIcon("edit.png"),
									  LocaleManager.getInstance().getString("fulledit"),
									  ShortcutManager.EDIT);
		this.jbEdit.setEnabled(false);
		this.editListener = new EditActionListener(null);
		this.jbEdit.addActionListener(new ActionListener() 
		{
			private DefaultMutableTreeNode dmtn;
			@Override
			public void actionPerformed(ActionEvent e) 
			{
				dmtn = (DefaultMutableTreeNode)AlbumTree.getInstance().getLastSelectedPathComponent();
				MainToolBar.this.editListener.setUserObject(dmtn.getUserObject());
				MainToolBar.this.editListener.actionPerformed(new ActionEvent(this, 0, "edit"));
			}
		});

		this.add(this.jbAdd);
		this.add(this.jbDelete);
		this.add(this.jbEdit);
	}

	private JButton makeButton(String s, Icon i, String desc, char mnemonic)
	{
		JButton button = new JButton(s,i);
		button.setToolTipText(desc);
		button.setMnemonic(mnemonic);
		return button;
	}
	
	public void activeEditButton(boolean b)
	{
		this.jbEdit.setEnabled(b);
	}
	
	public void setUserObject(Object o)
	{
		this.userObject = o;
		this.deleteListener.setUserObject(this.userObject);
	}
	
	/**
	 * At the first Selection, active the edit button
	 * @param o Album/Author/Serie/Type
	 */
	public void activeTools(Object o, boolean active)
	{
		this.jbEdit.setEnabled(active);
		this.jbDelete.setEnabled(active);
		this.userObject = o;
		this.deleteListener.setUserObject(this.userObject);
	}
	
	public static MainToolBar getInstance()
	{
		if(instance == null)
			instance = new MainToolBar();
		return instance;
	}
	
	public static void setInstance(MainToolBar in)
	{
		instance = in;
	}
}
