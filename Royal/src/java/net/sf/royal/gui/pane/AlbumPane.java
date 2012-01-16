package net.sf.royal.gui.pane;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Enumeration;

import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;

import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.PropertyManager;
import net.sf.royal.gui.pane.album_pane.ComboBoxSortItem;
import net.sf.royal.gui.tree.AlbumTree;
import net.sf.royal.gui.tree.search.TreeSearchEngine;
import net.sf.royal.util.Tools;

/**
 * @author Soulou
 * Left pane of the main window, search and sort albums
 */
public class AlbumPane extends JPanel 
{
	/* Fields */
	private JLabel jlSearch;
	private JTextField jtfSearch;
	private JComboBox jcbSort;
	private AlbumTree atTree;
	private JPanel jpTools;
	
	private TreeModel backupTreeModel;
	
	private static AlbumPane instance;

	/* Constructors */
	private AlbumPane()
	{
		this.jlSearch = new JLabel(LocaleManager.getInstance().getString("search") + " : ");
		this.jtfSearch = new JTextField(10);
		this.jcbSort = new JComboBox();
		this.atTree = AlbumTree.getInstance();
		this.jpTools = new JPanel();
		GridBagConstraints gbc = new GridBagConstraints();
		
		JScrollPane jsp = new JScrollPane(this.atTree);
		this.setLayout(new GridBagLayout());
		
		gbc.fill = GridBagConstraints.BOTH;
		gbc.gridwidth = GridBagConstraints.REMAINDER;
		
		jpTools.add(this.jlSearch);
		jpTools.add(this.jtfSearch);	
		jpTools.add(this.jcbSort);

		this.add(jpTools,gbc);
		
		/* Another row */
		gbc.weighty = 1.0;
		gbc.weightx = 1.0;
		gbc.gridheight = GridBagConstraints.REMAINDER;
		
		this.add(jsp,gbc);
		this.setMinimumSize(new Dimension(400, this.jcbSort.getSize().height+10));
		this.initJcbSort();
		this.jtfSearch.getDocument().addDocumentListener(new DocumentListener() {
			
			@Override
			public void removeUpdate(DocumentEvent e) {
				AlbumPane.this.atTree.setModel(AlbumPane.this.backupTreeModel);
				AlbumPane.this.backupTreeModel = AlbumPane.this.atTree.doBackupTreeModel();
				if(!AlbumPane.this.jtfSearch.getText().isEmpty())
					TreeSearchEngine.search(AlbumPane.this.jtfSearch.getText());
			}
			
			@Override
			public void insertUpdate(DocumentEvent e) {
				TreeSearchEngine.search(AlbumPane.this.jtfSearch.getText());
			}
			
			@Override
			public void changedUpdate(DocumentEvent e) {				
			}
		});
	}
	
	private void initJcbSort()
	{
		String album = Tools.initCap(LocaleManager.getInstance().getString("album"));
		String author = Tools.initCap(LocaleManager.getInstance().getString("author"));
		String serie = Tools.initCap(LocaleManager.getInstance().getString("serie"));
		String type = Tools.initCap(LocaleManager.getInstance().getString("type"));
		String bib = Tools.initCap(LocaleManager.getInstance().getString("library"));
		String sep = " - ";
		String name = album;
		this.jcbSort.addItem(new ComboBoxSortItem(name,AlbumTree.SORT_ALBUM));
		name = serie + sep + album;
		this.jcbSort.addItem(new ComboBoxSortItem(name,AlbumTree.SORT_SERIE));
		name = author + sep + album;
		this.jcbSort.addItem(new ComboBoxSortItem(name,AlbumTree.SORT_AUTHOR));
		name = author + sep + serie + sep + album;
		this.jcbSort.addItem(new ComboBoxSortItem(name,AlbumTree.SORT_AUTHOR | AlbumTree.SORT_SERIE));
		name = type + sep + album;
		this.jcbSort.addItem(new ComboBoxSortItem(name,AlbumTree.SORT_TYPE));
		name = type + sep + serie + sep + album;
		this.jcbSort.addItem(new ComboBoxSortItem(name,AlbumTree.SORT_TYPE | AlbumTree.SORT_SERIE));
		name = bib + sep + album;
		this.jcbSort.addItem(new ComboBoxSortItem(name,AlbumTree.SORT_LIBRARY));
		/* We get the last sort, default = Author - Serie - Album */
		int sort_flags = Integer.valueOf(PropertyManager.getInstance().getProperty("sort"));
		/* We select the corresponding combobox item */
		for(int i = 0; i < this.jcbSort.getItemCount() ; i++)
			if(((ComboBoxSortItem)this.jcbSort.getItemAt(i)).getFlags() == sort_flags)
				this.jcbSort.setSelectedIndex(i);
		/* And we sort corresponding to the flags */
		this.atTree.sortAlbums(sort_flags);
		this.backupTreeModel = this.atTree.doBackupTreeModel();
		
		/* Add a listener on the combobox when the selected item change */
		this.jcbSort.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent ae)
			{
				ComboBoxSortItem cbse = (ComboBoxSortItem)AlbumPane.this.jcbSort.getSelectedItem();
				AlbumPane.this.atTree.sortAlbums(cbse.getFlags());
				AlbumPane.this.backupTreeModel = AlbumPane.this.atTree.doBackupTreeModel();
			}
		});
	}
	
	public static AlbumPane getInstance()
	{
		if(instance == null)
			instance = new AlbumPane();
		return instance;
	}
	
	public static void setInstance(AlbumPane in)
	{
		instance = in;
	}
	
	public String getSort()
	{
		return Integer.toString(((ComboBoxSortItem)this.jcbSort.getSelectedItem()).getFlags());
	}
	
	/**
	 * Refresh the tree view
	 */
	public void refresh()
	{
		this.atTree.sortAlbums(this.atTree.getCurrentFlags());
		this.backupTreeModel = this.atTree.doBackupTreeModel();
	}
}
