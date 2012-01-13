package net.sf.royal.gui.wizard.add_dialog;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.JTextField;

import net.sf.royal.persistency.PersistencyManager;

/**
 * This class provides a simple panel composed by a JComboBox which 
 * can be used to display a list of suggestions
 * @author Steven Nguyen
 * TODO : A chaque insertion de caractere, afficher une liste correspondants/contenant la chaine introduite.
 */
@SuppressWarnings("serial")
public class JEntryPane extends JPanel
{
	/**
	 * For autocompletion.<br/>Will use the data about the
	 * albums in the database
	 */
	public static final int AUTHOR = 1;
	/**
	 * For autocompletion<br/>
	 * Use series for autocompletion
	 */
	public static final int SERIE = 2;
	/**
	 * For autocompletion<br/>
	 * Use Collections for autocompletion
	 */
	public static final int COLLECTION = 3;
	/**
	 * For autocompletion
	 * Use Editors for autocompletion
	 */
	public static final int EDITOR = 4;
	/**
	 * For autocompletion <br/>
	 * Use Types for autocompletion
	 */
	public static final int TYPE = 5;
	
	public static final int BIBLIO = 6;
	/**
	 * For autocompletion<br/>
	 * Use a custom List of data
	 * @see #fill
	 */
	public static final int CUSTOM = 0;
	/**
	 * The Layout of the JEntryPane
	 */
	private GridBagLayout gbLayout;
	/**
	 * GridBagConstraints used by the GridBagLayout
	 * @see #getGBC
	 */
	private GridBagConstraints gbc;
	/**
	 * The JComboBox of the JEntryPane
	 * Un bouton pour parcourir tout les choix ?
	 */
	private JComboBox jcbEntry;
	/**
	 * The type of completion to use
	 */
	private int type;
	/**
	 * The JTextField of the JComboBox
	 */
	private JTextField jtf;
	/**
	 * The controller
	 */
	private JEntryDocument controller;
	
// Constructors
	
	/**
	 * Creates a JEntryPane
	 */
	public JEntryPane()
	{
		/* The layout */
		this.gbLayout = new GridBagLayout();
		this.setLayout(this.gbLayout);
		
		this.gbc = new GridBagConstraints();
		this.gbc.gridx = 0;
		this.gbc.gridy = 0;
		this.gbc.anchor = GridBagConstraints.WEST;
		this.gbc.fill = GridBagConstraints.HORIZONTAL;
		this.gbc.weightx = 1.0;
		this.gbc.weighty = 0;
		this.gbc.insets = new Insets(0,0,0,5);
		
		/* The combobox */
		this.jcbEntry = new JComboBox();
		this.jcbEntry.setEditable(true);
		this.jcbEntry.setAlignmentY(LEFT_ALIGNMENT);

		this.add(this.jcbEntry, this.gbc);
		this.gbc.weightx = 0;
		this.gbc.gridx ++;
	}
	
	/**
	 * Create a new JEntryPane with an autocomplete JComboBox.
	 * You have to specify which type of elements you want to have
	 * in the ComboBox. If the type is not CUSTOM, the JEntryPane
	 * will complete the ComboBox, otherwise you have to fill it with
	 * the method {@link #fill(List)}
	 * @param type The type of items
	 * 
	 */
	public JEntryPane(int type)
	{
		this();
		this.type = type;
		this.refresh();
	}
	
	
	/**
	 * Get the text of the JTextField
	 * @return The text of the JTextField
	 */
	public String getText()
	{
		JTextField tmp = (JTextField) this.jcbEntry.getEditor().getEditorComponent();
		return tmp.getText();
	}

	/**
	 * Set the text of the textfield
	 * @param text The text to be set
	 */
	public void setText(String text)
	{
		//System.out.println("setText");
		this.controller.setText(text);
	}
	
	/**
	 * Get the GridBagLayout of the JEntryPane
	 * The next position is after the ComboBox
	 * @return The GridBagLayout
	 */
	protected GridBagConstraints getGBC()
	{
		return this.gbc;
	}
	
	/**
	 * Add an item into the ComboBox
	 * @param a The item
	 */
	public void addItem(Object a)
	{
		this.jcbEntry.addItem(a);
	}
	
	/**
	 * Add an ActionListener to the JComboBox
	 * @param al The ActionListener to be added
	 */
	public void addActionListener(ActionListener al)
	{
		this.jcbEntry.addActionListener(al);
	}
	
	/**
	 * Get the combobox of the JEntryPane
	 * @return The comboBox
	 */
	public JComboBox getComboBox()
	{
		return this.jcbEntry;
	}
	
	
	/**
	 * Get the type of the items in the JEntryPane
	 * @return Type defined by the static constants in JEntryPane
	 */
	public int getType()
	{
		return this.type;
	}
	
	/**
	 * Set the selected item in the ComboBox.<br/> If the id cannot 
	 * be found, there is no change
	 * @param id
	 */
	public void setID(long id)
	{
		this.controller.setID(id);
	}
	
	/**
	 * Get the ID of the selected Item. <br/>If there is no selected 
	 * item or the text is different, the method will return <code>null</code>
	 * @return
	 */
	public Long getID()
	{
		EntryPaneObject tmp = this.controller.getSelectedItem();
		return tmp.getID();
	}
	
	/**
	 * Fill the JEntryPane with the desired data.<br/>
	 * The JEntryPane will use the method toString() of the elements
	 * for autocompletion.
	 * @param ressource The list of element to autocomplete
	 */
	public void fill(List<EntryPaneObject> ressource)
	{
		this.jtf.setDocument(new JEntryDocument(this.jcbEntry, ressource));
	}
	
	/**
	 * Refresh the list of items in the combobox
	 */
	public void refresh()
	{
		List<EntryPaneObject> res;
		if(this.type == JEntryPane.AUTHOR)
		{
			res = EntryObjectConverter.getAuthorEntryObjects(PersistencyManager.findAuthors());
		}
		else if (this.type == JEntryPane.SERIE)
		{
			res = EntryObjectConverter.getSerieEntryObjects(PersistencyManager.findSeries());
			//this.searcher.start();
		}
		else if(this.type == JEntryPane.COLLECTION)
		{
			res = EntryObjectConverter.getCollectionEntryObjects(PersistencyManager.findCollections());
		}
		else if(this.type == JEntryPane.EDITOR)
		{
			res = EntryObjectConverter.getEditorEntryObjects(PersistencyManager.findEditors());
		}
		else if(this.type == JEntryPane.TYPE)
		{
			res = EntryObjectConverter.getTypeEntryObjects(PersistencyManager.findTypes());
		}
		else if(this.type == JEntryPane.BIBLIO){
			res = EntryObjectConverter.getBibliothequeEntryObjects(PersistencyManager.findLibs());
		}
		else
		{
			res = new ArrayList<EntryPaneObject>();
		}
		this.jtf = (JTextField) this.jcbEntry.getEditor().getEditorComponent();
		this.controller = new JEntryDocument(this.jcbEntry, res);
		this.jtf.setDocument(this.controller);
	}
}


