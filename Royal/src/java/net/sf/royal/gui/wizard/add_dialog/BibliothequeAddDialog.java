package net.sf.royal.gui.wizard.add_dialog;

import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import net.sf.royal.datamodel.Collection;
import net.sf.royal.datamodel.Editor;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.MessagePaneManager;
import net.sf.royal.gui.manager.ShortcutManager;
import net.sf.royal.gui.pane.AlbumPane;
import net.sf.royal.gui.util.RegexpTextField;
import net.sf.royal.persistency.PersistencyManager;
import net.sf.royal.persistency.SaveItemPersistency;

/**
 * Dialog to create or to give details about a collection
 * @author Soulou
 */

public class BibliothequeAddDialog extends JDialog 
{
	//Fields	
	/**
	 * The JTextField for the name of the serie
	 */
	private JTextField jtfCollectionName;
	/**
	 * The JEntryPane for the name of the editor
	 */
	private JEntryPane jepEditor;
	/**
	 * Website of the collection
	 */
	private RegexpTextField rtfWeb;
	/**
	 * The JTextAera for a description of the collection
	 */
	private JTextArea jtaDescription;
	
	private Collection currentCollection;
	private Editor editorCollection;
	
	/**
	 * Button to validate the collection
	 */
	private JButton jbOk;
	/**
	 * Button to cancel all modifications
	 */
	private JButton jbCancel;
	
// Constructors
	/**
	 * Create a new CollectionAddDialog, and tell it to be at the center of the parent.<br/>
	 * The dialog will be created and paint in the SwingUtilities Thread and then be showed.
	 * @param parent The parent component of the CollectionAddDialog
	 * @param isForeground true if the dialog has to stay on the foreground of its parent
	 */
	public BibliothequeAddDialog(Window parent, boolean isForeground)
	{
		super(parent, LocaleManager.getInstance().getString("add_collection"),
				isForeground? Dialog.ModalityType.TOOLKIT_MODAL : Dialog.ModalityType.MODELESS);
		this.currentCollection = new Collection();
		
		SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				BibliothequeAddDialog.this.init();
				BibliothequeAddDialog.this.initListener();
				BibliothequeAddDialog.this.display();
			}
		});
	}
	
	/**
	 * Create a CollectionAddDialog. <br/>
	 * The created dialog will be placed at the center of the screen and 
	 * behave like other window
	 */
	public BibliothequeAddDialog()
	{
		this(null, true);
	}
	
	/**
	 * Constructor called to add a Collection of the Editor e
	 * @param e : Editor of the Collection
	 */
	public BibliothequeAddDialog(Editor e)
	{
		this(null,true);
		this.setTitle(LocaleManager.getInstance().getString("edit_collection"));
		this.editorCollection = e;
		this.currentCollection.setEditor(this.editorCollection);
		
		SwingUtilities.invokeLater(new Runnable() {
			
			@Override
			public void run() {
				BibliothequeAddDialog.this.jepEditor.setText(BibliothequeAddDialog.this.editorCollection.getName());
			}
		});
	}
	
	/**
	 * Constructor called to edit a Collection
	 * @param c Collection to edit
	 */
	public BibliothequeAddDialog(Collection c)
	{
		this(null, true);
		if(!c.getName().isEmpty())
		{
			this.setTitle(LocaleManager.getInstance().getString("edit_collection"));
		}
		this.currentCollection = c;
		
		SwingUtilities.invokeLater(new Runnable() {
			
			@Override
			public void run() {
				String name = currentCollection.getName();
				Editor editor = currentCollection.getEditor();
				String web = currentCollection.getWeb();
				String desc = currentCollection.getDescription();
				
				jtfCollectionName.setText(name);
				if(editor != null)
				{
					editorCollection = editor;
					jepEditor.setID(editor.getId());
				}
				if(web != null)
				{
					rtfWeb.setText(web);
				}
				if(desc != null)
				{
					jtaDescription.setText(desc);
				}
			}
		});
	}
	
// Methods
	/**
	 * Initialize the Dialog Components.
	 * You need to use the method display to make the dialog visible
	 * @see CollectionAddDialog#display
	 */
	private void init()
	{
		//this.setTitle(this.sTitle);
		this.setMinimumSize(new Dimension(400,300));
		this.setLayout(new GridBagLayout());
		
		Insets iComp = new Insets(5, 5, 5, 5);
		
		GridBagConstraints gbc = new GridBagConstraints();

		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.weightx = 0;
		gbc.weighty = 0;
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.gridheight = 1;
		gbc.gridwidth = 1;
		
		String[] labels = { 
				LocaleManager.getInstance().getString("name"), 
				LocaleManager.getInstance().getString("editor"), 
				LocaleManager.getInstance().getString("web"), 
				LocaleManager.getInstance().getString("description")
		};

		gbc.insets = iComp;
		int i;
		for(i=0; i<labels.length -1; i++)
		{
			JLabel jl = new JLabel(labels[i]);
			this.add(jl, gbc);
			gbc.gridy ++;
		}
		
		gbc.insets = new Insets(10, 5, 5, 5);
		gbc.anchor = GridBagConstraints.NORTH;
		JLabel jl = new JLabel(labels[i]);
		this.add(jl,gbc);

		gbc.insets = iComp;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.weightx = 1.0;
		gbc.weighty = 0;
		gbc.gridx ++;
		gbc.gridy = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;

		/* jepSerieName */
		this.jtfCollectionName = new JTextField();
		this.add(jtfCollectionName, gbc);
		
		
		/* jepTypeName */
		gbc.gridy ++;
		this.jepEditor = new JEntryPane(JEntryPane.EDITOR);
		this.add(this.jepEditor, gbc);
		
		/* jcbOneShot */
		gbc.gridy ++;
		this.rtfWeb= new RegexpTextField(RegexpTextField.WEB);
		this.add(this.rtfWeb, gbc);
				
		gbc.gridy++;
		gbc.weighty = 1.0; // for the textarea to be resized vertically too
		gbc.fill = GridBagConstraints.BOTH;
		/* jtaDescription */
		this.jtaDescription = new JTextArea();
		this.add(jtaDescription, gbc);

		JPanel b = new JPanel(new FlowLayout());
		jbOk = new JButton(LocaleManager.getInstance().getString("ok"));
		jbOk.setMnemonic(ShortcutManager.OK);
		jbCancel = new JButton(LocaleManager.getInstance().getString("cancel"));
		jbCancel.setMnemonic(ShortcutManager.CANCEL);
		/* Close the window when cancelling */
		b.add(jbOk);
		b.add(jbCancel);
		
		gbc.gridx=0;
		gbc.gridy++;
		gbc.weighty = 0;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.gridwidth=2;
		this.add(b, gbc);
	}

	/**
	 * Set all the listeners of the SerieAddDialog
	 */
	private void initListener()
	{
		jbOk.addActionListener(new SaveActionListener());
		jbCancel.addActionListener(new CancelActionListener());
	}
	
	/**
	 * Use this method to show the SerieAddDialog 
	 */
	public void display()
	{
		this.pack();
		this.setLocationRelativeTo(this.getParent());
		this.setDefaultCloseOperation(HIDE_ON_CLOSE);
	}
	
	/**
	 * Save current Collection to in the database
	 */
	public void saveCollection()
	{
		Collection c = this.currentCollection;
		boolean hasChanged = false;
			
		/* We check if the name has changed
		 * If it's the case, we change it
		 */
		if(!jtfCollectionName.getText().equals(c.getName()))
		{
			c.setName(jtfCollectionName.getText());
			hasChanged = true;
		}

		Editor e;
		if(c.getEditor() == null)
		{
			if(jepEditor.getID() != null)
			{
				e = PersistencyManager.findEditorByID(jepEditor.getID());
				SaveItemPersistency.saveEditor(e);
			}
			else if(!jepEditor.getText().isEmpty() && jepEditor.getID() == null)
			{
				e = new Editor();
				e.setName(jepEditor.getText());
				e.addCollection(c);
				c.setEditor(e);
				SaveItemPersistency.saveEditor(e);
				hasChanged = true;
			}
		}
		else
		{
			if(jepEditor.getID() == null)
			{
				e = c.getEditor();
				e.removeCollection(c);
				c.setEditor(null);
				SaveItemPersistency.saveCollection(c);
				hasChanged = true;
			}
			else if(!jepEditor.getID().equals(c.getEditor().getId()))
			{
				e = c.getEditor();
				e.removeCollection(c);
				SaveItemPersistency.saveEditor(e);
				e = PersistencyManager.findEditorByID(jepEditor.getID());
				e.addCollection(c);
				c.setEditor(e);
				SaveItemPersistency.saveEditor(e);
				hasChanged = true;
			}
		}
		
		
		if(!jtaDescription.getText().equals(c.getDescription()))
		{
			c.setDescription(jtaDescription.getText());
			hasChanged = true;
		}
		
		if(!rtfWeb.getText().equals(c.getWeb()))
		{
			c.setWeb(rtfWeb.getText());
			hasChanged = true;
		}
		
		if(hasChanged)
		{
			SaveItemPersistency.saveCollection(c);
		}
		
		AlbumPane.getInstance().refresh();
	}
	
	public Long getID()
	{
		return this.currentCollection.getId();
	}
	
	
// Classes
	class CancelActionListener implements ActionListener
	{
		@Override
		public void actionPerformed(ActionEvent ae)
		{
			BibliothequeAddDialog.this.dispose();
		}
	}

	class SaveActionListener implements ActionListener
	{
		@Override
		public void actionPerformed(ActionEvent ae)
		{
			if((BibliothequeAddDialog.this.jtfCollectionName.getText()).isEmpty())
			{
				MessagePaneManager.showInfoPane(LocaleManager.getInstance().getString("collection_noname"));
			}
			else if(!BibliothequeAddDialog.this.rtfWeb.getText().isEmpty() && 
					BibliothequeAddDialog.this.rtfWeb.check())
			{
				BibliothequeAddDialog.this.rtfWeb.setIncorrect();
				MessagePaneManager.showInfoPane(LocaleManager.getInstance().getString("web_invalid"));
			}
			else
			{
				BibliothequeAddDialog.this.saveCollection();
				BibliothequeAddDialog.this.dispose();
			}
		}
	}
}
