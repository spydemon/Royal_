package net.sf.royal.gui.wizard.add_dialog;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.Enumeration;
import java.util.List;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JTextField;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.PlainDocument;

/**
 * The document and controler for the JEntryPane's JComboBox.
 * This class controls the insert and the remove of the JCombobox
 * to determine if the list of items shown by the ComboBox has to 
 * be refreshed.
 * @author steven
 *
 */
@SuppressWarnings("serial")
public class JEntryDocument extends PlainDocument {
// FIELDS
	/**
	 * The Combobox controlled.<br/>
	 * This object is used to get the other object and to 
	 * show or hide the popup menu
	 */
	private JComboBox jcb;
	/**
	 * The model of the comboBox which contains the list of 
	 * items to display.
	 */
	private DefaultComboBoxModel cbm;
	/**
	 * The initial list of elements of the JEntryPane. It will 
	 * be given to the searcher.
	 */
	private List<EntryPaneObject> ressource;
	/**
	 * Is used to research all items of the list research which fit
	 * the text if the ComboBox and add them to a new ComboBoxModel
	 */
	private EntryObjectSearchTask searcher;
	/**
	 * A new JEntryObject which is used when the text of the ComboBox
	 * is different to the items. -> The user wants a new EntryObject
	 */
	private DefaultEntryObject newObject;
	/** 
	 * Used to determine if the thread is searching and adding items to 
	 * the model of the ComboBox. <br/>
	 * The aim is to prevent the modifications of the content of the TextField 
	 * after each addItem of the searcher.<br/>
	 * <code>true</code> If the searcher is working
	 */
	private boolean working;
	/**
	 * Used to test if the remove action should lead to a refreshment
	 * of the content of the ComboBox list or not.<br/>
	 * <code>true</code> : Refresh after removing text<br/>
	 * <code>false</code> : Just remove the text
	 */
	private boolean normalRemove = true;
	/**
	 * Used to test if the insertion should lead to the refreshment of the 
	 * ComboBox's content.<br/>
	 * <code>true</code> : insertion of text then refreshment<br/>
	 * <code>false</code> : just insert the text
	 */
	private boolean normalInsert = true;
	/**
	 * Used when the user does a selection in the ComboBox with the array keys.<br/>
	 * The text should change but not the list of items.<br/>
	 * <code>true</code> : The next removing and the next insertion do not refresh
	 * the list of items in the ComboBox
	 */
	private boolean arrowSelection = false;
	/**
	 * True if first research (do not show the popup menu)
	 */
	private boolean firstResearch = false;
	
// CONSTRUCTOR
	public JEntryDocument(JComboBox owner, List<EntryPaneObject> ressource)
	{
		this.jcb = owner;
		this.cbm = (DefaultComboBoxModel) this.jcb.getModel();
		this.cbm.removeAllElements();
		this.working = false;
		this.ressource = ressource;
		this.searcher = new EntryObjectSearchTask(ressource, "", 
				(DefaultComboBoxModel) this.jcb.getModel(), this);
		
		JTextField editor = (JTextField) this.jcb.getEditor().getEditorComponent();
		editor.addKeyListener(new EditorKeyListener());
		
		this.newObject = new DefaultEntryObject(-1, new String[] {""});
		
		/* start a research to add all elements in the list */
		try
		{
			this.firstResearch = true;
			this.research();
		}
		catch(BadLocationException e)
		{
			;
		}
	}
	
// METHODS
	@Override
	public void insertString(int offs, String str, AttributeSet a) throws BadLocationException
	{
		
		if(working)
		// new insertion cause by the add of an item by the searcher. Ignoring the insertion
		{
			
		}
		else if(this.normalInsert)
		// normal case : insertion then research 
		{
			super.insertString(offs, str, a);
			this.research();
		}
		else if(this.arrowSelection)
		// Arrow keys selection : insertion, do not change content
		{
			super.insertString(offs, str, a);
			this.arrowSelection = false;
			this.normalInsert = true;
			this.normalRemove = false;
			this.jcb.showPopup();
		}
		else
		// insertion when selecting an item with the mouse : just insertion
		{
			super.insertString(offs, str, a);
			this.normalInsert = true;
			this.jcb.hidePopup();
		}
	}
	
	@Override 
	public void remove(int offs,int len) throws BadLocationException
	{
		if(working)
		// removing while searching : just ignore because due to the add of an item by the searcher
		{
			this.jcb.hidePopup();
		}
		else if(this.normalRemove)
		// normal case : remove and refresh
		{
			super.remove(offs, len);
			this.research();
			this.normalRemove = false;
			this.normalInsert = true;
		}
		else if(arrowSelection)
		// arrow selection : just remove
		{
			//System.out.println("remove2");
			super.remove(offs, len);
		}
		else 
		// remove because the user click on an item : just remove and set the booleans
		{
			//System.out.println("remove3");
			super.remove(offs, len);
			this.normalRemove = true;
			this.normalInsert = false;
			this.arrowSelection = false;
		}
	}
	
	/**
	 * Starts the research of the text entered in the different elements 
	 * of the resources
	 * @throws BadLocationException Normally no
	 */
	private void research() throws BadLocationException
	{
		/* Stopping the thread */
		if(this.searcher.isWorking())
		{
			this.searcher.interrupt();
		
		}
		/* Specify that a work is being done */
		this.working = true;
		/* change the prefix */
		this.searcher.setPrefix(super.getText(0, super.getLength()).trim());
		/* remove all the elements */
		this.cbm.removeAllElements();
		/* Start searching */	
		this.searcher.start(cbm);
	}
	
	
	/**
	 * Called when the work is done 
	 * @param ok <code>true</code> if the work is done
	 * @see EntryObjectSearchTask#done()
	 */
	public void setOK()
	{
		this.working = false;
		/* If there is nothing in the document
		 * do not show the popup of the combobox 
		 */
		if(this.firstResearch)
		{
			this.jcb.hidePopup();
			this.firstResearch = false;
		}
		else if(this.getLength() != 0)
		{
			this.jcb.showPopup();
		}
		else
		{
			this.jcb.hidePopup();
		}
		
		/* reinitializing the searcher */
		this.searcher = new EntryObjectSearchTask(this.ressource, "", 
				(DefaultComboBoxModel) this.cbm, this);
	}
	
	/** 
	 * Set the text of the document
	 * @param text The text
	 */
	public void setText(String text)
	{
		try
		{
			//System.out.println("JEntryDocument : setText : string : " + text);
			super.remove(0, this.getLength());

			super.insertString(0, text, new AttributeSet() {
			
			@Override
			public boolean isEqual(AttributeSet attr) {
				// TODO Auto-generated method stub
				return false;
			}
			
			@Override
			public boolean isDefined(Object attrName) {
				// TODO Auto-generated method stub
				return false;
			}
			
			@Override
			public AttributeSet getResolveParent() {
				// TODO Auto-generated method stub
				return null;
			}
			
			@Override
			public Enumeration<?> getAttributeNames() {
				// TODO Auto-generated method stub
				return null;
			}
			
			@Override
			public int getAttributeCount() {
				// TODO Auto-generated method stub
				return 0;
			}
			
			@Override
			public Object getAttribute(Object key) {
				// TODO Auto-generated method stub
				return null;
			}
			
			@Override
			public AttributeSet copyAttributes() {
				// TODO Auto-generated method stub
				return null;
			}
			
				@Override
				public boolean containsAttributes(AttributeSet attributes) {
					// TODO Auto-generated method stub
					return false;
				}
			
				@Override
				public boolean containsAttribute(Object name, Object value) {
					// TODO Auto-generated method stub
					return false;
				}
			});
			//System.out.println("Insert OK");
		}
		catch(BadLocationException e)
		{
			;
		}
	}
	
	/**
	 * Get the selected Item. Return an new EntryPaneObject
	 * if the text doesn't match the text of the selected Item
	 * @return The EntryPaneObject selected
	 */
	public EntryPaneObject getSelectedItem()
	{
		EntryPaneObject res;
		String text;
		/* Getting the Text of the document */
		try
		{
			text = this.getText(0, this.getLength());
		}
		catch(BadLocationException e)
		{
			text = "";
		}
		
		/* If there is an item selected */
		if(this.jcb.getSelectedItem() != null)
		{
			Object tmp = this.jcb.getSelectedItem();
			
			if(tmp instanceof EntryPaneObject)
			{
				EntryPaneObject tmp1 = (EntryPaneObject) tmp;
				if(tmp1.toString().equals(text))
				{
					res = tmp1;
				}
				else
				{
					this.newObject.setNames(new String[]{text});
					res = this.newObject;
				}
			}
			else
			{
				this.newObject.setNames(new String[]{text});
				res = this.newObject;
			}
		}
		else
		{
			this.newObject.setNames(new String[]{text});
			res = this.newObject;
		}
		return res;
	}
	
	/**
	 * Set the selected item of the JEntryPane using its ID.
	 * If no matches, do nothing.
	 * @param id The ID of the desired item.
	 */
	public void setID(long id)
	{
		for(EntryPaneObject e : this.ressource)
		{
			if(e.getID() == id)
			{
				JEntryDocument.this.arrowSelection = true;
				JEntryDocument.this.normalRemove = false;
				JEntryDocument.this.normalInsert = false;
				this.cbm.setSelectedItem(e);
				this.setText(e.toString());
				this.jcb.hidePopup();
				break;
			}
		}
	}
	
	private class EditorKeyListener implements KeyListener
	{

		@Override
		public void keyPressed(KeyEvent arg0) {
			// TODO Auto-generated method stub
			int key = arg0.getKeyCode();
			if(key == KeyEvent.VK_BACK_SPACE)
			{
				JEntryDocument.this.normalRemove = true;
			}
			else if(   key == KeyEvent.VK_LEFT ||
						key == KeyEvent.VK_RIGHT ||
						key == KeyEvent.VK_UP ||
						key == KeyEvent.VK_DOWN)
			{
				JEntryDocument.this.arrowSelection = true;
				JEntryDocument.this.normalRemove = false;
				JEntryDocument.this.normalInsert = false;
			}
			else if(key == KeyEvent.VK_ENTER)
			{
				EntryPaneObject epo = (EntryPaneObject) JEntryDocument.this.cbm.getSelectedItem();
				JEntryDocument.this.setID(epo.getID());
			}
		}

		@Override
		public void keyReleased(KeyEvent arg0) {
		}

		@Override
		public void keyTyped(KeyEvent arg0) {
		}
		
	}
}
