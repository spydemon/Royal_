package net.sf.royal.gui.wizard.add_dialog;

/**
 * Interface specifying the operations needed for items
 * in a JEntryPane to be autocompleted
 * @author Steven Nguyen
 *
 */
public interface EntryPaneObject
{
	/**
	 * Get the ID of the EntryPaneObject.<br/>
	 * @return The ID
	 */
	public Long getID();
	
	/**
	 * Get the name component of the EntryPaneObject.
	 * <br/>
	 * The String in the array are the Strings which the JEntryPane will 
	 * for completion 
	 * @return The array of name that can be used for research
	 */
	public String[] getName();
}