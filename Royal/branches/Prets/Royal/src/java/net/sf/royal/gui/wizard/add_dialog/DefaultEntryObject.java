package net.sf.royal.gui.wizard.add_dialog;

public class DefaultEntryObject implements EntryPaneObject {

	/**
	 * The names of the DefaultEntryObject
	 */
	private String[] names;
	/**
	 * The id of the DefaultEntryObject
	 */
	private long id;
	
	public DefaultEntryObject(long id, String[] names)
	{
		this.id = id;
		this.names = names;
	}
	
	/**
	 * Get the id of the DefaultEntryObject
	 * @return The id
	 */
	@Override
	public Long getID() {
		if(this.id == -1)
			return null;
		else
			return this.id;
	}

	/**
	 * Get all the names of the DefaultEntryObject
	 * @return An array with the names
	 */
	@Override
	public String[] getName() {
		return this.names;
	}

	/**
	 * Set a new ID for the DefautlEntryObject
	 * @param id
	 */
	public void setID(long id)
	{
		this.id = id;
	}
	
	/**
	 * Set the names of the DefaultEntryObject
	 * @param names
	 */
	public void setNames(String[] names)
	{
		this.names = names;
	}
	
	@Override
	public String toString()
	{
		String res = "";
		int i;
		for(i = 0; i < this.names.length-1; i++)
		{
			res += this.names[i] + " ";
		}
		if(this.names.length != 0)
		{
			res += this.names[i];
		}
		return res;
	}
}
