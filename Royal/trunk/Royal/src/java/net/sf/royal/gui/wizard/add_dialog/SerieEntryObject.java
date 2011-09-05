package net.sf.royal.gui.wizard.add_dialog;

public class SerieEntryObject implements EntryPaneObject
{
	/**
	 * The id of the serie
	 */
	private long id = 0;
	/**
	 * The name of the serie
	 */
	private String name = "";
	
	public SerieEntryObject(long id, String name)
	{
		this.id = id;
		this.name = name;
	}
	
	@Override
	public Long getID() {
		if(this.id == -1)
			return null;
		else
			return this.id;
	}

	@Override
	public String[] getName() {
		String[] res = {this.name};
		return res;
	}
	
	@Override
	public String toString()
	{
		return this.name;
	}
}