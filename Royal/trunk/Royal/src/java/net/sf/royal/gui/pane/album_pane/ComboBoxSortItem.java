package net.sf.royal.gui.pane.album_pane;

/**
 * @author Soulou
 * Item of the sort combo box, containing the printed name and
 * the corresponding flag
 */
public class ComboBoxSortItem
{
	/* Fields */
	private String name;
	private int flags;
	
	/* Constructor */
	public ComboBoxSortItem(String n, int f)
	{
		this.name = n;
		this.flags = f;
	}
	
	/* Methods */
	@Override
	public String toString()
	{
		return this.name;
	}
	
	public int getFlags()
	{
		return this.flags;
	}
}
