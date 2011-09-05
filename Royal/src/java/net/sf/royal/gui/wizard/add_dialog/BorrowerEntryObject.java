package net.sf.royal.gui.wizard.add_dialog;

import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.datamodel.Borrower;

public class BorrowerEntryObject implements EntryPaneObject
{
	private long id;
	private String name;
	private String firstname;
	
	public BorrowerEntryObject(Borrower bw)
	{
		this.setName(bw.getName());
		this.setFirstname(bw.getFirstName());
		this.setID(bw.getId());
	}
	
	@Override
	public Long getID() {
		return this.id;
	}

	@Override
	public String[] getName() {
		return new String[]{this.name, this.firstname};
	}
	
	public void setName(String name)
	{
		this.name = name==null ? "" : name;
	}
	
	public void setFirstname(String firstname)
	{
		this.firstname = firstname==null ? "" : firstname;
	}
	
	public void setID(long id)
	{
		this.id = id;
	}
	
	@Override
	public String toString()
	{
		String res = "";
		if(LocaleManager.getInstance().getCurrentLocale().equals(LocaleManager.FRENCH))
		{
			if(!this.name.isEmpty())
			{
				res += this.name + " ";
			}
			if(! this.firstname.isEmpty())
			{
				res += this.firstname;
			}
		}
		else
		{
			if(! this.firstname.isEmpty())
			{
				res += this.firstname + " ";
			}
			if(!this.name.isEmpty())
			{
				res += this.name;
			}
		}
		return res;
	}
}
