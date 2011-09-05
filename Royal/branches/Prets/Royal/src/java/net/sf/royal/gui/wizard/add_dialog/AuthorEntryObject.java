package net.sf.royal.gui.wizard.add_dialog;

import net.sf.royal.gui.manager.LocaleManager;

/**
 * EntryPaneObject representing a
 * @author Steven Nguyen
 *
 */
public class AuthorEntryObject implements EntryPaneObject
{
// FIELDS
	private long id;
	private String nickname;
	private String name;
	private String surname;
	
	public AuthorEntryObject(long id, String nickname, String name, String surname)
	{
		this.id = id;
		this.nickname = nickname == null ? "" : nickname;
		this.name = name==null ? "" : name;
		this.surname = surname==null ? "" : surname;
	}
	
	@Override 
	public Long getID()
	{
		if(this.id == -1)
			return null;
		else
			return this.id;
	}
	
	public void setID(long id)
	{
		this.id = id;
	}
	
	public void setName(String name)
	{
		this.name = name;
	}

	public void setNickname(String nick)
	{
		this.nickname = nick;
	}
	
	public void setSurname(String surname)
	{
		this.surname = surname;
	}
	
	@Override
	public String[] getName()
	{
		String[] res = {this.nickname, this.name};
		return res;
	}
	
	/**
	 * Get the name of the Author.<br/>
	 * Will be like "nickname (surname name)" if the Locale is EN,
	 * and "nickname (name surname)" if the Locale is FR. <br/>
	 * If there is no nickname, will be "name surname".
	 * If there are no name and surname, will be "nickname"
	 */
	@Override
	public String toString()
	{
		String res;
		if(this.nickname.isEmpty())
		{
			if(LocaleManager.getInstance().getCurrentLocale().equals(LocaleManager.ENGLISH))
				res = this.surname + " " + this.name;
			else
				res = this.name + " " + this.surname;
		}
		else
		{
			res = this.nickname;
			if(this.name.isEmpty() && this.surname.isEmpty())
				; //Nothing to add
			else
			{
				res += " (";
				if(LocaleManager.getInstance().getCurrentLocale().equals(LocaleManager.ENGLISH))
					res += this.surname + " " + this.name;
				else
					res += this.name +  " " + this.surname;
				res += ")";
			}//else
		}//else
		return res;
	}
}