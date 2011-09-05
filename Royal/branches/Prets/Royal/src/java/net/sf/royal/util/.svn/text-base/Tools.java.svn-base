package net.sf.royal.util;

import net.sf.royal.datamodel.Author;
import net.sf.royal.datamodel.Borrower;
import net.sf.royal.gui.manager.LocaleManager;

public class Tools
{
	public static String initCap(String s)
	{
		return s.substring(0,1).toUpperCase() + s.substring(1).toLowerCase();
	}
	
	/**
	 * Get a string representing the name of an author.
	 * The name will be displayed as : <br/>
	 * if there is a nickname <b>Nickname (Surname Name)</b><br/>
	 * else <b>Surname Name</b><br/>
	 * The order depends on the Locale
	 * @param au
	 * @return
	 */
	public static String authorToString(Author au)
	{
		String res;
		String surname = au.getFirstName();
		String name = au.getName();
		String nickname = au.getNickName();
		
		if(nickname == null || nickname.isEmpty())
		{
			if(LocaleManager.getInstance().getCurrentLocale().equals(LocaleManager.ENGLISH))
				res = surname + " " + name;
			else
				res = name + " " + surname;
		}
		else
		{
			res = nickname;
			if((name == null || name.isEmpty()) && 
			   (surname == null || surname.isEmpty()))
				; //Nothing to add
			else
			{
				res += " (";
				if(LocaleManager.getInstance().getCurrentLocale().equals(LocaleManager.ENGLISH))
					res += surname + " " + name;
				else
					res += name +  " " + surname;
				res += ")";
			}//else
		}//else
		return res;
	}
	
	public static String borrowerToString(Borrower b)
	{
		String res = "", name, firstname;
		
		name = b.getName();
		firstname = b.getFirstName();
		if(LocaleManager.getInstance().getCurrentLocale().equals(LocaleManager.ENGLISH))
		{
			if(firstname == null || firstname.isEmpty())
			{
				res += initCap(firstname) + " ";
			}
			if(name != null && !name.isEmpty())
			{
				res += initCap(name);
			}
		}
		else if(LocaleManager.getInstance().getCurrentLocale().equals(LocaleManager.FRENCH))
		{
			if(name != null && ! name.isEmpty())
			{
				res += initCap(name) + " ";
			}
			if(firstname != null && !firstname.isEmpty())
			{
				res += initCap(firstname);
			}
		}
		
		return res;
	}
}
