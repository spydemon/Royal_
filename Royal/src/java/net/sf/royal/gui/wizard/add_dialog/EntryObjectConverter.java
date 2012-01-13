package net.sf.royal.gui.wizard.add_dialog;

import java.util.ArrayList;
import java.util.List;

import net.sf.royal.datamodel.Author;
import net.sf.royal.datamodel.Bibliotheque;
import net.sf.royal.datamodel.Collection;
import net.sf.royal.datamodel.Editor;
import net.sf.royal.datamodel.Serie;
import net.sf.royal.datamodel.Type;

/**
 * The class allows transforming "normal" datas from the database
 * (Author, Editor, Collection, Serie) into corresponding EntryPaneObject.
 * @author Steven Nguyen
 *
 */
public class EntryObjectConverter {
	/**
	 * Convert a list of Author into a List of EntryPaneObject
	 * @param list The list of Author
	 * @return The list of EntryPaneObject
	 */
	public static List<EntryPaneObject> getAuthorEntryObjects(List<Author> list)
	{
		List<EntryPaneObject> res = new ArrayList<EntryPaneObject>();
		AuthorEntryObject tmp;
		for(Author a : list)
		{
			tmp = new AuthorEntryObject(a.getId(),a.getNickName(),
					a.getName(), a.getFirstName());
			res.add(tmp);
		}
		return res;
	}
	
	public static EntryPaneObject getAuthorEntryObject(Author a)
	{
		EntryPaneObject res = new AuthorEntryObject(a.getId(), a.getNickName(), 
				a.getName(), a.getFirstName());
		return res;
	}
	
	public static List<EntryPaneObject> getSerieEntryObjects(List<Serie> list)
	{
		List<EntryPaneObject> res = new ArrayList<EntryPaneObject>();
		SerieEntryObject tmp;
		for(Serie a : list)
		{
			tmp = new SerieEntryObject(a.getId(),a.getName());
			res.add(tmp);
		}
		return res;
	}
	public static EntryPaneObject getSerieEntryObject(Serie s)
	{
		return new SerieEntryObject(s.getId(), s.getName());
	}
	
	public static List<EntryPaneObject> getEditorEntryObjects(List<Editor> list)
	{
		List<EntryPaneObject> res = new ArrayList<EntryPaneObject>();
		DefaultEntryObject tmp;
		if(list != null && list.size() != 0)
		{
			for(Editor e : list)
			{
				tmp = new DefaultEntryObject(e.getId(), new String[] {e.getName()});
				res.add(tmp);
			}
		}
		else
			res = new ArrayList<EntryPaneObject>();
		return res;
	}
	
	public static EntryPaneObject getEditorEntryObject(Editor e)
	{
		return new DefaultEntryObject(e.getId(), new String[]{e.getName()});
	}
	
	public static List<EntryPaneObject> getCollectionEntryObjects(List<Collection> list)
	{
		List<EntryPaneObject> res = new ArrayList<EntryPaneObject>();
		DefaultEntryObject tmp;
		if(list != null && list.size() != 0)
		{
			for(Collection c : list)
			{
				tmp =  new DefaultEntryObject(c.getId(), new String[] {c.getName()});
				res.add(tmp);
			}
		}
		else
		{
			res = new ArrayList<EntryPaneObject>();
		}
		return res;
	}
	
	public static EntryPaneObject getCollectionEntryObject(Collection c)
	{
		return new DefaultEntryObject(c.getId(), new String[]{c.getName()});
	}
	
	public static List<EntryPaneObject> getTypeEntryObjects(List<Type> list)
	{
		List<EntryPaneObject> res = new ArrayList<EntryPaneObject>();
		DefaultEntryObject tmp;
		if(list != null && list.size() != 0)
		{
			for(Type t : list)
			{
				tmp =  new DefaultEntryObject(t.getId(), new String[] {t.getName()});
				res.add(tmp);
			}
		}
		else
		{
			//res = new ArrayList<EntryPaneObject>();
		}
		return res;
	}
	
	public static List<EntryPaneObject> getBibliothequeEntryObjects(List<Bibliotheque> list)
	{
		List<EntryPaneObject> res = new ArrayList<EntryPaneObject>();
		DefaultEntryObject tmp;
		if(list != null && list.size() != 0)
		{
			for(Bibliotheque b : list)
			{
				tmp =  new DefaultEntryObject(b.getId(), new String[] {b.getName()});
				res.add(tmp);
			}
		}
		else
		{
			//res = new ArrayList<EntryPaneObject>();
		}
		return res;
	}
	
	public static EntryPaneObject getTypeEntryObject(Type t)
	{
		return new DefaultEntryObject(t.getId(), new String[]{t.getName()});
	}
}
