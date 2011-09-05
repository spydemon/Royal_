 package net.sf.royal.gui.pane;

import javax.swing.JPanel;

import net.sf.royal.datamodel.Album;
import net.sf.royal.datamodel.Author;
import net.sf.royal.datamodel.Serie;
import net.sf.royal.datamodel.Type;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.pane.info.AlbumInfoPane;
import net.sf.royal.gui.pane.info.AuthorInfoPane;
import net.sf.royal.gui.pane.info.SerieInfoPane;
import net.sf.royal.gui.pane.info.TypeInfoPane;

/**
  * @author Soulou
  * Class which manage which panel we must choose to print
  * depending on the type selected on the tree
  */
public class InfoPane
{
	/* Public constants used in *InfoPane in package ...gui.info */
	public static final String name = LocaleManager.getInstance().getString("name") + " : ";
	public static final String firstname = LocaleManager.getInstance().getString("firstname") + " : ";
	public static final String nickname = LocaleManager.getInstance().getString("nickname") + " : ";
	public static final String serie = LocaleManager.getInstance().getString("serie") + " : ";
	public static final String author = LocaleManager.getInstance().getString("author") + " : ";
	public static final String type = LocaleManager.getInstance().getString("type") + " : ";
	public static final String title = LocaleManager.getInstance().getString("title") + " : ";
	public static final String nbalbums = LocaleManager.getInstance().getString("nbalbums") + " : ";
	public static final String desc = LocaleManager.getInstance().getString("description") + " : ";
	public static final String oneshot = LocaleManager.getInstance().getString("oneshot") + " : ";
	public static final String nbseries = LocaleManager.getInstance().getString("nbseries") + " : ";
	public static final String nonickname = LocaleManager.getInstance().getString("nonickname");
	
	
	private static AlbumInfoPane aip = null;
	private static SerieInfoPane sip = null;
	private static AuthorInfoPane auip = null;
	private static TypeInfoPane tip = null;
	private static JPanel instance = null;

	private static Object currentObject = null;
	
	/* Constructors */
	private InfoPane()
	{
	}

	public static void init() {
	}

	public static void update(Object o)
	{
		currentObject = o;
		if(o instanceof Album)
		{
			Album a = (Album)o;
			if(aip == null)
				aip = new AlbumInfoPane(a);
			else
				aip.update(a);
			instance = aip;
		}
		else if(o instanceof Serie)
		{
			Serie s = (Serie)o;
			if(sip == null)
				sip = new SerieInfoPane(s);
			else
				sip.update(s);
			instance = sip;
		}
		else if(o instanceof Author)
		{
			Author au = (Author)o;
			if(auip == null)
				auip = new AuthorInfoPane(au);
			else
				auip.update(au);
			instance = auip;
		}
		else if(o instanceof Type)
		{
			Type t = (Type)o;
			if(tip == null)
				tip = new TypeInfoPane(t);
			else
				tip.update(t);
			instance = tip;
		}
	}
	
	public static void update()
	{
		if(currentObject != null)
			update(currentObject);
	}

	public static JPanel getPane() 
	{
		if(instance == null)
			instance = new JPanel();
		return instance;
	}
}
