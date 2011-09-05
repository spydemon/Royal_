package net.sf.royal.gui.tree;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import net.sf.royal.datamodel.Album;
import net.sf.royal.datamodel.Author;
import net.sf.royal.datamodel.Serie;
import net.sf.royal.datamodel.Type;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.MessagePaneManager;
import net.sf.royal.gui.pane.AlbumPane;
import net.sf.royal.gui.pane.BottomBarPane;
import net.sf.royal.persistency.PersistencyManager;

public class DeleteActionListener implements ActionListener {

	private Object userObject;
	
	public DeleteActionListener(Object o)
	{
		this.userObject = o;
	}
	
	@Override
	public void actionPerformed(ActionEvent e) {
		Object o = this.userObject;
		if(o instanceof Album)
		{
			if(MessagePaneManager.showConfirmDeletion(LocaleManager.getInstance().getString("confirm_deletion") 
					+ " " + LocaleManager.getInstance().getString("thealbum").toLowerCase() + " : " + ((Album)o).toString()))
			{
				BottomBarPane.getInstance().rmAlbum();
				PersistencyManager.removeAlbum((Album)o);
			}
		}
		else if(o instanceof Serie)
		{
			if(MessagePaneManager.showConfirmDeletion(LocaleManager.getInstance().getString("confirm_deletion") 
					+ " " + LocaleManager.getInstance().getString("theserie").toLowerCase() + " : " + ((Serie)o).toString()));
			{
				BottomBarPane.getInstance().rmSerie();
				PersistencyManager.removeSerie((Serie)o);
			}
		}
		else if(o instanceof Author)
		{
			if(MessagePaneManager.showConfirmDeletion(LocaleManager.getInstance().getString("confirm_deletion")
					+ " " + LocaleManager.getInstance().getString("theauthor").toLowerCase() + " : " + ((Author)o).toString()))
			{
				PersistencyManager.removeAuthor((Author)o);
			}
		}
		else if(o instanceof Type)
		{
			PersistencyManager.removeType((Type)o);
		}
		AlbumPane.getInstance().refresh();
	}

	public void setUserObject(Object o)
	{
		this.userObject = o;
	}
}
