package net.sf.royal.gui.tree;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import net.sf.royal.datamodel.Album;
import net.sf.royal.datamodel.Author;
import net.sf.royal.datamodel.Serie;
import net.sf.royal.gui.pane.AlbumPane;
import net.sf.royal.gui.pane.InfoPane;
import net.sf.royal.gui.wizard.add_dialog.AlbumAddDialog;
import net.sf.royal.gui.wizard.add_dialog.AuthorAddDialog;
import net.sf.royal.gui.wizard.add_dialog.SerieAddDialog;

public class EditActionListener implements ActionListener 
{
	private Object userObject;
	
	public EditActionListener(Object o)
	{
		this.userObject = o;
	}
	
	@Override
	public void actionPerformed(ActionEvent e) 
	{
		Object o = this.userObject;
		if(o instanceof Album)
		{
			AlbumAddDialog aad = new AlbumAddDialog((Album)o);
			aad.setLocationRelativeTo(null);
			aad.setVisible(true);
			
		}
		if(o instanceof Author)
		{
			AuthorAddDialog aad = new AuthorAddDialog((Author)o);
			aad.setLocationRelativeTo(null);
			aad.setVisible(true);
		}
		if(o instanceof Serie)
		{
			SerieAddDialog sad = new SerieAddDialog((Serie)o);
			sad.setLocationRelativeTo(null);
			sad.setVisible(true);
		}
		AlbumPane.getInstance().refresh();
		//TODO
		//InfoPane.update();
	}

	public void setUserObject(Object o)
	{
		this.userObject = o;
	}
}
