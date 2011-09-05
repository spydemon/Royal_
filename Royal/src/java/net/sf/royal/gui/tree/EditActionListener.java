package net.sf.royal.gui.tree;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JDialog;

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
		JDialog jd = null;
		if(o instanceof Album)
		{
			jd = new AlbumAddDialog((Album)o);
		}
		else if(o instanceof Author)
		{
			jd = new AuthorAddDialog((Author)o);
		}
		else if(o instanceof Serie)
		{
			jd = new SerieAddDialog((Serie)o);
		}
		
		if(jd != null)
		{
			jd.setLocationRelativeTo(null);
			jd.setVisible(true);
			//AlbumPane.getInstance().refresh();
			//TODO
			//InfoPane.update();
		}
	}

	public void setUserObject(Object o)
	{
		this.userObject = o;
	}
}
