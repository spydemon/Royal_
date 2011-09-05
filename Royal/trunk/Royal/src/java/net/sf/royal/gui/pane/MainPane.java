package net.sf.royal.gui.pane;

import javax.swing.JPanel;
import javax.swing.JSplitPane;

/**
  * @author Soulou
  * Center panel of the main window
  */
public class MainPane extends JSplitPane
{
	/* Fields */
	private JPanel pInfo;
	private AlbumPane pAlbum;
	private static MainPane instance;

	/* Constructors */
	private MainPane()
	{
		this.pInfo = InfoPane.getPane();
		this.pAlbum = AlbumPane.getInstance();
		
		this.setDoubleBuffered(true);
		this.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
		this.setLeftComponent(this.pAlbum);
		this.setRightComponent(this.pInfo);
		this.setResizeWeight(0.3);
		this.setDividerLocation(400);
	}
	/* Methods */
	/**
	  * Repaint the right info panel with the good information
	  * @param o Album/Author/Type/Serie
	  */
	public void updateInfoPane(Object o)
	{
		InfoPane.update(o);
		/* We get the new Panel */
		this.pInfo = InfoPane.getPane();
		/* Save the elder divider position */
		int l = this.getDividerLocation();
		/* Update the right panel */
		this.setRightComponent(this.pInfo);
		/* Restore the divider */
		this.setDividerLocation(l);
	}
	
	public static MainPane getInstance()
	{
		if(instance == null)
		{
			instance = new MainPane();
		}
		return instance;
	}
	
	public static void setInstance(MainPane in)
	{
		instance = in;
	}
}
