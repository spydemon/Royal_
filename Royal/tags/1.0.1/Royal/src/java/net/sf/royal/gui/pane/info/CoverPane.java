package net.sf.royal.gui.pane.info;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;

import net.sf.royal.datamodel.Album;
import net.sf.royal.datamodel.Author;
import net.sf.royal.datamodel.CommentedImage;
import net.sf.royal.datamodel.HibernateUtil;
import net.sf.royal.datamodel.Serie;
import net.sf.royal.gui.manager.PropertyManager;
import net.sf.royal.gui.pane.InfoPane;
import net.sf.royal.gui.pane.MainPane;
import net.sf.royal.gui.util.ImageHelper;
import net.sf.royal.persistency.PersistencyManager;


/**
  * @author Soulou
  * Panel use to look at covers, or page.
  * Resize the  different user covers thanks to the {@link ImageResizerTask}}
  */
public class CoverPane extends JPanel
{
	/* Constants */
	/**
	 *  Width of the future comic covers
	 */
	public static final int COVER_WIDTH = 180;
	/**
	 * Height of the future comic covers
	 */
	public static final int COVER_HEIGHT = 254;
	/**
	 * Path to the current covers
	 */
	public static final String coverDir = PropertyManager.getInstance().getPathProperty("path_cover") + PropertyManager.sep +
	HibernateUtil.getCurrentDatabase() + PropertyManager.sep;
	
	/* Fields */
	private JScrollPane jspCovers;
	private FlowLayout flCovers;
	private JPanel jpCovers;
	private ImageResizerTask imageResizerTask;
	private ArrayList<Album> albumsToPrint;
	
	/* Constructor */
	public CoverPane()
	{
		this.albumsToPrint = new ArrayList<Album>();
		this.flCovers = new FlowLayout(FlowLayout.LEFT, 20, 0);
		this.jpCovers = new JPanel(this.flCovers);
		this.jspCovers = new JScrollPane(this.jpCovers);
		JScrollBar scrollbar = new JScrollBar(JScrollBar.HORIZONTAL);
		scrollbar.setUnitIncrement(COVER_WIDTH/2);
		this.jspCovers.setHorizontalScrollBar(scrollbar);
		this.add(jspCovers);
		this.jspCovers.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER);
	}

	/* Methods */
	public void update(Author au)
	{
		this.jpCovers = new JPanel(new FlowLayout(FlowLayout.LEFT));
		this.jspCovers.setViewportView(this.jpCovers);
		List<Album> albums = PersistencyManager.findAlbumsByAuthorID(au.getId());
		ArrayList<String> coversToPrint = new ArrayList<String>();
		String cover;

		this.albumsToPrint.clear();
		for(Album album : albums)
		{
			cover = album.getCover();
			if(cover != null && !cover.equals(""))
			{
				this.albumsToPrint.add(album);
				coversToPrint.add(coverDir + album.getCover());
			}
		}
		this.finalize(coversToPrint);
	}

	public void update(Album a)
	{
		this.jpCovers = new JPanel(new FlowLayout(FlowLayout.LEFT));
		this.jspCovers.setViewportView(this.jpCovers);
		this.albumsToPrint.clear();
		this.albumsToPrint.add(a);
		jpCovers.add(new CoverLabel(ImageHelper.getImageIcon(coverDir + a.getCover(), COVER_WIDTH, COVER_HEIGHT)));
		
		List<CommentedImage> imageList = PersistencyManager.findCommentedImagesByAlbumID(a.getId(), 
										 PersistencyManager.SORTED);
		ArrayList<String> coversToPrint = new ArrayList<String>();
		
		for(CommentedImage ci : imageList)
		{
			this.albumsToPrint.add(a);
			coversToPrint.add(ci.getImageURL());
		}
		this.finalize(coversToPrint);
	}
	
	public void update(Serie s)
	{
		this.jpCovers = new JPanel(new FlowLayout(FlowLayout.LEFT));
		this.jspCovers.setViewportView(this.jpCovers);
		this.albumsToPrint.clear();
		
		List<String> coversToPrint = new ArrayList<String>();
		List<Album> albumList = PersistencyManager.findAlbumsBySerieID(s.getId()); 
					
		for(Album a : albumList)
		{
			this.albumsToPrint.add(a);
			coversToPrint.add(coverDir + a.getCover());
		}
		this.finalize(coversToPrint);
	}
	
	private void finalize(List<String> coversToPrint)
	{
		this.imageResizerTask = new ImageResizerTask(this.jpCovers, coversToPrint);
		imageResizerTask.execute();
		this.jspCovers.setPreferredSize(new Dimension(InfoPane.getPane().getSize().width - 40, COVER_HEIGHT));
	}
	
	
	public static class ClickListener extends MouseAdapter
	{
		private Album album;
		public ClickListener(Album a)
		{
			this.album = a;
		}
		@Override
		public void mouseClicked(MouseEvent e) 
		{
			if(e.getClickCount() > 1)
			{
				MainPane.getInstance().updateInfoPane(this.album);
			}
		}
	}
}
