package net.sf.royal.gui.pane.info;

import java.util.ArrayList;
import java.util.List;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;

/**
  * @author Soulou
  * Put cover pictures in the given panel
  * Goal : resize them in a new thread
  */
public class ImageResizerTask extends SwingWorker<List<JLabel> ,JLabel>
{
	/* Fields */

	/**
	  * List of AlbumCoverLabel created
	  */
	List<JLabel> listLabels;
	/**
	  * List of the covers to manage
	  */
	List<String> covers;
	/**
	  * JPanel to update
	  */
	JPanel jpCover;

	/**
	  * Main Constructor
	  * @param jp : JPanel to update
	  * @param URLs : List of covers
	  */
	public ImageResizerTask(JPanel jp, List<String> coversToPrint)
	{
		this.jpCover = jp;
		this.covers = coversToPrint;
		this.listLabels = new ArrayList<JLabel>();
	}

	/**
	  * Override of the method launch in the new thread
	  * @return List of JLabel containing an image
	  */
	@Override
	public List<JLabel> doInBackground()
	{
		CoverLabel img;
		for(int i = 0; i < this.covers.size(); i++)
		{
			String cover = this.covers.get(i);
			img = new CoverLabel(cover , CoverPane.COVER_WIDTH, CoverPane.COVER_HEIGHT);
			this.listLabels.add(img);
			this.publish(img);
		}
		return this.listLabels;
	}

	/**
	  * Override of the method call after a publish
	  * @param chunks : List of JLabel published
	  */

	private List<JLabel> imagesToAdd;
	@Override
	public void process(List<JLabel> chunks)
	{
		this.imagesToAdd = chunks;
		SwingUtilities.invokeLater(new Runnable(){
			public void run()
			{
				for(JLabel jl : ImageResizerTask.this.imagesToAdd)
				{
					ImageResizerTask.this.jpCover.add(jl);
				}
				ImageResizerTask.this.jpCover.revalidate();
			}
		});
	}
}
