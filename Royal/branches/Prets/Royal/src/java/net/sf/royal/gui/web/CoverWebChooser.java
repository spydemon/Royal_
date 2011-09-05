package net.sf.royal.gui.web;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingWorker;
import javax.swing.border.BevelBorder;

import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.util.ImageHelper;
import net.sf.royal.gui.wizard.add_dialog.AlbumAddDialog;
import net.sf.royal.web.GoogleImageSearcher;

/**
 * @author Soulou
 * Class to show at the user different pictures.
 * He can choose one of them as the cover of the current comics he wants to add.
 */
public class CoverWebChooser extends JDialog 
{
	/**
	 * Width of the thumbnail
	 */
	private static final int COVER_WIDTH = 210;
	/**
	 * Height of the thumbnail
	 */
	private static final int COVER_HEIGHT = 297;
	
	private String title;
	private ArrayList<String> resultURLs;
	private JScrollPane jspCoverView;
	private JPanel jpContent;
	private JButton jbOk;
	private JButton jbCancel;
	private AlbumAddDialog parent;
	
	/**
	 * Main constructor
	 * @param parent Parent window the set up this dialog
	 * @param comicsSearch Words which will be search on the web
	 */
	public CoverWebChooser(AlbumAddDialog parent, String comicsSearch)
	{
		this.parent = parent;
		this.title = comicsSearch;
		this.resultURLs = null;
		this.setLayout(new BorderLayout());
		
		// We put the different cover in an horizontal JScrollPane
		this.jspCoverView = new JScrollPane();
		this.jspCoverView.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
		this.jspCoverView.setSize(new Dimension(COVER_WIDTH*4, (int)(COVER_HEIGHT*1.3)
						+ this.jspCoverView.getHorizontalScrollBar().getSize().height));
		this.add(jspCoverView, BorderLayout.CENTER);
		this.jpContent = new JPanel();
		this.jspCoverView.setViewportView(this.jpContent);
		
		this.jbOk = new JButton(LocaleManager.getInstance().getString("ok"));
		this.jbOk.setEnabled(false);
		this.jbOk.addActionListener(new ValidateImageListener());
		
		this.jbCancel = new JButton(LocaleManager.getInstance().getString("cancel"));
		jbCancel.addActionListener(new ActionListener() 
		{
			@Override
			public void actionPerformed(ActionEvent arg0) 
			{
				CoverWebChooser.this.dispose();
			}
		});
		
		JPanel jpButton = new JPanel();
		jpButton.add(this.jbOk);
		jpButton.add(this.jbCancel);
		this.add(jpButton,BorderLayout.SOUTH);
		
		// Location of the window
		Dimension size = new Dimension();
		size.width = this.jspCoverView.getSize().width;
		size.height = this.jspCoverView.getSize().height + jpButton.getSize().height;
		this.setSize(size);
		this.setLocationRelativeTo(parent);
		this.setTitle(LocaleManager.getInstance().getString("cover_fetcher"));
		this.backgroundSearch();
		this.setModal(true);
		this.setVisible(true);
	}
	
	/**
	 * Begin a search on the web in a backgroud thread
	 */
	public void backgroundSearch()
	{
		Thread th = new Thread(new Runnable() 
		{
			@Override
			public void run() 
			{
				GoogleImageSearcher gis = new GoogleImageSearcher(CoverWebChooser.this.title);
				gis.executeSearch();
				CoverWebChooser.this.resultURLs = gis.getResults();
				CoverWebChooser.this.displayCovers();
			}
		});
		
		th.start();
	}
	
	 /** 
	  * print one by one the different results
	  */
	public void displayCovers()
	{
		SwingWorker<ArrayList<JLabel>, JLabel> sw = new SwingWorker<ArrayList<JLabel>, JLabel>() 
		{
			@Override
			protected ArrayList<JLabel> doInBackground() throws Exception 
			{
				ArrayList<JLabel> results = new ArrayList<JLabel>();
				
				for(String url : CoverWebChooser.this.resultURLs)
				{
					JLabel jl = new JLabel(ImageHelper.getScaledImage(ImageWebGetter.getImageFromURL(url), 
							COVER_WIDTH, COVER_HEIGHT));
					jl.addMouseListener(new SelectionListener());
					this.publish(jl);
					results.add(jl);
				}
				return results;
			}
			
			@Override
			protected void process(List<JLabel> chunks) 
			{
				for(JLabel jl : chunks)
				{
					CoverWebChooser.this.jpContent.add(jl);	
				}
				CoverWebChooser.this.jpContent.revalidate();
			}
		};
		sw.execute();
	}
	
	public class SelectionListener extends MouseAdapter
	{
		@Override
		public void mouseClicked(MouseEvent e) 
		{
			CoverWebChooser.this.jbOk.setEnabled(true);
			if(e.getButton() == 1)
			{
				JLabel jl = (JLabel)e.getSource();
				jl.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
				
				JLabel tmp;
				for(Component c : CoverWebChooser.this.jpContent.getComponents())
				{
					tmp = (JLabel)c;
					if(tmp != jl)
					{
						tmp.setBorder(BorderFactory.createEmptyBorder());
					}
				}
			}
		}
	}
	
	public class ValidateImageListener implements ActionListener
	{
		@Override
		public void actionPerformed(ActionEvent ae)
		{
			JLabel tmp = null;
			ImageIcon icon = null;
			for(Component c : CoverWebChooser.this.jpContent.getComponents())
			{
				tmp = (JLabel)c;
				if(tmp.getBorder() instanceof BevelBorder)
				{
					icon = (ImageIcon)tmp.getIcon();
					CoverWebChooser.this.parent.setAlbumCover(icon.getDescription());
					CoverWebChooser.this.dispose();
				}
			}
		}
	}
}
