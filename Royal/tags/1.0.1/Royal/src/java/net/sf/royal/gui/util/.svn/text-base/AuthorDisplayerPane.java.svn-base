package net.sf.royal.gui.util;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
//import javax.swing.SwingUtilities;

import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;

import net.sf.royal.datamodel.Serie;
import net.sf.royal.datamodel.Album;
import net.sf.royal.datamodel.Author;
import net.sf.royal.datamodel.Work;
import net.sf.royal.datamodel.Illustration;
import net.sf.royal.datamodel.Scenario;
import net.sf.royal.datamodel.Color;
import net.sf.royal.gui.manager.IconManager;
import net.sf.royal.gui.manager.LocaleManager;

import net.sf.royal.util.Tools;

/**
 * This panel is used to display the authors of an album
 * or a serie and their work.
 * @author Steveo
 *
 */
@SuppressWarnings("serial")
public class AuthorDisplayerPane extends JPanel {
	public static AuthorDisplayerPane instance = new AuthorDisplayerPane();

	private GridBagConstraints gbc;
	
	
	/*
	 * NOTE : normally, the different lists contain the same number
	 * of elements.
	 * The x line in list matches the x label in labels, the x icon in color, etc.
	 */
	/**
	 * Data of the AuthorDisplayerPane
	 */
	private List<AuthorDisplayerPane.AuthorLine> list;
	/**
	 * Names of the Authors
	 */
	private List<JTextField> labels;
	/**
	 * Icons for colorist 
	 */
	private List<JLabel> color;
	/**
	 * Icons for illustration
	 */
	private List<JLabel> illustration;
	/**
	 * Icons for scenario
	 */
	private List<JLabel> scenario;
	
	public AuthorDisplayerPane()
	{
		this.gbc = new GridBagConstraints();
		this.gbc.weightx = 0.0;
		this.gbc.weighty = 0.0;
		this.gbc.insets = new Insets(0,0,5,5);
		this.gbc.anchor = GridBagConstraints.NORTHWEST;
		
		this.setLayout(new GridBagLayout());
		
		this.list = new ArrayList<AuthorDisplayerPane.AuthorLine>();
		this.illustration = new ArrayList<JLabel>();
		this.color = new ArrayList<JLabel>();
		this.scenario = new ArrayList<JLabel>();
		this.labels = new ArrayList<JTextField>();
	}
	
	
	public void update(Album a)
	{
		Set<Work> sw = a.getWorks();
		this.removeAllElements();
		
		for(Work w : sw)
		{
			this.addWork(w);
		}
		
		this.display();
	}
	
	/**
	 * Update the AuthorDisplayerPane with a Serie.<br/>
	 * Will select the first Album and displays its Authors.
	 * 
	 * @param s The Serie
	 */
	public void update(Serie s)
	{
		Set<Album> sa = s.getAlbums();
		Album a = (Album) sa.toArray()[0];
		
		this.update(a);
	}

	public void display()
	{
		/*SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				AuthorDisplayerPane.this.display(true);
			}
		});*/
		this.display(false);
	}
	
	/**
	 * Display to content of the list list
	 */
	private void display(boolean nimp)
	{
		int state;
		String name;
		
		// Initialize the Components
		for(AuthorLine al : this.list)
		{
			name = al.getName();
			state = al.getState();
			
			JTextField jl = new JTextField(name, name.length() + 3);
			jl.setEditable(false);
			jl.setBackground(new java.awt.Color(200, 200, 205));
			this.labels.add(jl);
			
			// Tests of the works
			if((state & AuthorLine.Illustration) != 0)
			{
				this.illustration.add(createLabel(AuthorLine.Illustration));
			}
			else
			{
				this.illustration.add(createLabel(-1));
			}
			
			if((state & AuthorLine.Color) != 0)
			{
				this.color.add(createLabel(AuthorLine.Color));
				//System.out.println("Color : " + name);
			}
			else
			{
				this.color.add(createLabel(-1));
			}
			
			if((state & AuthorLine.Scenario) != 0)
			{
				this.scenario.add(createLabel(AuthorLine.Scenario));
				//System.out.println("Scenario : " + name);
			}
			else
			{
				this.scenario.add(createLabel(-1));
			}
		}
		
		for(int i=0; i< this.list.size(); i++)
		{
			this.addLine(i);
		}
		
		this.repaint();
	}
	
	/**
	 * Add a line in the panel. The elements of the line are in the different lists 
	 * (labels, illustration, color, scenario)
	 * @param index The index of the line component in the lists
	 */
	private void addLine(int index)
	{
		this.gbc.gridx = 0;
		this.gbc.gridy = index;
		
		this.gbc.fill = GridBagConstraints.HORIZONTAL;
		this.gbc.weightx = 0;
		this.add(this.labels.get(index), this.gbc);
		
		this.gbc.fill = GridBagConstraints.NONE;
		this.gbc.gridx ++;
		this.gbc.weightx = 0;
		this.add(this.scenario.get(index), this.gbc);
		
		this.gbc.gridx ++;
		this.add(this.illustration.get(index), this.gbc);
		
		this.gbc.gridx ++;
		this.gbc.weightx = 1.0;
		this.gbc.gridwidth = GridBagConstraints.REMAINDER;
		this.add(this.color.get(index), this.gbc);
		
		this.gbc.gridwidth = 1;
	}
	
	private JLabel createLabel(int type)
	{
		JLabel res;
		ImageIcon icon;
		String tooltip;
		
		switch(type)
		{
		case AuthorLine.Illustration : 
			icon = IconManager.getIcon("illustration.png");
			tooltip = LocaleManager.getInstance().getString("illustration");
			break;
		case AuthorLine.Color : 
			icon = IconManager.getIcon("color.png");
			tooltip = LocaleManager.getInstance().getString("coloration");
			break;
		case AuthorLine.Scenario :
			icon = IconManager.getIcon("scenario.png");
			tooltip = LocaleManager.getInstance().getString("scenario");
			break;
		default : 
			icon = new ImageIcon();
			tooltip = "";
		}
		
		res = new JLabel(icon);
		res.setToolTipText(tooltip);
		
		return res;
	}
	
	/**
	 * Remove a line specified by the Author
	 * @param au The Author to remove
	 */
	public void removeLine(Author au)
	{
		String name = Tools.authorToString(au);
		
		for(int i=0; i<this.list.size(); i++)
		{
			if(this.list.get(i).getName().equals(name))
			{
				this.list.remove(i);
				return;
			}
		}
	}
	
	/**
	 * Remove all the lines
	 */
	public void removeAllElements()
	{
		this.list.clear();
		
		for(JTextField jl : this.labels)
			this.remove(jl);
		this.labels.clear();
		
		for(JLabel jl : this.color)
			this.remove(jl);
		this.color.clear();
		
		for(JLabel jl : this.illustration)
			this.remove(jl);
		this.illustration.clear();
		
		for(JLabel jl : this.scenario)
			this.remove(jl);
		this.scenario.clear();
	}
	
	/**
	 * Add a line. If the author already exists, just add the work role
	 * @param w The work to add
	 */
	public void addWork(Work w)
	{
		Author au = w.getAuthor();
		
		if(au == null)
			return;
		
		String name;
		for(AuthorLine al : this.list)
		{
			name = al.getName();
			if(name.equals(Tools.authorToString(au)))
			{
				al.addWork(w);
				return;
			}
		}
		
		AuthorLine al = new AuthorLine(au);
		al.addWork(w);
		
		this.list.add(al);
	}
	
	public static class AuthorLine
	{
		public static final int Color = 0x1;
		public static final int Illustration = 0x10;
		public static final int Scenario = 0x100;
		
		private String name;
		private int state;
		
		/**
		 * 
		 * @param name
		 */
		public AuthorLine(Author au)
		{
			this.name = Tools.authorToString(au);
			this.state = 0;
		}
		
		/**
		 * Get the name of the author in the AuthorLine
		 * 
		 * @return The name of the author
		 */
		public String getName()
		{
			return this.name;
		}
		
		/**
		 * Add a work to this author.<br/>
		 * Use the real class of the work.
		 * @see net.sf.royal.datamodel.Color
		 * @see net.sf.royal.datamodel.Illustration
		 * @see net.sf.royal.datamodel.Scenario
		 * 
		 * @see #removeWork(Work)
		 * 
		 * @param w The new Work to add
		 */
		public void addWork(Work w)
		{
			if(w instanceof Illustration)
			{
				this.state |= AuthorLine.Illustration;
				//System.out.println("addWork : Illustration : " + this.name);
				//System.out.println("State : " + this.state);
			}
			else if(w instanceof Color)
			{
				this.state |= AuthorLine.Color;
				//System.out.println("addWork : Color : " + this.name);
				//System.out.println("State : " + this.state);
			}
			else if(w instanceof Scenario)
			{
				this.state |= AuthorLine.Scenario;
				//System.out.println("addWork : Scenario : " + this.name);
				//System.out.println("State : " + this.state);
			}
		}
		
		/**
		 * Remove a  work from the AuthorLine.<br/>
		 * Use the real class of the work to know which work role to remove<br/>
		 * 
		 * @param w The work to remove
		 */
		public void removeWork(Work w)
		{
			if(w instanceof Illustration)
			{
				this.state = this.state ^ AuthorLine.Illustration;
			}
			else if(w instanceof Color)
			{
				this.state = this.state ^ AuthorLine.Color;
			}
			else if(w instanceof Scenario)
			{
				this.state = this.state ^ AuthorLine.Scenario;
			}
		}
		
		/**
		 * Get the state of the AuthorLine
		 * 
		 * @return The state
		 */
		public int getState()
		{
			return this.state;
		}
	}
}
