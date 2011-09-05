package net.sf.royal.gui.pane.info;

import java.util.Set;

import javax.swing.JLabel;
import javax.swing.JTextField;

import net.sf.royal.datamodel.Serie;
import net.sf.royal.datamodel.Type;
import net.sf.royal.gui.pane.InfoPane;

/**
 * @author Soulou
 * Right panel when a type is selected
 */
public class TypeInfoPane extends AbstractInfoPane
{
	/* Fields */
	private JTextField jtfName;
	private JTextField jtfNbSeries;
	private JTextField jtfNbAlbums;
	private Type type;
	
	/* Constructor */
	/**
	 * Initialize the fields, and configure the GridBagConstraint
	 */
	public TypeInfoPane(Type t)
	{
		super();
		
		this.type = t;
		this.jtfName = this.createTextField();
		this.jtfNbSeries = this.createTextField();
		this.jtfNbAlbums = this.createTextField();
		
		/**** Name ****/
		this.gbc.gridy = 0;
		this.gbc.gridx = 0;
		this.changeGridProperties(LABEL);
		this.add(new JLabel(InfoPane.name),this.gbc);
		
		this.gbc.gridx++;
		this.changeGridProperties(COMPONENT);
		this.add(this.jtfName, this.gbc);
		
		/**** NbSeries ****/
		this.gbc.gridx = 0;
		this.gbc.gridy ++;
		this.changeGridProperties(LABEL);
		this.add(new JLabel(InfoPane.nbseries), this.gbc);
		
		this.gbc.gridx++;
		this.changeGridProperties(COMPONENT);
		this.add(this.jtfNbSeries, this.gbc);
		
		/**** NbAlbums ****/
		this.gbc.gridx = 0;
		this.gbc.gridy ++;
		this.changeGridProperties(LABEL);
		this.add(new JLabel(InfoPane.nbalbums), this.gbc);
		
		this.gbc.gridx++;
		this.changeGridProperties(COMPONENT);
		this.add(this.jtfNbAlbums, this.gbc);
		
		this.update(this.type);
	}

	/* Methods */
	/**
	 * Update the panel instead of create a new
	 * Less memory consumption
	 * @param t
	 */
	public void update(Type t) 
	{	
		this.type = t;
		/**** Name ****/
		this.jtfName.setText(this.type.getName());
		
		/**** NbSeries ****/
		Set<Serie> ss = this.type.getSeries();
		int nb = 0;
		if(ss != null)
		{
			nb = ss.size();
		}
		this.jtfNbSeries.setText("" + nb);
		
		/**** NbAlbums ****/
		nb = 0;
		if(ss != null)
		{
			for(Serie s : ss)
			{
				nb += s.getAlbums()==null ? 0 : s.getAlbums().size();
			}
		}
		this.jtfNbAlbums.setText("" + nb);
	}
}
