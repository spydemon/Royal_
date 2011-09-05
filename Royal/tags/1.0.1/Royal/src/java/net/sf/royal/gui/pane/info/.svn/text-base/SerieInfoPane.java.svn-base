package net.sf.royal.gui.pane.info;

import java.awt.GridBagConstraints;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import net.sf.royal.datamodel.Serie;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.pane.InfoPane;
import net.sf.royal.gui.util.AuthorDisplayerPane;

@SuppressWarnings("serial")
public class SerieInfoPane extends AbstractInfoPane
{
	private Serie serie;
	
	private JLabel jlName;
	private JLabel jlNbAlbums;
	private AuthorDisplayerPane adpAuthors;
	private JTextField jtfType;
	private JCheckBox jcbOneshot;
	private JTextArea jtaDesc;
	
	public SerieInfoPane(Serie s)
	{	
		this.serie = s;
		this.jlNbAlbums = new JLabel();
		this.jlName = new JLabel();
		
		this.gbc.weighty = 0.0;
		this.gbc.gridx = 0;
		this.gbc.gridy = 0;
		this.changeGridProperties(LABEL);
		this.add(new JLabel(InfoPane.name), this.gbc);
		
		this.gbc.gridx++;
		this.changeGridProperties(LABEL);
		this.add(this.jlName, this.gbc);
		
		this.gbc.gridx = 0;
		this.gbc.gridy++;
		this.changeGridProperties(LABEL);
		this.add(new JLabel(InfoPane.nbalbums), this.gbc);
		
		this.gbc.gridx++;
		this.changeGridProperties(LABEL);
		this.add(this.jlNbAlbums, this.gbc);
		
		/**** AuthorDisplayerPane ****/
		this.gbc.gridy ++;
		this.gbc.gridx = 0;
		this.changeGridProperties(LABEL);
		this.add(new JLabel(InfoPane.author), this.gbc);
		
		this.gbc.gridx++;
		this.changeGridProperties(COMPONENT);
		this.gbc.insets.set(5, 5, 5, 5);
		this.gbc.fill = GridBagConstraints.HORIZONTAL;
		this.gbc.anchor = GridBagConstraints.WEST;
		this.adpAuthors = new AuthorDisplayerPane();
		this.add(this.adpAuthors, this.gbc);
		
		/* Type */
		this.gbc.gridy++;
		this.gbc.gridx = 0;
		this.add(new JLabel(InfoPane.type), this.gbc);
		
		this.gbc.gridx++;
		this.jtfType = this.createTextField();
		this.add(this.jtfType, this.gbc);
		
		/* One shot */
		this.gbc.gridy++;
		this.gbc.gridx = 0;
		this.add(new JLabel(InfoPane.oneshot), this.gbc);
		
		this.gbc.gridx++;
		this.jcbOneshot = new JCheckBox();
		this.jcbOneshot.addActionListener(new DoNotChangeListener(this.jcbOneshot));
		this.add(this.jcbOneshot, this.gbc);
		
		/* Description */
		this.gbc.gridy++;
		this.gbc.gridx = 0;
		this.add(new JLabel(InfoPane.desc), this.gbc);
		
		this.gbc.gridx++;
		this.jtaDesc = new JTextArea();
		this.add(this.jtaDesc, this.gbc);
		
		/* Coverpane */
		this.gbc.gridy++;
		this.gbc.gridx = 0;
		this.changeGridProperties(COMPONENT);
		this.gbc.weighty = 1.0;
		this.gbc.gridheight = GridBagConstraints.REMAINDER;
		this.gbc.anchor = GridBagConstraints.NORTHWEST;
		this.add(this.cpAlbums,this.gbc);
		
		this.update(this.serie);
	}

	public void update(Serie s) 
	{
		this.serie = s;
		/**** Name ****/
		this.jlName.setText(s.getName());
		
		/**** Number of albums ****/
		this.jlNbAlbums.setText(Integer.toString(s.getAlbums().size()));
		
		/**** AuthorDisplayerPane ****/
		this.adpAuthors.update(this.serie);
		this.cpAlbums.update(s);
		this.resizeCoverPane();
		this.revalidate();
		this.repaint();
		
		/**** Type ****/
		if(this.serie.getType() != null && this.serie.getType().getName() != null)
		{
			this.jtfType.setText(s.getType().getName());
		}
		else
		{
			this.jtfType.setText(LocaleManager.getInstance().getString("no_type"));
		}
		
		/***** One shot *****/
		this.jcbOneshot.setSelected(this.serie.isOneShot());
		
		/***** Description *****/
		if(this.serie.getDescription() != null)
		{
			this.jtaDesc.setText(this.serie.getDescription());
		}
	}
}
