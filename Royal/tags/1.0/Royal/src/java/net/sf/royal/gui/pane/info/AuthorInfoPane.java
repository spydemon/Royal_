package net.sf.royal.gui.pane.info;

import java.awt.GridBagConstraints;
import java.util.Date;
import java.util.List;

import javax.swing.JLabel;
import javax.swing.JTextField;

import net.sf.royal.datamodel.Album;
import net.sf.royal.datamodel.Author;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.pane.InfoPane;
import net.sf.royal.persistency.PersistencyManager;

/**
 * @author Soulou
 * Right panel when an Author is selected
 */
public class AuthorInfoPane extends AbstractInfoPane
{
	private Author author;
	private JTextField jtfName;
	private JTextField jtfFirstName;
	private JTextField jtfNickName;
	private JLabel jlLabelNickName;
	private JTextField jtfBirth;
	private JTextField jtfDeath;
	private JTextField jtfAlbum;
	private JTextField jtfSerie;

	public static final String birth = LocaleManager.getInstance().getString("birth") + " : ";
	public static final String death = LocaleManager.getInstance().getString("death") + " : ";
	
	public AuthorInfoPane(Author au)
	{
		super();
		
		this.author = au;
		this.jtfName = this.createTextField();
		this.jtfFirstName = this.createTextField();
		this.jtfNickName = this.createTextField();
		this.jlLabelNickName = new JLabel(InfoPane.nickname);
		this.jtfBirth = this.createTextField();
		this.jtfDeath = this.createTextField();
		this.jtfAlbum = this.createTextField();
		this.jtfSerie = this.createTextField();
		
		/**** Name ****/
		this.gbc.gridy = 0;
		this.gbc.gridx = 0;
		this.changeGridProperties(LABEL);
		this.add(new JLabel(InfoPane.name), this.gbc);
		
		this.gbc.gridx++;
		this.changeGridProperties(COMPONENT);
		this.add(this.jtfName,this.gbc);
		
		/**** Firstname ****/
		this.gbc.gridy++;
		this.gbc.gridx = 0;
		this.changeGridProperties(LABEL);
		this.add(new JLabel(InfoPane.firstname), this.gbc);
		
		this.gbc.gridx++;
		this.changeGridProperties(COMPONENT);
		this.add(this.jtfFirstName, this.gbc);
		
		/**** Nickname ****/
		this.gbc.gridy++;
		this.gbc.gridx = 0;
		this.changeGridProperties(LABEL);
		this.add(this.jlLabelNickName, this.gbc);
		
		this.gbc.gridx++;
		this.changeGridProperties(COMPONENT);
		this.add(this.jtfNickName,this.gbc);
		
		/**** BirstDate ****/
		this.gbc.gridy ++;
		this.gbc.gridx = 0;
		this.changeGridProperties(LABEL);
		this.add(new JLabel(birth), this.gbc);
		
		this.gbc.gridx++;
		this.changeGridProperties(COMPONENT);
		this.gbc.gridwidth = 1;
		this.add(this.jtfBirth, this.gbc);
		
		/**** DeathDate ****/
		this.gbc.gridx ++;
		this.changeGridProperties(LABEL);
		this.add(new JLabel(death), this.gbc);
		
		this.gbc.gridx++;
		this.changeGridProperties(COMPONENT);
		this.add(this.jtfDeath, this.gbc);
		
		/**** Number of albums ****/
		this.gbc.gridx = 0;
		this.gbc.gridy ++;
		this.changeGridProperties(LABEL);
		this.add(new JLabel(InfoPane.nbalbums), this.gbc);
		
		this.gbc.gridx++;
		this.changeGridProperties(COMPONENT);
		this.gbc.gridwidth = 1;
		this.add(this.jtfAlbum, this.gbc);
		
		/**** Number of Series ****/
		this.gbc.gridx ++;
		this.changeGridProperties(LABEL);
		this.add(new JLabel(InfoPane.nbseries), this.gbc);
		
		this.gbc.gridx++;
		this.changeGridProperties(COMPONENT);
		this.add(this.jtfSerie, this.gbc);
		
		/**** CoverPane ****/
		this.gbc.gridy++;
		this.gbc.gridx = 0;
		this.changeGridProperties(COMPONENT);
		this.gbc.weighty = 1.0;
		this.gbc.gridheight = GridBagConstraints.REMAINDER;
		this.gbc.anchor = GridBagConstraints.NORTHWEST;
		this.update(this.author);
		this.add(this.cpAlbums,gbc);
	}
	
	public void update(Author au) 
	{
		this.author = au;
		
		/***** Name *****/
		this.jtfName.setText(this.author.getName());
		
		/***** Firstname *****/
		this.jtfFirstName.setText(this.author.getFirstName());
		
		/***** NickName ****/
		if(this.author.getNickName() == null || this.author.getNickName().isEmpty())
		{
			this.jlLabelNickName.setText(InfoPane.nonickname);
			this.jtfNickName.setText("");
		}
		else
		{
			this.jlLabelNickName.setText(InfoPane.nickname);
			this.jtfNickName.setText(this.author.getNickName());
		}
		
		/**** birthdate ****/
		Date tmpd = this.author.getBirth();
		String tmp;
		if(tmpd != null)
		{
			tmp = LocaleManager.getInstance().getTextDate(tmpd, false);
		}
		else
		{
			tmp = "";
		}
		this.jtfBirth.setText(tmp);
		
		/**** deathdate ****/
		tmpd = this.author.getDeath();
		if(tmpd != null)
		{
			tmp = LocaleManager.getInstance().getTextDate(tmpd, false);
		}
		else
		{
			tmp = "";
		}
		this.jtfDeath.setText(tmp);
		
		/**** number of albums ****/
		List<?> la = PersistencyManager.findAlbumsByAuthorID(this.author.getId());
		int nb;
		if(la!= null)
		{
			nb = la.size();
		}
		else
		{
			nb = 0;
		}
		this.jtfAlbum.setText("" + nb);
		
		/**** number of series ****/
		la = PersistencyManager.findSeriesByAuthorID(this.author.getId());
		nb = 0;
		if(la != null)
		{
			nb = la.size();
		}
		this.jtfSerie.setText("" + nb);
		
		this.cpAlbums.update(this.author);
		this.resizeCoverPane();
		this.revalidate();
		this.repaint();
	}
}
