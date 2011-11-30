package net.sf.royal.gui.wizard.add_dialog;

import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Set;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JPanel;

import net.sf.royal.datamodel.Album;
import net.sf.royal.datamodel.Author;
import net.sf.royal.datamodel.Color;
import net.sf.royal.datamodel.Illustration;
import net.sf.royal.datamodel.Scenario;
import net.sf.royal.datamodel.Work;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.persistency.PersistencyManager;
import net.sf.royal.persistency.SaveItemPersistency;

/**
 * This class provides a simple panel composed by a JComboBox which 
 * can be used to display a list of suggestions, 3 checkboxes to select the author's job
 * and a "+" button to add another author.
 * @author Unbekandt Léo
 * @author Maxime Kientz
 */
public class JLibraryPane extends JPanel
{
	private GridBagConstraints gbc;
	private ArrayList<JEntryPane> jepEntryList;
	private ArrayList<JCheckBox> jchScenaristList;
	private ArrayList<JCheckBox> jchDrawerList;
	private ArrayList<JCheckBox> jchColoristList;
	private ArrayList<JButton> jbAddAuthorList;
	private ArrayList<JButton> jbDetailsAuthorList;
	private ArrayList<Author> authorList;
	private JDialog parent;
	private boolean firstLine;

	/**
	 * Main constructor
	 * @param aad, Containing window
	 */
	public JLibraryPane(JDialog parent)
	{
		jepEntryList = new ArrayList<JEntryPane>();
		jchColoristList = new ArrayList<JCheckBox>();
		jchDrawerList = new ArrayList<JCheckBox>();
		jchScenaristList = new ArrayList<JCheckBox>();
		jbAddAuthorList = new ArrayList<JButton>();
		jbDetailsAuthorList = new ArrayList<JButton>();
		authorList = new ArrayList<Author>();
		
		this.parent = parent;
		this.firstLine = true;
		this.setLayout(new GridBagLayout());
		this.gbc = new GridBagConstraints();
		this.gbc.insets = new Insets(0, 0, 0, 10);
		this.gbc.anchor = GridBagConstraints.WEST;
		this.gbc.gridy = 0;
		this.addAuthorLine();
	}
	
	/**
	 * Add a line defining an Author to the GUI
	 * Better to call it in an invokeLater
	 */
	private void addAuthorLine()
	{
		this.gbc.gridwidth=1;
		this.gbc.gridx = 0;
		this.gbc.fill = GridBagConstraints.HORIZONTAL;
		this.gbc.weightx = 1.0;
		
		/* the combobox */
		JEntryPane jepEntry = new JEntryPane(JEntryPane.AUTHOR);
		this.add(jepEntry, gbc);

		//this.gbc.weightx = 0;
		/* the checkboxes */
		//this.gbc.gridx++;
		//JCheckBox jchScenarist = new JCheckBox(LocaleManager.getInstance().getString("scenario"));
		//this.add(jchScenarist, gbc);
		
		//this.gbc.gridx++;
		//JCheckBox jchDrawer = new JCheckBox(LocaleManager.getInstance().getString("illustration"));
		//this.add(jchDrawer, gbc);
		
		//this.gbc.gridx++;
		//JCheckBox jchColorist = new JCheckBox(LocaleManager.getInstance().getString("coloration"));
		//this.add(jchColorist, gbc);

		/* the button */
		//this.gbc.gridx++;
		//JButton jbAddAuthor;
		//if(this.firstLine)
		//	jbAddAuthor = new JButton("+");
		//else
		//	jbAddAuthor = new JButton("-");
		//this.add(jbAddAuthor, this.gbc);
		//jbAddAuthor.addActionListener(new AuthorActionListener());
	
		//this.gbc.gridx++;
		//JButton jbDetailsAuthor = new JButton("-->");
		//jbDetailsAuthor.addActionListener(new AuthorDetailsListener(this.jbDetailsAuthorList.size()));
		//this.add(jbDetailsAuthor, gbc);
		
		jepEntryList.add(jepEntry);
		//jchColoristList.add(jchColorist);
		//jchDrawerList.add(jchDrawer);
		//jchScenaristList.add(jchScenarist);
		//jbAddAuthorList.add(jbAddAuthor);
		//jbDetailsAuthorList.add(jbDetailsAuthor);
		
		this.gbc.gridy++;
		this.gbc.gridx=0;
		
		parent.setSize(parent.getSize().width,
				parent.getSize().height	+ jepEntryList.get(0).getHeight());
		
		JLibraryPane.this.revalidate();
		JLibraryPane.this.repaint();
	}
	
	/**
	 * Thanks to a set of work, we add the correct number of line
	 * filled by the information of the authors.
	 * @param works : Set of work to manage 
	 */
	public void setAuthorLines(Set<Work> works)
	{
		int i, j;
		for(i = 1; i < jbAddAuthorList.size(); i++)
		{
			removeAuthorLine(jbAddAuthorList.get(i));
		}
		this.firstLine = true;
		authorList.clear();
		
		Author au;
		Work work;
		ArrayList<Work> worksList = new ArrayList<Work>();
		Object[] worksTab = works.toArray();
		
		Object elem;
		for (i = 1; i < worksTab.length; ++i) 
		{
		    elem = worksTab[i];
		    for (j = i; j > 0 && ((Work)worksTab[j-1]).getAuthor().getId()
		    		.compareTo(((Work)elem).getAuthor().getId()) > 0; j--)
		        worksTab[j] = worksTab[j-1];
		    worksTab[j] = elem;
		}
		
		int index;
		// We browse the given works
		for(j = 0, i = 0; i < worksTab.length; i++)
		{
			work = (Work)worksTab[i];
			au = work.getAuthor();
			
			// We check if an JEntrtyPane matchs the current author
			//index = authorList.indexOf(au);
		/*	if(index != -1)
			{
				// If it's the case, we check the correct checkbox
				if(work instanceof Scenario)
					this.jchScenaristList.get(index).setSelected(true);
				if(work instanceof Illustration)
					this.jchDrawerList.get(index).setSelected(true);
				if(work instanceof Color)
					this.jchColoristList.get(index).setSelected(true);
				// Next work
				continue;
			}
			*/
			// It's a new author, we add the author to the list
			authorList.add(au);
			// If it's the first one, we fill the JEntryPane
			if(JLibraryPane.this.firstLine)
			{
				JLibraryPane.this.firstLine = false;
			}
			// In the other case, we add a new line
			else
			{
				this.addAuthorLine();
			}
			
			// We fill the fields
			this.jepEntryList.get(j).setID(au.getId());
			
		/*	if(work instanceof Scenario)
				this.jchScenaristList.get(j).setSelected(true);
			if(work instanceof Illustration)
				this.jchDrawerList.get(j).setSelected(true);
			if(work instanceof Color)
				this.jchColoristList.get(j).setSelected(true);
		*/
			j++;
		}
	}
	
	/**
	 * Remove the line where "-" button is clicked
	 */
	public void removeAuthorLine(JButton jb)
	{
		int index = jbAddAuthorList.indexOf(jb);
		ArrayList<Component> compToDelete = new ArrayList<Component>();
		
		compToDelete.add(jbAddAuthorList.get(index));
		jbAddAuthorList.remove(index);
		compToDelete.add(jepEntryList.get(index));
		jepEntryList.remove(index);
		compToDelete.add(jchColoristList.get(index));
		jchColoristList.remove(index);
		compToDelete.add(jchDrawerList.get(index));
		jchDrawerList.remove(index);
		compToDelete.add(jchScenaristList.get(index));
		jchScenaristList.remove(index);
		compToDelete.add(jbDetailsAuthorList.get(index));
		jbDetailsAuthorList.remove(index);
		
		for(Component comp : compToDelete)
		{
			this.remove(comp);
		}
		
		JLibraryPane.this.repaint();
		
	}
	
	/**
	 * PRE : a is persistent (has an ID)
	 * @param a : save the authors of this Album
	 */
	public void saveAuthors(Album albumArg)
	{
		 // TODO : Delete Authors which aren't present anymore
		
		// We open the session again if it was close for the hibernate lazy loading
		Album album = PersistencyManager.findAlbumByID(albumArg.getId());
		
		ArrayList<Author> authors = new ArrayList<Author>();
		ArrayList<Author> authorsToSave = new ArrayList<Author>();
		ArrayList<Work> worksToSave = new ArrayList<Work>();
		ArrayList<Work> worksToDelete = new ArrayList<Work>();
		Set<Work> albumWorks = album.getWorks();
		Author author;
		boolean specialized = false;
		boolean exist = false;
		
		// We browse the existent Works
		for(Work w : albumWorks)
		{
			author = w.getAuthor();
			// We add all author ONE time to authors
			if(!authors.contains(author))
			{
				authors.add(author);
			}
		}
		
		for(int i = 0; i < jepEntryList.size(); i++)
		{
			if(!jepEntryList.get(i).getText().isEmpty())
			{
				if(jepEntryList.get(i).getID() == null)
				{
					author = new Author();
					author.setNickName(jepEntryList.get(i).getText());
					authorsToSave.add(author);
				}
				else
				{
					author = PersistencyManager.findAuthorByID(jepEntryList.get(i).getID());
				}
				
				/* 
				 * For each author, we check the checkboxes whiche are selected
				 * If that's the case, we look if this author already have this role
				 * for the given album, if it's the case, we do nothing, however, if it's
				 * new, we create a new Work.
				 */
				if(jchColoristList.get(i).isSelected())
				{
					for(Work w : albumWorks)
					{
						if(w instanceof Color)
						{
							if(w.getAuthor().getId().equals(author.getId()) &&
							   w.getAlbum().getId().equals(album.getId()))
							   {
									exist = true;
									break;
							   }
						}
					}
					if(!exist)
					{
						Color col = new Color();
						col.setAlbum(album);
						col.setAuthor(author);
						worksToSave.add(col);
					}
					specialized = true;
				}
				else
					for(Work w : albumWorks)
						if(w instanceof Color && w.getAuthor().getId().equals(author.getId()) &&
								   w.getAlbum().getId().equals(album.getId()))
							worksToDelete.add(w);
				
				exist = false;
				if(jchDrawerList.get(i).isSelected())
				{
					for(Work w : albumWorks)
					{
						if(w instanceof Illustration)
						{
							if(w.getAuthor().getId().equals(author.getId()) &&
							   w.getAlbum().getId().equals(album.getId()))
							   {
									exist = true;
									break;
							   }
						}
					}
					if(!exist)
					{
						Illustration ill = new Illustration();
						ill.setAlbum(album);
						ill.setAuthor(author);
						worksToSave.add(ill);
					}
					specialized = true;
				}
				else
					for(Work w : albumWorks)
						if(w instanceof Illustration && w.getAuthor().getId().equals(author.getId()) &&
								   w.getAlbum().getId().equals(album.getId()))
							worksToDelete.add(w);

				exist = false;
				if(jchScenaristList.get(i).isSelected())
				{
					for(Work w : albumWorks)
					{
						if(w instanceof Scenario)
						{
							if(w.getAuthor().getId().equals(author.getId()) &&
							   w.getAlbum().getId().equals(album.getId()))
							   {
									exist = true;
									break;
							   }
						}
					}
					if(!exist)
					{
						Scenario sce = new Scenario();
						sce.setAlbum(album);
						sce.setAuthor(author);
						worksToSave.add(sce);
					}
					specialized = true;
				}
				else
					for(Work w : albumWorks)
						if(w instanceof Scenario && w.getAuthor().getId().equals(author.getId()) &&
								   w.getAlbum().getId().equals(album.getId()))
							worksToDelete.add(w);
				
				if(!specialized)
				{
					Work w = new Work();
					w.setAlbum(album);
					w.setAuthor(author);
					worksToSave.add(w);
				}
				specialized = false;
				exist = false;
			}
		}
		for(Author au : authorsToSave)
		{
			SaveItemPersistency.saveAuthor(au);
		}
		for(Work work : worksToDelete)
		{
			PersistencyManager.removeWork(work);
		}
		for(Work work : worksToSave)
		{
			SaveItemPersistency.saveWork(work);
		}
	}
	
	protected GridBagConstraints getGBC()
	{
		return this.gbc;
	}
	

	public class AuthorActionListener implements ActionListener
	{
		@Override
		public void actionPerformed(ActionEvent ae)
		{
			JButton jb = (JButton)ae.getSource();
			if(jb.getText().equals("+"))
			{
				JLibraryPane.this.firstLine = false;
				JLibraryPane.this.gbc.gridy++;
				JLibraryPane.this.gbc.gridx=0;
				parent.setSize(parent.getSize().width,
							   parent.getSize().height
							   + JLibraryPane.this.jepEntryList.get(0).getHeight());
				JLibraryPane.this.addAuthorLine();
			}
			else
			{
				parent.setSize(parent.getSize().width,
						   parent.getSize().height
						   - JLibraryPane.this.jepEntryList.get(0).getHeight());
				JLibraryPane.this.removeAuthorLine(jb);
			}
		}
	}
	
	public class AuthorDetailsListener implements ActionListener
	{
		int _r;
		
		public AuthorDetailsListener(int rank)
		{
			this._r = rank;
		}
		
		@Override
		public void actionPerformed(ActionEvent e) 
		{
			Author au = PersistencyManager.findAuthorByID(JLibraryPane.this.jepEntryList.get(_r).getID());
			if(au == null)
			{
				au = new Author();
				au.setNickName(JLibraryPane.this.jepEntryList.get(_r).getText());
			}
			
			AuthorAddDialog aad = new AuthorAddDialog(au);
			aad.setLocationRelativeTo(null);
			aad.setModal(true);
			aad.setVisible(true);
			if(aad.getID() != null)
			{
				JLibraryPane.this.jepEntryList.get(_r).refresh();
				JLibraryPane.this.jepEntryList.get(_r).setID(aad.getID());
			}
			JLibraryPane.this.parent.toFront();
		}
	}
}