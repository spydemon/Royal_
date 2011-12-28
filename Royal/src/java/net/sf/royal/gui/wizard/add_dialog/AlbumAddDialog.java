package net.sf.royal.gui.wizard.add_dialog;

import java.awt.ComponentOrientation;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;
import java.util.Date;
import java.util.Set;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import net.sf.royal.datamodel.Album;
import net.sf.royal.datamodel.Author;
import net.sf.royal.datamodel.Collection;
import net.sf.royal.datamodel.HibernateUtil;
import net.sf.royal.datamodel.Bibliotheque;
import net.sf.royal.datamodel.Serie;
import net.sf.royal.datamodel.Tome;
import net.sf.royal.datamodel.Work;
import net.sf.royal.exception.DefaultException;
import net.sf.royal.gui.datepicker.JDatePicker;
import net.sf.royal.gui.filechooser.ImageChooser;
import net.sf.royal.gui.manager.FileManager;
import net.sf.royal.gui.manager.IconManager;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.MessagePaneManager;
import net.sf.royal.gui.manager.PropertyManager;
import net.sf.royal.gui.manager.ShortcutManager;
import net.sf.royal.gui.pane.AlbumPane;
import net.sf.royal.gui.pane.BottomBarPane;
import net.sf.royal.gui.util.ImageHelper;
import net.sf.royal.gui.util.ImageViewer;
import net.sf.royal.gui.util.RegexpTextField;
import net.sf.royal.gui.web.CoverWebChooser;
import net.sf.royal.persistency.PersistencyManager;
import net.sf.royal.persistency.SaveItemPersistency;

import net.sf.royal.gui.wizard.add_dialog.JLibraryPane;

import org.apache.log4j.Logger;


/**
 * This is the dialog used to add a new BD
 * @author Unbekandt Léo
 * @author Steven Nguyen
 * @author Maxime Kientz
 * @author Kevin Hagner
 *
 */

@SuppressWarnings("serial")
public class AlbumAddDialog extends JDialog
{
	/**
	 * Definition of the Logger object 
	 */
	@SuppressWarnings("unused")
	private static Logger logger = Logger.getLogger(AlbumAddDialog.class);
	
	
// Fields
	/**
	 * The title of the dialog in the correct Locale
	 */
	private static String sTitle = LocaleManager.getInstance().getString("add_album");
	
	/**
	 * The default layout of the AlbumAddDialog
	 */
	private GridBagLayout gbLayout;
	
	private ImageViewer ivImage;
	private RegexpTextField rtfTitle;
	private JButtonPane jbpSerie;
	private JAuthorPane japAuthor;
	private JComboBox buyOrNot;
	private RegexpTextField rtfNumber;
	private JLabel jlPages;
	private RegexpTextField rtfPages;
	private JLabel jlLongueur;
	private RegexpTextField rtfWidth;
	private RegexpTextField rtfHeight;
	private JCheckBox jcbOriginalEd;
	private JButtonPane jbpCollection;
	private JButtonPane jbpBibliotheque;
	private JTextField jtfISBN;
	private JTextArea jtaComment;
	private JDatePicker jdpPurchaseDate;
	private JDatePicker jdpReleaseDate;
	private JLabel libTitle;
	private JLibraryPane libTextField;
	
	private Album currentAlbum = null;
	private Serie albumSerie;
	
	private JButton jbImageBrowse;
	private JButton jbGoogleImage;
	
	private JButton jbOk;
	private JButton jbCancel;
	
	private JLabel titreAchat;
	
	/**
	 * Item use in the buyOrNot combo box
	 */
	public class Item {
		private String name;
		private int id;
		public Item(String n, int i) {
			this.name = n;
			this.id = i; 
		}
		public String toString() {
			return name;
		}
		public int getId() {
			return id;
		}
	};
	
	Item itm[] = new Item[]{new Item(LocaleManager.getInstance().getString("buy"), 1), new Item(LocaleManager.getInstance().getString("borrow"), 2)};
	
// Constructors
	/**
	 * Create a new AlbumAddDialog, specifying its parent component.<br/>
	 * The dialog will be placed at the center of it parent. <br/>
	 * The painting and the creation are done in the SwingUtilities Thread for more thread safe
	 * @param parent The parent Window of the AlbumAddDialog
	 */
	public AlbumAddDialog(Window parent, boolean isForeground)
	{
		super(parent, sTitle, isForeground?Dialog.ModalityType.DOCUMENT_MODAL:Dialog.ModalityType.MODELESS);
				
		this.setMinimumSize(new Dimension(910, 450));
		
		SwingUtilities.invokeLater(new Runnable()
		{	
			@Override
			public void run()
			{				
				AlbumAddDialog.this.init();
				AlbumAddDialog.this.initListener();
				AlbumAddDialog.this.display();
			}
		});
	}
	
	/**
	 * Create a new AlbumAddDialog.<br/>
	 * The dialog will be displayed at the center of the window and behave like a normal window
	 */
	public AlbumAddDialog()
	{
		this(null, false);
	}
	
	/**
	 * Create a new AlbumAddDialog.<br />
	 * Fields will be filled with the Album a information
	 * @param a Album to edit
	 */
	public AlbumAddDialog(Album a)
	{
		this(null, false);
		this.currentAlbum = a;
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run()
			{
				Set<Work> albumWorks = (Set<Work>)AlbumAddDialog.this.currentAlbum.getWorks();
				AlbumAddDialog.this.japAuthor.setAuthorLines(albumWorks);
				
				// Select the collection in the ComboBox
				Collection c = AlbumAddDialog.this.currentAlbum.getCollection(); 
				if(c != null)
				{
					jbpCollection.setID(c.getId());
				}
				
				// Select the Serie in the ComboBox
				Serie s = AlbumAddDialog.this.currentAlbum.getSerie();
				if(s != null)
				{
					jbpSerie.setID(s.getId());
				}
				
				// Show the comment
				String comment = AlbumAddDialog.this.currentAlbum.getComment();
				if(comment != null)
				{
					AlbumAddDialog.this.jtaComment.setText(comment);
				}
				
				String cover = AlbumAddDialog.this.currentAlbum.getCover();
				String dimension = AlbumAddDialog.this.currentAlbum.getDimension();
				String isbn = AlbumAddDialog.this.currentAlbum.getIsbn();
				String title = AlbumAddDialog.this.currentAlbum.getTitle();
				int pagecount = AlbumAddDialog.this.currentAlbum.getPageCount();
				Date purchaseDate = AlbumAddDialog.this.currentAlbum.getPurchaseDate();
				Date releaseDate = AlbumAddDialog.this.currentAlbum.getRegistration();
				boolean isOriginal = AlbumAddDialog.this.currentAlbum.isOriginal();
				
				if(dimension != null)
				{
					String[] dims = dimension.split("x");
					if(dims.length == 2)
					{
						String width = dims[0];
						String height = dims[1];
						AlbumAddDialog.this.rtfWidth.setText(width);
						AlbumAddDialog.this.rtfHeight.setText(height);
					}
				}
				if(isbn != null)
					AlbumAddDialog.this.jtfISBN.setText(isbn);
				if(title != null)
					AlbumAddDialog.this.rtfTitle.setText(title);
				
				if(AlbumAddDialog.this.currentAlbum.getTome() != null)
				{
					int number = AlbumAddDialog.this.currentAlbum.getTome().getNumber();
					if(number != 0)
						AlbumAddDialog.this.rtfNumber.setText(Integer.toString(number));
				}
				
				if(Integer.valueOf(pagecount) != null)
					AlbumAddDialog.this.rtfPages.setText(Integer.toString(pagecount));				

				if(cover != null)
					AlbumAddDialog.this.ivImage.setImage(ImageHelper.getImageIcon(
							PropertyManager.getInstance().getPathProperty("path_cover") +
							PropertyManager.sep + HibernateUtil.getCurrentDatabase() +
							PropertyManager.sep + cover));		
				
				if(purchaseDate != null)
					AlbumAddDialog.this.jdpPurchaseDate.setDate(purchaseDate);
				
				if(releaseDate != null)
					AlbumAddDialog.this.jdpReleaseDate.setDate(releaseDate);
				
				AlbumAddDialog.this.jcbOriginalEd.setSelected(isOriginal);
			}
		});
	}
	
	
	/**
	 * Constructor used to add an album to the Author
	 * @param s
	 */
	public AlbumAddDialog(Author au)
	{
		this(null,false);
		SwingUtilities.invokeLater(new Runnable() {
			
			@Override
			public void run() {
			
			}
		});
	}
	
	/**
	 * Constructor used to add an album to the Serie
	 * @param s
	 */
	public AlbumAddDialog(Serie s)
	{
		this(null,false);
		this.albumSerie = s;
		
		SwingUtilities.invokeLater(new Runnable() {
			
			@Override
			public void run() {
				AlbumAddDialog.this.jbpSerie.setID(AlbumAddDialog.this.albumSerie.getId());
				Set<Album> albums = AlbumAddDialog.this.albumSerie.getAlbums();
				for(Album a : albums)
				{
					Set<Work> works = a.getWorks();
					if(!works.isEmpty())
					{
						AlbumAddDialog.this.japAuthor.setAuthorLines(works);
						break;
					}
				}
			}
		});
	}
	
// Methods
	/**
	 * Initialize the Dialog Components.
	 * You need to use the method display to make the dialog visible
	 * @see AlbumAddDialog#display
	 */
	private void init()
	{
		this.gbLayout = new GridBagLayout();
		this.setLayout(gbLayout);
		
		/* Insets : one for the label and one for the components, otherwise
		 * the differents labels are a little bit on the top compared to the 
		 * components
		 */
		Insets iLabel = new Insets(10, 10, 5, 5);
		Insets iComp = new Insets(5, 5, 5, 5);

		/* Constraints for the main Panel */	
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.insets = iLabel;
		
		gbc.gridy = 0;
		gbc.gridx = 0;
		gbc.gridwidth = 2;
		gbc.gridheight = 11;
		gbc.weightx = 0.0;
		gbc.weighty = 0.0;
		/* ImageViewer for the cover of the album */
		this.ivImage = new ImageViewer(new Dimension(210,297));
		this.add(ivImage, gbc);
		gbc.weightx = 0.0;
		gbc.weighty = 0.0;
		gbc.gridwidth = 1;
		gbc.gridy += 9;
		
		JPanel jpImage = new JPanel();
		this.jbImageBrowse = new JButton(IconManager.getIcon("browse.gif"));
		this.add(this.jbImageBrowse, gbc);
		jpImage.add(this.jbImageBrowse);
		this.jbGoogleImage = new JButton(IconManager.getIcon("google.png"));
		jpImage.add(this.jbGoogleImage);
		
		this.add(jpImage, gbc);
		
		gbc.gridy++;
		gbc.gridheight = GridBagConstraints.REMAINDER;
		gbc.anchor = GridBagConstraints.EAST;
		/* JCheckBox for the original release */
		this.jcbOriginalEd = new JCheckBox(LocaleManager.getInstance().getString("original_ed"));
		this.jcbOriginalEd.setSelected(false);
		this.add(this.jcbOriginalEd, gbc);		
		
		gbc.anchor = GridBagConstraints.NORTHWEST;
		/* Setting up the labels to use */
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.gridx += 4;
		gbc.gridy = 1;
		gbc.gridheight = 1;
		gbc.gridwidth = 1;
		gbc.weightx = 1.0;
		gbc.weighty = 0.0;
		
		String[] labels = {LocaleManager.getInstance().getString("title") + " : ",
				LocaleManager.getInstance().getString("serie") + " : ", 
				LocaleManager.getInstance().getString("author") + " : ",
				LocaleManager.getInstance().getString("numero") + " : ", 
				LocaleManager.getInstance().getString("width") + " : ", 
				LocaleManager.getInstance().getString("book") + " : ",
				" ",
				LocaleManager.getInstance().getString("collection") + " : ", 
				LocaleManager.getInstance().getString("isbn") + " : ", 
				LocaleManager.getInstance().getString("comment") + " : " 
		};
		gbc.insets = iLabel;
		for(int i=0; i< labels.length; i++)
		{
			JLabel jlTmp = new JLabel(labels[i]);
			jlTmp.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
			this.add(jlTmp, gbc);
			gbc.gridy ++;
		}
		gbc.insets = iComp;
		
		gbc.gridx ++;
		gbc.gridy = 1;
		gbc.weightx = 1.0;
		gbc.gridwidth = 8;
		
		/* TextField pour le titre */
		this.rtfTitle = new RegexpTextField(30,RegexpTextField.NONEMPTY, true);
		this.add(rtfTitle, gbc);
		
		gbc.gridy ++;
		/* JButtonPane pour le choix de la série */
		this.jbpSerie = new JButtonPane(JEntryPane.SERIE,"-->");
		this.add(jbpSerie, gbc);
		
		gbc.gridy ++;
		/* JButtonPane pour l'auteur 
		this.jbpAuthor = new JButtonPane("-->");
		this.add(jbpAuthor, gbc);*/

		gbc.gridwidth = 3;
		/* JAuthorPane pour l'auteur */
		this.japAuthor = new JAuthorPane(this);
		this.add(japAuthor, gbc);
		
		gbc.gridwidth = 1;
		gbc.gridy ++;
		/* TextField pour le numero */
		this.rtfNumber = new RegexpTextField(10,RegexpTextField.NUMBER);
		this.add(rtfNumber, gbc);
		
		gbc.gridx ++;
		gbc.insets = iLabel;
		/* Label pour "Nombre Page" */
		this.jlPages = new JLabel(LocaleManager.getInstance().getString("page_count") + " : ");
		this.jlPages.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
		this.add(jlPages, gbc);
		
		gbc.insets = iComp;
		gbc.gridx ++;
		/* TextField pour le nb de pages */
		this.rtfPages = new RegexpTextField(10,RegexpTextField.NUMBER);
		this.add(rtfPages, gbc);
		
		gbc.gridx-=2;
		
		gbc.gridy ++;
		/* TextField pour la largeur */
		this.rtfWidth = new RegexpTextField(10,RegexpTextField.DIMENSION);
		this.add(rtfWidth, gbc);	
		
		gbc.gridx ++;
		gbc.insets = iLabel;
		/* Label pour "Longueur" */
		this.jlLongueur = new JLabel(LocaleManager.getInstance().getString("height") + " : ");
		this.jlLongueur.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
		this.add(jlLongueur, gbc);
		
		gbc.insets = iComp;
		gbc.gridx ++;
		/* TextField pour la longueur */
		this.rtfHeight = new RegexpTextField(10,RegexpTextField.DIMENSION);
		this.add(rtfHeight, gbc);
		
		gbc.gridx -= 2;
		gbc.gridy ++;
		
		/* Livre acheté ou emprunté ?*/
		this.buyOrNot = new JComboBox();
		this.buyOrNot.setPreferredSize(new Dimension(100, 20));
		this.buyOrNot.addItem(itm[0]);
		this.buyOrNot.addItem(itm[1]);
		this.add(buyOrNot, gbc);
		
		//gbc.gridx -= 2;
		gbc.gridx++;
		
		/* Date */
		titreAchat = new JLabel(LocaleManager.getInstance().getString("purchaseDate") + " :");
		titreAchat.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
		this.add(titreAchat, gbc);
		gbc.gridx++;
		gbc.gridwidth = 1;
		this.jdpPurchaseDate = new JDatePicker();
		this.add(this.jdpPurchaseDate, gbc);
		
		gbc.gridy++;
		gbc.gridx-=4;
		gbc.insets = iLabel;
		gbc.gridx++;
		JLabel jlReleaseDate = new JLabel(LocaleManager.getInstance().getString("releaseDate") + " : ");
		jlReleaseDate.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
		this.add(jlReleaseDate,gbc);
		
		gbc.insets = iComp;
		gbc.gridx++;
		this.jdpReleaseDate = new JDatePicker();
		this.add(jdpReleaseDate,gbc);
		
		/* Library */
		//*
		gbc.gridx++;
		libTitle = new JLabel(LocaleManager.getInstance().getString("library") + " :");
		libTitle.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
		libTitle.setVisible(false);
		this.add(libTitle, gbc);
		gbc.gridx++;
		libTextField = new JLibraryPane(this);
		libTextField.setVisible(false);
		this.add(libTextField, gbc);
		jbpBibliotheque = new JButtonPane(JEntryPane.COLLECTION,"-->");
		jbpBibliotheque.setVisible(false);
		this.add(jbpBibliotheque, gbc);
		//*/
		
		gbc.gridwidth = GridBagConstraints.REMAINDER;
		gbc.gridy++;
		gbc.gridx -= 2;
		/* JButtonPane pour la collection */
		this.jbpCollection = new JButtonPane(JEntryPane.COLLECTION,"-->");
		this.add(jbpCollection, gbc);
		
		gbc.gridy ++;
		/* JTextField pour l'ISBN */
		this.jtfISBN = new JTextField();
		this.add(jtfISBN, gbc);
		
		gbc.gridy ++;
		gbc.fill = GridBagConstraints.BOTH;
		gbc.gridheight = 2;
		gbc.weighty = 1.0;
		/* JTextArea pour les commentaires */
		this.jtaComment = new JTextArea();
		this.add(jtaComment, gbc);
		
		/* Buttons at the bottom of the window */
		jbOk = new JButton(LocaleManager.getInstance().getString("ok"));
		jbOk.setMnemonic(ShortcutManager.OK);
		/* Create or update the Album when validating */
		jbCancel = new JButton(LocaleManager.getInstance().getString("cancel"));
		jbCancel.setMnemonic(ShortcutManager.CANCEL);
		
		gbc.gridheight = 1;
		gbc.gridwidth = 1;
		gbc.weighty = 0.0;
		gbc.gridy+=2;
		gbc.gridx++;
		this.add(jbOk,gbc);
		gbc.gridx++;
		this.add(jbCancel,gbc);
	}

	/**
	 * Set all the listeners of the AlbumAddDialog
	 */
	private void initListener()
	{
		jbOk.addActionListener(new SaveActionListener());
		jbCancel.addActionListener(new CancelActionListener());
		// When the user click on the button to give details for the serie
		this.jbpSerie.addButtonListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent ae)
			{
				Serie s = PersistencyManager.findSerieByID(AlbumAddDialog.this.jbpSerie.getID());
				if(s == null)
				{
					BottomBarPane.getInstance().addSerie();
					s = new Serie();
					s.setName(jbpSerie.getText());
				}
				
				SerieAddDialog sad = new SerieAddDialog(s);
				sad.setLocationRelativeTo(null);
				sad.setModal(true);
				sad.setVisible(true);
				if(sad.getID() != null)
				{
					AlbumAddDialog.this.jbpSerie.refresh();
					AlbumAddDialog.this.jbpSerie.setID(sad.getID());
				}
				AlbumAddDialog.this.toFront();
			}
		});
		
		this.buyOrNot.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				Item item = (Item) buyOrNot.getSelectedItem();
				//Bought book
				if (item.getId() == 1) {		
					libTitle.setVisible(false);
					libTextField.setVisible(false);
					jbpBibliotheque.setVisible(false);
					titreAchat.setText(LocaleManager.getInstance().getString("purchaseDate") + " :");
				}
				//Borrowed book
				else {
					libTitle.setVisible(true);
					libTextField.setVisible(true);
					jbpBibliotheque.setVisible(true);
					titreAchat.setText(LocaleManager.getInstance().getString("borrowDate") + " :");
				}
			}
		});
		
		this.jbpSerie.addActionListener(new ActionListener() 
		{
			@Override
			public void actionPerformed(ActionEvent e) 
			{
				JButtonPane jbp = AlbumAddDialog.this.jbpSerie;
				Serie s;
				Set<Album> albums;
				if(jbp.getID() != null)
				{
					s = PersistencyManager.findSerieByID(jbp.getID());
					albums = s.getAlbums();
					for(Album a : albums)
					{
						Set<Work> works = a.getWorks();
						if(!works.isEmpty())
						{
							AlbumAddDialog.this.japAuthor.setAuthorLines(works);
							break;
						}
					}
				}
			}
		});
		
		this.jbpBibliotheque.addButtonListener(new ActionListener() 
		{
			@Override
			public void actionPerformed(ActionEvent e) 
			{
				BibliothequeAddDialog cad = new BibliothequeAddDialog();
			}
		});
		
		this.jbpCollection.addButtonListener(new ActionListener() 
		{
			@Override
			public void actionPerformed(ActionEvent e) 
			{
				Collection c = PersistencyManager.findCollectionByID(AlbumAddDialog.this.jbpCollection.getID());
				if(c == null)
				{
					c = new Collection();
					c.setName(jbpCollection.getText());
				}
				
				CollectionAddDialog cad = new CollectionAddDialog(c);
				cad.setModal(true);
				cad.setLocationRelativeTo(null);
				cad.setVisible(true);
				if(cad.getID() != null)
				{
					AlbumAddDialog.this.jbpCollection.refresh();
					AlbumAddDialog.this.jbpCollection.setID(cad.getID());
				}
				AlbumAddDialog.this.toFront();
			}
		});
		
		/* Browse option to select an image */
		this.jbImageBrowse.addActionListener(new ActionListener() {
			
			private ImageChooser ic = null;
			@Override
			public void actionPerformed(ActionEvent arg0) {
				if(ic == null)
				{
					this.ic = new ImageChooser("~", "");
				}
				int retVal = ic.showOpenDialog(AlbumAddDialog.this);
				if(retVal == JFileChooser.APPROVE_OPTION)
				{
					AlbumAddDialog.this.setAlbumCover(ic.getSelectedFile().getAbsolutePath());
				}
			}
		});
		
		this.jbGoogleImage.addActionListener(new ActionListener()
		{	
			@Override
			public void actionPerformed(ActionEvent e) 
			{
				if(!AlbumAddDialog.this.rtfTitle.getText().isEmpty())
				{
					new CoverWebChooser(AlbumAddDialog.this, AlbumAddDialog.this.rtfTitle.getText() + " " 
							+ AlbumAddDialog.this.jbpSerie.getText());	
				}
			}
		});
	}
	
	/**
	 * Use this method to show the AlbumAddDialog 
	 */
	public void display()
	{
		this.pack();
		this.setLocationRelativeTo(this.getParent());
		this.setDefaultCloseOperation(HIDE_ON_CLOSE);
		this.setVisible(true);
	}
	
	/**
	 * Check if the fields for the width and the height are either both empty or both completed
	 * @return true if they are correctly used 
	 */
	public boolean checkSize()
	{
		boolean res = false;
		if(this.rtfHeight.getText().isEmpty() && this.rtfWidth.getText().isEmpty())
			res = true;
		else if (!this.rtfHeight.getText().isEmpty() && !this.rtfWidth.getText().isEmpty())
			res = true;
		return res;
	}
	
	/**
	 * Get the album completed by the user
	 * @return The album
	 */
	public Album getAlbum()
	{
		Album res = new Album();
		
		res.setTitle(this.rtfTitle.getText());
		return res;
	}
	
	/**
	 * If the user is inserting a new Album 
	 */
	public void saveAlbum()
	{
		Album a = new Album();
		
		//Title
		if(!rtfTitle.getText().isEmpty())
		{
			a.setTitle(rtfTitle.getText());
		}
		
		//Serie
		if(!jbpSerie.getText().isEmpty())
		{
			Serie s;
			// If it's a new serie, we create it
			if(jbpSerie.getID() == null)
			{
				s = new Serie();
				s.setName(jbpSerie.getText());
			}
			// In the other case, we get the Serie, and add the album to it
			else
			{
				s = PersistencyManager.findSerieByID(jbpSerie.getID());
			}
			s.addAlbum(a);
			a.setSerie(s);
			SaveItemPersistency.saveSerie(s);
		}
		
		// Collection
		if(!jbpCollection.getText().isEmpty())
		{
			Collection c;
			if(jbpCollection.getID() == null)
			{
				c = new Collection();
				c.setName(jbpCollection.getText());
			}
			else
			{
				c = PersistencyManager.findCollectionByID(jbpCollection.getID());
			}
			c.addAlbum(a);
			a.setCollection(c);
			SaveItemPersistency.saveCollection(c);
		}
		
		// Tome in the serie
		if(!rtfNumber.getText().isEmpty())
		{
			if(a.getTome() != null)
			{
				a.getTome().setNumber(Integer.parseInt(rtfNumber.getText()));
			}
			else
			{
				Tome t = new Tome();
				t.setNumber(Integer.parseInt(rtfNumber.getText()));
				a.setTome(t);
			}
		}
		
		// Number of pages
		if(!rtfPages.getText().isEmpty())
		{
			a.setPageCount(Integer.parseInt(rtfNumber.getText()));
		}
		
		/* Dimension, we have pre-checked if both or neither are filled
		 * so we check only one of them to see if there are empry or not
		 */
		if(!rtfHeight.getText().isEmpty())
		{
			a.setDimension(Float.parseFloat(rtfWidth.getText()) + "x" +
							Float.parseFloat(rtfHeight.getText()));
		}
		
		// PurchaseDate
		if(jdpPurchaseDate.getDate() != null)
		{
			a.setPurchaseDate(jdpPurchaseDate.getDate());
		}
		
		// ReleaseDate
		if(jdpReleaseDate.getDate() != null)
		{
			a.setRegistration(jdpReleaseDate.getDate());
		}
		
		// ISBN
		if(!jtfISBN.getText().isEmpty())
		{
			a.setIsbn(jtfISBN.getText());
		}
		
		// Comment
		if(!jtaComment.getText().isEmpty())
		{
			a.setComment(jtaComment.getText());
		}
		
		// Cover
		if(ivImage.getImageURL() != null)
		{
			String[] path = ivImage.getImageURL().split("/");
			a.setCover(path[path.length-1]);
		}
		
		// Original Edition 
		if(this.jcbOriginalEd.isSelected())
		{
			a.setOriginal(true);
		}
		else
		{
			a.setOriginal(false);
		}

		//Book buy or borrowed ?
		Item item = (Item) this.buyOrNot.getSelectedItem();
		if (item.getId() == 1){
			a.setBuy(true);
		}
		//If the book was borrowed
		else {
			a.setBuy(false);
		}
		
		BottomBarPane.getInstance().addAlbum();
		SaveItemPersistency.saveAlbum(a);
		japAuthor.saveAuthors(a);
		//SaveItemPersistency.saveAlbum(a);
	}
	
	/**
	 * If the user wish to edit an Album
	 */
	public void updateAlbum()
	{		
		Album a = PersistencyManager.findAlbumByID(this.currentAlbum.getId());
		boolean hasChanged = false;
		
		String title = rtfTitle.getText().trim();
		String serie = jbpSerie.getText().trim();
		String collection = jbpCollection.getText().trim();
		
		/*
		 * If the title has been modified, we update the album
		 */
		if((a.getTitle() == null && !title.isEmpty())||
				!a.getTitle().equals(title))
		{
			a.setTitle(title);
			hasChanged = true;
		}
		
		/*
		 * If the ID of the Serie has changed, we update it
		 */
		Serie s;
		// First case, the album doesn't have a serie
		if(a.getSerie() == null)
		{
			// If there is nothing new, we do nothing
			if(serie.isEmpty());
			// If there is a new Serie
			else if(jbpSerie.getID() == null)
			{
				s = new Serie();
				s.setName(serie);
				s.addAlbum(a);
				a.setSerie(s);
				SaveItemPersistency.saveSerie(s);
				hasChanged = true;
			}
			// there is another existing serie
			else
			{
				// Add it to the other one
				s = PersistencyManager.findSerieByID(jbpSerie.getID());
				s.addAlbum(a);
				a.setSerie(s);
				SaveItemPersistency.saveSerie(s);
				hasChanged = true;
			}
		}
		// Second case, the album has one serie
		else
		{
			// if there is no serie in the field, we delete it from the album
			if(serie.isEmpty())
			{
				a.getSerie().removeAlbum(a);
				SaveItemPersistency.saveSerie(a.getSerie());
				a.setSerie(null);
			}
			// if there is a new Serie
			else if(jbpSerie.getID() == null)
			{
				// Remove the album of the previous serie
				a.getSerie().removeAlbum(a);
				SaveItemPersistency.saveSerie(a.getSerie());
				// Save the new serie
				s = new Serie();
				s.setName(serie);
				s.addAlbum(a);
				a.setSerie(s);
				SaveItemPersistency.saveSerie(s);
				hasChanged = true;
			}
			// Another existing serie, different from the previous one
			else if(!a.getSerie().getId().equals(jbpSerie.getID()))
			{
				// Remove the album of the previous serie
				a.getSerie().removeAlbum(a);
				SaveItemPersistency.saveSerie(a.getSerie());
				// Add it to the other one
				s = PersistencyManager.findSerieByID(jbpSerie.getID());
				s.addAlbum(a);
				a.setSerie(s);
				SaveItemPersistency.saveSerie(s);
			}
		}
		
		/*
		 * If the ID of the Collection has changed, we update it
		 */
		Collection c;
		// First case, the album doesn't have a collection
		if(a.getCollection() == null)
		{
			// If there is nothing new, we do nothing
			if(collection.isEmpty());
			// If there is a new Collection
			else if(jbpCollection.getID() == null)
			{
				c = new Collection();
				c.setName(collection);
				a.setCollection(c);
				c.addAlbum(a);
				SaveItemPersistency.saveCollection(c);
			}
			// there is another existing collection
			else
			{
				// Add it to the other one
				c = PersistencyManager.findCollectionByID(jbpCollection.getID());
				a.setCollection(c);
				c.addAlbum(a);
				SaveItemPersistency.saveCollection(c);
			}
		}
		// Second case, the album has one collection
		else
		{
			// if there is no collection in the field, we delete it from the album
			if(collection.isEmpty())
			{
				a.getCollection().removeAlbum(a);
				SaveItemPersistency.saveCollection(a.getCollection());
				a.setCollection(null);
			}
			// if there is a new Collection
			else if(jbpCollection.getID() == null)
			{
				// Remove the album of the previous collection
				a.getCollection().removeAlbum(a);
				SaveItemPersistency.saveCollection(a.getCollection());
				// Save the new collection
				c = new Collection();
				c.setName(collection);
				a.setCollection(c);
				c.addAlbum(a);
				SaveItemPersistency.saveCollection(c);
			}
			// Another existing collection, different from the previous one
			else if(!a.getCollection().getId().equals(jbpCollection.getID()))
			{
				System.out.println(a.getCollection().getId());
				System.out.println(jbpCollection.getID());
				// Remove the album of the previous collection
				a.getSerie().removeAlbum(a);
				SaveItemPersistency.saveCollection(a.getCollection());
				// Add it to the other one
				c = PersistencyManager.findCollectionByID(jbpCollection.getID());
				a.setCollection(c);
				c.addAlbum(a);
				SaveItemPersistency.saveCollection(c);
			}
		}
		
		/*
		 * If the number of pages has changed, we update the album
		 */
		if(!rtfPages.getText().isEmpty() && 
				(a.getPageCount() != Integer.parseInt(rtfPages.getText())))
		{
			a.setPageCount(Integer.parseInt(rtfPages.getText()));
			hasChanged = true;
		}
		
		
		if(a.getTome() != null && rtfNumber.getText().isEmpty())
		{
			a.setTome(null);
			hasChanged = true;
		}
		else if(!rtfNumber.getText().isEmpty() && a.getTome() != null && 
				(a.getTome().getNumber() != Integer.parseInt(rtfNumber.getText())))
		{
			a.getTome().setNumber(Integer.parseInt(rtfNumber.getText()));
			hasChanged = true;
		}
		else if (!rtfNumber.getText().isEmpty() && a.getTome() == null)
		{
			Tome t = new Tome();
			t.setNumber(Integer.parseInt(rtfNumber.getText()));
			a.setTome(t);
			hasChanged = true;
		}
		
		/* We have already checked if both or neither are filled */
		String width = rtfWidth.getText().trim();
		String height = rtfHeight.getText().trim();
		if(!width.isEmpty())
		{
			if(a.getDimension() == null)
			{
				a.setDimension(width + "x" + height);
				hasChanged = true;
			}
			else if(!a.getDimension().equals(width + "x" + height))
			{
				a.setDimension(width + "x" + height);
				hasChanged = true;
			}
		}
		else
		{
			if(a.getDimension() != null)
			{
				a.setDimension(null);
				hasChanged = true;
			}
		}

		if(a.getPurchaseDate() == null && this.jdpPurchaseDate.getDate() == null);
		else if(a.getPurchaseDate() == null && this.jdpPurchaseDate.getDate() != null ||
			!a.getPurchaseDate().equals(this.jdpPurchaseDate.getDate()))
		{
			a.setPurchaseDate(jdpPurchaseDate.getDate());
			hasChanged = true;
		}
		
		if(a.getRegistration() == null && this.jdpReleaseDate.getDate() == null);
		else if(a.getRegistration() == null && this.jdpReleaseDate.getDate() != null ||
			!a.getRegistration().equals(this.jdpReleaseDate.getDate()))
		{
			a.setRegistration(jdpReleaseDate.getDate());
			hasChanged = true;
		}
		
		if(a.getIsbn() == null && this.jtfISBN.getText().isEmpty());
		else if((a.getIsbn() == null && !this.jtfISBN.getText().isEmpty()) ||
		  (!a.getIsbn().equals(this.jtfISBN.getText())))
		{
			a.setIsbn(this.jtfISBN.getText());
			hasChanged = true;
		}
		
		if(a.getComment() == null && this.jtaComment.getText().isEmpty());
		else if((a.getComment() == null && !this.jtaComment.getText().isEmpty()) ||
				!a.getComment().equals(this.jtaComment.getText()))
		{
			a.setComment(this.jtaComment.getText());
			hasChanged = true;
		}
		
		/* Return null if there is no image, so it's what we want */
		if(a.getCover() == null)
		{
			if(this.ivImage.getImageURL() != null)
			{
				String[] path = this.ivImage.getImageURL().split("/");
				a.setCover(path[path.length-1]);
				hasChanged = true;
			}
		}
		else
		{
			if(this.ivImage.getImageURL() == null)
			{
				a.setCover(null);
				hasChanged = true;
			}
			else if(!a.getCover().equals(this.ivImage.getImageURL()))
			{
				String[] path = this.ivImage.getImageURL().split("/");
				a.setCover(path[path.length-1]);
				hasChanged = true;
			}
		}
		
		if(!a.isOriginal() && this.jcbOriginalEd.isSelected())
		{
			a.setOriginal(true);
			hasChanged = true;
		}
		else if(a.isOriginal() && !this.jcbOriginalEd.isSelected())
		{
			a.setOriginal(false);
			hasChanged = true;
		}
		
		if(hasChanged)
		{
			SaveItemPersistency.saveAlbum(a);
		}
		// The hasChanged is done inside the function, it is more easier to develop
		japAuthor.saveAuthors(a);
	}	
	
	/**
	 * Set the album cover from a file
	 * @param path The path to the file
	 */
	public void setAlbumCover(String path)
	{
		File toCopy = new File(path);
		File dest = new File(PropertyManager.getInstance().getPathProperty("path_cover") + PropertyManager.sep
				+ HibernateUtil.getCurrentDatabase() + PropertyManager.sep + toCopy.getName());
		
		if(dest.exists())
		{
			dest = new File(PropertyManager.getInstance().getPathProperty("path_cover") + PropertyManager.sep +
					HibernateUtil.getCurrentDatabase() + PropertyManager.sep + "1-" + toCopy.getName());
		}
		try
		{
			FileManager.copyFile(toCopy, dest);
		}
		catch (DefaultException e)
		{
			e.printStackTrace();
		}
		AlbumAddDialog.this.ivImage.setImage(ImageHelper.getImageIcon(
				PropertyManager.getInstance().getPathProperty("path_cover") + PropertyManager.sep + 
				HibernateUtil.getCurrentDatabase() + PropertyManager.sep + dest.getName()));
	}

// Classes
	class CancelActionListener implements ActionListener
	{
		@Override
		public void actionPerformed(ActionEvent ae)
		{
			AlbumAddDialog.this.dispose();
		}
	}

	class SaveActionListener implements ActionListener
	{
		@Override 
		public void actionPerformed(ActionEvent ae)
		{
			if(!AlbumAddDialog.this.rtfTitle.check())
			{
				MessagePaneManager.showInfoPane(LocaleManager.getInstance().getString("no_title"));
				return;
			}
			
			/* Check if the fields width and height are correctly filled
			 * Popup to the user if it's not the case
			 */
			boolean tmp = !AlbumAddDialog.this.rtfHeight.check();
			tmp |= !AlbumAddDialog.this.rtfWidth.check();
			tmp |= !AlbumAddDialog.this.rtfNumber.check();
			tmp |= !AlbumAddDialog.this.rtfPages.check();
			if(tmp)
			{
				MessagePaneManager.showInfoPane(LocaleManager.getInstance().getString("number_error"));
				return;
			}
			
			/* Check if the width and height are empty or filled
			 * Popup to the user to notice him 
			 * and highlight in red the field(s) concerned
			 */
			if(!AlbumAddDialog.this.checkSize())
			{
				AlbumAddDialog.this.rtfHeight.setIncorrect();
				AlbumAddDialog.this.rtfWidth.setIncorrect();
				MessagePaneManager.showInfoPane(LocaleManager.getInstance().getString("missing_height_or_width"));
				return;
			}
			/* Check if the purchase date is lower than the buying date */
			Date release = AlbumAddDialog.this.jdpReleaseDate.getDate();
			Date purchase = AlbumAddDialog.this.jdpPurchaseDate.getDate();

			if(purchase != null && release != null && release.compareTo(purchase) > 0)
			{
				MessagePaneManager.showInfoPane(LocaleManager.getInstance().getString("purchase_lt_release"));
				return;
			}
			if(AlbumAddDialog.this.currentAlbum == null)
			{
				AlbumAddDialog.this.saveAlbum();
			}
			else
			{
				AlbumAddDialog.this.updateAlbum();
			}
			AlbumAddDialog.this.dispose();
			AlbumPane.getInstance().refresh();
		}
	}
}
