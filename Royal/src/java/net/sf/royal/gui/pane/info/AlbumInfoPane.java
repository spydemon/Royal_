package net.sf.royal.gui.pane.info;

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.util.Date;

import javax.swing.Icon;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JTextField;

import net.sf.royal.datamodel.Album;
import net.sf.royal.datamodel.Collection;
import net.sf.royal.datamodel.Serie;
import net.sf.royal.datamodel.Tome;
import net.sf.royal.gui.manager.IconManager;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.pane.InfoPane;
import net.sf.royal.gui.util.AuthorDisplayerPane;
import net.sf.royal.persistency.PersistencyManager;

/**
  * @author Soulou
  * @author Steveo
  * Right pane when selecting an album. Shows information about an album
  */
@SuppressWarnings("serial")
public class AlbumInfoPane extends AbstractInfoPane
{
	private Album album;
	private JTextField jtfTitle;
	private JCheckBox jcbxOriginalEd;
	private JTextField jtfISBN;
	private JTextField jtfSerie;
	private JTextField jtfTomeNumber;
	private JTextField jtfPages;
	private JTextField jtfCollection;
	private JTextField jtfDimension;
	private JTextField jtfPurchase;
	private JTextField jtfRelease;
	//private JTextArea jtaDesc;
	//private JLabel jlStatus;
	//private BorrowDialog bd;
	private AuthorDisplayerPane adpAuthors;

	
	public static final String AUTHORS = LocaleManager.getInstance().getString("authors") + " : ";
	public static final String ret = LocaleManager.getInstance().getString("return");
	public static final String lend = LocaleManager.getInstance().getString("lend");
	public static final String lent_by = LocaleManager.getInstance().getString("lent_by") + " : ";
	public static final String sAvailable = LocaleManager.getInstance().getString("available");
	
	public static final Icon available = IconManager.getIcon("icon_check.png");
	public static final Icon not_available = IconManager.getIcon("red_cross.png");
	
	/**
	 * Create a new AlbumInfoPane with a specified album.
	 * @param a The album which information have to be displayed
	 */
	public AlbumInfoPane(Album a)
	{
		this.album = a;
		this.init();
		this.update(this.album);
	}

	/**
	 * Initialize the layout and the components of the AlbumInfoPane
	 */
	private void init()
	{
		String tmp;
		Color cText = new Color(200, 200, 205);
		this.gbc.anchor = GridBagConstraints.WEST;
		this.gbc.insets.set(5,5,5,5);
		this.jtfTitle = new JTextField();
		
		/***** The title of the Album *****/
		this.gbc.gridx = 0;
		this.gbc.gridy = 0;	
		this.changeGridProperties(LABEL);
		this.add(new JLabel(InfoPane.title), this.gbc);
		
		this.gbc.gridx++;
		this.changeGridProperties(COMPONENT);
		this.jtfTitle.setEditable(false);
		this.jtfTitle.setBackground(cText);
		this.add(this.jtfTitle,this.gbc);
		
		/**** ISBN ****/
		this.gbc.gridx = 0;
		this.gbc.gridy ++;
		this.changeGridProperties(LABEL);
		this.add(new JLabel("ISBN : "), this.gbc);
		
		this.gbc.gridx ++;
		this.changeGridProperties(COMPONENT);
		this.jtfISBN = new JTextField();
		this.jtfISBN.setEditable(false);
		this.jtfISBN.setBackground(cText);
		this.add(this.jtfISBN, this.gbc);
		
		/**** Dimension ****/
		this.gbc.gridy ++;
		this.gbc.gridx = 0;
		this.changeGridProperties(LABEL);
		tmp = LocaleManager.getInstance().getString("dimension");
		this.add(new JLabel(tmp),this.gbc);

		this.gbc.gridx ++;
		this.changeGridProperties(COMPONENT);
		this.gbc.gridwidth = 1;
		this.jtfDimension = new JTextField();
		this.jtfDimension.setEditable(false);
		this.jtfDimension.setBackground(cText);
		this.add(this.jtfDimension, this.gbc);
		
		/**** Properties : Original Edition ****/
		this.gbc.gridx ++;
		this.changeGridProperties(LABEL);
		tmp = LocaleManager.getInstance().getString("original_ed") + " : ";
		this.add(new JLabel(tmp), this.gbc);
		
		this.gbc.gridx ++;
		this.changeGridProperties(COMPONENT);
		this.jcbxOriginalEd = new JCheckBox();
		this.jcbxOriginalEd.addActionListener(new DoNotChangeListener(this.jcbxOriginalEd));
		this.add(this.jcbxOriginalEd, this.gbc);
		
		/**** Serie ****/
		this.gbc.gridx = 0;
		this.gbc.gridy ++;
		this.changeGridProperties(LABEL);
		this.add(new JLabel(InfoPane.serie), this.gbc);
		
		this.gbc.gridx++;
		this.changeGridProperties(COMPONENT);
		this.jtfSerie = new JTextField();
		this.jtfSerie.setEditable(false);
		this.jtfSerie.setBackground(cText);
		this.add(this.jtfSerie, this.gbc);
		
		/**** Number of tome ****/
		this.gbc.gridx = 0;
		this.gbc.gridy ++;
		this.changeGridProperties(LABEL);
		this.add(new JLabel(LocaleManager.getInstance().getString("tome_number") + " : "), 
				this.gbc);
		
		this.gbc.gridx ++;
		this.changeGridProperties(COMPONENT);
		this.gbc.gridwidth = 1;
		this.jtfTomeNumber = new JTextField();
		this.jtfTomeNumber.setEditable(false);
		this.jtfTomeNumber.setBackground(cText);
		this.add(this.jtfTomeNumber, this.gbc);
		
		/**** Number of pages ****/
		this.gbc.gridx ++;
		this.changeGridProperties(LABEL);
		tmp = LocaleManager.getInstance().getString("page_count");
		this.add(new JLabel(tmp), this.gbc);
		
		this.gbc.gridx ++;
		this.changeGridProperties(COMPONENT);
		//this.gbc.gridwidth = 1;
		this.jtfPages = new JTextField();
		this.jtfPages.setEditable(false);
		this.jtfPages.setBackground(cText);
		this.add(this.jtfPages, this.gbc);
		
		/**** Collection ****/
		this.gbc.gridx = 0;
		this.gbc.gridy ++;
		this.changeGridProperties(LABEL);
		tmp = LocaleManager.getInstance().getString("collection") + " : ";
		this.add(new JLabel(tmp), this.gbc);
		
		this.gbc.gridx++;
		this.changeGridProperties(COMPONENT);
		this.jtfCollection = new JTextField();
		this.jtfCollection.setEditable(false);
		this.jtfCollection.setBackground(cText);
		this.add(this.jtfCollection, this.gbc);
		
		/**** Purchase date ****/
		this.gbc.gridx = 0;
		this.gbc.gridy ++;
		this.changeGridProperties(LABEL);
		this.add(new JLabel(LocaleManager.getInstance().getString("purchaseDate") + " : "), this.gbc);
		
		this.gbc.gridx++;
		this.changeGridProperties(COMPONENT);
		this.gbc.gridwidth = 1;
		this.jtfPurchase = new JTextField();
		this.jtfPurchase.setEditable(false);
		this.jtfPurchase.setBackground(cText);
		this.add(this.jtfPurchase, this.gbc);
		
		/**** Release date ****/
		this.gbc.gridx ++;
		this.changeGridProperties(LABEL);
		tmp = LocaleManager.getInstance().getString("releaseDate") + " : ";
		this.add(new JLabel(tmp), this.gbc);
		
		this.gbc.gridx ++;
		this.changeGridProperties(COMPONENT);
		this.jtfRelease = new JTextField();
		this.jtfRelease.setEditable(false);
		this.jtfRelease.setBackground(cText);
		this.add(this.jtfRelease, this.gbc);
		
		/**** Authors ****/
		this.gbc.gridy ++;
		this.gbc.gridx = 0;
		this.changeGridProperties(LABEL);
		this.gbc.insets.set(8, 5, 5, 5);
		this.gbc.anchor = GridBagConstraints.NORTHWEST;
		this.add(new JLabel(AUTHORS), this.gbc);
		
		this.gbc.gridx ++;
		this.changeGridProperties(COMPONENT);
		this.gbc.insets.set(5, 5, 5, 5);
		this.gbc.fill = GridBagConstraints.HORIZONTAL;
		this.gbc.anchor = GridBagConstraints.WEST;
		this.adpAuthors = new AuthorDisplayerPane();
		this.add(this.adpAuthors, this.gbc);
		
		// TODO
		/*this.gbc.gridx = 0;
		this.gbc.gridy++;
		this.gbc.weighty = 1.0;
		this.gbc.gridheight = 2;
		this.changeGridProperties(LABEL);
		this.add(new JLabel(InfoPane.desc), this.gbc);
		
		this.gbc.gridx++;
		this.changeGridProperties(COMPONENT);
		this.jtaDesc = new JTextArea();
		this.add(this.jtaDesc, this.gbc);
		
		this.gbc.gridx = 0;
		this.gbc.gridy++;
		this.changeGridProperties(LABEL);
		this.add(new JLabel(LocaleManager.getInstance().getString("state") + " : "), this.gbc);
		
		this.gbc.gridx++;
		this.changeGridProperties(COMPONENT);
		this.jlStatus = new JLabel();
		this.add(this.jlStatus, this.gbc);
		this.gbc.gridy++;
		this.gbc.weighty = 0.0;
		this.gbc.fill = GridBagConstraints.NONE;
		this.jbLent = new JButton("preter");
		/*jb.addActionListener(new ActionListener() 
		{			
			@Override
			public void actionPerformed(ActionEvent e) {
				AlbumInfoPane.this.bd = BorrowDialog.getInstance(null);
				AlbumInfoPane.this.bd.display();
				AlbumInfoPane.this.bd.addActionListener(new ActionListener() 
				{	
					@Override
					public void actionPerformed(ActionEvent e) {
						long id = bd.getID();
						MessagePaneManager.showInfoPane("" + id);
					}
				});
			}
		});
		this.jbLent.setEnabled(false);
		this.add(this.jbLent, this.gbc);*/
		
		
		/***** The coverpane *****/
		this.gbc.gridy++;
		this.gbc.gridx = 0;
		this.changeGridProperties(COMPONENT);
		this.gbc.weighty = 1.0;
		this.gbc.gridheight = GridBagConstraints.REMAINDER;
		this.gbc.anchor = GridBagConstraints.NORTHWEST;
		this.add(this.cpAlbums,this.gbc);
	}
	
	/**
	 * Update the content of the AlbumInfoPane. It now displays the information
	 * of another Album
	 * @param a The album which information have to be displayed
	 */
	public void update(Album a) {
		this.album = a;
		
		/**** Title ****/
		this.jtfTitle.setText(this.album.getTitle());
		
		/**** Properties Original edition ****/
		if(this.album.isOriginal())
		{
			this.jcbxOriginalEd.setSelected(true);
		}
		else
		{
			this.jcbxOriginalEd.setSelected(false);
		}
		
		/**** ISBN ****/
		this.jtfISBN.setText(this.album.getIsbn());
		
		/**** Serie ****/
		Serie s = PersistencyManager.findAlbumByID(a.getId()).getSerie();
		this.jtfSerie.setText(s == null ? "" : s.getName());
		
		/**** Number of tome ****/
		Tome t = a.getTome();
		this.jtfTomeNumber.setText(t == null? "" : "" + t.getNumber());
		
		/**** collection ****/
		Collection c = this.album.getCollection();
		this.jtfCollection.setText(c == null? "" : c.getName());
		
		/**** Number of pages ****/
		this.jtfPages.setText("" + this.album.getPageCount());
		
		/**** Dimension ****/
		this.jtfDimension.setText(this.album.getDimension());
		
		/**** Purchase date ****/
		Date d = this.album.getPurchaseDate();
		this.jtfPurchase.setText(d==null ? "" : LocaleManager.getInstance().getTextDate(d, false));
		
		/**** Release date ****/
		d = this.album.getRegistration();
		this.jtfRelease.setText(d==null ? "" : LocaleManager.getInstance().getTextDate(d, false));

		/**** Authors ****/
		this.adpAuthors.update(this.album);
		
		/**** Borrow System ****/
		/* TODO :
		 * - display if the album is available or lent
		 */
		/*List<Loan> loan = PersistencyManager.findLoansByAlbum(this.album.getId());
		if(loan.size() != 0)
		{
			Borrower b = loan.get(0).getBorrower();
			this.jlStatus.setIcon(IconManager.getIcon("red_cross.png"));
			this.jlStatus.setText(lent_by + Tools.borrowerToString(b));
			
			this.jbLent.setText(ret);
		}
		else
		{
			this.jlStatus.setIcon(available);
			this.jlStatus.setText(sAvailable);
			
			this.jbLent.setText(lend);
		}*/
		
		/**** CoverPane ****/
		super.cpAlbums.update(this.album);
		super.resizeCoverPane();
		this.revalidate();
		this.repaint();
	}
}
