package net.sf.royal.gui.wizzard.mail_import;

import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Date;
import java.util.List;
import java.util.Set;

import javax.imageio.ImageIO;
import javax.mail.MessagingException;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import net.sf.royal.datamodel.Album;
import net.sf.royal.datamodel.Author;
import net.sf.royal.datamodel.Bibliotheque;
import net.sf.royal.datamodel.HibernateUtil;
import net.sf.royal.datamodel.Serie;
import net.sf.royal.datamodel.Work;
import net.sf.royal.exception.DefaultException;
import net.sf.royal.gui.datepicker.JDatePicker;
import net.sf.royal.gui.manager.FileManager;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.PropertyManager;
import net.sf.royal.gui.pane.AlbumPane;
import net.sf.royal.gui.util.ImageHelper;
import net.sf.royal.gui.util.ImageViewer;
import net.sf.royal.gui.wizard.add_dialog.AlbumAddDialog;
import net.sf.royal.gui.wizard.add_dialog.BibliothequeAddDialog;
import net.sf.royal.gui.wizard.add_dialog.CollectionAddDialog;
import net.sf.royal.gui.wizard.add_dialog.JAuthorPane;
import net.sf.royal.gui.wizard.add_dialog.JButtonPane;
import net.sf.royal.gui.wizard.add_dialog.JEntryPane;
import net.sf.royal.gui.wizzard.mail_import.MailImportDialog.CancelActionListener;
import net.sf.royal.gui.wizzard.mail_import.MailImportDialog.ImportActionListener;
import net.sf.royal.mail.Emailisbn;
import net.sf.royal.mail.EmailisbnLine;
import net.sf.royal.persistency.PersistencyManager;
import net.sf.royal.persistency.SaveItemPersistency;
import net.sf.royal.util.ISBN;
import net.sf.royal.web.ComicNotFoundException;
import net.sf.royal.web.ConnectionProblemException;
import net.sf.royal.web.GoogleBook;

import com.google.api.services.books.model.VolumeVolumeInfo;
import com.google.api.services.books.model.VolumeVolumeInfoDimensions;
/**
 * This is the JDialog used to all the albums contained in an Emailisbn.
 * @author jean
 */
public class MailAddDialog extends JDialog{
	/**
	 * The parent JDialog
	 */
	private MailImportDialog mid;
	/**
	 * the specific email we're using
	 */
	private Emailisbn cur_mail;
	/**
	 * the choose Bibliotheque Component
	 */
	private JButtonPane bib;
	/**
	 * the choose Borrowed Date Component
	 */
	private JDatePicker jdpPurchaseDate;
	private JButton jbOk;
	private JButton jbCancel;
	private ImageViewer ivImage;
	
	/**
	 * This is the Constructor of the MailAddDialog.
	 * @param mid is used to let us delete the Tree node from the parent window after importing all the emails
	 * @param cur_mail is the specific emails we will import
	 */
	public MailAddDialog(MailImportDialog mid, Emailisbn cur_mail) {
		super(null, LocaleManager.getInstance().getString("mail_import"),
				true? Dialog.ModalityType.TOOLKIT_MODAL : Dialog.ModalityType.MODELESS);
		this.mid = mid;
		this.cur_mail = cur_mail;
		SwingUtilities.invokeLater(new Runnable()
		{
			@Override
			public void run()
			{
				if(true){
						MailAddDialog.this.init();
						MailAddDialog.this.initListener();
						MailAddDialog.this.display();
						MailAddDialog.this.setVisible(true);
				}
				else{
					JOptionPane.showMessageDialog(MailAddDialog.this, "");
				}
			}
		});
	}

	// Methods
	/**
	 * Initialize the Dialog Components.
	 * You need to use the method display to make the dialog visible
	 * @see MailAddDialog#display
	 */
	private void init()
	{
		this.jbOk = new JButton(LocaleManager.getInstance().getString("ok"));
		this.jbCancel = new JButton(LocaleManager.getInstance().getString("cancel"));
		this.setTitle(this.cur_mail.toString());
		this.setLayout(new GridBagLayout());
		GridBagConstraints gc = new GridBagConstraints();
		this.bib = new JButtonPane(JEntryPane.BIBLIO, "-->");
		this.bib.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		JLabel bibtitle = new JLabel("Bibliothèque :");
		bibtitle.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		this.jbCancel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		this.jbOk.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		JLabel datetitle = new JLabel("Date :");
		datetitle.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		this.jdpPurchaseDate = new JDatePicker();
		this.jdpPurchaseDate.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		gc.gridx = 0;
		gc.gridy = 0;
		this.add(bibtitle, gc);
		gc.gridx+=1;
		this.add(this.bib,gc);
		gc.gridy +=1;
		gc.gridx = 0;
		this.add(datetitle,gc);
		gc.gridx+=1;
		gc.fill = GridBagConstraints.HORIZONTAL;
		this.add(this.jdpPurchaseDate,gc);
		gc.gridy+=1;
		gc.gridx=0;
		gc.anchor = GridBagConstraints.EAST;
		this.add(this.jbOk,gc);
		gc.gridx +=1;
		gc.anchor = GridBagConstraints.WEST;
		this.add(this.jbCancel,gc);
	
	}

	/**
	 * Set all the listeners of the MailAddDialog
	 */
	private void initListener()
	{
		// let you create a Bibliotheque with all its parameters
		this.bib.addButtonListener(	new ActionListener() 
		{
			@Override
			public void actionPerformed(ActionEvent e) 
			{
				BibliothequeAddDialog cad = new BibliothequeAddDialog();
			}
		});
		// Save the email
		jbOk.addActionListener(new ActionListener(){
			/**
			 * @see MailAddDialog#SaveEmail
			 */
			@Override
			public void actionPerformed(ActionEvent arg0) {
				Date date = null;
				// does the user specified a Date
				if(MailAddDialog.this.jdpPurchaseDate.getDate() != null){
					date = MailAddDialog.this.jdpPurchaseDate.getDate();
				}
				// does he specified a bibliotheque
				if(!MailAddDialog.this.bib.getText().isEmpty()){
					Bibliotheque biblio;
					if(MailAddDialog.this.bib.getID() != null){
						biblio = PersistencyManager.FindBibliothequeByID(MailAddDialog.this.bib.getID());
					}
					// Create it persistency if it's a new one
					else{
						biblio = new Bibliotheque();
						biblio.setName(MailAddDialog.this.bib.getText());
					}
					SaveItemPersistency.saveBibliotheque(biblio);
					// The saving method
					MailAddDialog.SaveEmail(MailAddDialog.this.cur_mail, date, biblio);
				}
				else{
					//the saving method with no library
					MailAddDialog.SaveEmail(MailAddDialog.this.cur_mail, date, null);
				}
				
				// deleting the node
				MailAddDialog.this.mid.deleteNode(MailAddDialog.this.cur_mail);
				MailAddDialog.this.dispose();
				
			}});
		jbCancel.addActionListener(new CancelActionListener());

	}
	
	/**
	 * Use this method to show the MailAddDialog 
	 */
	public void display()
	{
		this.pack();
		this.setLocationRelativeTo(this.getParent());
		this.setDefaultCloseOperation(HIDE_ON_CLOSE);
	}
	
	/**
	 * This method will used each line of an email to search the corresponding book on the GoogleBook API.
	 * It converts the result into a new Album.
	 * @param ei the email
	 * @param date the date that will be set to each new Album
	 * @param biblio the Bibliotheque that will be set to each new Album
	 */
	public static void SaveEmail(Emailisbn ei, Date date, Bibliotheque biblio){
		for(EmailisbnLine eil : ei.getEmailisbnLine()){
			ISBN is = eil.getIsbn();			
			GoogleBook gb = new GoogleBook(is);
			try {
				// find the corresponding Gbooks.
				gb.execute();
				VolumeVolumeInfo vvi = gb.getVolumeInfo();
				Album a = new Album();
				a.setTitle(vvi.getTitle());						
				a.setIsbn(is.toString(true));
				SaveItemPersistency.saveAlbum(a);
				addCover(a, gb.getCoverImage());
				if(biblio != null){
					a.setBibliotheque(biblio);
					a.setBuy(false);
				}
				else{
					a.setBuy(true);
				}
				if(date !=null){
					a.setPurchaseDate(date);
				}
				// specific method for the authors to find whether it already exists or not
				addAuthors(a, vvi.getAuthors());
				addDimensions(a,vvi.getDimensions());
				SaveItemPersistency.saveAlbum(a);
				AlbumPane.getInstance().refresh();						
			} catch (ConnectionProblemException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (ComicNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		try {
			// Deleting the email
			ei.deleteMessage();
		} catch (MessagingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public static Author findAuthor(String aut){
		if(aut == null || aut.equals("null")){
			return null;
		}
		List<Author> laut = PersistencyManager.findAuthors();
		for(Author a : laut){
			if(a.getName() != null && a.getName().equals(aut)){
				return a;
			}
		}
		Author a = new Author();
		a.setName(aut);
		SaveItemPersistency.saveAuthor(a);
		return a;
	}
	
	public static void addAuthors(Album a, List<String> lat){
		if(lat != null){
			for(String aut : lat){
				Author au = findAuthor(aut);
				if(au != null){
					Work wo = new Work();
					wo.setAlbum(a);
					wo.setAuthor(au);
					SaveItemPersistency.saveWork(wo);
					a.getWorks().add(wo);
				}
			}
		}
	}
	
	public static void addDimensions(Album a, VolumeVolumeInfoDimensions vvid){
		if(vvid != null){
			String h = (vvid.getHeight() == null || vvid.getHeight().equals("null")) ? "?": vvid.getHeight();
			String w = (vvid.getWidth() == null || vvid.getWidth().equals("null")) ? "?": vvid.getWidth();
			String t = (vvid.getThickness() == null || vvid.getThickness().equals("null")) ? "?": vvid.getThickness();
			a.setDimension(h+" x "+w+" x "+t);
		}
	}
	public static void addCover(Album a, BufferedImage bi){
		String name = a.getTitle();
		String tmpImagePath = PropertyManager.getInstance().getPathProperty("path_cover_tmp");
		File tmpDir = new File(tmpImagePath);
		if(bi != null){
			File fileImage = new File(tmpDir + PropertyManager.sep + name+".jpg");
			try {
				ImageIO.write(bi, "jpg", fileImage);
			} catch (IOException e) {
				e.printStackTrace();
			}
			ImageIcon ii = new ImageIcon(bi);
			ii.setDescription(tmpImagePath + PropertyManager.sep + name+".jpg");	
			MailAddDialog.setAlbumCover(ii.getDescription());
			a.setCover(name+"."+"jpg");
			SaveItemPersistency.saveAlbum(a);
		}
	}
	public static void setAlbumCover(String path)
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

	}
	class CancelActionListener implements ActionListener
	{
		@Override
		public void actionPerformed(ActionEvent ae)
		{
			MailAddDialog.this.dispose();
		}
	}

}
