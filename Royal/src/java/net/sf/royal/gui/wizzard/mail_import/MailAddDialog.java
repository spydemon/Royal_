package net.sf.royal.gui.wizzard.mail_import;

import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Date;
import java.util.List;
import java.util.Set;

import javax.mail.MessagingException;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import net.sf.royal.datamodel.Album;
import net.sf.royal.datamodel.Author;
import net.sf.royal.datamodel.Bibliotheque;
import net.sf.royal.datamodel.Serie;
import net.sf.royal.datamodel.Work;
import net.sf.royal.gui.datepicker.JDatePicker;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.pane.AlbumPane;
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

public class MailAddDialog extends JDialog{
	private MailImportDialog mid;
	private Emailisbn cur_mail;
	private JButtonPane bib;
	private JDatePicker jdpPurchaseDate;
	private JButton jbOk;
	private JButton jbCancel;
	
	
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
	 * @see CollectionAddDialog#display
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
	 * Set all the listeners of the SerieAddDialog
	 */
	private void initListener()
	{
		this.bib.addButtonListener(	new ActionListener() 
		{
			@Override
			public void actionPerformed(ActionEvent e) 
			{
				BibliothequeAddDialog cad = new BibliothequeAddDialog();
			}
		});
		jbOk.addActionListener(new ActionListener(){

			@Override
			public void actionPerformed(ActionEvent arg0) {
				Date date = null;
				if(MailAddDialog.this.jdpPurchaseDate.getDate() != null){
					date = MailAddDialog.this.jdpPurchaseDate.getDate();
				}
				if(!MailAddDialog.this.bib.getText().isEmpty()){
					Bibliotheque biblio;
					if(MailAddDialog.this.bib.getID() != null){
						biblio = PersistencyManager.FindBibliothequeByID(MailAddDialog.this.bib.getID());
					}
					else{
						biblio = new Bibliotheque();
						biblio.setName(MailAddDialog.this.bib.getText());
					}
					SaveItemPersistency.saveBibliotheque(biblio);
					if(MailAddDialog.this.jdpPurchaseDate.getDate() != null)
					MailAddDialog.SaveEmail(MailAddDialog.this.cur_mail, date, biblio);
				}
				else{
					MailAddDialog.SaveEmail(MailAddDialog.this.cur_mail, date, null);
				}
				
				
				MailAddDialog.this.mid.deleteNode(MailAddDialog.this.cur_mail);
				MailAddDialog.this.dispose();
				
			}});
		jbCancel.addActionListener(new CancelActionListener());

	}
	
	/**
	 * Use this method to show the SerieAddDialog 
	 */
	public void display()
	{
		this.pack();
		this.setLocationRelativeTo(this.getParent());
		this.setDefaultCloseOperation(HIDE_ON_CLOSE);
	}
	
	
	public static void SaveEmail(Emailisbn ei, Date date, Bibliotheque biblio){
		for(EmailisbnLine eil : ei.getEmailisbnLine()){
			ISBN is = eil.getIsbn();
			GoogleBook gb = new GoogleBook(is);
			try {
				gb.execute();
				VolumeVolumeInfo vvi = gb.getVolumeInfo();
				Album a = new Album();
				a.setTitle(vvi.getTitle());						
				a.setIsbn(is.toString(true));
				SaveItemPersistency.saveAlbum(a);
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
	class CancelActionListener implements ActionListener
	{
		@Override
		public void actionPerformed(ActionEvent ae)
		{
			MailAddDialog.this.dispose();
		}
	}

}
