package net.sf.royal.gui.wizzard.mail_import;

import java.awt.BorderLayout;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.util.ArrayList;

import javax.mail.MessagingException;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeSelectionModel;

import com.google.api.services.books.model.VolumeVolumeInfo;

import net.sf.royal.datamodel.Album;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.PropertyManager;
import net.sf.royal.gui.pane.AlbumPane;
import net.sf.royal.gui.pane.BottomBarPane;
import net.sf.royal.gui.wizard.add_dialog.CollectionAddDialog;
import net.sf.royal.mail.Emailisbn;
import net.sf.royal.mail.EmailisbnLine;
import net.sf.royal.mail.Mail;
import net.sf.royal.mail.Misbn;
import net.sf.royal.persistency.SaveItemPersistency;
import net.sf.royal.util.Base64Utils;
import net.sf.royal.util.ISBN;
import net.sf.royal.web.ComicNotFoundException;
import net.sf.royal.web.ConnectionProblemException;
import net.sf.royal.web.GoogleBook;

public class MailImportDialog extends JDialog {
		//Fields	
		
		/**
		 * Button to validate the collection
		 */
		private JButton jbOk;
		/**
		 * Button to cancel all modifications
		 */
		private JButton jbCancel;
		private JPanel jpFind;
		private JPanel jpList;
		private JLabel jlInfo;
		private String serror;
		private DefaultMutableTreeNode Jroot;
		private Misbn mailaccount;
		private JButton jbImport;
		private ArrayList<Emailisbn> eisbn;
		
	// Constructors
		/**
		 * Create a new CollectionAddDialog, and tell it to be at the center of the parent.<br/>
		 * The dialog will be created and paint in the SwingUtilities Thread and then be showed.
		 * @param parent The parent component of the CollectionAddDialog
		 * @param isForeground true if the dialog has to stay on the foreground of its parent
		 */
		public MailImportDialog(Window parent, boolean isForeground)
		{
			super(parent, LocaleManager.getInstance().getString("mail_import"),
					isForeground? Dialog.ModalityType.TOOLKIT_MODAL : Dialog.ModalityType.MODELESS);
			
			SwingUtilities.invokeLater(new Runnable()
			{
				@Override
				public void run()
				{
					if(MailImportDialog.this.tryConnection()){
						MailImportDialog.this.init();
						MailImportDialog.this.initListener();
						MailImportDialog.this.display();
						MailImportDialog.this.setVisible(true);
					}
					else{
						JOptionPane.showMessageDialog(MailImportDialog.this, MailImportDialog.this.serror);
					}
				}
			});
		}
		
		/**
		 * Create a CollectionAddDialog. <br/>
		 * The created dialog will be placed at the center of the screen and 
		 * behave like other window
		 */
		public MailImportDialog()
		{
			this(null, true);
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
			this.jbImport = new JButton(LocaleManager.getInstance().getString("import"));
			this.setLayout(new BorderLayout());
			this.jpFind = new JPanel();
			this.jpFind.setMinimumSize(new Dimension(400,300));
			this.jpFind.setLayout(new BorderLayout());
			this.jpFind.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
			JPanel jp = new JPanel();
			jp.setLayout(new GridBagLayout());
			GridBagConstraints gbc = new GridBagConstraints();
			gbc.gridx = 0;
			gbc.gridy = 0;
			jp.add(this.jbOk, gbc);
			gbc.gridx += 1;
			jp.add(this.jbCancel, gbc);				
			this.jlInfo = new JLabel("Cliquer sur Ok pour procéder à l'inspection de vos mails :"); //TODO LocaleManager
			this.jpFind.add(jlInfo,BorderLayout.NORTH);
			this.jpFind.add(jp, BorderLayout.CENTER);
			this.add(jpFind, BorderLayout.CENTER);
			gbc = new GridBagConstraints();
			gbc.gridy = 0;
			this.jpList = new JPanel();
			this.jpList.setLayout(new GridBagLayout());
			this.jpList.setMinimumSize(new Dimension(400,300));
			this.Jroot = new DefaultMutableTreeNode(LocaleManager.getInstance().getString("get_mail"));
			JTree jtree = new JTree(Jroot);
			ToolTipManager.sharedInstance().registerComponent(jtree);
			jtree.setCellRenderer(new MailTreeRenderer());
			JScrollPane treeView = new JScrollPane(jtree);
			treeView.setPreferredSize(new Dimension(400,100));
			jtree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
			this.jpList.add(treeView, gbc);
			gbc.gridy += 1;
			this.jpList.add(this.jbImport, gbc);
		
		}

		/**
		 * Set all the listeners of the SerieAddDialog
		 */
		private void initListener()
		{
			jbOk.addActionListener(new ImportActionListener());
			jbCancel.addActionListener(new CancelActionListener());
			jbImport.addActionListener(new SaveAlbumsListener());
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
		
		
		
		private boolean tryConnection(){
			boolean verif = true;
			if(PropertyManager.getInstance().getProperty("mail_protocol").equals("IMAP") || PropertyManager.getInstance().getProperty("mail_protocol").equals("POP3")){
				this.mailaccount = new Misbn((PropertyManager.getInstance().getProperty("mail_protocol").equals("IMAP")) ? Mail.IMAP : Mail.POP3);
				String username = PropertyManager.getInstance().getProperty("mail_login");
				String host = PropertyManager.getInstance().getProperty("mail_server");
				String password = null;
				try {
					password = Base64Utils.decode(PropertyManager.getInstance().getProperty("mail_password"));
				} catch (Exception e) {
					// TODO Auto-generated catch block
					serror="Erreur de conversion du mot de passe !";
					verif = false;
				}
				if(verif){
					this.mailaccount.setUserPass(username, password, host);
					try {
						this.mailaccount.connect();
					} catch (Exception e) {
						serror="Erreur de connexion !";
						verif = false;
					}
					if(verif){
						try {
							this.mailaccount.openFolder("INBOX");
						} catch (Exception e) {
							serror="Erreur d'ouverture de votre boite mail !";
							verif = false;
						}
						return verif;
					}
				}
				
			}
			return false;		
		}
		
		
		
	// Classes
		class CancelActionListener implements ActionListener
		{
			@Override
			public void actionPerformed(ActionEvent ae)
			{
				MailImportDialog.this.dispose();
			}
		}

		class ImportActionListener implements ActionListener
		{
			@Override
			public void actionPerformed(ActionEvent ae)
			{
					MailImportDialog.this.eisbn = null;
					try {
						MailImportDialog.this.eisbn = MailImportDialog.this.mailaccount.getIsbnBySubject("ISBN-",100);
					} catch (MessagingException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					GridBagConstraints gbc = new GridBagConstraints();
					for(Emailisbn i : MailImportDialog.this.eisbn){
						DefaultMutableTreeNode top = new DefaultMutableTreeNode(i);
						MailImportDialog.this.Jroot.add(top);
					}
					JTree jtree = new JTree(MailImportDialog.this.Jroot);
					ToolTipManager.sharedInstance().registerComponent(jtree);
					jtree.setCellRenderer(new MailTreeRenderer());
					jtree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
					MailImportDialog.this.remove(MailImportDialog.this.jpFind);
					MailImportDialog.this.jpList.repaint();
					MailImportDialog.this.add(MailImportDialog.this.jpList, BorderLayout.NORTH);
					MailImportDialog.this.repaint();
					MailImportDialog.this.display();				
			}
		}
		
		class SaveAlbumsListener implements ActionListener{

			@Override
			public void actionPerformed(ActionEvent ae) {
				for(Emailisbn ei : MailImportDialog.this.eisbn){
					for(EmailisbnLine eil : ei.getEmailisbnLine()){
						ISBN is = eil.getIsbn();
						GoogleBook gb = new GoogleBook(is);
						try {
							gb.execute();
							VolumeVolumeInfo vvi = gb.getVolumeInfo();
							Album a = new Album();
							a.setTitle(vvi.getTitle());
							a.setDimension(vvi.getDimensions().getHeight()+" x "+vvi.getDimensions().getWidth()+" x "+vvi.getDimensions().getThickness());
							a.setIsbn(is.toString(true));
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
				MailImportDialog.this.dispose();				
			}
			
		}
		

}
