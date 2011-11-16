package net.sf.royal.gui.wizzard.mail_import;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.text.DateFormat;
import java.util.ArrayList;

import javax.mail.MessagingException;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;

import net.sf.royal.datamodel.Album;
import net.sf.royal.datamodel.Collection;
import net.sf.royal.datamodel.Editor;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.MessagePaneManager;
import net.sf.royal.gui.manager.PropertyManager;
import net.sf.royal.gui.manager.ShortcutManager;
import net.sf.royal.gui.pane.AlbumPane;
import net.sf.royal.gui.pane.BottomBarPane;
import net.sf.royal.gui.util.RegexpTextField;
import net.sf.royal.gui.wizard.add_dialog.CollectionAddDialog;
import net.sf.royal.gui.wizard.add_dialog.JEntryPane;
import net.sf.royal.mail.Emailisbn;
import net.sf.royal.mail.EmailisbnLine;
import net.sf.royal.mail.Mail;
import net.sf.royal.mail.Misbn;
import net.sf.royal.persistency.PersistencyManager;
import net.sf.royal.persistency.SaveItemPersistency;
import net.sf.royal.util.Base64Utils;
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
					MailImportDialog.this.init();
					MailImportDialog.this.initListener();
					MailImportDialog.this.display();
					MailImportDialog.this.setVisible(true);
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
			//this.setTitle(this.sTitle);
			this.jpFind = new JPanel();
			this.setMinimumSize(new Dimension(400,300));
			this.jpFind.setMinimumSize(new Dimension(400,300));
			this.setLayout(new BorderLayout());
			this.jpFind.setLayout(new GridBagLayout());
			Insets iComp = new Insets(5, 5, 5, 5);
			
			GridBagConstraints gbc = new GridBagConstraints();

			gbc.anchor = GridBagConstraints.WEST;
			gbc.fill = GridBagConstraints.HORIZONTAL;
			gbc.weightx = 0;
			gbc.weighty = 0;
			gbc.gridx = 0;
			gbc.gridy = 1;
			gbc.gridheight = 1;
			gbc.gridwidth = 1;
			this.jbOk = new JButton(LocaleManager.getInstance().getString("ok"));
			this.jbCancel = new JButton(LocaleManager.getInstance().getString("cancel"));
			gbc.gridwidth = 1;
			this.jpFind.add(this.jbOk, gbc);
			gbc.gridx +=1;
			this.jpFind.add(this.jbCancel, gbc);
			this.add(jpFind, BorderLayout.CENTER);
			
		}

		/**
		 * Set all the listeners of the SerieAddDialog
		 */
		private void initListener()
		{
			jbOk.addActionListener(new SaveActionListener());
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
		
		/**
		 * Save current Collection to in the database
		 */
		public void saveAlbums(Album[] as)
		{
			for(Album a : as){
				BottomBarPane.getInstance().addAlbum();
				SaveItemPersistency.saveAlbum(a);
				AlbumPane.getInstance().refresh();
			}
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

		class SaveActionListener implements ActionListener
		{
			@Override
			public void actionPerformed(ActionEvent ae)
			{
				try {
					Misbn m = new Misbn((PropertyManager.getInstance().getProperty("mail_protocol").equals("IMAP")) ? Mail.IMAP : Mail.POP3);
					String username = PropertyManager.getInstance().getProperty("mail_login");
					String password = Base64Utils.decode(PropertyManager.getInstance().getProperty("mail_password"));
					String host = PropertyManager.getInstance().getProperty("mail_server");
					m.setUserPass(username, password, host);
					m.connect();
					m.openFolder("INBOX");					
					ArrayList<Emailisbn> eisbn = m.getIsbnBySubject("ISBN-",100);
					GridBagConstraints gbc = new GridBagConstraints();
					gbc.gridy = 0;
					MailImportDialog.this.jpList = new JPanel();
					MailImportDialog.this.jpList.setLayout(new GridBagLayout());
					MailImportDialog.this.jpList.setMinimumSize(new Dimension(400,300));
					DefaultMutableTreeNode Jroot = new DefaultMutableTreeNode(LocaleManager.getInstance().getString("get_mail"));
					for(Emailisbn i : eisbn){
						DefaultMutableTreeNode top = new DefaultMutableTreeNode(i);
						ArrayList<EmailisbnLine> eisbnline =  i.getEmailisbnLine();
						for(EmailisbnLine il : eisbnline){
							 DefaultMutableTreeNode isbnt = new DefaultMutableTreeNode(il);
							 top.add(isbnt);
						}
						Jroot.add(top);
					}
					JTree jtree = new JTree(Jroot);
					jtree.addTreeSelectionListener(new ShowPrewiewTreeListener());
					JScrollPane treeView = new JScrollPane(jtree);
					treeView.setPreferredSize(new Dimension(400,300));
					MailImportDialog.this.jpList.add(treeView, gbc);
					MailImportDialog.this.remove(jpFind);
					MailImportDialog.this.add(jpList, BorderLayout.CENTER);
					MailImportDialog.this.repaint();
					MailImportDialog.this.display();
				} catch (Exception e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}

			}
			class ShowPrewiewTreeListener implements TreeSelectionListener{

				@Override
				public void valueChanged(TreeSelectionEvent arg0) {
					DefaultMutableTreeNode dmt = (DefaultMutableTreeNode) arg0.getPath().getLastPathComponent();
					if(dmt.getUserObject() instanceof EmailisbnLine){
						EmailisbnLine eil = (EmailisbnLine) dmt.getUserObject();
						GoogleBook gb = new GoogleBook(eil.getIsbn());
						try {
							gb.execute();
							System.out.println(gb.getVolumeInfo().getTitle()+" - "+gb.getVolumeInfo().getDescription());
						} catch (ConnectionProblemException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						} catch (ComicNotFoundException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					}
					else if(dmt.getUserObject() instanceof Emailisbn){

					}
				}
				
			}
		}
		

}
