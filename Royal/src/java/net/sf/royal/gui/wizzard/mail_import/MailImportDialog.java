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
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeSelectionModel;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.PropertyManager;
import net.sf.royal.mail.Emailisbn;
import net.sf.royal.mail.Mail;
import net.sf.royal.mail.Misbn;
import net.sf.royal.util.Base64Utils;
import net.sf.royal.util.Md5;
/**
 * This is the dialog used to import the specific emails sended from the Android App.
 * @author jean
 */
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
		private JPanel jp;
		private JLabel jlInfo;
		private String serror;
		private DefaultMutableTreeNode Jroot;
		private JTree jtree;
		private Misbn mailaccount;
		private ArrayList<Emailisbn> eisbn;
		
	// Constructors
		/**
		 * Create a new MailImportDialog, and tell it to be at the center of the parent.<br/>
		 * The dialog will be created and paint in the SwingUtilities Thread and then be showed.
		 * @param parent The parent component of the MailImportDialog
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
		 * Create a MailImportDialog. <br/>
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
		 * @see MailImportDialog#display
		 */
		private void init()
		{
			this.jbOk = new JButton(LocaleManager.getInstance().getString("ok"));
			this.jbCancel = new JButton(LocaleManager.getInstance().getString("cancel"));
			this.setLayout(new BorderLayout());
			this.jpFind = new JPanel();
			this.jpFind.setMinimumSize(new Dimension(400,300));
			this.jpFind.setLayout(new BorderLayout());
			this.jpFind.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
			this.jp = new JPanel();
			this.jp.setLayout(new GridBagLayout());
			GridBagConstraints gbc = new GridBagConstraints();
			gbc.gridx = 0;
			gbc.gridy = 0;
			this.jp.add(this.jbOk, gbc);
			gbc.gridx += 1;
			this.jp.add(this.jbCancel, gbc);				
			this.jlInfo = new JLabel("Cliquer sur Ok pour procéder à l'inspection de vos mails :"); //TODO LocaleManager
			this.jpFind.add(jlInfo,BorderLayout.NORTH);
			this.jpFind.add(this.jp, BorderLayout.CENTER);
			this.add(jpFind, BorderLayout.CENTER);
			gbc = new GridBagConstraints();
			gbc.gridy = 0;
			this.jpList = new JPanel();
			this.jpList.setLayout(new GridBagLayout());
			this.jpList.setMinimumSize(new Dimension(400,300));
			this.Jroot = new DefaultMutableTreeNode(LocaleManager.getInstance().getString("get_mail"));
			this.jtree = new JTree(Jroot);
			ToolTipManager.sharedInstance().registerComponent(this.jtree);
			this.jtree.setCellRenderer(new MailTreeRenderer());
			JScrollPane treeView = new JScrollPane(this.jtree);
			treeView.setPreferredSize(new Dimension(400,100));
			this.jtree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
			this.jpList.add(treeView, gbc);
			gbc.gridy += 1;

		
		}

		/**
		 * Set all the listeners of the MailImportDialog
		 */
		private void initListener()
		{
			jbOk.addActionListener(new ImportActionListener());
			jbCancel.addActionListener(new CancelActionListener());

		}
		
		/**
		 * Use this method to show the MailImportDialog 
		 */
		public void display()
		{
			this.pack();
			this.setLocationRelativeTo(this.getParent());
			this.setDefaultCloseOperation(HIDE_ON_CLOSE);
		}
		
		/**
		 * This method test your connection to the mail service. 
		 * It fills the error string to know what's wrong.
		 * @return boolean true if the connection succeeded 
		 */		
		private boolean tryConnection(){
			boolean verif = true;
			// Verification if the user has configured his email address
			if(PropertyManager.getInstance().getProperty("mail_protocol").equals("IMAP") || PropertyManager.getInstance().getProperty("mail_protocol").equals("POP3")){
				this.mailaccount = new Misbn((PropertyManager.getInstance().getProperty("mail_protocol").equals("IMAP")) ? Mail.IMAP : Mail.POP3);
				String username = PropertyManager.getInstance().getProperty("mail_login");
				String host = PropertyManager.getInstance().getProperty("mail_server");
				String password = null;
				// No problem with the password
				try {
					password = Base64Utils.decode(PropertyManager.getInstance().getProperty("mail_password"));
				} catch (Exception e) {
					serror="Erreur de conversion du mot de passe !";
					verif = false;
				}
				if(verif){
					this.mailaccount.setUserPass(username, password, host);
					// Connecting the email service
					try {
						this.mailaccount.connect();
					} catch (Exception e) {
						serror="Erreur de connexion !";
						verif = false;
					}
					if(verif){
						// Opening the default folder INBOX
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
		/**
		 * Suppress a Node in the Tree by giving the corresponding Emailisbn. 
		 * if there are no more branch, dispose the Dialog.
		 * @param ei the Emailisbn to delete from the Tree
		 */
		public void deleteNode(Emailisbn ei){
			boolean stop = false;
			for(int i = 0; i <this.Jroot.getChildCount() && !stop;i++){
				if(ei == (Emailisbn)((DefaultMutableTreeNode)this.Jroot.getChildAt(i)).getUserObject()){
					this.Jroot.remove(i);
					((DefaultTreeModel)this.jtree.getModel()).reload();
					this.jtree.repaint();
					stop = true;
					if(this.Jroot.getChildCount() == 0){
						this.dispose();
					}
				}
			}
		}
		
		
	// Classes
		/**
		 * Intern class for the CancelButton.
		 * Dispose the current JDialog.
		 * @author jean
		 */
		class CancelActionListener implements ActionListener
		{
			@Override
			public void actionPerformed(ActionEvent ae)
			{
				MailImportDialog.this.dispose();
			}
		}
		/**
		 * Import the emails containing the ISBNs
		 * @see MailImportDialog#tryConnection()
		 * @author jean
		 */
		class ImportActionListener implements ActionListener
		{
			@Override
			public void actionPerformed(ActionEvent ae)
			{
					MailImportDialog.this.eisbn = null;
					try {
						MailImportDialog.this.eisbn = MailImportDialog.this.mailaccount.getIsbnBySubject(Md5.encode(PropertyManager.getInstance().getProperty("mail_login")),100);
					} catch (MessagingException e) {
						e.printStackTrace();
					} catch (IOException e) {
						e.printStackTrace();
					}
					GridBagConstraints gbc = new GridBagConstraints();
					// Adding the email to the Tree
					for(Emailisbn i : MailImportDialog.this.eisbn){
						DefaultMutableTreeNode top = new DefaultMutableTreeNode(i);
						MailImportDialog.this.Jroot.add(top);
					}
					// To show the Tree opened
					for(int i=0; i< MailImportDialog.this.jtree.getRowCount();i++){
						MailImportDialog.this.jtree.expandRow(i);
					}
					// Show the Tree Panel
					if(MailImportDialog.this.Jroot.getChildCount() > 0){
						MailImportDialog.this.jtree.setRootVisible(false);
						MailImportDialog.this.jtree.addMouseListener(new SaveAlbumListener(MailImportDialog.this));
						MailImportDialog.this.remove(MailImportDialog.this.jpFind);
						MailImportDialog.this.jpList.repaint();
						MailImportDialog.this.add(MailImportDialog.this.jpList, BorderLayout.NORTH);
						MailImportDialog.this.repaint();
						MailImportDialog.this.display();	
					}
					// If no emails 
					else{
						MailImportDialog.this.jlInfo.setText("Pas de mails à importer !");
						MailImportDialog.this.jp.remove(MailImportDialog.this.jbOk);
						MailImportDialog.this.jp.repaint();
					}
			}
		}
		
}
