package net.sf.royal.gui.wizzard.mail_import;

import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import net.sf.royal.datamodel.Album;
import net.sf.royal.datamodel.Collection;
import net.sf.royal.datamodel.Editor;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.MessagePaneManager;
import net.sf.royal.gui.manager.ShortcutManager;
import net.sf.royal.gui.pane.AlbumPane;
import net.sf.royal.gui.pane.BottomBarPane;
import net.sf.royal.gui.util.RegexpTextField;
import net.sf.royal.gui.wizard.add_dialog.CollectionAddDialog;
import net.sf.royal.gui.wizard.add_dialog.JEntryPane;
import net.sf.royal.persistency.PersistencyManager;
import net.sf.royal.persistency.SaveItemPersistency;

public class MailImportDialog extends JDialog {
		//Fields	
		/**
		 * The JTextField for the name of the serie
		 */
		private JTextField jtfCollectionName;
		/**
		 * The JEntryPane for the name of the editor
		 */
		private JEntryPane jepEditor;
		/**
		 * Website of the collection
		 */
		private RegexpTextField rtfWeb;
		/**
		 * The JTextAera for a description of the collection
		 */
		private JTextArea jtaDescription;
		
		private Collection currentCollection;
		private Editor editorCollection;
		
		/**
		 * Button to validate the collection
		 */
		private JButton jbOk;
		/**
		 * Button to cancel all modifications
		 */
		private JButton jbCancel;
		
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
				public void run()
				{
					MailImportDialog.this.init();
					MailImportDialog.this.initListener();
					MailImportDialog.this.display();
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
			this.setMinimumSize(new Dimension(400,300));
			this.setLayout(new GridBagLayout());
			
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
			BottomBarPane.getInstance().addAlbum();
			SaveItemPersistency.saveAlbum(a);
			AlbumPane.getInstance().refresh();
		}
		
		public Long getID()
		{
			return this.currentCollection.getId();
		}
		
		
	// Classes
		class CancelActionListener implements ActionListener
		{
			@Override
			public void actionPerformed(ActionEvent ae)
			{
				CollectionAddDialog.this.dispose();
			}
		}

		class SaveActionListener implements ActionListener
		{
			@Override
			public void actionPerformed(ActionEvent ae)
			{
				if((CollectionAddDialog.this.jtfCollectionName.getText()).isEmpty())
				{
					MessagePaneManager.showInfoPane(LocaleManager.getInstance().getString("collection_noname"));
				}
				else if(!CollectionAddDialog.this.rtfWeb.getText().isEmpty() && 
						CollectionAddDialog.this.rtfWeb.check())
				{
					CollectionAddDialog.this.rtfWeb.setIncorrect();
					MessagePaneManager.showInfoPane(LocaleManager.getInstance().getString("web_invalid"));
				}
				else
				{
					CollectionAddDialog.this.saveCollection();
					CollectionAddDialog.this.dispose();
				}
			}
		}	

}
