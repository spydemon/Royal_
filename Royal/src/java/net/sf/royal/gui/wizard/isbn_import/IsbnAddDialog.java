package net.sf.royal.gui.wizard.isbn_import;

import java.awt.BorderLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.PlainDocument;
import javax.swing.text.Position;
import javax.swing.text.Segment;

import com.google.api.services.books.model.VolumeVolumeInfo;

import net.sf.royal.datamodel.Album;
import net.sf.royal.gui.manager.IconManager;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.pane.AlbumPane;
import net.sf.royal.gui.wizard.settings.SettingsTab;
import net.sf.royal.gui.wizzard.mail_import.MailAddDialog;
import net.sf.royal.persistency.SaveItemPersistency;
import net.sf.royal.util.ISBN;
import net.sf.royal.util.InvalidStandardIDException;
import net.sf.royal.web.ComicNotFoundException;
import net.sf.royal.web.ConnectionProblemException;
import net.sf.royal.web.GoogleBook;

public class IsbnAddDialog extends JDialog{

		private JButton jbOk;
		private JTextField jtIsbn;
		private JButton jbCancel;


		public IsbnAddDialog(Window parent)
		{
			super(parent);

			
			this.setTitle(LocaleManager.getInstance().getString("import_isbn"));
			this.setIconImage(IconManager.getIcon("import.gif").getImage());
			jtIsbn = new JTextField(10);
			jtIsbn.setDocument(new IsbnDocument());	
			jtIsbn.getDocument().addDocumentListener(new DocumentListener(){

				@Override
				public void changedUpdate(DocumentEvent arg0) {
					
				}

				@Override
				public void insertUpdate(DocumentEvent arg0) {
					updateOk();
				}

				@Override
				public void removeUpdate(DocumentEvent arg0) {
					updateOk();
				}
				public void updateOk(){
					if(((IsbnDocument)IsbnAddDialog.this.jtIsbn.getDocument()).isIsbn()){
						IsbnAddDialog.this.jbOk.setEnabled(true);
					}
					else{
						IsbnAddDialog.this.jbOk.setEnabled(false);
					}
				}
				
			});
			jbOk = new JButton(LocaleManager.getInstance().getString("ok"));
			jbOk.setEnabled(false);
			jbOk.addActionListener(new ActionListener(){

				@Override
				public void actionPerformed(ActionEvent arg0) {
					try {
						ISBN is = new ISBN(IsbnAddDialog.this.jtIsbn.getDocument().getText(0, IsbnAddDialog.this.jtIsbn.getDocument().getLength()));			
						GoogleBook gb = new GoogleBook(is);
						try {
							// find the corresponding Gbooks.
							gb.execute();
							VolumeVolumeInfo vvi = gb.getVolumeInfo();
							Album a = new Album();
							a.setTitle(vvi.getTitle());						
							a.setIsbn(is.toString(true));
							a.setBuy(true);
							SaveItemPersistency.saveAlbum(a);
							MailAddDialog.addCover(a, gb.getCoverImage());
							// specific method for the authors to find whether it already exists or not
							MailAddDialog.addAuthors(a, vvi.getAuthors());
							MailAddDialog.addDimensions(a,vvi.getDimensions());
							SaveItemPersistency.saveAlbum(a);
							AlbumPane.getInstance().refresh();						
						} catch (ConnectionProblemException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						} catch (ComicNotFoundException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					} catch (BadLocationException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (InvalidStandardIDException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					IsbnAddDialog.this.dispose();
				}
				
			});
			this.setLayout(new BorderLayout());
			this.add(jtIsbn, BorderLayout.WEST);
			this.add(jbOk, BorderLayout.EAST);

			this.pack();
			this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);
			this.setVisible(true);
			// We center the JDialog on the parent JFrame)
			this.setLocationRelativeTo(parent);
		}

		/**
		 * This ActionListener is used to save all the modifications made in the tabs.
		 * For all the tabs in the array, we check if something has changed, and if it's
		 * the case, we fire the listener to save the changes.
		 * @see SettingsTab
		 */
		public class SaveActionListener implements ActionListener
		{

			@Override
			public void actionPerformed(ActionEvent e) {
				// TODO Auto-generated method stub
				
			}

		}

		public class CancelActionListener implements ActionListener
		{

			@Override
			public void actionPerformed(ActionEvent e) {
				// TODO Auto-generated method stub
				
			}

		}
		public class IsbnDocument extends PlainDocument{
			private boolean isIsbn = false;
			@Override
			public void insertString(int offset, String str, AttributeSet a)
					throws BadLocationException {
						if(str==null){
							return;
						}
						 StringBuilder newstr = new StringBuilder();
						for(int i =0; i <str.length();i++){
							char cur =str.charAt(i); 
							if(cur>=48 && cur<=57 ){
								newstr.append(cur);
							}
						}
						super.insertString(offset,newstr.toString(),a);
						this.isIsbnUpdate();
			}
			private void isIsbnUpdate(){
				try {
					String text = this.getText(0,this.getLength());
					if(text != null && !text.isEmpty()){
						try {
							ISBN check = new ISBN(text);
							isIsbn = true;
						} catch (InvalidStandardIDException e) {
							isIsbn = false;
						}
					}
					else{
						isIsbn = false;
					}
				} catch (BadLocationException e) {
					isIsbn = false;
				}
			}
			@Override
			protected void postRemoveUpdate(DefaultDocumentEvent chng){
				super.postRemoveUpdate(chng);
				this.isIsbnUpdate();
			}
			@Override
			protected void insertUpdate(DefaultDocumentEvent chng, AttributeSet attr){
				super.insertUpdate(chng, attr);
				this.isIsbnUpdate();
			}
			public boolean isIsbn(){
				return this.isIsbn;
			}
		}
			

}
