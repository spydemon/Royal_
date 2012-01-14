package net.sf.royal.gui.wizzard.mail_import;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;
import net.sf.royal.mail.Emailisbn;
/**
 * This event is used when the user double-clicked on one branch of the mail-tree
 * @author jean
 */
public class SaveAlbumListener extends MouseAdapter{
	private MailImportDialog mid;
	private Emailisbn cur_mail;
	public SaveAlbumListener(MailImportDialog m){
		this.mid = m;
	}
	@Override
	public void mouseClicked(MouseEvent arg0) {
		// if double-clicked
		if (arg0.getClickCount() == 2){
			JTree jt = (JTree) arg0.getSource();
			TreePath tp= jt.getPathForLocation(arg0.getX(), arg0.getY());
			// if it's a leaf that is an eimal
			if(tp != null && tp.getPathCount() == 2){
				DefaultMutableTreeNode dmtn = (DefaultMutableTreeNode) tp.getPathComponent(1);
				this.cur_mail = (Emailisbn) dmtn.getUserObject();
				new MailAddDialog(this.mid,this.cur_mail);
				
			}
			
			
		}
		
	}
}
