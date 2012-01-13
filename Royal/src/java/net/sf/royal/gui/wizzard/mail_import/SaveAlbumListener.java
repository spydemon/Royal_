package net.sf.royal.gui.wizzard.mail_import;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;
import net.sf.royal.mail.Emailisbn;

public class SaveAlbumListener implements MouseListener{
	private MailImportDialog mid;
	private Emailisbn cur_mail;
	public SaveAlbumListener(MailImportDialog m){
		this.mid = m;
	}
	@Override
	public void mouseClicked(MouseEvent arg0) {
		// TODO Auto-generated method stub
		if (arg0.getClickCount() == 2){
			JTree jt = (JTree) arg0.getSource();
			TreePath tp= jt.getPathForLocation(arg0.getX(), arg0.getY());
			if(tp != null && tp.getPathCount() == 2){
				DefaultMutableTreeNode dmtn = (DefaultMutableTreeNode) tp.getPathComponent(1);
				this.cur_mail = (Emailisbn) dmtn.getUserObject();
				new MailAddDialog(this.mid,this.cur_mail);
				
			}
			
			
		}
		
	}
	@Override
	public void mouseEntered(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}
	@Override
	public void mouseExited(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}
	@Override
	public void mousePressed(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}
	@Override
	public void mouseReleased(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}

}
