package net.sf.royal.gui.wizzard.mail_import;
import java.awt.Component;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;

import net.sf.royal.mail.Emailisbn;
import net.sf.royal.mail.EmailisbnLine;
/**
 * Used to let the tree have a Tooltip Text corresponding the isbns that are in the email.
 * @author jean
 */
public class MailTreeRenderer extends DefaultTreeCellRenderer{

	
	@Override
	public Component getTreeCellRendererComponent(JTree tree,Object value,boolean sel,boolean expanded,boolean leaf,int row,boolean hasFocus) {
		super.getTreeCellRendererComponent( tree, value, sel, expanded, leaf, row, hasFocus);
		if (leaf && (((DefaultMutableTreeNode)value).getUserObject() instanceof Emailisbn)) {
			Emailisbn ei = (Emailisbn) ((DefaultMutableTreeNode)value).getUserObject();
			String tooltiptext = "<html>";
			for(EmailisbnLine eil : ei.getEmailisbnLine()){
				tooltiptext = tooltiptext+eil.toString()+"<br />";
			}
			tooltiptext = tooltiptext +"</html>";
			this.setToolTipText(tooltiptext);
		} else {
			setToolTipText(null); //no tool tip
		} 

		return this;
	}

}
