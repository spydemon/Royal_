package net.sf.royal.gui.tree;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import net.sf.royal.gui.manager.IconManager;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.ShortcutManager;

/**
 * @author Soulou
 * Popup menu which appears when the user right click 
 * on something in the Tree view.
 */


public abstract class AbstractMenu extends JPopupMenu 
{
	protected JMenuItem delete;
	protected JMenuItem edit;
	
	public AbstractMenu(Object o)
	{		
		edit = new JMenuItem(LocaleManager.getInstance().getString("edit"), IconManager.getIcon("edit_enabled_click.png"));
		edit.setMnemonic(ShortcutManager.EDIT);
		delete = new JMenuItem(LocaleManager.getInstance().getString("delete"), IconManager.getIcon("delete_enabled.gif"));
		delete.setMnemonic(ShortcutManager.DELETE);
		delete.addActionListener(new DeleteActionListener(o));
		edit.addActionListener(new EditActionListener(o));
		
		this.add(edit);
		this.add(delete);
		this.addSeparator();
	}
}
