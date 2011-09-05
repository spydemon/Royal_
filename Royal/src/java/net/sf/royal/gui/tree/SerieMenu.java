package net.sf.royal.gui.tree;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenuItem;

import net.sf.royal.datamodel.Serie;
import net.sf.royal.gui.manager.IconManager;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.ShortcutManager;
import net.sf.royal.gui.wizard.add_dialog.AlbumAddDialog;

/**
 * @author Soulou
 * Popup menu which appears when the user right click on a Serie
 * in the Tree view.
 */
public class SerieMenu extends AbstractMenu
{
	private Serie serie;
	public SerieMenu(Serie s)
	{
		super(s);
		this.serie = s;
		JMenuItem addToSerie = new JMenuItem(LocaleManager.getInstance().getString("add_album"), 
										 	IconManager.getIcon("add_mini.png"));
		addToSerie.setMnemonic(ShortcutManager.ADD);
		addToSerie.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e)
			{
				new AlbumAddDialog(SerieMenu.this.serie);
			}
		});
		this.add(addToSerie);
	}
}
