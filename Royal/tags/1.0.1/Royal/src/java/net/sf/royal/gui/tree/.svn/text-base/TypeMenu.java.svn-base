package net.sf.royal.gui.tree;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenuItem;

import net.sf.royal.datamodel.Type;
import net.sf.royal.gui.manager.IconManager;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.wizard.add_dialog.SerieAddDialog;


/**
 * @author Soulou
 * Popup menu which appears when the user right click on a Type
 * in the Tree view.
 */
public class TypeMenu extends AbstractMenu 
{
	private Type type;
	
	public TypeMenu(Type t)
	{
		super(t);
		this.type = t;
		JMenuItem addSerie = new JMenuItem(LocaleManager.getInstance().getString("add_serie"), 
										   IconManager.getIcon("serie.png"));
		addSerie.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent arg0) {
				new SerieAddDialog(TypeMenu.this.type);
			}
		});
		this.add(addSerie);
		this.edit.setEnabled(false);
	}
}
