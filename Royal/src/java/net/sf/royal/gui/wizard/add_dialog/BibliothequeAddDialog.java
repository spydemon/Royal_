package net.sf.royal.gui.wizard.add_dialog;

import java.awt.*;
import java.awt.event.*;
import java.util.List;
import javax.swing.*;

import net.sf.royal.datamodel.Bibliotheque;
import net.sf.royal.datamodel.Collection;
import net.sf.royal.datamodel.Editor;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.MessagePaneManager;
import net.sf.royal.gui.manager.ShortcutManager;
import net.sf.royal.gui.pane.AlbumPane;
import net.sf.royal.gui.util.RegexpTextField;
import net.sf.royal.persistency.PersistencyManager;
import net.sf.royal.persistency.SaveItemPersistency;
import net.sf.royal.persistency.*;
//import net.sf.royal.persistency.PersistencyManager;

/**
 * Dialog to create or to give details about a collection
 * @author Kevin Hagner
 */


public class BibliothequeAddDialog extends JDialog 
{
	private JFrame window;
	private JLabel jlabel;
	private JTextField new_lib_name;
	private JTextField new_lib_address;
	private JTextField new_lib_phone;
	private GridBagLayout grid;
	private GridBagConstraints constraints;
	private JButton button;
	private JList list_lib_view;
	private JScrollPane list_scroller;
	private DefaultListModel list_model;
	private List<Bibliotheque> lib_list;
	
	public BibliothequeAddDialog() {
		window = new JFrame();
		window.setTitle(LocaleManager.getInstance().getString("library_window_title"));
		window.setSize(400, 500);
		window.setLocationRelativeTo(null);
		
		grid = new GridBagLayout();
		window.setLayout(grid);

		constraints = new GridBagConstraints();
		constraints.gridwidth = 2;
		constraints.gridheight = 1;
		constraints.weightx = 5;
		constraints.weighty = 35;
		
		//--------- NEW LIBRARY -------------
		jlabel = new JLabel(LocaleManager.getInstance().getString("library_new_party_title") + " :");
		
		constraints.gridwidth = 1;
		constraints.gridx = 0;
		constraints.gridy = 0;
		grid.setConstraints(jlabel, constraints);
		window.add(jlabel);
		
		//Name of the library
		jlabel = new JLabel(LocaleManager.getInstance().getString("library_new_name") + " :");
		jlabel.setVisible(true);
		
		new_lib_name = new JTextField();
		new_lib_name.setVisible(true);
		new_lib_name.setColumns(15);
		
		constraints.gridx = 0;
		constraints.gridy = 1;
		grid.setConstraints(jlabel, constraints);
		window.add(jlabel);

		constraints.gridx = 1;
		constraints.gridy = 1;
		grid.setConstraints(new_lib_name, constraints);
		window.add(new_lib_name);
		
		//Address of the library
		jlabel = new JLabel(LocaleManager.getInstance().getString("library_new_address") + " :");
		new_lib_address = new JTextField();
		new_lib_address.setColumns(15);

		constraints.gridx = 0;
		constraints.gridy = 2;
		grid.setConstraints(jlabel, constraints);
		window.add(jlabel);
		
		constraints.gridx = 1;
		constraints.gridy = 2;
		grid.setConstraints(new_lib_address, constraints);
		window.add(new_lib_address);

		//Phone of the library 
		jlabel = new JLabel(LocaleManager.getInstance().getString("library_new_phone") + " :");
		new_lib_phone = new JTextField();
		new_lib_phone.setColumns(15);
		
		constraints.gridx = 0;
		constraints.gridy = 3;
		grid.setConstraints(jlabel, constraints);
		window.add(jlabel);
		
		constraints.gridx = 1;
		constraints.gridy = 3;
		grid.setConstraints(new_lib_phone, constraints);
		window.add(new_lib_phone);
		
		//Ok button
		button = new JButton(LocaleManager.getInstance().getString("add"));
		constraints.gridwidth = 2;
		constraints.gridx = 0;
		constraints.gridy = 4;
		grid.setConstraints(button, constraints);
		window.add(button);
		
		//Record of the new library with the fucking Hibernate
		button.addActionListener( new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent ae)
			{
				//Attribution to a library
				Bibliotheque b = new Bibliotheque();
				b.setName(new_lib_name.getText());
				b.setAddress(new_lib_address.getText());
				b.setPhone(new_lib_phone.getText());
				
				SaveItemPersistency.saveBibliotheque(b);
				
				//Reset of all values in JTextField
				new_lib_name.setText(null);
				new_lib_address.setText(null);
				new_lib_phone.setText(null);
				
				//Update of the list of existing libs
				displayListOfLibs();
			}
		});

		//--------- EXISTING LIBRARIES -------------
		jlabel = new JLabel(LocaleManager.getInstance().getString("library_list_title"));
		constraints.gridx = 0;
		constraints.gridy = 5;
		constraints.gridwidth = 1;
		grid.setConstraints(jlabel, constraints);
		
		window.add(jlabel);
		
		list_model = new DefaultListModel();
		list_lib_view = new JList(list_model);
		list_scroller = new JScrollPane(list_lib_view);
		list_scroller.setSize(new Dimension(300, 300));
		
		constraints.gridx = 0;
		constraints.gridy = 6;
		constraints.gridwidth = 2;
		grid.setConstraints(list_lib_view, constraints);
		
		window.add(list_lib_view);

		displayListOfLibs();
		
		button = new JButton(LocaleManager.getInstance().getString("remove"));
		button.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				System.out.println("Suppression de " + list_lib_view.getSelectedValue().toString());
				PersistencyManager.delLib((Bibliotheque) list_lib_view.getSelectedValue());
				displayListOfLibs();
			}
		});
		
		constraints.gridx = 0;
		constraints.gridy = 7;
		grid.setConstraints(button, constraints);
		
		window.add(button);
		window.setVisible(true);
	}
	
	//LIST OF EXISTING LIBRARIES
	public void displayListOfLibs() {
		list_model.removeAllElements();
		lib_list = PersistencyManager.findLibs();
		for (Bibliotheque b : lib_list) {
			list_model.addElement(b);
		}
	}
}
