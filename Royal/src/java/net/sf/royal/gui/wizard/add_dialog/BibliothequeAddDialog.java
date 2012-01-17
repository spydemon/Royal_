package net.sf.royal.gui.wizard.add_dialog;

import java.awt.*;
import java.awt.event.*;
import java.util.List;
import java.util.ArrayList;
import javax.swing.*;

import net.sf.royal.datamodel.Bibliotheque;
import net.sf.royal.datamodel.Collection;
import net.sf.royal.datamodel.Editor;
import net.sf.royal.datamodel.Album;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.MessagePaneManager;
import net.sf.royal.gui.manager.ShortcutManager;
import net.sf.royal.gui.pane.AlbumPane;
import net.sf.royal.gui.util.RegexpTextField;
import net.sf.royal.persistency.*;

//import net.sf.royal.persistency.PersistencyManager;

/**
 * Dialog to create or to give details about a collection
 * @author Kevin Hagner
 */


public class BibliothequeAddDialog extends JDialog 
{
	private JLabel jlabel;
	private RegexpTextField new_lib_name;
	private JTextField new_lib_address;
	private RegexpTextField new_lib_phone;
	private GridBagConstraints constraints;
	private JButton button;
	private JList list_lib_view;
	private JScrollPane list_scroller;
	private DefaultListModel list_model;
	private List<Bibliotheque> lib_list;
	
	public BibliothequeAddDialog() {
		super(null, LocaleManager.getInstance().getString("library_window_title"),
				true? Dialog.ModalityType.TOOLKIT_MODAL : Dialog.ModalityType.MODELESS);
		this.setSize(400, 500);
		this.setLocationRelativeTo(null);
		
		this.setLayout(new GridBagLayout());

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
		this.add(jlabel, constraints);
		
		//Name of the library
		jlabel = new JLabel(LocaleManager.getInstance().getString("library_new_name") + " :");
		jlabel.setVisible(true);
		
		//new_lib_name = new JTextField();
		new_lib_name = new RegexpTextField(20, RegexpTextField.NONEMPTY);
		new_lib_name.setVisible(true);
		new_lib_name.setColumns(15);
		
		constraints.gridx = 0;
		constraints.gridy = 1;
		this.add(jlabel, constraints);

		constraints.gridx = 1;
		constraints.gridy = 1;
		this.add(new_lib_name, constraints);
		
		//Address of the library
		jlabel = new JLabel(LocaleManager.getInstance().getString("library_new_address") + " :");
		new_lib_address = new JTextField();
		new_lib_address.setColumns(15);

		constraints.gridx = 0;
		constraints.gridy = 2;
		this.add(jlabel, constraints);
		
		constraints.gridx = 1;
		constraints.gridy = 2;
		this.add(new_lib_address, constraints);

		//Phone of the library 
		jlabel = new JLabel(LocaleManager.getInstance().getString("library_new_phone") + " :");
		new_lib_phone = new RegexpTextField(20, RegexpTextField.PHONE);
		new_lib_phone.setColumns(15);
		
		constraints.gridx = 0;
		constraints.gridy = 3;
		this.add(jlabel, constraints);
		
		constraints.gridx = 1;
		constraints.gridy = 3;
		this.add(new_lib_phone, constraints);
		
		//Ok button
		button = new JButton(LocaleManager.getInstance().getString("add"));
		constraints.gridwidth = 2;
		constraints.gridx = 0;
		constraints.gridy = 4;
		this.add(button, constraints);
		
		//Record of the new library with Hibernate
		button.addActionListener( new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent ae)
			{
				//If the library hasn't name.
				if (new_lib_name.getText().isEmpty()) {
					new_lib_name.setIncorrect();
					MessagePaneManager.showInfoPane(LocaleManager.getInstance().getString("library_missing_name"));
					return;
				}
				
				//If the phone numer is invalid
				if (new_lib_phone.getText().isEmpty() || !new_lib_phone.check()) {
					new_lib_phone.setIncorrect();
					MessagePaneManager.showInfoPane(LocaleManager.getInstance().getString("library_invalid_tel"));
					return;
				}
				
				//Attribution to a library
				new_lib_name.setCorrect();
				new_lib_phone.setCorrect();
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
		
		this.add(jlabel, constraints);
		
		list_model = new DefaultListModel();
		list_model.setSize(3);
		list_lib_view = new JList(list_model);
		list_lib_view.setSize(new Dimension(300, 300));
		list_scroller = new JScrollPane(list_lib_view);
		
		constraints.gridx = 0;
		constraints.gridy = 6;
		constraints.gridwidth = 2;
		this.add(list_scroller, constraints);

		displayListOfLibs();
		
		//Remove library button
		button = new JButton(LocaleManager.getInstance().getString("remove"));
		button.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				ArrayList<Album> libUsed = PersistencyManager.findAlbumsInLib((Bibliotheque) list_lib_view.getSelectedValue());
				if (libUsed.isEmpty()) {
					PersistencyManager.delLib((Bibliotheque) list_lib_view.getSelectedValue());
					displayListOfLibs();
				}
				else 
					MessagePaneManager.showInfoPane(LocaleManager.getInstance().getString("library_used_not_deleted"));
			}
		});
		
		constraints.gridx = 0;
		constraints.gridy = 7;
		
		this.add(button, constraints);
		this.pack();
		this.setLocationRelativeTo(this.getParent());
		this.setDefaultCloseOperation(HIDE_ON_CLOSE);
		this.setVisible(true);
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
