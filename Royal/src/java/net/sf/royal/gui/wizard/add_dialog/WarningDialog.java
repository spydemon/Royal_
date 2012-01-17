package net.sf.royal.gui.wizard.add_dialog;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.*;

import net.sf.royal.datamodel.Album;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.PropertyManager;
import net.sf.royal.persistency.PersistencyManager;

/**
 * This is the dialog used to print warnings to the user when he will give back a borrowed book soon.
 * @author Kevin Hagner
 *
 */

public class WarningDialog extends JDialog {
	private boolean display;
	private List<Album> lstAlbums;
	private List<String> bookLate;
	private JButton okButton;
	private DefaultListModel listeLivre;
	private JList listeLivre_jlist;
	private JPanel panel;
	private JScrollPane jscroll;
	private int days;
	
	public WarningDialog(Window v) {
		super(v, LocaleManager.getInstance().getString("warning") + " :", Dialog.ModalityType.TOOLKIT_MODAL);

		SwingUtilities.invokeLater(new Runnable()
		{	
			@Override
			public void run()
			{				
				WarningDialog.this.init();
				WarningDialog.this.initListener();
				WarningDialog.this.display();
			}
		});
	}
	
	private void initListener() {
		this.okButton.addActionListener(new ActionListener() 
		{
			@Override
			public void actionPerformed(ActionEvent e) 
			{
				WarningDialog.this.dispose();
			}
		});
	}
	
	private void display() {
		if (this.display)
			this.setVisible(true);
		//If no book is to give back soon, the window isn't displayed.
		else this.setVisible(false);
	}
	
	private void init() {
        lstAlbums = PersistencyManager.findAlbums();
        bookLate = new ArrayList<String>(); 
		Date mnt = new Date();
		days = Integer.parseInt(PropertyManager.getInstance().getProperty("days_remaning_before_warning"));
		
		this.setSize(new Dimension(200, 140));
		
		//Make the list of all books to give back soon.
        for (Album a : lstAlbums) {
        	if (a.getPurchaseDate() != null && a.getBuy() == false) {
        		long daysLeft = (a.getPurchaseDate().getTime() - mnt.getTime())/(3600*24*1000);
        		if (daysLeft < days && daysLeft > 0)
        			bookLate.add(a.toString());
        	}
        }

		panel = new JPanel();
		okButton = new JButton(LocaleManager.getInstance().getString("warningIWillThinkOfIt"));
		
		//Build the window only if books are to display.
        if (!bookLate.isEmpty()) {
        	listeLivre = new DefaultListModel();
        	listeLivre.setSize(3);
        	listeLivre_jlist = new JList(listeLivre);
        	listeLivre_jlist.setSize(140,70);
        	jscroll = new JScrollPane(listeLivre_jlist);
        
        	panel.add(new JLabel(LocaleManager.getInstance().getString("warningBookTitle") + " :"), BorderLayout.NORTH);
        	
        	for (String s : bookLate) {
            	listeLivre.addElement(s);
        	}
        	panel.add(jscroll, BorderLayout.CENTER);
        	//panel.add(listeLivre_jlist, BorderLayout.CENTER);
        	panel.add(okButton, BorderLayout.SOUTH);
        	this.display = true;
        }
        else {
        	System.out.println("C'est vide…");
        	this.display = false;
        }
        this.add(panel);
	}
}
