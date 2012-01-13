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
import net.sf.royal.persistency.PersistencyManager;

public class WarningDialog extends JDialog {
	private boolean display;
	private List<Album> lstAlbums;
	private List<String> bookLate;
	private JButton okButton;
	private DefaultListModel listeLivre;
	private JList listeLivre_jlist;
	private JPanel panel;
	
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
				System.out.println("La fenêtre est senser se fermer maintenant.");
			}
		});
	}
	
	private void display() {
		if (this.display)
			this.setVisible(true);
		else this.setVisible(false);
	}
	
	private void init() {
        lstAlbums = PersistencyManager.findAlbums();
        bookLate = new ArrayList<String>(); 
		Date mnt = new Date();
		
		this.setMinimumSize(new Dimension(200, 300));
		
        for (Album a : lstAlbums) {
        	if (a.getPurchaseDate() != null && a.getBuy() == false) {
        		long daysLeft = (a.getPurchaseDate().getTime() - mnt.getTime())/(3600*24*1000);
        		if (daysLeft < 5 && daysLeft > 0)
        			bookLate.add(a.toString());
        	}
        }
		System.out.println("Il y a OK OBJET : " + lstAlbums.size());

		panel = new JPanel();
		okButton = new JButton(LocaleManager.getInstance().getString("warningIWillThinkOfIt"));
		
        if (!bookLate.isEmpty()) {
        	listeLivre = new DefaultListModel();
        	listeLivre_jlist = new JList(listeLivre);
        
        	panel.add(new JLabel(LocaleManager.getInstance().getString("warningBookTitle") + " :"), BorderLayout.NORTH);
        	
        	for (String s : bookLate) {
            	listeLivre.addElement(s);
        	}
        	panel.add(listeLivre_jlist, BorderLayout.CENTER);
        	
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
