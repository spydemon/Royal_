package net.sf.royal.gui.wizard.add_dialog;

import java.awt.event.ActionListener;

import javax.swing.JButton;

/**
 * This class provides a simple panel composed by a JLabel, a JTextField and a JButton
 * @author Maxime Kientz
 *
 */
public class JButtonPane extends JEntryPane
{
	/**
	 * The Jbutton
	 */
	private JButton jb;
	
	/**
	 * The other components are already set by the JEntryPane
	 */	
	public JButtonPane(int type, String btnName)
	{
		super(type);
		this.jb = new JButton(btnName);
		this.add(jb);
	}
	
	/**
	 * Add an actionListener to the button of the panel
	 * @param a The actionListener to be registered
	 */
	public void addButtonListener(ActionListener a)
	{
		this.jb.addActionListener(a);
	}
	//TODO : Methode pour recuperer le contenu du JTextField ??
}


