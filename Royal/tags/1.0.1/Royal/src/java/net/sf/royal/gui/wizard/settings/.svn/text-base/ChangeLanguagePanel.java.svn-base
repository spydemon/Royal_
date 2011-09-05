package net.sf.royal.gui.wizard.settings;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Hashtable;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.PropertyManager;

/**
 * Tab of the {@SettingsTabbedDialog} containing the stuff to switch the languages.
 * @author Maxime Kientz
 */
public class ChangeLanguagePanel extends JPanel implements SettingsTab
{
	private JComboBox jcb_langs;
	private JLabel jl_langs;
	private JPanel jp;

	private Hashtable<String,String> H;

	// the boolean which will be returned by the methods
	// of the SettingsTab interface
	private boolean isChanged = false;

	public ChangeLanguagePanel()
	{
		// Filling the Hashtable
		H = new Hashtable<String,String>();
		H.put("Francais","french");
		H.put("French","french");
		H.put("Anglais","english");
		H.put("English","english");

		// setup of the layouts
		this.setLayout(new BorderLayout());
		
		// Panel for the ComboBox 
		jp = new JPanel();
		
		// Write the Label and fill the ComboBox
		jl_langs = new JLabel(LocaleManager.getInstance().getString("lang_choice"));
		DefaultComboBoxModel cbm = new DefaultComboBoxModel();
		jcb_langs = new JComboBox(cbm);
		jcb_langs.addActionListener(new ItemChangeListener());

		jp.add(jl_langs);
		jp.add(jcb_langs);

		this.add(jp, BorderLayout.CENTER);
		
		String[] appLangs = LocaleManager.getInstance().getAvailableLanguages(); 
		String currAppLanguage = PropertyManager.getInstance().getProperty("language");
		String curLang;
		for(String lang : appLangs)
		{
			curLang = LocaleManager.getInstance().getString(lang);
			cbm.addElement(curLang);
			if(lang.equals(currAppLanguage))
			{
				cbm.setSelectedItem(curLang);
			}
		}
	}

	public boolean apply()
	{
		if(isChanged)
		{
			PropertyManager.getInstance().changeProperty("language",H.get((String) jcb_langs.getSelectedItem()));
			this.isChanged = false;
			return true;
		}
		else
		{
			return false;
		}
	}

	public class ItemChangeListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			if(PropertyManager.getInstance().getProperty("language")
					.equals(H.get((String)ChangeLanguagePanel.this.jcb_langs.getSelectedItem())))
			{
				ChangeLanguagePanel.this.isChanged = false;
			}
			else
			{
				ChangeLanguagePanel.this.isChanged = true;
			}
		}
	}
	
	@Override
	public boolean hasBeenChanged()
	{
		return this.isChanged;
	}
}

