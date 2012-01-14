package net.sf.royal.gui.wizard.settings;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Rectangle;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;

import net.sf.royal.Royal;
import net.sf.royal.gui.manager.IconManager;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.MessagePaneManager;
import net.sf.royal.gui.manager.ShortcutManager;

/**
 * Window containing all the tabs used to change something in the main program.
 * @author Maxime Kientz
 */
public class SettingsTabbedDialog extends JDialog
{
	// The main Panel
	private JTabbedPane jtp;
	
	// The button panel and the buttons
	private JPanel jpButton;
	
	private JButton jbApply;
	private JButton jbOk;
	private JButton jbCancel;
	
	// The Array containing all the Tabs and its index, used for the SaveActionListener
	private ArrayList<SettingsTab> settingsTabs;

	public SettingsTabbedDialog(Window parent)
	{
		super(parent);
        
		this.settingsTabs = new ArrayList<SettingsTab>();
		
		this.setTitle(LocaleManager.getInstance().getString("settings"));
		this.setIconImage(IconManager.getIcon("settings.png").getImage());
		// Layout init
		this.setLayout(new BorderLayout());
		
		// We create the panel and buttons
		jbApply = new JButton(LocaleManager.getInstance().getString("apply"));
		jbApply.setMnemonic(ShortcutManager.APPLY);
		jbApply.addActionListener(new SaveActionListener(false));
		jbOk = new JButton(LocaleManager.getInstance().getString("ok"));
		jbOk.setMnemonic(ShortcutManager.OK);
		jbOk.addActionListener(new SaveActionListener(true));
		jbCancel = new JButton(LocaleManager.getInstance().getString("cancel"));
		jbCancel.setMnemonic(ShortcutManager.CANCEL_);
		jbCancel.addActionListener(new CancelActionListener(this));

		jpButton = new JPanel();
		jpButton.add(jbApply);
		((FlowLayout)jpButton.getLayout()).setAlignment(FlowLayout.RIGHT);
		jpButton.add(jbOk);
		jpButton.add(jbCancel);

		// We start creating and filling the main panel with the tabs
		jtp = new JTabbedPane();
		jtp.setTabPlacement(JTabbedPane.LEFT);
		jtp.setTabLayoutPolicy(JTabbedPane.WRAP_TAB_LAYOUT);

		// 1st tab : Change Database
		ChangeDatabasePanel cdp = new ChangeDatabasePanel(this);
		jtp.addTab(LocaleManager.getInstance().getString("database"), cdp);
		settingsTabs.add(cdp);

		// 2nd tab : Change Language
		ChangeLanguagePanel clp = new ChangeLanguagePanel();
		jtp.addTab(LocaleManager.getInstance().getString("language"), clp);
		settingsTabs.add(clp);
		
		// 3rd tab : Change Mail
		ChangeEmailPanel cep = new ChangeEmailPanel();
		jtp.addTab(LocaleManager.getInstance().getString("email"), cep);
		settingsTabs.add(cep);
		
		// 4rd tab : Change the time remaining before alert for borrowed book
		//TimeLeftPanel
		ChangeTimeRemainingPanel ctmr = new ChangeTimeRemainingPanel();
		jtp.addTab("Temps restant", ctmr);
		settingsTabs.add(ctmr);
		
		this.add(jtp, BorderLayout.CENTER);
		this.add(jpButton, BorderLayout.SOUTH);

		this.pack();
		this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		
		// We center the JDialog on the parent JFrame)
		this.setLocationRelativeTo(parent);
	}

	/**
	 * This ActionListener is used to save all the modifications made in the tabs.
	 * For all the tabs in the array, we check if something has changed, and if it's
	 * the case, we fire the listener to save the changes.
	 * @see SettingsTab
	 */
	public class SaveActionListener implements ActionListener
	{
		private boolean close;
		/**
		 * @param close if we want to close the window on click
		 */
		public SaveActionListener(boolean close)
		{
			this.close = close;
		}
		public void actionPerformed(ActionEvent e)
		{
			boolean restart = false;
			for(SettingsTab s : SettingsTabbedDialog.this.settingsTabs)
			{
				if(s.hasBeenChanged())
				{
					restart |= s.apply();
				}
			}
			if(restart && MessagePaneManager.showRestartConfirmation())
			{
				SettingsTabbedDialog.this.dispose();
				Royal.close(true);
			}
			else if(close)
			{
				SettingsTabbedDialog.this.dispose();
			}
		}
	}

	public class CancelActionListener implements ActionListener
	{
		JDialog jf_main;

		public CancelActionListener(SettingsTabbedDialog settingsTabbedDialog)
		{
			this.jf_main = settingsTabbedDialog;
		}
		public void actionPerformed(ActionEvent e)
		{
			this.jf_main.dispose();	
		}
	}
}

