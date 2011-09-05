package net.sf.royal.gui.wizard.settings;


/**
 * This interface is used to add 2 methods to all the tabs in the SettingsTabbedPane.
 * @author Maxime Kientz 
 */
public interface SettingsTab
{
	/**
	 * This method is used to apply the modification done by the user choices
	 * if return true, the application will restart when the user clicks on "apply" or "ok"
	 */
	public boolean apply();

	/**
	 * This method is used to know if something has been changed in the current tab
	 * @return true if something has been changed, otherwise returns false
	 */
	public boolean hasBeenChanged();
}
