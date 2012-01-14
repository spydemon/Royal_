package net.sf.royal.gui.wizard.settings;

import java.awt.BorderLayout;

import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.MessagePaneManager;
import net.sf.royal.gui.manager.PropertyManager;
import javax.swing.*;
import net.sf.royal.gui.util.*;

public class ChangeTimeRemainingPanel extends JPanel implements SettingsTab{
	//Go dans setting table dialog.
	private RegexpTextField nbDays;
	private String days;
	
	public ChangeTimeRemainingPanel() {
		//this.add(new JLabel("Jours avant alerte :"), BorderLayout.WEST);
		this.add(new JLabel(LocaleManager.getInstance().getString("warningDaysBeforeAlert")), BorderLayout.WEST);
		days = PropertyManager.getInstance().getProperty("days_remaning_before_warning");
		this.nbDays = new RegexpTextField(2, RegexpTextField.NUMBER);
		this.nbDays.setText(days);
		this.add(this.nbDays, BorderLayout.EAST);
	}
	
	public boolean apply() {
		if (this.nbDays.getText().isEmpty() || !this.nbDays.check()) {
			this.nbDays.setIncorrect();
			MessagePaneManager.showInfoPane(LocaleManager.getInstance().getString("warningDaysInvalid"));
			return false;
		}
		PropertyManager.getInstance().changeProperty("days_remaning_before_warning",this.nbDays.getText());
		return true;
	}
	
	public boolean hasBeenChanged() {
		if (this.days.toString().equals(this.nbDays.getText()))
				return false;
		return true;
	}
}
