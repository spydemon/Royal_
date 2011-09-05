package net.sf.royal.gui.pane.info;

import javax.swing.ImageIcon;
import javax.swing.JLabel;

import net.sf.royal.gui.util.ImageHelper;

public class CoverLabel extends JLabel
{
	public CoverLabel(String url, int w, int h)
	{
		this(ImageHelper.getImageIcon(url,w,h));
	}

	public CoverLabel(ImageIcon img)
	{
		super();
		this.setText(null);
		this.setIcon(img);
	}
}
