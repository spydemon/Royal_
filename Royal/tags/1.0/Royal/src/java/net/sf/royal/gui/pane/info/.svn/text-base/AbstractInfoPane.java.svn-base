package net.sf.royal.gui.pane.info;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JTextField;


@SuppressWarnings("serial")
public abstract class AbstractInfoPane extends JPanel
{
	protected GridBagLayout gbl;
	protected GridBagConstraints gbc;
	protected CoverPane cpAlbums;
	
	protected final int LABEL = 0;
	protected final int COMPONENT = 1;
	
	protected AbstractInfoPane()
	{
		this.setMinimumSize(new Dimension(0,0));
		this.gbl = new GridBagLayout();
		this.gbc = new GridBagConstraints();
		this.setLayout(gbl);
		this.cpAlbums = new CoverPane();
		this.gbc.insets = new Insets(5, 10, 5, 5);
		this.gbc.fill = GridBagConstraints.BOTH;
		
		/* When the JSplitPane Divider is moved or the window resized */
		this.addComponentListener(new ComponentAdapter() {
			@Override
			public void componentResized(ComponentEvent e)
			{
				AbstractInfoPane.this.resizeCoverPane();
				AbstractInfoPane.this.validate();
				AbstractInfoPane.this.repaint();
			}
		});
	}
	
	protected void changeGridProperties(int type)
	{
		if(type == LABEL)
		{
			this.gbc.fill = GridBagConstraints.HORIZONTAL;
			this.gbc.weightx = 0.0;
			this.gbc.weighty = 0.0;
			this.gbc.gridwidth = 1;
		}
		else
		{
			this.gbc.fill = GridBagConstraints.HORIZONTAL;
			this.gbc.weightx = 0.5;
			this.gbc.weighty = 0.0;
			this.gbc.gridwidth = GridBagConstraints.REMAINDER;
		}
	}
	
	protected void resizeCoverPane()
	{
		this.gbc.fill = GridBagConstraints.HORIZONTAL;
		this.gbc.anchor = GridBagConstraints.PAGE_END;
		this.gbc.gridwidth = GridBagConstraints.REMAINDER;
		this.gbc.ipadx = this.cpAlbums.getPreferredSize().width - 30;
		this.gbc.ipady = CoverPane.COVER_HEIGHT;
		this.gbl.setConstraints(cpAlbums, gbc);
		this.revalidate();
		this.repaint();
	}
	
	/**
	 * ActionListener on a JCheckBox to avoid changing its selected status
	 * @author Steveo
	 *
	 */
	protected class DoNotChangeListener implements ActionListener
	{
		private JCheckBox jcb;
		
		public DoNotChangeListener(JCheckBox jcb)
		{
			this.jcb = jcb;
		}
		
		public void actionPerformed(ActionEvent ae)
		{
			if(jcb.isSelected())
			{
				jcb.setSelected(false);
			}
			else
			{
				jcb.setSelected(true);
			}
		}
	}
	
	protected JTextField createTextField()
	{
		JTextField res = new JTextField();
		res.setEditable(false);
		res.setBackground(new Color(200, 200, 205));
		return res;
	}
}
