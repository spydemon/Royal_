package net.sf.royal.gui.wizard.add_dialog;

import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import net.sf.royal.datamodel.Serie;
import net.sf.royal.datamodel.Type;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.MessagePaneManager;
import net.sf.royal.gui.manager.ShortcutManager;
import net.sf.royal.gui.pane.AlbumPane;
import net.sf.royal.persistency.PersistencyManager;
import net.sf.royal.persistency.SaveItemPersistency;


/**
 * This is the dialog used to add a new Serie
 * @author Maxime Kientz, Steven Nguyen
 */
@SuppressWarnings("serial")
public class SerieAddDialog extends JDialog
{
// Fields
	/**
	 * The title of the dialog in the correct Locale
	 */
	private static String sTitle = LocaleManager.getInstance().getString("add_serie");
	
	/**
	 * The default layout of the SerieAddDialog
	 */
	private GridBagLayout gbLayout;
	
	/**
	 * The JTextField for the name of the serie
	 */
	private JTextField jtfSerieName;
	/**
	 * The JEntryPane for the name of the type
	 */
	private JEntryPane jepTypeName;
	/**
	 * The Checkbox for if the serie is a One Shot or not
	 */
	private JCheckBox jcbOneShot;
	/**
	 * The JTextAera for a description of the serie
	 */
	private JTextArea jtaDescription;
	
	private Serie currentSerie;
	private Type typeSerie;
	
// Constructors
	/**
	 * Create a new SerieAddDialog, and tell it to be at the center of the parent.<br/>
	 * The dialog will be created and paint in the SwingUtilities Thread and then be showed.
	 * @param parent The parent component of the SerieAddDialog
	 * @param isForeground true if the dialog has to stay on the foreground of its parent
	 */
	public SerieAddDialog(Window parent, boolean isForeground)
	{
		super(parent, sTitle, isForeground? Dialog.ModalityType.TOOLKIT_MODAL : Dialog.ModalityType.MODELESS);
		this.currentSerie = new Serie();
		
		SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				SerieAddDialog.this.init();
				SerieAddDialog.this.initListener();
				SerieAddDialog.this.display();
			}
		});
	}
	
	/**
	 * Create a SerieAddDialog. <br/>
	 * The created dialog will be placed at the center of the screen and 
	 * behave like other window
	 */
	public SerieAddDialog()
	{
		this(null, true);
	}
	
	/**
	 * Constructor called to add a Serie of the Type t
	 * @param t
	 */
	public SerieAddDialog(Type t)
	{
		this(null,true);
		this.typeSerie = t;
		this.currentSerie.setType(this.typeSerie);
		
		SwingUtilities.invokeLater(new Runnable() {
			
			@Override
			public void run() {
				SerieAddDialog.this.jepTypeName.setText(SerieAddDialog.this.typeSerie.getName());
			}
		});
	}
	
	/**
	 * Constructor called to edit a Serie
	 * @param s
	 */
	public SerieAddDialog(Serie s)
	{
		this(null, true);
		if(!s.getName().isEmpty())
		{
			this.setTitle(LocaleManager.getInstance().getString("edit_serie"));
		}
		
		this.currentSerie = s;
		
		SwingUtilities.invokeLater(new Runnable() {
			
			@Override
			public void run() {
				String name = currentSerie.getName();
				Type type = currentSerie.getType();
				boolean oneshot = currentSerie.isOneShot();
				String desc = currentSerie.getDescription();
				
				jtfSerieName.setText(name);
				if(type != null)
				{
					typeSerie = type;
					jepTypeName.setID(type.getId());
				}
				jcbOneShot.setSelected(oneshot);
				if(desc != null)
				{
					jtaDescription.setText(desc);
				}
			}
		});
	}
	
// Methods
	/**
	 * Initialize the Dialog Components.
	 * You need to use the method display to make the dialog visible
	 * @see SerieAddDialog#display
	 */
	private void init()
	{
		//this.setTitle(this.sTitle);
		this.setMinimumSize(new Dimension(400,300));
		this.gbLayout = new GridBagLayout();
		this.setLayout(gbLayout);
		
		Insets iComp = new Insets(5, 5, 5, 5);
		
		GridBagConstraints gbc = new GridBagConstraints();

		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.weightx = 0;
		gbc.weighty = 0;
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.gridheight = 1;
		gbc.gridwidth = 1;
		
		String[] labels = { 
				LocaleManager.getInstance().getString("name"), 
				LocaleManager.getInstance().getString("type"), 
				LocaleManager.getInstance().getString("oneshot"), 
				LocaleManager.getInstance().getString("description")
		};

		gbc.insets = iComp;
		int i;
		for(i=0; i<labels.length -1; i++)
		{
			JLabel jl = new JLabel(labels[i]);
			this.add(jl, gbc);
			gbc.gridy ++;
		}
		
		gbc.insets = new Insets(10, 5, 5, 5);
		gbc.anchor = GridBagConstraints.NORTH;
		JLabel jl = new JLabel(labels[i]);
		this.add(jl,gbc);

		gbc.insets = iComp;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.weightx = 1.0;
		gbc.weighty = 0;
		gbc.gridx ++;
		gbc.gridy = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;

		/* jepSerieName */
		this.jtfSerieName = new JTextField();
		this.add(jtfSerieName, gbc);
		
		
		/* jepTypeName */
		gbc.gridy ++;
		this.jepTypeName = new JEntryPane(JEntryPane.TYPE);
		this.add(jepTypeName, gbc);
		
		/* jcbOneShot */
		gbc.gridy ++;
		this.jcbOneShot = new JCheckBox();
		this.add(jcbOneShot, gbc);
				
		gbc.gridy++;
		gbc.weighty = 1.0; // for the textarea to be resized vertically too
		gbc.fill = GridBagConstraints.BOTH;
		/* jtaDescription */
		this.jtaDescription = new JTextArea();
		this.add(jtaDescription, gbc);

		JPanel b = new JPanel(new FlowLayout());
		JButton ok = new JButton(LocaleManager.getInstance().getString("ok"));
		ok.setMnemonic(ShortcutManager.OK);
		JButton annuler = new JButton(LocaleManager.getInstance().getString("cancel"));
		annuler.setMnemonic(ShortcutManager.CANCEL);

		ok.addActionListener(new SaveActionListener(this));
		/* Close the window when cancelling */
		annuler.addActionListener(new CancelActionListener(this));
		b.add(ok);
		b.add(annuler);
		
		gbc.gridx=0;
		gbc.gridy++;
		gbc.weighty = 0;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.gridwidth=2;
		this.add(b, gbc);
	}

	/**
	 * Set all the listeners of the SerieAddDialog
	 */
	private void initListener()
	{
			
	}
	
	/**
	 * Use this method to show the SerieAddDialog 
	 */
	public void display()
	{
		this.pack();
		this.setLocationRelativeTo(this.getParent());
		this.setDefaultCloseOperation(HIDE_ON_CLOSE);
	}
	
	/**
	 * Save current Serie to in the database
	 */
	public void saveSerie()
	{
		Serie s = this.currentSerie;
		boolean hasChanged = false;
			
		/* We check if the name has changed
		 * If it's the case, we change it
		 */
		if(!jtfSerieName.getText().equals(s.getName()))
		{
			s.setName(jtfSerieName.getText());
			hasChanged = true;
		}

		Type t;
		if(s.getType() == null)
		{
			if(jepTypeName.getID() != null)
			{
				t = PersistencyManager.findTypeByID(jepTypeName.getID());
				SaveItemPersistency.saveType(t);
			}
			else if(!jepTypeName.getText().isEmpty() && jepTypeName.getID() == null)
			{
				t = new Type();
				t.setName(jepTypeName.getText());
				t.addSerie(s);
				s.setType(t);
				SaveItemPersistency.saveType(t);
				hasChanged = true;
			}
		}
		else
		{
			if(jepTypeName.getID() == null)
			{
				t = s.getType();
				t.removeSerie(s);
				s.setType(null);
				SaveItemPersistency.saveType(t);
				hasChanged = true;
			}
			else if(!jepTypeName.getID().equals(s.getType().getId()))
			{
				t = s.getType();
				t.removeSerie(s);
				SaveItemPersistency.saveType(t);
				t = PersistencyManager.findTypeByID(jepTypeName.getID());
				t.addSerie(s);
				s.setType(t);
				SaveItemPersistency.saveType(t);
				hasChanged = true;
			}
		}
		
		
		if(!jtaDescription.getText().equals(s.getDescription()))
		{
			s.setDescription(jtaDescription.getText());
			hasChanged = true;
		}
		
		if(jcbOneShot.isSelected() != s.isOneShot())
		{
			s.setOneShot(jcbOneShot.isSelected());
			hasChanged = true;
		}
		
		if(hasChanged)
		{
			SaveItemPersistency.saveSerie(s);
		}
		
		AlbumPane.getInstance().refresh();
	}
	
	public Long getID()
	{
		return this.currentSerie.getId();
	}
	
	
// Classes
	class CancelActionListener implements ActionListener
	{
		protected JDialog parent;
		
		public CancelActionListener(JDialog parent)
		{
			this.parent = parent;
		}
		
		@Override
		public void actionPerformed(ActionEvent ae)
		{
			this.parent.dispose();
		}
	}

	class SaveActionListener implements ActionListener
	{
		protected SerieAddDialog parent;

		public SaveActionListener(SerieAddDialog p)
		{
			this.parent=p;
		}
		@Override
		public void actionPerformed(ActionEvent ae)
		{
			if((SerieAddDialog.this.jtfSerieName.getText()).isEmpty())
			{
				MessagePaneManager.showInfoPane("Vous devez au moins spécifier un nom pour votre série");
			}
			else
			{
				SerieAddDialog.this.saveSerie();
				this.parent.dispose();
			}
		}
		
	}
	
}
