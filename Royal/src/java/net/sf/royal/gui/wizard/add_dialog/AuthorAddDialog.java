package net.sf.royal.gui.wizard.add_dialog;

import java.awt.Dialog;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Date;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import net.sf.royal.datamodel.Author;
import net.sf.royal.gui.datepicker.JDatePicker;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.MessagePaneManager;
import net.sf.royal.gui.manager.ShortcutManager;
import net.sf.royal.gui.pane.AlbumPane;
import net.sf.royal.gui.util.RegexpTextField;
import net.sf.royal.persistency.SaveItemPersistency;


/**
 * This is the dialog used to add a new Author
 * @author Maxime Kientz
 * @author Leo Unbekandt
 * @author Steven Nguyen
 */
@SuppressWarnings("serial")
public class AuthorAddDialog extends JDialog
{
// Fields
	/**
	 * The title of the dialog in the correct Locale
	 */
	private static String sTitle = LocaleManager.getInstance().getString("add_author");
	
	/**
	 * The default layout of the AuthorAddDialog
	 */
	private GridBagLayout gbLayout;
	
	/**
	 * The RegexpTextField for the nickname of the author
	 */
	private RegexpTextField rtfAuthorNickname;
	/**
	 * The RegexpTextField for the name of the author
	 */
	private RegexpTextField rtfAuthorName;
	/**
	 * The JTextField for the firstname of the author
	 */
	private JTextField jtfAuthorFirstname;
	
	/**
	 * The JDatePane for the date of birth
	 */
	private JDatePicker jdpAuthorBirth;

	/**
	 * The JDatePane for the date of death
	 */
	private JDatePicker jdpAuthorDeath;

	/**
	 * The classical buttons 
	 */
	private JButton jbCancel;
	private JButton jbOk;
	
	private Author currentAuthor;	
	
	
// Constructors
	/**
	 * Create a new AuthorAddDialog
	 */
	public AuthorAddDialog(Window parent, boolean isForeground)
	{
		super(parent, sTitle, isForeground?Dialog.ModalityType.DOCUMENT_MODAL:Dialog.ModalityType.MODELESS);
		
		SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				AuthorAddDialog.this.init();
				AuthorAddDialog.this.initListener();
				AuthorAddDialog.this.display();
			}
		});
	}
	
	public AuthorAddDialog()
	{
		this(null,false);
	}
	
	public AuthorAddDialog(Author au)
	{
		this(null,false);
		this.currentAuthor = au;
		SwingUtilities.invokeLater(new Runnable() {
			public void run()
			{
				String name = AuthorAddDialog.this.currentAuthor.getName();
				String firstname = AuthorAddDialog.this.currentAuthor.getFirstName();
				String nickname = AuthorAddDialog.this.currentAuthor.getNickName();
				Date birth = AuthorAddDialog.this.currentAuthor.getBirth();
				Date death = AuthorAddDialog.this.currentAuthor.getDeath();
				
				if(name != null);
					AuthorAddDialog.this.rtfAuthorName.setText(name);
				if(firstname != null);
					AuthorAddDialog.this.jtfAuthorFirstname.setText(firstname);
				if(nickname != null);
					AuthorAddDialog.this.rtfAuthorNickname.setText(nickname);
				if(birth != null)
					AuthorAddDialog.this.jdpAuthorBirth.setDate(birth);
				if(death != null)
					AuthorAddDialog.this.jdpAuthorDeath.setDate(death);
			}
		});
	}
	
// Methods
	/**
	 * Initialize the Dialog Components.
	 * You need to use the method display to make the dialog visible
	 * @see AuthorAddDialog#display
	 */
	private void init()
	{
		//this.setTitle(this.sTitle);
		
		this.gbLayout = new GridBagLayout();
		this.setLayout(gbLayout);
		
		GridBagConstraints gbc = new GridBagConstraints();

		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(5,5,5,5);
		gbc.weightx = 0;
		gbc.weighty = 1.0;
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 2;
		gbc.gridheight = 1;
		
		JLabel a = new JLabel(sTitle);
		this.add(a,gbc);

		gbc.gridy ++;
		gbc.gridwidth = 1;
		
		/* Strings for the labels of the window, got by Locale */
		String[] labels = { 
				LocaleManager.getInstance().getString("nickname"),
				LocaleManager.getInstance().getString("name"), 
				LocaleManager.getInstance().getString("firstname"), 
				LocaleManager.getInstance().getString("birth"), 
				LocaleManager.getInstance().getString("death") 
		};

		for(int i=0; i<labels.length; i++)
		{
			JLabel jl = new JLabel(labels[i]);
			this.add(jl, gbc);
			gbc.gridy ++;
		}

		gbc.weightx = 1.0;
		gbc.gridx ++;
		gbc.gridy = 1;
		gbc.fill = GridBagConstraints.BOTH;

		/* jepAuthorNickName */
		this.rtfAuthorNickname = new RegexpTextField(RegexpTextField.NONEMPTY);
		this.add(rtfAuthorNickname, gbc);

		/* jepAuthorName */
		gbc.gridy ++;
		this.rtfAuthorName = new RegexpTextField(RegexpTextField.NONEMPTY);
		this.add(rtfAuthorName, gbc);

		/* jepAuthorSurname */
		gbc.gridy ++;
		this.jtfAuthorFirstname = new JTextField();
		this.add(jtfAuthorFirstname, gbc);

		/* jepAuthorBirth */
		gbc.gridy ++;
		this.jdpAuthorBirth = new JDatePicker();
		this.add(jdpAuthorBirth, gbc);

		/* jepAuthorDeath */
		gbc.gridy ++;
		this.jdpAuthorDeath = new JDatePicker();
		this.add(jdpAuthorDeath, gbc);		

		JPanel b = new JPanel(new FlowLayout());
		
		jbOk = new JButton(LocaleManager.getInstance().getString("ok"));
		jbOk.setMnemonic(ShortcutManager.OK);
		jbCancel = new JButton(LocaleManager.getInstance().getString("cancel"));
		jbCancel.setMnemonic(ShortcutManager.CANCEL);

		/* Close the window when cancelling */
		b.add(jbOk);
		b.add(jbCancel);
		
		gbc.gridx=0;
		gbc.gridy++;
		gbc.gridwidth=2;
		this.add(b, gbc);
	}

	/**
	 * Set all the listeners of the AuthorAddDialog
	 */
	private void initListener()
	{
		jbCancel.addActionListener(new CancelActionListener());
		jbOk.addActionListener(new SaveActionListener());				
	}
	
	/**
	 * When Ok button is pushed, and that all conditions are filled
	 */
	private void saveAuthor()
	{
		Author au = this.currentAuthor;
		
		String name = rtfAuthorName.getText().trim();
		String firstname = jtfAuthorFirstname.getText().trim();
		String nickname = rtfAuthorNickname.getText().trim();
		Date deathdate = jdpAuthorDeath.getDate();
		Date birthdate = jdpAuthorBirth.getDate();
		
		boolean hasChanged = false;
		
		// Check the name
		if(au.getName() == null && !name.isEmpty())
		{
			au.setName(name);
			hasChanged = true;
		}
		else if(au.getName() != null)
		{
			if(name.isEmpty())
			{
				au.setName(null);
				hasChanged = true;
			}
			else if(!name.equals(au.getName()))
			{
				au.setName(name);
				hasChanged = true;
			}
		}

		// Check the firstname
		if(au.getFirstName() == null && !firstname.isEmpty())
		{
			au.setFirstName(firstname);
			hasChanged = true;
		}
		else if(au.getFirstName() != null)
		{
			if(firstname.isEmpty())
			{
				au.setFirstName(null);
				hasChanged = true;
			}
			else if(!firstname.equals(au.getFirstName()))
			{
				au.setFirstName(firstname);
				hasChanged = true;
			}
		}
		
		// Check the nickname
		if(au.getNickName() == null && !nickname.isEmpty())
		{
			au.setNickName(nickname);
			hasChanged = true;
		}
		else if(au.getNickName() != null)
		{
			if(nickname.isEmpty())
			{
				au.setNickName(null);
				hasChanged = true;
			}
			else if(!nickname.equals(au.getNickName()))
			{
				au.setNickName(nickname);
				hasChanged = true;
			}
		}
		
		if(au.getBirth() == null && birthdate != null)
		{
			au.setBirth(birthdate);
			hasChanged = true;
		}
		else if(au.getBirth() != null)
		{
			if(birthdate == null)
			{
				au.setBirth(null);
				hasChanged = true;
			}
			else if(!au.getBirth().equals(birthdate))
			{
				au.setBirth(birthdate);
				hasChanged = true;
			}
		}
		
		if(au.getDeath() == null && deathdate != null)
		{
			au.setDeath(deathdate);
			hasChanged = true;
		}
		else if(au.getDeath() != null)
		{
			if(deathdate == null)
			{
				au.setDeath(null);
				hasChanged = true;
			}
			else if(!au.getDeath().equals(deathdate))
			{
				au.setDeath(deathdate);
				hasChanged = true;
			}
		}
		
		if(hasChanged)
		{
			SaveItemPersistency.saveAuthor(au);
			AlbumPane.getInstance().refresh();
		}
	}
	
	/**
	 * Use this method to show the AuthorAddDialog 
	 */
	public void display()
	{
		this.pack();
		this.setLocationRelativeTo(this.getParent());
		this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);
	}
	
// Classes
	class CancelActionListener implements ActionListener
	{
		@Override
		public void actionPerformed(ActionEvent ae)
		{
			AuthorAddDialog.this.dispose();
		}
	}

	class SaveActionListener implements ActionListener
	{		
		@Override
		public void actionPerformed(ActionEvent ae)
		{
			if(!AuthorAddDialog.this.rtfAuthorNickname.check() || !AuthorAddDialog.this.rtfAuthorName.check())
			{
				// Neither the name, nor the nickname are filled
				AuthorAddDialog.this.rtfAuthorNickname.setIncorrect();
				AuthorAddDialog.this.rtfAuthorName.setIncorrect();
				MessagePaneManager.showInfoPane("Veuillez renseigner le pseudo ou le nom de l'auteur");
			}
			else if(AuthorAddDialog.this.jdpAuthorBirth.getDate() != null && AuthorAddDialog.this.jdpAuthorDeath.getDate() != null &&
					AuthorAddDialog.this.jdpAuthorDeath.getDate().before(AuthorAddDialog.this.jdpAuthorBirth.getDate()))
			{	
				// Death date < Birth date				
				MessagePaneManager.showInfoPane("Veuillez renseigner une date de décès supérieur à la date de naissance");
			}
			else
			{
				AuthorAddDialog.this.saveAuthor();
				AuthorAddDialog.this.dispose();
			}
		}
	}
	public Long getID()
	{
		return this.currentAuthor.getId();
	}
}
