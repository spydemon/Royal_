package net.sf.royal.gui.manager;

import java.awt.event.KeyEvent;

/**
  * @author Soulou
  * Management of the shortcut for the application
  */
public class ShortcutManager
{
	/* MenuBar */
	public static char MENU_FILE = 'F';
	public static char MENU_HELP = 'E';
	public static char MENU_TOOLS = 'T';

	/* Menu actions */
	public static char QUIT = 'Q';
	public static char HELP = KeyEvent.VK_F1;
	public static char LOG = 'L';
	public static char ABOUT = 'T';
	public static char SETTINGS = 'S';
	public static char IMPORT = 'I';
	public static char ISBN = 'B';

	/* Album actions */
	public static char ADD = 'A';
	public static char EDIT = 'E';
	public static char DELETE = 'D';
	public static char SYNC = 'Y';
	public static char UPDATE = 'U';
	public static char DELETE_ = KeyEvent.VK_DELETE;
	
	/* Wizards action */
	public static char APPLY = 'P';
	public static char OK = 'O';
	public static char CANCEL = 'A';
	public static char CANCEL_ = 'N';

}
