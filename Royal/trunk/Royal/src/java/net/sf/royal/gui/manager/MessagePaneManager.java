package net.sf.royal.gui.manager;

import java.util.List;

import javax.swing.JOptionPane;

import net.sf.royal.gui.util.FileDialog;
import net.sf.royal.gui.util.LogDialog;


/**
 * Class which manages message dialogs (errors/warning/confirmation)
 * @author bibounde
 */
public class MessagePaneManager {

	/**
	 * Instance of the object we must use
	 */
    public static MessagePaneManager instance = new MessagePaneManager();
    
    private MessagePaneManager(){
        
    }
    
    /**
     * Show an information pane
     * @param text object String which will be displayed 
     */
    public static void showInfoPane(String text){
        JOptionPane.showMessageDialog(null, 
                                      text, 
                                      "info", 
                                      JOptionPane.INFORMATION_MESSAGE);
    }
    
    /**
     * Show an error dialog
     */
    public static void showCheckErrorPane(){
        JOptionPane.showMessageDialog(null, LocaleManager.getInstance().getString("field_incorrect"), LocaleManager.getInstance().getString("error_input"), JOptionPane.ERROR_MESSAGE);
    }
    
    /**
     * Show an error pane with a personalize message
     * @param compMessage object String which will be displayed 
     */
    public static void showCheckErrorPane(String compMessage){
        JOptionPane.showMessageDialog(null, compMessage, LocaleManager.getInstance().getString("error_input"), JOptionPane.ERROR_MESSAGE);
    }
    
    /**
     * if critical, show an error pane, else a warning pane
     * @param critical true if the error is critical and false else
     */
    public static void showMultiSelectionErrorPane(boolean critical){
        if (PropertyManager.getInstance().getProperty("show_pane_selection").equals("true")){
            int type;
            String message;
            if (critical){
                type = JOptionPane.ERROR_MESSAGE;
                message = LocaleManager.getInstance().getString("multiselection_error_critical");
            } else {
                type = JOptionPane.WARNING_MESSAGE;
                message = LocaleManager.getInstance().getString("multiselection_error");
            }
            JOptionPane.showMessageDialog(null, message, LocaleManager.getInstance().getString("error_selection"), type);
        }
    }
    
    /**
     * Show a confirm deletion pane
     * @param object String which will be displayed 
     * @return true if the user confirm deletion, false otherwise
     */
    public static boolean showConfirmDeletion(String object){
        int res = JOptionPane.showConfirmDialog(null, object, LocaleManager.getInstance().getString("confirm_delete"), JOptionPane.YES_NO_OPTION);
        if (res == JOptionPane.YES_OPTION){
            return Boolean.TRUE.booleanValue();
        }
        return Boolean.FALSE.booleanValue();
    }
    
    /**
     * Show a confirmation pane
     * @param object String which will be displayed 
     * @return true if the user confirm deletion, false otherwise
     */
    public static boolean showConfirmation(String message){
        int res = JOptionPane.showConfirmDialog(null, message, LocaleManager.getInstance().getString("confirmation"), JOptionPane.YES_NO_OPTION);
        if (res == JOptionPane.YES_OPTION){
            return Boolean.TRUE.booleanValue();
        }
        return Boolean.FALSE.booleanValue();
    }
    
    /**
     * Ask to the user if he wants to restart the application
     * Called after that the user change an important setting
     * @return boolean true if the user want to restart the app
     */
    public static boolean showRestartConfirmation()
    {
    	int res = JOptionPane.showConfirmDialog(null, LocaleManager.getInstance().getString("confirm_restart"), 
    			LocaleManager.getInstance().getString("confirmation"), JOptionPane.YES_NO_OPTION);
    	if(res == JOptionPane.YES_OPTION)
    	{
    		return Boolean.TRUE.booleanValue();
    	}
    	return Boolean.FALSE.booleanValue();
    }
    
    /**
     * Show a warning pane when the user tries to select the disblaed chooser
     *
     */
    public static void showChooserDisablePane(){
        if (PropertyManager.getInstance().getProperty("show_pane_chooser").equals("true")){            
            JOptionPane.showMessageDialog(null, LocaleManager.getInstance().getString("chooser_error_enabled"), LocaleManager.getInstance().getString("forbidden_action"), JOptionPane.WARNING_MESSAGE);
        }
    }
    
    public static void showChooserNoSelectionPane(){
        JOptionPane.showMessageDialog(null, LocaleManager.getInstance().getString("chooser_error_selection"), LocaleManager.getInstance().getString("error_selection"), JOptionPane.WARNING_MESSAGE);
    }
    
    public static void showUnRemovableDockablePane(String dockableName){
        JOptionPane.showMessageDialog(null, LocaleManager.getInstance().getString("no_close_view") + " : " + dockableName, LocaleManager.getInstance().getString("perspective_error"), JOptionPane.ERROR_MESSAGE);
    }
    
    /**
     * Show an exception pane. 
     * @param e exception
     * @param fatal 
     * @return true if the user want to quit
     */
    public static boolean showExceptionPane(Throwable e, boolean fatal){
        if (fatal){
            JOptionPane.showMessageDialog(null,
                    LocaleManager.getInstance().getString("exception") + "\n    - " + e.getMessage() + "\n\n" + LocaleManager.getInstance().getString("fatal_exception"),
                    LocaleManager.getInstance().getString("exception_error"),
                    JOptionPane.ERROR_MESSAGE);
            return Boolean.TRUE.booleanValue();
        } else {
            int res = JOptionPane.showConfirmDialog(null,
                    LocaleManager.getInstance().getString("exception") + "\n    - " + e.getMessage() + "\n\n" + LocaleManager.getInstance().getString("confirm_exception"),
                    LocaleManager.getInstance().getString("exception_error"),
                    JOptionPane.WARNING_MESSAGE,
                    JOptionPane.YES_NO_OPTION);
            if (res == JOptionPane.YES_OPTION){
                return Boolean.TRUE.booleanValue();
            }
            return Boolean.FALSE.booleanValue();
        }
        
    }
    
    public static int showWouldYouSaveView(){
        return JOptionPane.showConfirmDialog(null,
                LocaleManager.getInstance().getString("save_view_before_edit_close"),
                LocaleManager.getInstance().getString("confirm"),
                JOptionPane.YES_NO_CANCEL_OPTION);
    }
    
    /**
     * Show the about dialog
     */
    public static void showAboutDialog(){
        FileDialog dialog = new FileDialog(PropertyManager.getInstance().getProperty("fileAbout"), LocaleManager.getInstance().getString("about"), Boolean.TRUE.booleanValue(), Boolean.TRUE.booleanValue());
        dialog.pack();
        dialog.setLocationRelativeTo(null);
        dialog.setVisible(Boolean.TRUE.booleanValue());
    }
    
    /**
     * Show the log dialog
     */
    public static void showLogDialog(){
        LogDialog dialog = new LogDialog(PropertyManager.getInstance().getProperty("fileLog"), LocaleManager.getInstance().getString("see_log"));        
        dialog.pack();
        dialog.setLocationRelativeTo(null);
        dialog.setVisible(Boolean.TRUE.booleanValue());
    }
    
    /**
     * Show the property pane
     */
    public static void showPropertyPane(){
        JOptionPane.showMessageDialog(null,
                LocaleManager.getInstance().getString("properties_changed"),
                LocaleManager.getInstance().getString("warning"),
                JOptionPane.WARNING_MESSAGE);
    }
    
    /**
     * Show the process pane
     * Appears if the lock file is present
     * @see ProcessManager
     */
    public static void showProcessPane(){
        JOptionPane.showMessageDialog(null,
                LocaleManager.getInstance().getString("process_running"),
                LocaleManager.getInstance().getString("fatal_exception"),
                JOptionPane.ERROR_MESSAGE);
    }
    
    /**
     * Show the dialog to rename an image
     * @param oldName
     * @return
     */
    public static String showChooseNewName(String oldName){
        return JOptionPane.showInputDialog(null, LocaleManager.getInstance().getString("image_exist"), oldName);
    }

    /**
     * Show an error pane about files which are not found
     * @param errorPaths Paths of the different unfound files
     */
    public static void showArchiErrorPane(List<String> errorPaths){
        StringBuffer buff = new StringBuffer(LocaleManager.getInstance().getString("archi_error")).append("\n");
        for(int i=0;i<errorPaths.size();i++){
            buff.append("  - ").append(errorPaths.get(i)).append("\n");
        }
        JOptionPane.showMessageDialog(null,
                buff.toString(),
                LocaleManager.getInstance().getString("fatal_exception"),
                JOptionPane.ERROR_MESSAGE); 
    }
    //TODO: wrap word....
}
