/**
 * This class is used to remember the locale used by Royal.
 */
package net.sf.birdy.help.core.locale;

public class LocaleManager {

    public static final String FR = "fr";
    public static final String EN = "en";
    
    private String locale = FR;

    private static LocaleManager instance = new LocaleManager();
    
	/**
	 * The default constructor LocaleManager.<br/>
	 * Use the method getInstance in order to get an instance.
	 */
    private LocaleManager(){
        
    }
    
	/**
	 * Set the used locale
	 */
    public String getLocale() {
        return locale;
    }

	/**
	 * Set the locale to use.
	 *
	 * @param locale The selected locale
	 */
    public void setLocale(String locale) {
        this.locale = locale;
    }

	/**
	 * Get the instance of the LocaleManager
	 *
	 * @return The instance of LocaleManager
	 */
    public static LocaleManager getInstance() {
        return instance;
    }
    
    
    
}
