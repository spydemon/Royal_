package net.sf.royal.gui.manager;

import java.text.NumberFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.ResourceBundle;

public class LocaleManager {

    public static Locale FRENCH = new Locale("fr","FR");
    public static Locale ENGLISH = new Locale("en","GB");
    
    private static LocaleManager instance;
    
    private Locale currentLocale;
    private SimpleDateFormat dateFormatter;
    private ResourceBundle resourceBundle;
    private NumberFormat nf;

    private LocaleManager()
    {
    }
    
    public String getString(String key){
		String str = key;
		if(this.resourceBundle.containsKey(key))
			str = this.resourceBundle.getString(key);
		else
		{
			System.err.println("Error, missing key : " + key);
		}
		return str;		
    }
    
    public String getTextDate(Date date, boolean more){
        if (date == null){
            return "";
        }
        if (more){
            this.dateFormatter.applyPattern("d MMMM yyyy");
        } else {
            this.dateFormatter.applyPattern("MMMM yyyy");
        }
        return this.dateFormatter.format(date);
    }
    
    public Date getDate(String date, boolean more) throws ParseException{
        if (more){
            this.dateFormatter.applyPattern("d MMMM yyyy");
        } else {
            this.dateFormatter.applyPattern("MMMM yyyy");   
        }
        return this.dateFormatter.parse(date);
    }
    
    public Date getMonthDate(String date)  throws ParseException {
        this.dateFormatter.applyPattern("MM-yyyy");
        return this.dateFormatter.parse(date);
    }
    
    /**
     * Get the month of a date
     * @param date
     * @return String
     */
    public String getMonthDate(Date date){
        this.dateFormatter.applyPattern("MMMM");
        return this.dateFormatter.format(date);
    }
    
    /**
     * Get the year of a date
     * @param date
     * @return
     */
    public String getYearDate(Date date){
        this.dateFormatter.applyPattern("yyyy");
        return this.dateFormatter.format(date);
    }
    
    public String getPriceText(float price){
        this.nf = NumberFormat.getCurrencyInstance(this.currentLocale);
        String res = nf.format(price);
        if (PropertyManager.getInstance().getProperty("show_currency").equals("true")){
            return res;
        } else {
            return res.substring(0, res.length() -1);
        }
    }
    
    public float getPriceFloat(String text) throws NumberFormatException{
        float res = -1;
        try {
            res = ((Number) this.nf.parse(text)).floatValue();
        } catch (Exception e) {
            res = Float.parseFloat(text.replaceAll(",", "."));
        }
        return res;
    }
    
    /**
     * @return Returns the currentLocale.
     */
    public Locale getCurrentLocale() {
        return currentLocale;
    }

       
    public Locale loadLocale(){
        String language = PropertyManager.getInstance().getProperty("language");
        if (language.equals("english")){
            this.currentLocale = LocaleManager.ENGLISH;
        } else if (language.equals("french")){
            this.currentLocale = LocaleManager.FRENCH;
        } else {
            this.currentLocale = LocaleManager.ENGLISH;
        }
        this.resourceBundle = ResourceBundle.getBundle("allwords", currentLocale);
        this.dateFormatter = new SimpleDateFormat("MMMM yyyy", this.currentLocale);
        nf = NumberFormat.getCurrencyInstance(this.currentLocale);
        return this.currentLocale;
    }
    
    public String[] getAvailableLanguages(){
        String[] res = {"french", "english"};
        return res;
    }
    
    public String getHelpLocale(){
        if (this.currentLocale.equals(ENGLISH)){
            return "en";
        } else if (this.currentLocale.equals(FRENCH)){
            return "fr";
        } else {
            return "en";
        }
    }
    
    public String getWelcomeLocale(){
        if (this.currentLocale.equals(ENGLISH)){
            return "welcome_en.html";
        } else if (this.currentLocale.equals(FRENCH)){
            return "welcome_fr.html";
        } else {
            return "welcome_en.html";
        }
    }
    
    public static LocaleManager getInstance()
    {
    	if(instance == null)
    	{
    		instance = new LocaleManager();
    	}
    	return instance;
    }
    
    public static void shutdown()
    {
    	instance = null;
    }
}
