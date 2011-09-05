package net.sf.royal.gui.util;

import java.awt.Color;

import javax.swing.JTextField;

import net.sf.royal.gui.manager.LocaleManager;

public class RegexpTextField extends JTextField implements Checkable {
    
    public final static String DIMENSION = "^[0-9]+[.,]?[0-9]*$";
    public final static String NUMBER = "^[0-9]*$";
    public final static String YEAR = "^[0-9]{4}$";
    public final static String FLOAT = "^[0-9]*(.[0-9])*$";
    public final static String TOME = "tome";
    public final static String LOGIN = "login";
    public final static String LOGIN_DB = "logindb";
    public final static String NONEMPTY = "^[éèêôàïa-zA-Z0-9 .-_!?']+$";
    public final static String WEB = "^http://.*{32}\\.[a-z0-9]{2,4}$";
    
    /**
     * The regexp to use in this object
     */
    private String regexp;
    /**
     * Determine if the field can be empty or not. <br/>
     * If true, the field cannot be empty
     */
    private boolean manadatory = Boolean.FALSE.booleanValue();
    
    public RegexpTextField(int row, String regexp){
        super(row);
        this.regexp = regexp;
        //this.setOpaque(Boolean.TRUE.booleanValue());
    }
    
    public RegexpTextField(String regexp)
    {
    	super();
    	this.regexp = regexp;
        //this.setOpaque(Boolean.TRUE.booleanValue());
    }
    
    public RegexpTextField(int row, String regexp, boolean mandatory){
        this(row, regexp);
        this.manadatory = mandatory;
    }

    /**
     * Checks if the value of the field matches the regexp <br/>
     * Will also change the background color to red if the return value is false.
     * @return True if the value matches the regexp
     */
    public boolean check() 
    {
        String text = this.getText();
        boolean res = Boolean.FALSE.booleanValue();
        if (text != null && !text.equals(""))
        {
            if (regexp.equals(TOME))
            {
                String[] split = text.split("\\.");
                res = split[0].matches("^[0-9]+$");
                res &= !text.endsWith(".");
                if (split.length > 1)
                {
                    res &= split[1].matches("^[0-9]*[a-z]+$");
                }
            }
            else if (regexp.equals(LOGIN))
            {
                res = text.matches("^[a-z,A-Z,0-9]*$");
            }
            else if (regexp.equals(LOGIN_DB))
            {
                res = !text.equals(LocaleManager.getInstance().getString("to_change"));
                res &= text.matches("^[a-z,A-Z,0-9]*$");
            }
            else if(regexp.equals(DIMENSION))
            {
            	text.replace(',', '.');
            	res = text.matches(this.regexp);
            }
            else 
            {
                res = text.matches(this.regexp);
            }
        } 
        else
        {
            res = Boolean.TRUE.booleanValue() && !this.manadatory;
        }
        if (res)
        {
            this.setCorrect();
        }
        else
        {
            this.setIncorrect();
        }
        return res;
    }

    public void setCorrect() {
        this.setForeground(Color.BLACK);
        this.setBackground(Color.WHITE);
    }

    public void setIncorrect() {
        this.setForeground(Color.WHITE);
        this.setBackground(Checkable.errorBackGroundColor);
    }
    
    
}
