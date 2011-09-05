package net.sf.royal.gui.util;

import java.awt.Color;

public interface Checkable {
    
    public static Color errorBackGroundColor = new Color(250,107,96);

    /**
     * Check the checkable
     * Don't forget to run setCorrect and setIncorrect 
     * @return true if the check is correct
     */
    public boolean check();
    
    /**
     * Set a correct state.
     *
     */
    public void setCorrect();
    
    /**
     * Set a incorrect state
     *
     */
    public void setIncorrect();
}
