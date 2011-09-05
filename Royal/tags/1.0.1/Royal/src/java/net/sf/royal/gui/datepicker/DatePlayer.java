package net.sf.royal.gui.datepicker;

import java.util.Date;

public interface DatePlayer {

    /**
     * Display the date
     * @param date
     */
    public void displayDate(Date date);
    
    /**
     * Get the value of the DatePlayer (month, year or day)
     * @return
     */
    public int getValue();
    
    /**
     * Set the selected date
     * @param date
     */
    public void setSelectedDate(Date date);
}
