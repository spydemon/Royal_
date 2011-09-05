package net.sf.royal.gui.datepicker;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

public class DateHelper {

    public static DateHelper instance = new DateHelper();
    private static SimpleDateFormat dateFormatter = new SimpleDateFormat("M", new Locale("EN"));
    private static int[] months = {1,0,0,0,0,0,0,0,0,0,0,2};
    
    private DateHelper(){}
    
    /**
     * Get the previous date, eg the previous date of 1 dec 2005 is 31 nov 2005
     * @param date
     * @return
     */
    public static Date getPreviousDateByMonth(Date date){
        dateFormatter.applyPattern("M");
        int iMonth = new Integer(dateFormatter.format(date)).intValue();
        dateFormatter.applyPattern("yyyy");
        int iYear = new Integer(dateFormatter.format(date)).intValue();
        if (months[iMonth - 1] == 1){
            //Change the year
            iYear = iYear - 1;
            iMonth = 12;
        } else {
            iMonth = iMonth -1;
        }
        dateFormatter.applyPattern("yyyy/M");
        try {
            Date res = dateFormatter.parse(iYear + "/" + iMonth);
            return res;
        } catch (ParseException e) {
            throw(new IllegalArgumentException("The year and the month cannot be parsed"));
        }
    }
    
    /**
     * Get the previous date, eg the next date of 31 dec 2005 is 31 dec 2004
     * @param date
     * @return
     */
    public static Date getPreviousDateByYear(Date date){
        dateFormatter.applyPattern("M");
        int iMonth = new Integer(dateFormatter.format(date)).intValue();
        dateFormatter.applyPattern("d");
        int iDay = new Integer(dateFormatter.format(date)).intValue();
        dateFormatter.applyPattern("yyyy");
        int iYear = new Integer(dateFormatter.format(date)).intValue();
        iYear--;
        dateFormatter.applyPattern("yyyy/M/d");
        try {
            Date res = dateFormatter.parse(iYear + "/" + iMonth + "/" + iDay);
            return res;
        } catch (ParseException e) {
            throw(new IllegalArgumentException("The year, the month and the day cannot be parsed"));
        }
    }
    
    /**
     * Get the next date, eg the next date of 31 dec 2005 is 1 jan 2006
     * @param date
     * @return
     */
    public static Date getNextDateByMonth(Date date){
        dateFormatter.applyPattern("M");
        int iMonth = new Integer(dateFormatter.format(date)).intValue();
        dateFormatter.applyPattern("yyyy");
        int iYear = new Integer(dateFormatter.format(date)).intValue();
        if (months[iMonth - 1] == 2){
            //Change the year
            iYear = iYear + 1;
            iMonth = 1;
        } else {
            iMonth = iMonth + 1;
        }
        dateFormatter.applyPattern("yyyy/M");
        try {
            Date res = dateFormatter.parse(iYear + "/" + iMonth);
            return res;
        } catch (ParseException e) {
            throw(new IllegalArgumentException("The year and the month cannot be parsed"));
        }
    }
    
    /**
     * Get the next date, eg the next date of 31 dec 2005 is 31 dec 2006
     * @param date)
     * @return
     */
    public static Date getNextDateByYear(Date date){
        dateFormatter.applyPattern("M");
        int iMonth = new Integer(dateFormatter.format(date)).intValue();
        dateFormatter.applyPattern("d");
        int iDay = new Integer(dateFormatter.format(date)).intValue();
        dateFormatter.applyPattern("yyyy");
        int iYear = new Integer(dateFormatter.format(date)).intValue();
        iYear++;
        dateFormatter.applyPattern("yyyy/M/d");
        try {
            Date res = dateFormatter.parse(iYear + "/" + iMonth + "/" + iDay);
            return res;
        } catch (ParseException e) {
            throw(new IllegalArgumentException("The year, the month and the day cannot be parsed"));
        }
    }
    
    /**
     * Get the number of first day in month, i.e Monday -> 0, Sunday -> 7
     * @param date
     * @return
     */
    public static int getFirstDayOfMonth(Date date){
        if (date == null){
            throw(new IllegalArgumentException("The date should not be null"));
        }
        dateFormatter.applyPattern("E");
        String day = dateFormatter.format(date);
        if (day.equals("Sun")){
            return 6;
        } else if (day.equals("Mon")){
            return 0;
        } else if (day.equals("Tue")){
            return 1;
        } else if (day.equals("Wed")){
            return 2;
        } else if (day.equals("Thu")){
            return 3;
        } else if (day.equals("Fri")){
            return 4;
        } else if (day.equals("Sat")){
            return 5;
        }
        return 0;
    }
    
    /**
     * Get the first date of month, i.e 1 dec 2005 for 12 dec 2005
     * @param date
     * @return
     */
    public static Date getFirstDateOfMonth(Date date){
        dateFormatter.applyPattern("M");
        int iMonth = new Integer(dateFormatter.format(date)).intValue();
        dateFormatter.applyPattern("yyyy");
        int iYear = new Integer(dateFormatter.format(date)).intValue();
        dateFormatter.applyPattern("yyyy/M/d");
        try {
            Date res = dateFormatter.parse(iYear + "/" + iMonth + "/1");
            return res;
        } catch (ParseException e) {
            throw(new IllegalArgumentException("The first date of month cannot be parsed"));
        }
    }
    
    /**
     * Get the day of the month, i.e 28
     * @param date
     * @return
     */
    public static int getDayInMonth(Date date){
        dateFormatter.applyPattern("d");
        return new Integer(dateFormatter.format(date)).intValue();
    }
    
    /**
     * Get the month of the year, i.e 11
     * @param date
     * @return
     */
    public static int getMonthInYear(Date date){
        dateFormatter.applyPattern("M");
        return new Integer(dateFormatter.format(date)).intValue();
    }
    
    /**
     * Get the year , i.e 2005
     * @param date
     * @return
     */
    public static int getYear(Date date){
        dateFormatter.applyPattern("yyyy");
        return new Integer(dateFormatter.format(date)).intValue();
    }
    
    public static Date getDate(int year, int month, int day){
        dateFormatter.applyPattern("yyyy/M/d");
        try {
            Date res = dateFormatter.parse(year + "/" + month + "/" + day);
            return res;
        } catch (ParseException e) {
            throw(new IllegalArgumentException("The first date of month cannot be parsed"));
        }
    }
}
