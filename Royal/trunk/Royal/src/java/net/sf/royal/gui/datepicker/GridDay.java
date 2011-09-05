package net.sf.royal.gui.datepicker;

import java.awt.Color;
import java.awt.GridLayout;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Date;

import javax.swing.JPanel;

import net.sf.royal.gui.manager.LocaleManager;

public class GridDay extends JPanel implements DatePlayer{

    //private JLabel[] days = new JLabel[42];
    private DateLabel[] days = new DateLabel[49];
    private Date currentDate;
    private Date selectedDate;
    private Date displayedDate;
    private Calendar calendar;
    private int selectedDay = -1;
    
    public GridDay(Date currentDate, Date displayedDate, Date selectedDate, Calendar calendar){
        this.currentDate = currentDate;
        this.displayedDate = displayedDate;
        this.selectedDate = selectedDate;
        if (this.selectedDate != null){
            this.selectedDay = DateHelper.getDayInMonth(this.selectedDate);
        }
        this.calendar = calendar;
        this.setBackground(Color.white);
        initLayout();
        this.initListener();
    }
    
    private void initLayout() {
        // init days
        this.setLayout(new GridLayout(7,7));
        //th
        days[0] = new DateLabel(LocaleManager.getInstance().getString("mon"),0,this);
        days[0].setState(DateLabel.TITLE);
        this.add(days[0]);
        days[1] = new DateLabel(LocaleManager.getInstance().getString("tue"),1,this);
        days[1].setState(DateLabel.TITLE);
        this.add(days[1]);
        days[2] = new DateLabel(LocaleManager.getInstance().getString("wed"),2,this);
        days[2].setState(DateLabel.TITLE);
        this.add(days[2]);
        days[3] = new DateLabel(LocaleManager.getInstance().getString("thu"),3,this);
        days[3].setState(DateLabel.TITLE);
        this.add(days[3]);
        days[4] = new DateLabel(LocaleManager.getInstance().getString("fri"),4,this);
        days[4].setState(DateLabel.TITLE);
        this.add(days[4]);
        days[5] = new DateLabel(LocaleManager.getInstance().getString("sat"),5,this);
        days[5].setState(DateLabel.TITLE);
        this.add(days[5]);
        days[6] = new DateLabel(LocaleManager.getInstance().getString("sun"),6,this);
        days[6].setState(DateLabel.TITLE);
        this.add(days[6]);
        
        for (int i = 7; i < days.length; i++) {
            days[i] = new DateLabel("",i,this);
            this.add(days[i]);
        }
        
    }
    
    /**
     * Get the number of day in the month, i.e for december it's 31
     * @return
     */
    private int getSelectedDayCount() {
        int month = DateHelper.getMonthInYear(displayedDate);
        int year = DateHelper.getYear(displayedDate)- 1;
        if (month == 1 || month == 3 || month == 5 || month == 7 || month == 8
                || month == 10 || month == 12)
            return 31;

        if (month == 2) {
            if ((year % 4 == 0 && year % 100 != 0) || year % 400 == 0)
                return 29;
            else
                return 28;
        }
        return 30;
    }
    
    /**
     * Get the value to display in label
     * @param labelLocation
     * @return
     */
    private int getLabelValue(int labelLocation){
        int dayOfMonth = this.getSelectedDayCount();
        int dayInWeek = DateHelper.getFirstDayOfMonth(this.displayedDate) + 6;
        int delta = labelLocation - dayInWeek;
        if (delta > 0 && delta <= dayOfMonth){
            return delta;
        } else {
            return -1;
        }
    }
    
    /**
     * Get the label location 
     * @param labelValue
     * @return
     */
    private int getLabelLocation(int labelValue){
        if (labelValue == -1){
            return -1;
        } else {
            return DateHelper.getFirstDayOfMonth(this.displayedDate) + 6 + labelValue;
        }
    }
    
    /**
     * Get the label location of the current day
     * @return
     */
    private int getLabelLocationCurrentDay(){
        int currentYear = DateHelper.getYear(currentDate);
        int displayedYear = DateHelper.getYear(displayedDate);
        int currentMonth = DateHelper.getMonthInYear(currentDate);
        int displayedMonth = DateHelper.getMonthInYear(displayedDate);
        if (currentMonth == displayedMonth && currentYear == displayedYear){
            int day = DateHelper.getDayInMonth(currentDate);
            return this.getLabelLocation(day);
        } else {
            return -1;
        }
    }
    
    /**
     * Get the label location of the current day
     * @return
     */
    private int getLabelLocationSelectedDay(){
        if (selectedDate != null){
            int selectedYear = DateHelper.getYear(selectedDate);
            int displayedYear = DateHelper.getYear(displayedDate);
            int selectedMonth = DateHelper.getMonthInYear(selectedDate);
            int displayedMonth = DateHelper.getMonthInYear(displayedDate);
            if (selectedMonth == displayedMonth && selectedYear == displayedYear){
                return this.getLabelLocation(selectedDay);
            } else {
                return -1;
            }
        }
        return -1;
    }
    
    /**
     * Return true if i is the location of a saturday or sunday
     * @param i
     * @return
     */
    private boolean isSold(int i){
        if (i == 12 || i == 13 || i == 19 || i == 20 || i == 26 || i == 27 || i == 33 || i == 34 || i == 40 || i == 41 || i == 47 || i == 48){
            return Boolean.TRUE.booleanValue();
        } else {
            return Boolean.FALSE.booleanValue();
        }
    }

    public void displayDate(Date date) {
        this.displayedDate = date;
        for (int i = 7; i < days.length; i++) {
            int delta = this.getLabelValue(i);
            days[i].setText(String.valueOf(delta));
            if (delta < 0){
                days[i].setState(DateLabel.INVISIBLE);
            } else {
                if (this.getLabelLocationSelectedDay() == i){
                    days[i].setState(DateLabel.SELECTED);
                } else if (this.getLabelLocationCurrentDay() == i){
                    days[i].setState(DateLabel.CURRENT);
                } else if (this.isSold(i)){
                    days[i].setState(DateLabel.SOLD);
                } else {
                    days[i].setState(DateLabel.ONLYVISIBLE);
                }
            }
        }
    }

    /**
     * @param labelLocation index of the label location.
     */
    public void setSelectedDay(int labelLocation) {
        this.selectedDay = this.getLabelValue(labelLocation);
        calendar.setSelectedDate();
    }

    public int getValue() {
        return this.selectedDay;
    }

    public void setSelectedDate(Date date) {
        this.selectedDate = date;
        this.selectedDay = DateHelper.getDayInMonth(date);
        this.displayDate(this.displayedDate);
    }
    
    private void initListener(){
        this.addMouseListener(new MouseAdapter(){
            public void mouseClicked(MouseEvent e) {
                if (e.isPopupTrigger()){
                    calendar.notifyClose();
                } else if (e.getClickCount() >= 2){
                    calendar.notifyClose();
                } else {
                    DateLabel label = (DateLabel) GridDay.this.getComponentAt(e.getPoint());
                    if (label.getState() != DateLabel.TITLE && label.getState() != DateLabel.INVISIBLE){
                        GridDay.this.setSelectedDay(label.getLabelLocation());
                    }
                }
            }

            public void mouseEntered(MouseEvent e) {
                if (e.isPopupTrigger()){
                    calendar.notifyClose();
                }
            }

            public void mouseExited(MouseEvent e) {
                if (e.isPopupTrigger()){
                    calendar.notifyClose();
                }
            }

            public void mousePressed(MouseEvent e) {
                if (e.isPopupTrigger()){
                    calendar.notifyClose();
                }
            }

            public void mouseReleased(MouseEvent e) {
                if (e.isPopupTrigger()){
                    calendar.notifyClose();
                }
            } 
        });
    }
}
