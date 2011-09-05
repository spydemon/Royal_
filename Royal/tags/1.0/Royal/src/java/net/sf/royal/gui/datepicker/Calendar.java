package net.sf.royal.gui.datepicker;

import java.awt.Component;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Date;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/**
 * @author bibounde
 * @author Soulou
 * Class representing the popup which appears when you click on a JDatePicker
 */
public class Calendar extends JPanel 
{
    private DatePlayer gridDay;
    private DatePlayer barMonth;
    private BarYear barYear;
    private Date currentDate = new Date();
    private Date displayedDate;
    private Date selectedDate;
    private JDatePicker datePicker;
    
    public Calendar(JDatePicker datePicker){
        this.datePicker = datePicker;
        if (this.datePicker.getDate() != null){
            this.selectedDate = this.datePicker.getDate();
            this.displayedDate = DateHelper.getFirstDateOfMonth(this.selectedDate);
        } else {
            this.displayedDate = DateHelper.getFirstDateOfMonth(this.currentDate);
        }
        this.gridDay = new GridDay(currentDate, this.displayedDate, this.selectedDate, this);
        this.gridDay.displayDate(this.displayedDate);
        this.barMonth = new BarMonth(currentDate, this.displayedDate);
        this.barMonth.displayDate(this.displayedDate);
        this.barYear = new BarYear(currentDate, this.displayedDate);
        this.barYear.displayDate(this.displayedDate);
        this.initLayout();
        this.initListener();
    }
    
    private void initLayout(){
        this.setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
        this.add((Component) barMonth);
        this.add((Component) gridDay);
        this.add((Component) barYear);
    }
    
    private void initListener(){
        ((BarMonth) this.barMonth).getPrevious().addMouseListener(new MouseAdapter(){
            public void mouseClicked(MouseEvent arg0) {
                setDate("month", "previous");
            }
        });
        ((BarMonth) this.barMonth).getNext().addMouseListener(new MouseAdapter(){
            public void mouseClicked(MouseEvent arg0) {
                setDate("month", "next");
            }
        });
        ((BarYear) this.barYear).getPrevious().addMouseListener(new MouseAdapter(){
            public void mouseClicked(MouseEvent arg0) {
                setDate("year", "previous");
            }
        });
        ((BarYear) this.barYear).getNext().addMouseListener(new MouseAdapter(){
            public void mouseClicked(MouseEvent arg0) {
                setDate("year", "next");
            }
        });
        ((BarYear) this.barYear).getYear().getDocument().addDocumentListener(new DocumentListener() {
			@Override
			public void removeUpdate(DocumentEvent e) 
			{	
				String year = ((BarYear)Calendar.this.barYear).getYear().getText(); 
				if(!(year.isEmpty()))
					setDate(((BarYear)Calendar.this.barYear).getYear().getText());
			}
			
			@Override
			public void insertUpdate(DocumentEvent e) 
			{
					setDate(((BarYear)Calendar.this.barYear).getYear().getText());
			}
			
			@Override
			public void changedUpdate(DocumentEvent e) {}
		});
    }
    
    /**
     * Set the displayed date of Calendar
     * @param typeDate month or year
     * @param typeAction previous or next
     */
    private void setDate(String typeDate, String typeAction)
    {
        Date tmp = null;
        if (typeDate.equals("month") && typeAction.equals("previous")){
            tmp = DateHelper.getPreviousDateByMonth(this.displayedDate);
        } else if (typeDate.equals("month") && typeAction.equals("next")){
            tmp = DateHelper.getNextDateByMonth(this.displayedDate);
        } else if (typeDate.equals("year") && typeAction.equals("previous")){
            tmp = DateHelper.getPreviousDateByYear(this.displayedDate);
        } else if (typeDate.equals("year") && typeAction.equals("next")){
            tmp = DateHelper.getNextDateByYear(this.displayedDate);
        }
        
        this.displayedDate = DateHelper.getFirstDateOfMonth(tmp);

        this.gridDay.displayDate(this.displayedDate);
        this.barMonth.displayDate(this.displayedDate);
        this.barYear.displayDate(this.displayedDate);
    }
    
    /**
     * Move the calendar to the given data
     * @param date
     */
    private void setDate(String year)
    {
    	java.util.Calendar c = java.util.Calendar.getInstance();
    	c.setTime(this.displayedDate);
    	
    	Date tmp = this.displayedDate;
    	
    	tmp = DateHelper.getDate(Integer.parseInt(year), 
    			c.get(java.util.Calendar.MONTH)+1, 
    			c.get(java.util.Calendar.DAY_OF_MONTH)+1);
    	
    	this.displayedDate = DateHelper.getFirstDateOfMonth(tmp);
    	
        this.gridDay.displayDate(this.displayedDate);
        this.barMonth.displayDate(this.displayedDate);	
        this.barYear.displayDate(this.displayedDate, false);
    }
    
    /**
     * This method is called when the user select a day in the gridDay
     *
     */
    public void setSelectedDate(){
        int year = this.barYear.getValue();
        int month = this.barMonth.getValue();
        int day = this.gridDay.getValue();
        selectedDate = DateHelper.getDate(year, month, day);
        this.gridDay.setSelectedDate(selectedDate);
        this.barMonth.setSelectedDate(selectedDate);
        this.barYear.setSelectedDate(selectedDate);
        this.datePicker.setDate(selectedDate);
    }
    
    /**
     * Notify to the JDatePicker that is must close popup
     *
     */
    public void notifyClose(){
        this.datePicker.closePopUp();
    }
}
