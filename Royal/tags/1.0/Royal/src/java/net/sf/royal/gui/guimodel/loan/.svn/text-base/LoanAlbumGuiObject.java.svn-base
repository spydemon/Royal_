package net.sf.royal.gui.guimodel.loan;

import java.io.Serializable;
import java.util.Date;

public class LoanAlbumGuiObject implements Serializable {

    private Long albumID;
    private String serieName;
    private String label;
    private String imageURL;
    private boolean loaned;
    private String borrowerLabel;
    private Date loanDate;
    private Long loanID;
    private float priceByDay;
    private float penaltyByDay;
    private int maxDays;
    
    public Long getAlbumID() {
        return albumID;
    }
    public void setAlbumID(Long albumID) {
        this.albumID = albumID;
    }
    public String getBorrowerLabel() {
        return borrowerLabel;
    }
    public void setBorrowerLabel(String borrowerLabel) {
        this.borrowerLabel = borrowerLabel;
    }
    public String getImageURL() {
        return imageURL;
    }
    public void setImageURL(String imageURL) {
        this.imageURL = imageURL;
    }
    public String getLabel() {
        return label;
    }
    public void setLabel(String label) {
        this.label = label;
    }
    public Date getLoanDate() {
        return loanDate;
    }
    public void setLoanDate(Date loanDate) {
        this.loanDate = loanDate;
    }
    public boolean isLoaned() {
        return loaned;
    }
    public void setLoaned(boolean loaned) {
        this.loaned = loaned;
    }
    public String getSerieName() {
        return serieName;
    }
    public void setSerieName(String serieName) {
        this.serieName = serieName;
    }
    public Long getLoanID() {
        return loanID;
    }
    public void setLoanID(Long loanID) {
        this.loanID = loanID;
    }
    public float getPenaltyByDay() {
        return penaltyByDay;
    }
    public void setPenaltyByDay(float penaltyByDay) {
        this.penaltyByDay = penaltyByDay;
    }
    public float getPriceByDay() {
        return priceByDay;
    }
    public void setPriceByDay(float priceByDay) {
        this.priceByDay = priceByDay;
    }
    public int getMaxDays() {
        return maxDays;
    }
    public void setMaxDays(int maxDays) {
        this.maxDays = maxDays;
    }
    
    public boolean isInLate(){
        Date dateMax = new Date(this.getLoanDate().getTime() + (this.getMaxDays() * 86400000));
        Date now = new Date();
        return !dateMax.after(now);
    }
    
    public float getInProgessTotalPenalties(){
        Date dateMax = new Date(this.getLoanDate().getTime() + (this.getMaxDays() * 86400000));
        Date now = new Date();
        if (!dateMax.after(now)){
            int days = new Float((now.getTime() - dateMax.getTime()) / 86400000).intValue();
            return this.penaltyByDay * days;
        }
        return 0;
        
    }
    
    public String toString(){
        return "LAGO : " + this.loanID + "/album : " + this.getLabel();
    }
}
