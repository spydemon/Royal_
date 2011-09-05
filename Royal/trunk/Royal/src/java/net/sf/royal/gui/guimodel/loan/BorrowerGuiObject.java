package net.sf.royal.gui.guimodel.loan;

import java.util.Date;

import net.sf.royal.datamodel.Borrower;

public class BorrowerGuiObject {

    private Long id;
    private String name;
    private String firstName;
    private String phone;
    private String email;
    private Date creation;
    private boolean inLate = false;
    
    public Date getCreation() {
        return creation;
    }
    public void setCreation(Date creation) {
        this.creation = creation;
    }
    public String getEmail() {
        return email;
    }
    public void setEmail(String email) {
        this.email = email;
    }
    public String getFirstName() {
        return firstName;
    }
    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }
    public Long getId() {
        return id;
    }
    public void setId(Long id) {
        this.id = id;
    }
    public String getName() {
        return name;
    }
    public void setName(String name) {
        this.name = name;
    }
    public String getPhone() {
        return phone;
    }
    public void setPhone(String phone) {
        this.phone = phone;
    }
    
    public static BorrowerGuiObject getBorrowerGuiObject(Borrower borrower){
        BorrowerGuiObject res = new BorrowerGuiObject();
        res.setCreation(borrower.getCreation());
        res.setEmail(borrower.getEmail());
        res.setFirstName(borrower.getFirstName());
        res.setId(borrower.getId());
        res.setName(borrower.getName());
        res.setPhone(borrower.getPhone());
        return res;
    }
    public boolean isInLate() {
        return inLate;
    }
    public void setInLate(boolean inLate) {
        this.inLate = inLate;
    }
}
