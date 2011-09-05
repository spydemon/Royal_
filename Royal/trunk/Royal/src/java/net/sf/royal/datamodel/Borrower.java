package net.sf.royal.datamodel;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

@SuppressWarnings("serial")
public class Borrower extends ModelImpl {

    private String name;
    private String firstName;
    private String phone;
    private String email;
    private Date creation;
    
    private Set<Loan> loans = new HashSet<Loan>();

    /**
     * @return Returns the firstName.
     */
    public String getFirstName() {
        return firstName;
    }

    /**
     * @param firstName The firstName to set.
     */
    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    /**
     * @return Returns the loans.
     */
    public Set<Loan> getLoans() {
        return loans;
    }

    /**
     * @param loans The loans to set.
     */
    public void setLoans(Set<Loan> loans) {
        this.loans = loans;
    }

    /**
     * @return Returns the name.
     */
    public String getName() {
        return name;
    }

    /**
     * @param name The name to set.
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return Returns the phone.
     */
    public String getPhone() {
        return phone;
    }

    /**
     * @param phone The phone to set.
     */
    public void setPhone(String phone) {
        this.phone = phone;
    }
    
    public void addLoan(Loan loan){
        this.loans.add(loan);
    }
    
    public void removeLoan(Loan loan){
        this.loans.remove(loan);
    }

    /**
     * @return Returns the creation.
     */
    public Date getCreation() {
        return creation;
    }

    /**
     * @param creation The creation to set.
     */
    public void setCreation(Date creation) {
        this.creation = creation;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }
}
