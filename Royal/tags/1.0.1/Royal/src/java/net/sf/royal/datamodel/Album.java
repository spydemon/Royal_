package net.sf.royal.datamodel;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

public class Album extends ModelImpl implements Cloneable
{
    private String title;
    private Date registration;
    private String cover;
    private int note;
    private float price;
    private String state;
    private String isbn;
    private String dimension;
    private boolean integral = Boolean.FALSE.booleanValue();
    private boolean original = Boolean.TRUE.booleanValue();
    private boolean special = Boolean.FALSE.booleanValue();
    private int pageCount;
    private String comment;
    private int copies;
    private Dedication dedication;
    private Tome tome;
    private Date purchaseDate;
    
    private Serie serie;
    private Set<Work> works = new HashSet<Work>();
    private Set<CommentedImage> gallery = new HashSet<CommentedImage>();
    private Collection collection;

    /**
     * @return Returns the works.
     */
    public Set<Work> getWorks() {
        return works;
    }

    /**
     * @param works The works to set.
     */
    public void setWorks(Set<Work> works) {
        this.works = works;
    }
    
    public void addWork(Work work) {
        this.works.add(work);
    }
    
    public void removeWork(Work work) {
        this.works.add(work);
    }

    /**
     * @return Returns the comment.
     */
    public String getComment() {
        return comment;
    }

    /**
     * @param comment The comment to set.
     */
    public void setComment(String comment) {
        this.comment = comment;
    }

    /**
     * @return Returns the copies.
     */
    public int getCopies() {
        return copies;
    }

    /**
     * @param copies The copies to set.
     */
    public void setCopies(int copies) {
        this.copies = copies;
    }

    /**
     * @return Returns the cover.
     */
    public String getCover() {
        return cover;
    }

    /**
     * @param cover The cover to set.
     */
    public void setCover(String cover) {
        this.cover = cover;
    }

    /**
     * @return Returns the dimension.
     */
    public String getDimension() {
        return dimension;
    }

    /**
     * @param dimension The dimension to set.
     */
    public void setDimension(String dimension) {
        this.dimension = dimension;
    }

    /**
     * @return Returns the integral.
     */
    public boolean isIntegral() {
        return integral;
    }

    /**
     * @param integral The integral to set.
     */
    public void setIntegral(boolean integral) {
        this.integral = integral;
    }

    /**
     * @return Returns the isbn.
     */
    public String getIsbn() {
        return isbn;
    }

    /**
     * @param isbn The isbn to set.
     */
    public void setIsbn(String isbn) {
        this.isbn = isbn;
    }

    /**
     * @return Returns the note.
     */
    public int getNote() {
        return note;
    }


    /**
     * @param note The note to set.
     */
    public void setNote(int note) {
        this.note = note;
    }

    /**
     * @return Returns the original.
     */
    public boolean isOriginal() {
        return original;
    }

    /**
     * @param original The original to set.
     */
    public void setOriginal(boolean original) {
        this.original = original;
    }

    /**
     * @return Returns the pageCount.
     */
    public int getPageCount() {
        return pageCount;
    }

    /**
     * @param pageCount The pageCount to set.
     */
    public void setPageCount(int pageCount) {
        this.pageCount = pageCount;
    }

    /**
     * @return Returns the price.
     */
    public float getPrice() {
        return price;
    }

    /**
     * @param price The price to set.
     */
    public void setPrice(float price) {
        this.price = price;
    }

    /**
     * @return Returns the registration.
     */
    public Date getRegistration() {
        return registration;
    }

    /**
     * @param registration The registration to set.
     */
    public void setRegistration(Date registration) {
        this.registration = registration;
    }

    /**
     * @return Returns the special.
     */
    public boolean isSpecial() {
        return special;
    }

    /**
     * @param special The special to set.
     */
    public void setSpecial(boolean special) {
        this.special = special;
    }

    /**
     * @return Returns the state.
     */
    public String getState() {
        return state;
    }

    /**
     * @param state The state to set.
     */
    public void setState(String state) {
        this.state = state;
    }

    /**
     * @return Returns the title.
     */
    public String getTitle() {
        return title;
    }

    /**
     * @param title The title to set.
     */
    public void setTitle(String title) {
        this.title = title;
    }

    /**
     * @return Returns the collection.
     */
    public Collection getCollection() {
        return collection;
    }

    /**
     * @param collection The collection to set.
     */
    public void setCollection(Collection collection) {
        this.collection = collection;
    }

    /**
     * @return Returns the dedication.
     */
    public Dedication getDedication() {
        return dedication;
    }

    /**
     * @param dedication The dedication to set.
     */
    public void setDedication(Dedication dedication) {
        this.dedication = dedication;
    }

    /**
     * @return Returns the tome.
     */
    public Tome getTome() {
        return tome;
    }

    /**
     * @param tome The tome to set.
     */
    public void setTome(Tome tome) {
        this.tome = tome;
    }

    /**
     * @return Returns the purchaseDate.
     */
    public Date getPurchaseDate() {
        return purchaseDate;
    }

    /**
     * @param purchaseDate The purchaseDate to set.
     */
    public void setPurchaseDate(Date purchaseDate) {
        this.purchaseDate = purchaseDate;
    }

    /**
     * @return Returns the serie.
     */
    public Serie getSerie() {
        return serie;
    }

    /**
     * @param serie The serie to set.
     */
    public void setSerie(Serie serie) {
        this.serie = serie;
    }

    /**
     * @return Returns the gallery.
     */
    public Set<CommentedImage> getGallery() {
        return gallery;
    }
    /**
     * @param gallery The gallery to set.
     */
    public void setGallery(Set<CommentedImage> gallery) {
        this.gallery = gallery;
    }
    
    /**
     * Add one commented image
     * @param commentedImage
     */
    public void addCommentedImage(CommentedImage commentedImage){
        this.gallery.add(commentedImage);
    }
    
    /**
     * Remove one commented image
     * @param commentedImage
     */
    public void removeCommentedImage(CommentedImage commentedImage){
        this.gallery.remove(commentedImage);
    }

	@Override
	public String toString() {
		if(this.tome != null && this.tome.getNumber() != 0)
		{
			return "(" + this.getTome().getNumber() + ") " + this.getTitle();
		}
		return this.getTitle();
	}
}

