package net.sf.royal.gui.util.tables;

import net.sf.royal.datamodel.CommentedImage;

public class CommentedImageObject extends VersionableImpl {

    private Long id;
    private String imageURL;
    private String comment;
    private String valign;
    private int halign;
    private int position;
    
    public CommentedImageObject(){
        
    }
    
    public CommentedImageObject(CommentedImage ci){
        this.id = ci.getId();
        this.comment = ci.getComment();
        this.halign = ci.getHalign();
        this.position = ci.getPosition();
        this.valign = ci.getValign();
        this.imageURL = ci.getImageURL();
        
    }
    
    public CommentedImageObject(CommentedImage ci, int version) {
        this(ci);
        this.version = version;
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
     * @return Returns the id.
     */
    public Long getId() {
        return id;
    }
    /**
     * @param id The id to set.
     */
    public void setId(Long id) {
        this.id = id;
    }
    /**
     * @return Returns the imageURL.
     */
    public String getImageURL() {
        return imageURL;
    }
    /**
     * @param imageURL The imageURL to set.
     */
    public void setImageURL(String imageURL) {
        this.imageURL = imageURL;
    }

    /**
     * @return Returns the halign.
     */
    public int getHalign() {
        return halign;
    }

    /**
     * @param halign The halign to set.
     */
    public void setHalign(int halign) {
        this.halign = halign;
    }

    /**
     * @return Returns the position.
     */
    public int getPosition() {
        return position;
    }

    /**
     * @param position The position to set.
     */
    public void setPosition(int position) {
        this.position = position;
    }

    /**
     * @return Returns the valign.
     */
    public String getValign() {
        return valign;
    }

    /**
     * @param valign The valign to set.
     */
    public void setValign(String valign) {
        this.valign = valign;
    }
    
    public Object clone(){
        CommentedImageObject ci = new CommentedImageObject();
        ci.setId(this.getId());
        ci.setImageURL(this.getImageURL());
        ci.setComment(this.getComment());
        ci.setValign(this.getValign());
        ci.setHalign(this.getHalign());
        ci.setPosition(this.getPosition());
        ci.setVersion(this.version);
        return ci;
    }
    
    /**
     * Update the values from another commented image object
     * @param in
     */
    public void updateValuesFrom(CommentedImageObject in){
        this.setComment(in.getComment());
        this.setHalign(in.getHalign());
        this.setId(in.getId());
        this.setImageURL(in.getImageURL());
        this.setPosition(in.getPosition());
        this.setValign(in.getValign());
        this.setVersion(in.getVersion());
    }
    
    /**
     * Get the uppable state. It means that the commented object can be move on top
     * @return true if the position is greather than 0. 
     */
    public boolean isUppable(){
        if (this.position > 0){
            return Boolean.TRUE.booleanValue();
        }
        return Boolean.FALSE.booleanValue();
    }
    
    /**
     * Get the downable state. It means that the commented object can be move on bottom
     * @return true if the position is lesser than max position. 
     */
    public boolean isDownable(int max){
        if (this.position < max){
            return Boolean.TRUE.booleanValue();
        }
        return Boolean.FALSE.booleanValue();
    }
}
