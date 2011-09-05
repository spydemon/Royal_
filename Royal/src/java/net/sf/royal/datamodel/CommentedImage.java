package net.sf.royal.datamodel;

public class CommentedImage extends ModelImpl implements Cloneable
{
	public static final String TOP = "top";
    public static final String BOTTOM = "bottom";
    public static final String MIDDLE = "middle";
    
    public static final int LEFT = 0;
    public static final int RIGHT = 1;
    
    
    private String imageURL;
    private String comment;
    private String valign;
    private int halign;
    private int position;
    
    private Album album;
    
    /**
     * @return Returns the album.
     */
    public Album getAlbum() {
        return album;
    }
    /**
     * @param album The album to set.
     */
    public void setAlbum(Album a) {
        this.album = a;
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
    
    @Override
	public Object clone()
    {
		CommentedImage ci = new CommentedImage();
		
		ci.setAlbum(this.getAlbum());
		ci.setComment(this.getComment());
		ci.setHalign(this.getHalign());
		ci.setId(this.getId());
		ci.setImageURL(this.getImageURL());
		ci.setPosition(this.getPosition());
		ci.setTekId(this.getTekId());
		ci.setValign(this.getValign());
		ci.setVersion(this.getVersion());
		
		return ci;
	}   
}
