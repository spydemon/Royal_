package net.sf.royal.datamodel;

import java.util.Date;

public class Dedication extends ModelImpl implements Cloneable
{

    private Album album;
    private String imageURL;
    private Date date;
    private String location;
    private String description;

    /**
     * @return Returns the album.
     */
    public Album getAlbum() {
        return album;
    }

    /**
     * @param album The album to set.
     */
    public void setAlbum(Album album) {
        this.album = album;
    }

    /**
     * @return Returns the date.
     */
    public Date getDate() {
        return date;
    }

    /**
     * @param date The date to set.
     */
    public void setDate(Date date) {
        this.date = date;
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
     * @return Returns the location.
     */
    public String getLocation() {
        return location;
    }

    /**
     * @param location The location to set.
     */
    public void setLocation(String location) {
        this.location = location;
    }

    /**
     * @return Returns the description.
     */
    public String getDescription() {
        return description;
    }

    /**
     * @param description The description to set.
     */
    public void setDescription(String comment) {
        this.description = comment;
    }

	@Override
	public Object clone()
	{
		Dedication d = new Dedication();
		
		d.setAlbum(this.getAlbum());
		d.setDate(this.getDate());
		d.setDescription(this.getDescription());
		d.setId(this.getId());
		d.setImageURL(this.getImageURL());
		d.setLocation(this.getLocation());
		d.setTekId(this.getTekId());
		d.setVersion(this.getVersion());
		
		return d;
	}
    
    
}
