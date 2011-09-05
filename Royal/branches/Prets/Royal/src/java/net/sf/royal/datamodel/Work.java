package net.sf.royal.datamodel;

/**
 * Represents the link between an Author and an Album,
 * i.e. the role of the author.<br/>
 * This class only represents the link.<br/>
 * For the job : 
 * @see Illustration
 * @see Color
 * @see Editor
 * 
 * @author steven
 *
 */
@SuppressWarnings("serial")
public class Work extends ModelImpl implements Cloneable
{

    protected Author author;
    protected Album album;

    /**
     * @return Returns the author.
     */
    public Author getAuthor() {
        return author;
    }

    /**
     * @param author The author to set.
     */
    public void setAuthor(Author author) {
        this.author = author;
    }

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

	@Override
	public Object clone()
	{
		Work w;
		if(this instanceof Scenario)
			w = new Scenario();
		else if(this instanceof Illustration)
			w = new Illustration();
		else 
			w = new Color();
		
		w.setAlbum(this.getAlbum());
		w.setAuthor(this.getAuthor());
		w.setId(this.getId());
		w.setTekId(this.getTekId());
		w.setVersion(this.getVersion());
		
		return w;
	}    
}
