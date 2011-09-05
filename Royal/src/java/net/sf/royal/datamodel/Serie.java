package net.sf.royal.datamodel;

import java.util.HashSet;
import java.util.Set;

public class Serie extends ModelImpl implements Cloneable {
     
    private String name;
    private String description;
    private boolean closed;
    private boolean oneShot;
    private String web;
    private Type type;
    
    private Set<Album> albums = new HashSet<Album>();
    
    /**
     * @return Returns the description.
     */
    public String getDescription() 
    {
        return description;
    }
    
	/**
     * @param description The description to set.
     */
    public void setDescription(String description) 
    {
        this.description = description;
    }
    /**
     * @return Returns the name.
     */
    public String getName() 
    {
        return name;
    }
    /**
     * @param name The name to set.
     */
    public void setName(String name) 
    {
        this.name = name;
    }
    /**
     * @return Returns the state.
     */

    /**
     * @return Returns the web.
     */
    public String getWeb() 
    {
        return web;
    }
    /**
     * @param web The web to set.
     */
    public void setWeb(String web) 
    {
        this.web = web;
    }
    /**
     * @return Returns the closed.
     */
    public boolean isClosed() 
    {
        return closed;
    }
    /**
     * @param closed The closed to set.
     */
    public void setClosed(boolean closed) 
    {
        this.closed = closed;
    }
    /**
     * @return Returns the oneShot.
     */
    public boolean isOneShot() 
    {
        return oneShot;
    }
    /**
     * @param oneShot The oneShot to set.
     */
    public void setOneShot(boolean oneShot) 
    {
        this.oneShot = oneShot;
    }
    /**
     * @return Returns the albums.
     */
    public Set<Album> getAlbums() 
    {
        return albums;
    }
    /**
     * @param albums The albums to set.
     */
    public void setAlbums(Set<Album> albums) 
    {
        this.albums = albums;
    }
    
    public void addAlbum(Album album)
    {
        this.albums.add(album);
    }
    
    public void removeAlbum(Album album)
    {
        this.albums.remove(album);
    }
    /**
     * @return Returns the type.
     */
    public Type getType() 
    {
        return type;
    }
    /**
     * @param type The type to set.
     */
    public void setType(Type type) 
    {
        this.type = type;
    }

	@Override
	public String toString() 
	{
		return this.getName();
	}
}
