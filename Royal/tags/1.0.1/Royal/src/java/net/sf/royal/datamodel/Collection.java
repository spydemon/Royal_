package net.sf.royal.datamodel;

import java.util.HashSet;
import java.util.Set;

/**
 * 
 * @author bibounde
 * 
 * <p>This class represents the collection of an album. It is linked with an editor</p>
 * <p>Persisted class</p>
 * 
 */
public class Collection extends ModelImpl implements Cloneable
{

   private String name;
   private String web;
   private String description;
   private boolean head;
   private Editor editor;
   private Set<Album> albums = new HashSet<Album>();
   

    /**
     * @return Returns the description.
     */
    public String getDescription() {
        return description;
    }

	/**
     * @param description
     *            The description to set.
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * @return Returns the editor.
     */
    public Editor getEditor() {
        return editor;
    }

    /**
     * @param editor
     *            The editor to set.
     */
    public void setEditor(Editor editor) {
        this.editor = editor;
    }

    /**
     * @return Returns the name.
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     *            The name to set.
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return Returns the web.
     */
    public String getWeb() {
        return web;
    }

    /**
     * @param web The web to set.
     */
    public void setWeb(String web) {
        this.web = web;
    }

    /**
     * @return Returns the albums.
     */
    public Set<Album> getAlbums() {
        return albums;
    }

    /**
     * @param albums The albums to set.
     */
    public void setAlbums(Set<Album> albums) {
        this.albums = albums;
    }
    
    public void addAlbum(Album album){
        this.albums.add(album);
    }
    
    public void removeAlbum(Album album){
        this.albums.remove(album);
    }

    /**
     * @return Returns the head.
     */
    public boolean isHead() {
        return head;
    }

    /**
     * @param head The head to set.
     */
    public void setHead(boolean head) {
        this.head = head;
    }
}
