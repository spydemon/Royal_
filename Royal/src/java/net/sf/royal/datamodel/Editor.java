package net.sf.royal.datamodel;

import java.util.HashSet;
import java.util.Set;


/**
 * 
 * @author bibounde
 *
 * <p>This class represents the editor of an album.</p>
 * <p>Persisted class</p>
 * 
 */

public class Editor extends ModelImpl implements Cloneable
{

    private String name;
    private String web;
    private String description;
    private Set<Collection> collections = new HashSet<Collection>();
    
    /**
     * @return Returns the description.
     */
    public String getDescription() {
        return description;
    }
    /**
     * @param description The description to set.
     */
    public void setDescription(String description) {
        this.description = description;
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
     * @return Returns the collections.
     */
    public Set<Collection> getCollections() {
        return collections;
    }
    
    /**
     * @param collections The collections to set.
     */
    public void setCollections(Set<Collection> collections) {
        this.collections = collections;
    }
    
    /**
     * Link a collection with the editor. Don't forget to "set" the collection 
     * @param collection Collection to link 
     * 
     */
    public void addCollection(Collection collection) {
        collections.add(collection);
    }
    
    /**
     * Unlink a collection from the editor. Don't forget to "unset" the collection
     * @param collection Collection to unlink
     */
    public void removeCollection(Collection collection) {
        collections.remove(collection);
    }
}
