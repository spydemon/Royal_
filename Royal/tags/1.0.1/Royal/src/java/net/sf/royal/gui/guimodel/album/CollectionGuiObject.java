package net.sf.royal.gui.guimodel.album;

import net.sf.royal.datamodel.Collection;
import net.sf.royal.gui.util.tables.VersionableImpl;

public class CollectionGuiObject extends VersionableImpl {

	private Long id;
	private String name;
    private boolean head;
    private String web;
    private String comment;
    private Long tekId;
	
	public CollectionGuiObject(Long id, String name, boolean head, String web, String comment) {
		this.id = id;
		this.name = name;
        this.head = head;
        this.web = web;
        this.comment = comment;
	}
    
    public CollectionGuiObject(Collection collection){
        this.id = collection.getId();
        this.name = collection.getName();
        this.head = collection.isHead();
        this.web = collection.getWeb();
        this.comment = collection.getDescription();
        this.tekId = collection.getTekId();
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
	
    public String toString(){
        return "CollectionGO : " + "\n  id -> " + this.id
                                   + "\n  name -> " + this.name
                                   + "\n  web  -> " + this.web
                                   + "\n  description -> " + this.comment
                                   + "\n  version -> " + this.getVersion()
                                   + "\n  head -> " + this.head;
    }

    public Long getTekId() {
        return tekId;
    }

    public void setTekId(Long tekId) {
        this.tekId = tekId;
    }
}
