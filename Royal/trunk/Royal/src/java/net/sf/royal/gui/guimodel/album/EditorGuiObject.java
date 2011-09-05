package net.sf.royal.gui.guimodel.album;

import net.sf.royal.datamodel.Editor;
import net.sf.royal.gui.util.tables.VersionableImpl;

public class EditorGuiObject extends VersionableImpl {

	private Long id;
	private String name;
    private String comment;
    private String web;
    private Long tekId;
    
    public EditorGuiObject(){
        
    }
	
	public EditorGuiObject(Long id, String name, String comment, String web) {
		this.id = id;
		this.name = name;
		this.comment = comment;
		this.web = web;
	}
    
    public EditorGuiObject(Editor editor){
        this.id = editor.getId();
        this.name = editor.getName();
        this.comment = editor.getDescription();
        this.web = editor.getWeb();
        this.tekId = editor.getTekId();
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
	    return "EditorGO : " + "\n  id -> " + this.id
                                   + "\n  name -> " + this.name
                                   + "\n  web  -> " + this.web
                                   + "\n  description -> " + this.comment
                                   + "\n  version -> " + this.getVersion();
    }

    public Long getTekId() {
        return tekId;
    }

    public void setTekId(Long tekId) {
        this.tekId = tekId;
    }
}
