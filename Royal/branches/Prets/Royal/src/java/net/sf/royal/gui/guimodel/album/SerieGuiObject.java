package net.sf.royal.gui.guimodel.album;

import net.sf.royal.datamodel.Serie;
import net.sf.royal.gui.util.tables.VersionableImpl;

public class SerieGuiObject extends VersionableImpl {
	
	private Long id;
	private String name;
    private String description;
    private boolean closed;
    private boolean oneShot;
    private String web;
    private TypeGuiObject type;
    
    private Long tekId;
	
    public SerieGuiObject(){
        
    }
    
	public SerieGuiObject(Serie serie){
		this.id = serie.getId();
		this.name = serie.getName();
		this.description = serie.getDescription();
		this.closed = serie.isClosed();
		this.oneShot = serie.isOneShot();
		this.web = serie.getWeb();
		if (serie.getType() != null){
		    this.type = new TypeGuiObject(serie.getType());
		}
        this.tekId = serie.getTekId();
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
     * @return Returns the closed.
     */
    public boolean isClosed() {
        return closed;
    }

    /**
     * @param closed The closed to set.
     */
    public void setClosed(boolean closed) {
        this.closed = closed;
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
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * @return Returns the oneShot.
     */
    public boolean isOneShot() {
        return oneShot;
    }

    /**
     * @param oneShot The oneShot to set.
     */
    public void setOneShot(boolean oneShot) {
        this.oneShot = oneShot;
    }

    /**
     * @return Returns the type.
     */
    public TypeGuiObject getType() {
        return type;
    }

    /**
     * @param type The type to set.
     */
    public void setType(TypeGuiObject type) {
        this.type = type;
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

    public int compareTo(Object arg) {
        SerieGuiObject toCompare = (SerieGuiObject) arg;
        return this.name.compareTo(toCompare.getName());
    }

    public Long getTekId() {
        return tekId;
    }

    public void setTekId(Long tekId) {
        this.tekId = tekId;
    }
    
    /**
     * Standard equals function
     * @return boolean true if equals
     */
    @Override
    public boolean equals(Object o)
    {
		if(!(o instanceof SerieGuiObject))
			return false;
		SerieGuiObject sgo = (SerieGuiObject)o;
		return this.id == sgo.id &&
			   this.name.equals(sgo.name) &&
			   this.description.equals(sgo.description) &&
			   this.closed == sgo.closed &&
			   this.oneShot == sgo.oneShot &&
			   this.web.equals(sgo.web) &&
			   this.tekId == sgo.tekId &&
			   this.type.equals(sgo.type);
	}
}
