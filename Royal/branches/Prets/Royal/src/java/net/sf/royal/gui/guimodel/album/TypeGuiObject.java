package net.sf.royal.gui.guimodel.album;

import net.sf.royal.datamodel.Type;
import net.sf.royal.gui.util.tables.VersionableImpl;

public class TypeGuiObject extends VersionableImpl {
    
    private Long id;
    private String name;
    private Long tekId;
    
    public TypeGuiObject(){
        
    }
    
    public TypeGuiObject(Type type){
        this.id = type.getId();
        this.name = type.getName();
        this.tekId = type.getTekId();
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

    public Long getTekId() {
        return tekId;
    }

    public void setTekId(Long tekId) {
        this.tekId = tekId;
    }    
    
    /**
     * Standard equals Override
     * @return boolean true if equals
     */
     @Override
     public boolean equals(Object o) {
		if(!(o instanceof TypeGuiObject))
			return false;
		TypeGuiObject tgo = (TypeGuiObject)o;
		return this.id == tgo.id &&
			   this.tekId == tgo.tekId &&
			   this.name.equals(tgo.name);
	 }
}
