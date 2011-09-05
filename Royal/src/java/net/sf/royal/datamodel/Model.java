package net.sf.royal.datamodel;

import java.io.Serializable;


public interface Model extends Serializable{

    /**
     * Set the id of the wizardModel. Only Hibernate can use this method
     * @param id
     */
    public void setId(Long id);
    
    /**
     * Get the id of the wizardModel
     * @return The id of the wizardModel 
     */
    public Long getId();
    
    /**
     * Set the version of the wizardModel. Used when user retrieve or modify remote wizardModel
     * @param version
     */
    public void setVersion(Long version);
    
    /**
     * Get the version of the wizardModel
     * @return The version of the wizardModel
     */
    public Long getVersion();
    
    /**
     * Set the id of its clone in BirDyTeque
     * @param id
     */
    public void setTekId(Long id);
    
    /**
     * Get the id of its clone in BirDyTeque
     *
     */
    public Long getTekId();
}
