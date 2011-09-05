package net.sf.royal.datamodel;


import net.sf.royal.exception.PersistencyException;

public interface PojoDAO {
    
    /**
     * Save or update the relatedObject
     * @param o relatedObject to save in DB
     * @throws PersistencyException 
     */
    public void save(Object o) throws PersistencyException;
    
    /**
     * Delete the relatedObject
     * @param o relatedObject to delete
     * @throws PersistencyException 
     */
    public void delete(Object o) throws PersistencyException;
    
    /**
     * Get a relatedObject from the DB by PK. Don't close session (to prevent lazy operations) 
     * @param id id of the relatedObject
     * @return the relatedObject stored in DB (null otherwise)
     * @throws PersistencyException 
     */
    public Object findByPrimaryKey(Long id) throws PersistencyException;
    
    /**
     * Get all objects sorted in DB (to prevent lazy operations)
     * @return an array which contains the relatedObject stored in DB
     * @throws PersistencyException 
     */
    public Object[] findAll() throws PersistencyException;
}
