package net.sf.royal.datamodel;


import net.sf.royal.exception.PersistencyException;

import org.hibernate.Session;
import org.hibernate.criterion.Restrictions;

public abstract class PojoDAOImpl implements PojoDAO {
    protected Session session;

    /**
     * @throws PersistencyException 
     * @see PojoDAO#save(Object)
     */
    public final void save(Object o) throws PersistencyException {
        if (o != null) {
            try{
                HibernateUtil.currentSession().saveOrUpdate(o);
            } catch (Exception e) {
                HibernateUtil.rollbackTransaction();
                throw new PersistencyException(e, PersistencyException.CONTINUE);
            }
        }
    }

    /**
     * @throws PersistencyException 
     * @see PojoDAO#delete(Object)
     */
    public final void delete(Object o) throws PersistencyException {
        if (o != null){
            try{
                HibernateUtil.currentSession().delete(o);
            } catch (Exception e) {
                HibernateUtil.rollbackTransaction();
                throw new PersistencyException(e, PersistencyException.CONTINUE);
            }
        }
    }

    /**
     * @throws PersistencyException 
     * @see PojoDAO#findByPrimaryKey(Long)
     */
    public final Object findByPrimaryKey(Long id) throws PersistencyException {
        if (id != null) {
            try {
                session = HibernateUtil.currentSession();
                return session.createCriteria(this.getCatClass()).add(
                    Restrictions.eq("id", id)).uniqueResult();
            } catch (Exception e) {
                HibernateUtil.rollbackTransaction();
                throw new PersistencyException(e, PersistencyException.CONTINUE);
            }
            
        } else {
            return null;
        }
    }
    
    /**
     * @throws PersistencyException 
     * @see PojoDAO#findAll()
     */
    public Object[] findAll() throws PersistencyException{
        try {
            session = HibernateUtil.currentSession();
            return (session.createCriteria(this.getCatClass()).list()).toArray();
        } catch (Exception e) {
            HibernateUtil.rollbackTransaction();
            throw new PersistencyException(e, PersistencyException.CONTINUE);
        } 
    }
    
    public abstract Class<?> getCatClass();
}
