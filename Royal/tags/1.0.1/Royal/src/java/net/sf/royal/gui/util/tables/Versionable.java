package net.sf.royal.gui.util.tables;

public interface Versionable {

    public static final int NEW = 0;
    public static final int UPDATED = 1;
    public static final int REMOVED = 2;
    public static final int NONE = 4;
    
    /**
     * Return the version of a table object
     * @return
     */
    public int getVersion();
    
    /**
     * Set the version of a table object
     * @param version
     */
    public void setVersion(int version);
}
