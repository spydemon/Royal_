package net.sf.royal.gui.util;

import javax.swing.tree.DefaultMutableTreeNode;

public class DataModelTreeNode extends DefaultMutableTreeNode {
    
    private Long id;
    private String name;
    private Object object;
    
    
    public DataModelTreeNode(Long id, String name, Object object) {
        super();
        this.id = id;
        this.name = name;
        this.object = object;
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
    
    public String toString(){
        return this.name;
    }

    /**
     * @return Returns the object.
     */
    public Object getObject() {
        return object;
    }

    /**
     * @param object The object to set.
     */
    public void setObject(Object object) {
        this.object = object;
    }
}
