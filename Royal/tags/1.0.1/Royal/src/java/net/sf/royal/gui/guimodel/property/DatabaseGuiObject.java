package net.sf.royal.gui.guimodel.property;

import net.sf.royal.datamodel.Database;

public class DatabaseGuiObject {
    
    private String name;
    private String password;
    private String url;
    private String username;
    private String dialect;
    private boolean defaultDB = false;

    public DatabaseGuiObject(){}
    
    public DatabaseGuiObject(Database db){
        this.name = db.getName();
        this.password = db.getPassword();
        this.url = db.getUrl();
        this.username = db.getUsername();
        this.dialect = db.getDialect();
        this.defaultDB = db.isDefaultDB();
    }

    public boolean isDefaultDB() {
        return defaultDB;
    }

    public void setDefaultDB(boolean defaultDB) {
        this.defaultDB = defaultDB;
    }

    public String getDialect() {
        return dialect;
    }

    public void setDialect(String dialect) {
        this.dialect = dialect;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }
}
