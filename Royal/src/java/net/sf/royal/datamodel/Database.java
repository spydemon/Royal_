package net.sf.royal.datamodel;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;

import net.sf.royal.gui.manager.PropertyManager;

import org.hibernate.dialect.Dialect;

public class Database {

    private String name;
    private String driver;
    private String password;
    private String url;
    private String username;
    private String dialect;
    private Dialect hibernate_dialect;
    private boolean defaultDB = false;
    
    public static Database createPostGreSQL(){
        Database res = new Database();
        res.setDialect("org.hibernate.dialect.PostgreSQLDialect");
        res.setDriver("org.postgresql.Driver");
        return res;
    }
    
    public static Database loadFromFile(File file) throws FileNotFoundException, IOException{
        Database res = new Database();
        Properties props = new Properties();
        props.load(new FileInputStream(file));
        res.hibernate_dialect = Dialect.getDialect(props);
        res.setDefaultDB(props.getProperty("default").equals("true"));
        res.setDialect(props.getProperty("hibernate.dialect"));
        res.setDriver(props.getProperty("hibernate.connection.driver_class"));
        res.setName(props.getProperty("name"));
        res.setPassword(props.getProperty("hibernate.connection.password"));
        res.setUrl(props.getProperty("hibernate.connection.url"));
        res.setUsername(props.getProperty("hibernate.connection.username"));
        return res;
    }
    
    public void saveToFile(File file) throws FileNotFoundException, IOException{
        Properties props = new Properties();
        props.put("default", String.valueOf(this.defaultDB));
        props.put("hibernate.dialect", this.dialect);
        props.put("hibernate.connection.driver_class", this.driver);
        props.put("name", this.name);
        if (this.password != null && !this.password.equals("")){
            props.put("hibernate.connection.password", this.password);
        }
        props.put("hibernate.connection.url", this.url);
        props.put("hibernate.connection.username", this.username);
        
        props.store(new FileOutputStream(PropertyManager.getInstance().getPathProperty("path_database") + "/" + this.name + ".properties"), "Generated by BirDy");
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

    public Dialect getHibernateDialect() {
        return hibernate_dialect;
    }
    
    public void setDialect(String dialect) {
        this.dialect = dialect;
    }

    public String getDriver() {
        return driver;
    }

    public void setDriver(String driver) {
        this.driver = driver;
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
