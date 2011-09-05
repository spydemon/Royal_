package net.sf.royal.datamodel;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import net.sf.royal.exception.PersistencyException;
import net.sf.royal.gui.guimodel.property.DatabaseGuiObject;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.MessagePaneManager;
import net.sf.royal.gui.manager.PropertyManager;
import net.sf.royal.gui.pane.AlbumPane;
import net.sf.royal.gui.pane.BottomBarPane;
import net.sf.royal.gui.util.DatabaseUpgrader;

import org.apache.log4j.Logger;
import org.hibernate.CacheMode;
import org.hibernate.FlushMode;
import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.hibernate.cfg.Configuration;
import org.hibernate.tool.hbm2ddl.SchemaUpdate;

public class HibernateUtil 
{
	private static Logger logger = Logger.getLogger(HibernateUtil.class);
	
    public static final String POSTGRESQL = "org.hibernate.dialect.PostgreSQLDialect";
    public static final String HSQLDB = "org.hibernate.dialect.HSQLDialect";
    public static final String MYSQL = "org.hibernate.dialect.MySQLDialect";
    
    /**
     * Session thread
     */
    private static final ThreadLocal<Session> threadSession = new ThreadLocal<Session>();
    
    /**
     * Transaction thread 
     */
    private static final ThreadLocal<Transaction> threadTransaction = new ThreadLocal<Transaction>();
    
    /**
     * Rollback thread
     */
    private static final ThreadLocal<String> threadRollback = new ThreadLocal<String>();
    
    /**
     * Session Factory 
     */
    private static SessionFactory sessionFactory;
    
    static Database selectedDB;
    
    private static Map<String,String> types = new HashMap<String,String>();
    
    /**
     * Constructor
     */
    public HibernateUtil() {
    	sessionFactory = null;
        threadSession.set(null);
        threadTransaction.set(null);
        threadRollback.set(null);
        
    }
    
    /**
     * Returns the thread attached session or open a new one if no exist before
     * @return session
     */
    public static Session currentSession() {
        Session s = (Session) threadSession.get();
        if (s == null && (threadRollback.get() == null)) {
            s = sessionFactory.openSession();
            s.setFlushMode(FlushMode.AUTO);
            s.setCacheMode(CacheMode.NORMAL);
            threadSession.set(s);
        }
        return s;
    }
    
    /**
     * Close the current opened session
     */
    public static void closeSession() {
        Session s = (Session) threadSession.get();
        threadSession.set(null);
        //String rb = threadRollback.get();
        //rb = null;
        threadRollback.set(null);
        if (s != null && s.isOpen()) {
            s.close();
        }
    }
    
    /**
     * Begin if not already opened a new transaction
     */
    public static void beginTransaction() {
        threadRollback.set(null);
        Transaction tx = (Transaction) threadTransaction.get();
        if (tx == null) {
            tx = currentSession().beginTransaction();
            threadTransaction.set(tx);

        }
    }

    /**
     * Commits the current opened transaction
     * @throws PersistencyException 
     */
    public static void commitTransaction() throws PersistencyException {
        Transaction tx = (Transaction) threadTransaction.get();
        try {
            if (tx != null && !tx.wasCommitted() && !tx.wasRolledBack()) {
                tx.commit();
                threadTransaction.set(null);
            }
        }
        catch (HibernateException e) {
            rollbackTransaction();
            throw new PersistencyException(e, PersistencyException.CONTINUE);
        }
        finally {
            closeSession();
        }
    }

    /**
     * Rollbacks the currently opened transaction.
     * @throws PersistencyException 
     */
    public static void rollbackTransaction() throws PersistencyException {
        threadRollback.set("");
        Transaction tx = (Transaction) threadTransaction.get();
        threadTransaction.set(null);
        try {
            if (tx != null && !tx.wasCommitted() && !tx.wasRolledBack()) {
                tx.rollback();
            }
        }
        catch (HibernateException e) {
            throw new PersistencyException(e, PersistencyException.FATAL);
        }
        finally {
            closeSession();
        }
    }
    
    /**
     * Shutdown the hsqldb database
     * @throws SQLException 
     * @throws HibernateException 
     *
     */
    public static void shutdown() throws HibernateException, SQLException {
        if(sessionFactory != null)
        {
            if (selectedDB.getDialect().equals(HSQLDB)){
                HibernateUtil.currentSession().connection().createStatement().execute("SHUTDOWN");
            }
        	sessionFactory.close();
        }
    	HibernateUtil.closeSession();
    	sessionFactory = null;
    }
    
    public static void selectDatabase(String name) throws FileNotFoundException, IOException{
        File file = new File(PropertyManager.getInstance().getPathProperty("path_database") + PropertyManager.sep + name + ".properties");
        selectedDB = Database.loadFromFile(file);

    }
    
    public static void loadSessionFactory(Database db) throws HibernateException, SQLException{
        Configuration cfg = new Configuration();
        cfg.addResource("net/sf/royal/datamodel/Editor.hbm.xml");
        cfg.addResource("net/sf/royal/datamodel/Collection.hbm.xml");
        cfg.addResource("net/sf/royal/datamodel/Author.hbm.xml");
        cfg.addResource("net/sf/royal/datamodel/Work.hbm.xml");
        cfg.addResource("net/sf/royal/datamodel/Album.hbm.xml");
        cfg.addResource("net/sf/royal/datamodel/Serie.hbm.xml");
        cfg.addResource("net/sf/royal/datamodel/CommentedImage.hbm.xml");
        cfg.addResource("net/sf/royal/datamodel/Type.hbm.xml");
        cfg.addResource("net/sf/royal/datamodel/Borrower.hbm.xml");
        cfg.addResource("net/sf/royal/datamodel/Loan.hbm.xml");
<<<<<<< HEAD
        cfg.addResource("net/sf/royal/datamodel/Bibliotheque.hbm.xml");
        //cfg.addResource("net/sf/royal/datamodel/Location_Emprunt.hbm.xml");
=======
>>>>>>> 01b011e... Ajout des sources
        cfg.setProperty("hbm2ddl.auto", "update");
        cfg.setProperty("hibernate.connection.driver_class", db.getDriver());
       
		// MySQL ou PostgreSQL
        if (db.getPassword() != null){
            cfg.setProperty("hibernate.connection.password", db.getPassword());
		// HSQLDB
        } else {
            cfg.setProperty("hibernate.connection.password", "");
            cfg.setProperty("hibernate.connection.shutdown", "true");
        }
        cfg.setProperty("hibernate.connection.url", db.getUrl());
        cfg.setProperty("hibernate.connection.username", db.getUsername());
        cfg.setProperty("hibernate.dialect", db.getDialect());
        
        if (sessionFactory != null){
            HibernateUtil.shutdown();
        }
        
        SchemaUpdate update = new SchemaUpdate(cfg);
        update.execute(false, true);
        sessionFactory = cfg.buildSessionFactory();
    }
    
    public static void initSessionFactory() throws FileNotFoundException, IOException, HibernateException, SQLException {
        types.put(POSTGRESQL, LocaleManager.getInstance().getString("db_postgresql"));
        types.put(MYSQL, LocaleManager.getInstance().getString("db_mysql"));
        types.put(HSQLDB, LocaleManager.getInstance().getString("db_hsqldb"));

        String dbName = HibernateUtil.getDefaultDatabase();
        try
        {
        	HibernateUtil.selectDatabase(dbName);
        }
        catch (FileNotFoundException e)
        {
        	Database newDatabase = new Database();
        	newDatabase.setName("database");
        	newDatabase.setDefaultDB(true);
        	newDatabase.setDialect(HSQLDB);
        	newDatabase.setDriver(getDriverClass(HSQLDB));
        	newDatabase.setUsername("sa");
        	newDatabase.setPassword("");
        	newDatabase.setUrl("jdbc:hsqldb:file:" + 
        			PropertyManager.getInstance().getPathProperty("path_resources") + 
        			PropertyManager.sep + "database");
        	newDatabase.saveToFile(new File(PropertyManager.getInstance().getPathProperty("path_database") + 
        			PropertyManager.sep + "database.properties"));
    		File coverDir = new File(PropertyManager.getInstance().getPathProperty("path_cover") +
        			PropertyManager.sep + "database");
        	if(!coverDir.mkdir())
        	{
        		MessagePaneManager.showCheckErrorPane(LocaleManager.getInstance().getString("error_cover_dir"));
        	}
        	HibernateUtil.selectDatabase("database");
        }
        
        HibernateUtil.loadSessionFactory(selectedDB);
        
		DatabaseUpgrader du = DatabaseUpgrader.getInstance();
		try {			
			if(!du.checkDatabase(selectedDB.getDriver(), selectedDB.getHibernateDialect()))
				du.upgradeDatabase();
		} catch (SQLException e) {
			MessagePaneManager.showExceptionPane(e,true);
			e.printStackTrace();
		}
    }
    
    public static List<String> getListForCombo(String[] selected) throws FileNotFoundException, IOException
    {
        List<String> res = new ArrayList<String>();
        File dir = new File(PropertyManager.getInstance().getPathProperty("path_database"));
        File files[] = dir.listFiles(new FileFilter(){

            public boolean accept(File pathname) {
                return pathname.getPath().endsWith(".properties");
            }
        });

        for (int i = 0; i < files.length; i++) {
            File file = files[i];
            Database db = Database.loadFromFile(file);
            if (db.isDefaultDB()){
                selected[0] = db.getName();
            }
            res.add(db.getName());
        }
        return res;
    }
    
    public static String[] getTypeListForCombo(){
        String[] res = new String[HibernateUtil.types.size()];
        Iterator<String> it = types.values().iterator();
        int i=0;
        while (it.hasNext()) {
            String element = (String) it.next();
            res[i] = element;
            i++;
        }
        return res;
    }
    
    public static String getTypefromLabel(String label){
        Iterator<String> it = types.keySet().iterator();
        while (it.hasNext()) {
            String key = (String) it.next();
            if (types.get(key).equals(label)){
                return key;
            }
        }
        return null;
    }
    
    public static String getDriverClass(String dialect){
        String res = null;
        if (MYSQL.equals(dialect)){
            res = "org.gjt.mm.mysql.Driver";
        } else if (POSTGRESQL.equals(dialect)) {
            res ="org.postgresql.Driver";
        } else if (HSQLDB.equals(dialect)) {
            res ="org.hsqldb.jdbcDriver";
        }
        return res;
    }
    
    public static void createDatabase(DatabaseGuiObject dgo) throws FileNotFoundException, IOException{
        Database db = new Database();
        db.setName(dgo.getName());
        db.setDefaultDB(dgo.isDefaultDB());
        db.setDialect(dgo.getDialect());
        if (dgo.getDialect().equals(HSQLDB)){
            db.setUrl("jdbc:hsqldb:file:" + PropertyManager.getInstance().getPathProperty("path_resources") + PropertyManager.sep + dgo.getName());
            db.setPassword("");
            db.setUsername("sa");
        } else {
            db.setUrl(dgo.getUrl());
            db.setPassword(dgo.getPassword());
            db.setUsername(dgo.getUsername());
        }
        db.setDriver(HibernateUtil.getDriverClass(dgo.getDialect()));
        db.saveToFile(new File(PropertyManager.getInstance().getPathProperty("path_database") + PropertyManager.sep + dgo.getName() + ".properties"));
        if (db.isDefaultDB()){
            HibernateUtil.setDatabaseToDefault(db, false);
        }
		File coverDir = new File(PropertyManager.getInstance().getPathProperty("path_cover") +
    			PropertyManager.sep + dgo.getName());
    	if(!coverDir.mkdir())
    	{
    		MessagePaneManager.showCheckErrorPane(LocaleManager.getInstance().getString("error_cover_dir"));
    	}
    }
    
    private static void setDatabaseToDefault(Database dbToExcept, boolean defaultDB) throws FileNotFoundException, IOException{
        String filePathToExcept = dbToExcept.getName() + ".properties";
        File dir = new File(PropertyManager.getInstance().getPathProperty("path_database"));
        File files[] = dir.listFiles(new FileFilter(){

            public boolean accept(File pathname) {
                return pathname.getPath().endsWith(".properties");
            }
        });
        for (int i = 0; i < files.length; i++) {
            File file = files[i];
            if (!file.getName().equals(filePathToExcept)){
                Properties props = new Properties();
                props.load(new FileInputStream(file));
                props.setProperty("default", String.valueOf(defaultDB));
            }
        }
    }
    
    private static String getDefaultDatabase() throws FileNotFoundException, IOException
    {
        File dir = new File(PropertyManager.getInstance().getPathProperty("path_database"));
        File files[] = dir.listFiles(new FileFilter(){

            public boolean accept(File pathname) {
                return pathname.getPath().endsWith(".properties");
            }
        });

        for (int i = 0; i < files.length; i++) {
            File file = files[i];
            Database db = Database.loadFromFile(file);
            if (db.isDefaultDB()){
                return db.getName();
            }
        }
        return null;	
    }
    
    public static boolean isSessionFactoryInitialized(){
        return sessionFactory != null;
    }

    public static boolean changeDatabase(String DBname)
    {
    	File dir = new File(PropertyManager.getInstance().getPathProperty("path_database"));
    	Database rollbackDB = selectedDB;
    	try	
    	{
    		selectedDB.setDefaultDB(false);
    		selectedDB.saveToFile(new File(dir.getAbsolutePath() + selectedDB.getName() + ".properties"));
    		HibernateUtil.shutdown();
    		HibernateUtil.selectDatabase(DBname);
    		selectedDB.setDefaultDB(true);
    		selectedDB.saveToFile(new File(dir.getAbsolutePath() + selectedDB.getName() + ".properties"));
    		HibernateUtil.initSessionFactory();
    		AlbumPane.getInstance().refresh();
    		BottomBarPane.getInstance().refreshStats();
    	}
    	catch(Exception ex)
    	{
    		MessagePaneManager.showExceptionPane(ex,true);
    		try
    		{
    			selectedDB.setDefaultDB(false);
    			selectedDB.saveToFile(new File(dir.getAbsolutePath() + selectedDB.getName() + ".properties"));
    			HibernateUtil.shutdown();
    			HibernateUtil.selectDatabase(rollbackDB.getName());
    			selectedDB.setDefaultDB(true);
    			selectedDB.saveToFile(new File(dir.getAbsolutePath() + selectedDB.getName() + ".properties"));
    		} catch (Exception e) { logger.error("Error during the rollback", e); }
    		return false;
    	}
    	return true;
    }
    
    public static void removeDatabase(String DBname)
    {
    	File prop = new File(PropertyManager.getInstance().getPathProperty("path_database") + 
    			PropertyManager.sep + DBname + ".properties");
    	if(!prop.delete())
    		return;
    	
    	boolean success = true;
    	prop = new File(PropertyManager.getInstance().getPathProperty("resources") +
    			PropertyManager.sep + DBname + ".properties");
    	
    	if(!prop.delete())
    		success &= false;
    	
    	File coverDir = new File(PropertyManager.getInstance().getPathProperty("path_cover") +
    			PropertyManager.sep + DBname);
    	
    	File[] covers = coverDir.listFiles();
    	for(File f : covers)
    	{
    		if(!f.delete())
    			success &= false;
    	}
    	
    	if(!coverDir.delete())
    		success &= false;
    	
    	if(!success)
    	{
    		MessagePaneManager.showInfoPane(LocaleManager.getInstance().getString("error_database_delete"));
    	}
    	else
    	{
    		MessagePaneManager.showInfoPane(LocaleManager.getInstance().getString("success_database_delete"));
    	}
    }
    
    /**
     * Get the current Database name
     */
    public static String getCurrentDatabase()
    {
    	return selectedDB.getName();
    }
}



