package net.sf.royal.gui.util;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

import net.sf.royal.datamodel.Album;
import net.sf.royal.datamodel.HibernateUtil;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.MessagePaneManager;
import net.sf.royal.persistency.PersistencyManager;
import net.sf.royal.persistency.SaveItemPersistency;

import org.apache.log4j.Logger;
import org.hibernate.dialect.Dialect;
import org.hibernate.tool.hbm2ddl.ColumnMetadata;
import org.hibernate.tool.hbm2ddl.DatabaseMetadata;
import org.hibernate.tool.hbm2ddl.TableMetadata;

/**
 * @author Soulou
 * If the current database was created with an older version of Birdy
 * Ask to upgrade it (Remove Cotes/ParaBD)
 */
public class DatabaseUpgrader
{
	private static Logger logger = Logger.getLogger(DatabaseUpgrader.class);
	
	private final static String SEARCH_COLUMN = "Cote";
    private final static String HSQLDB_DRIVER_NAME = "org.hsqldb.jdbcDriver";	

	private static DatabaseUpgrader instance;

	private String sDBDriver;
	private ArrayList<String> fkToDelete;

	// Empty constructor (static class)
	private DatabaseUpgrader() {}

	/**
	  * Check if we are using an older BirDy database
	  * @return false if the database must be upgrade
	  * @throws SQLException if there is a problem with the Database
	  */
	public boolean checkDatabase(String sDBDriver, Dialect dialect) throws SQLException
	{
		boolean res = true;
		this.sDBDriver = sDBDriver;
		// We get the table names and check if there is Cote (or COTE for HSQLDB)
		for(String str : PersistencyManager.getTableNames())
				if(str.equals(SEARCH_COLUMN) || str.equals(SEARCH_COLUMN.toUpperCase()))
					res = false;
		
		// We get the attribute of the Album and Collection to see if the foreign key are nullable
		Connection connecDB = HibernateUtil.currentSession().connection();
		DatabaseMetadata dm = new DatabaseMetadata(connecDB, dialect);
		
		// We search the name of the Constraint we want to delete
		java.sql.DatabaseMetaData dmfk = connecDB.getMetaData();
		ResultSet rs;
		TableMetadata tm; 
		ColumnMetadata cm;
		
		String[] sqlTables = {"Album", "Collection"};
		/* The name collectionID stands for editorID, the mapping is bad
		 * but for compatibility we let it like that
		 */
		String[] sqlFields = {"serieID", "collectionID"};
		String[] foreignTables = {"Serie", "Editor"};
		String sqlTable, sqlField, foreignTable;
		fkToDelete = new ArrayList<String>();
		
		for(int i = 0; i < sqlTables.length; i++)
		{
			if(this.sDBDriver.equals(HSQLDB_DRIVER_NAME))
			{
				sqlTable = sqlTables[i].toUpperCase();
				sqlField = sqlFields[i].toUpperCase();
				foreignTable = foreignTables[i].toUpperCase();
			}
			else
			{
				sqlTable = sqlTables[i];
				sqlField = sqlFields[i];
				foreignTable = foreignTables[i];
			}
		
			tm = dm.getTableMetadata(sqlTable, null, null);
			cm = tm.getColumnMetadata(sqlField);
			
			rs = dmfk.getExportedKeys(connecDB.getCatalog(), null, foreignTable);
		    while (rs.next()) 
		    {
		    	if(rs.getString("FKTABLE_NAME").equals(sqlTable))
		    		fkToDelete.add(rs.getString("FK_NAME"));
		    }
		    
		    try
		    {
		    	if(cm.getNullable().equals("NO"))
		    	{
		    		res = false;
		    	}
		    } catch (NullPointerException npe) {
		    	res = false;
		    }
		}
		connecDB.close();
		try
		{
			HibernateUtil.commitTransaction();
		} catch (Exception e)
		{
			e.printStackTrace();
		}
		return res;
	}

	/**
	  * DROP Cote, ParaBD, ParaFabrication and ParaType tables
	  * @throws SQLException if there is a problem with the Database
	  */
	public void upgradeDatabase() throws SQLException
	{
		if(MessagePaneManager.showConfirmation(LocaleManager.getInstance().getString("obsolete_db")))
		{
			String[] queries = new String[6 + fkToDelete.size()];
			String[] fkTableName = {"Album", "Collection"};
						
			if(this.sDBDriver.equals(HSQLDB_DRIVER_NAME))
			{
				for(int i = 4; i < fkToDelete.size()+4; i++)
				{
					queries[i] = "ALTER TABLE " + fkTableName[i-4].toUpperCase() + " DROP CONSTRAINT " + fkToDelete.get(i-4);
					logger.debug("SQL Query " + queries[i]);
				}
				queries[0] = "DROP TABLE COTE IF EXISTS";
				queries[1] = "DROP TABLE PARABD IF EXISTS";
				queries[2] = "DROP TABLE PARAFABRICATION IF EXISTS";
				queries[3] = "DROP TABLE PARATYPE IF EXISTS";
				queries[fkToDelete.size() + 4] = "ALTER TABLE ALBUM ALTER COLUMN SERIEID BIGINT";
				queries[fkToDelete.size() + 5] = "ALTER TABLE COLLECTION ALTER COLUMN COLLECTIONID BIGINT";
			}
			else
			{
				for(int i = 4; i < fkToDelete.size()+4; i++)
				{
					queries[i] = "ALTER TABLE " + fkTableName[i-4] + " DROP CONSTAINT " + fkToDelete.get(i-4);
				}
				queries[0] = "DROP TABLE IF EXISTS Cote";
				queries[1] = "DROP TABLE IF EXISTS ParaBD";
				queries[2] = "DROP TABLE IF EXISTS ParaFabrication";
				queries[3] = "DROP TABLE IF EXISTS ParaType";
				queries[fkToDelete.size() + 4] = "ALTER TABLE Album MODIFY serieID BIGINT";
				queries[fkToDelete.size() + 5] = "ALTER TABLE Collection MODIFY collectionID BIGINT";
			}

			Statement s;
			Connection connecDB = HibernateUtil.currentSession().connection();
			for(String query : queries)
			{
				s = connecDB.createStatement();
				logger.debug("SQL Query : " + query);
				s.execute(query);
			}
			connecDB.close();
			
			List<Album> albumList = PersistencyManager.findAlbums();
			String[] path;
			for(Album a : albumList)				
			{
				String cover = a.getCover();
				if(cover != null)
				{
					path = cover.split("/");
					logger.debug(cover + " --> " + path[path.length-1]);
					a.setCover(path[path.length-1]);
					SaveItemPersistency.saveAlbum(a);
				}
			}
			
			MessagePaneManager.showInfoPane(LocaleManager.getInstance().getString("upgrade_success"));
		}
	}
	
	public static DatabaseUpgrader getInstance()
	{
		if(instance == null)
		{
			instance = new DatabaseUpgrader();
		}
		return instance;
	}
}
