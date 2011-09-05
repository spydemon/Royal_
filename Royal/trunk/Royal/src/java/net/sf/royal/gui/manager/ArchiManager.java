package net.sf.royal.gui.manager;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JOptionPane;

import org.apache.log4j.Logger;

import net.sf.royal.exception.DefaultException;


/**
	*	Initialize the resources architecture in the home directory of the User
	*	to store the DB and GUI xml files
	*   @author bibounde
	*/
public class ArchiManager {

    private static ArchiManager instance = new ArchiManager();
    private static Logger logger = Logger.getLogger(ArchiManager.class);
    
    private String userHome = System.getProperty("user.home") + PropertyManager.sep + ".royal";
    private Resource resourceDir = new Resource(PropertyManager.getInstance().getSystemProperty("path_resources"), false);
    private Resource databaseDir = new Resource(PropertyManager.getInstance().getSystemProperty("path_database"), false);
    private Resource helpDir = new Resource(PropertyManager.getInstance().getSystemProperty("path_help"), false);
    private Resource iconDir = new Resource(PropertyManager.getInstance().getSystemProperty("path_icon"), false);
    private Resource imageDir = new Resource(PropertyManager.getInstance().getSystemProperty("path_images"), false);
    private Resource perspectiveDir = new Resource(PropertyManager.getInstance().getSystemProperty("path_perspective"), false);
    private Resource photoDir = new Resource(PropertyManager.getInstance().getSystemProperty("path_photo"), false);
    private Resource galleryDir = new Resource(PropertyManager.getInstance().getSystemProperty("path_gallery"), false);
    private Resource dedcationDir = new Resource(PropertyManager.getInstance().getSystemProperty("path_dedication"), false);
    private Resource coverDir = new Resource(PropertyManager.getInstance().getSystemProperty("path_cover"), false);
    private Resource coverTmpDir = new Resource(PropertyManager.getInstance().getSystemProperty("path_cover_tmp"), false);
    private Resource datasourceDir = new Resource(PropertyManager.getInstance().getSystemProperty("path_datasource"), false);
    
   
    /**  
	* Constructor
	*/
    private ArchiManager(){
    }

    /**
	* Get the static instance of ArchiManager
	* @return an instance of Archimanager
	*/
    public static ArchiManager getInstance() {
        return instance;
    }
    

    /**
	* Check which user resource exists in his home
	* If they don't exist we create them
	* @see ArchiManager#checkUserResourcesExist
	* @see ArchiManager#createUserResources(List)
	* @see Resource
	*/
    public void initArchitecture() {
        List<Resource> toCreate = ArchiManager.getInstance().checkUserResourcesExist();
        List<String> errors;
        try {
            errors = ArchiManager.getInstance().createUserResources(toCreate);
            if (errors.size() == 0){
                for(int i=0;i<toCreate.size();i++){
                    Resource res = toCreate.get(i);
                    ArchiManager.getInstance().copyFilesToResource(res);
                }
            } else {
                StringBuffer buff = new StringBuffer("Could not create resources : \n");
                for(int i=0;i<errors.size();i++){
                    buff.append("  - ").append(errors.get(i)).append("\n");
                }
                JOptionPane.showMessageDialog(null,
                        buff.toString(),
                        "Fatal exception",
                        JOptionPane.ERROR_MESSAGE);
                System.exit(0);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    /**
	* Check each one of the resources
	* @see ArchiManager#checkUserResourceExist(Resource,List)
	* @see Resource
	*/   
    public List<Resource> checkUserResourcesExist(){
        List<Resource> res = new ArrayList<Resource>();
        this.checkUserResourceExist(resourceDir, res);
        this.checkUserResourceExist(databaseDir, res);
        this.checkUserResourceExist(iconDir, res);
        this.checkUserResourceExist(helpDir, res);
        this.checkUserResourceExist(imageDir, res);
        this.checkUserResourceExist(perspectiveDir, res);
        this.checkUserResourceExist(photoDir, res);
        this.checkUserResourceExist(galleryDir, res);
        this.checkUserResourceExist(dedcationDir, res);
        this.checkUserResourceExist(coverDir, res);
        this.checkUserResourceExist(coverTmpDir, res);
        this.checkUserResourceExist(datasourceDir, res);
        return res;
    }
    
    /**
	* Check if the resource exist in the .royal folder
	* if it's not the case we add it to the list of the 
	* resources to had.
	* @param resource The resource we check
	* @param res List of the future added Resources
	* @see Resource
	*/
    private void checkUserResourceExist(Resource resource, List<Resource> res){
        File file = new File(userHome + PropertyManager.sep + resource.getRelativePath());
        if (!file.exists()){
            res.add(resource);
        }
    }
    
    /**
	*
	* Create the different empty resource files and folders
	* @param resources List of the Resources we want to create
	* @return The List of the path where the method can't write
	* @throws IOExcetion if there is an error during the resource file creation
	* @see IOException
	* @see Resource
	*/
    public List<String> createUserResources(List<Resource> resources) throws IOException {
        new File(userHome).mkdir();
        List<String> res = new ArrayList<String>();
        for(int i=0;i<resources.size();i++){
            Resource resource = resources.get(i);
            File file = new File(userHome + PropertyManager.sep + resource.getRelativePath());
            if (file.getParentFile().canWrite()){
                if (resource.isFile()){
                    file.createNewFile();
                } else {
                    file.mkdir();
                }
            } else {
                res.add(file.getParentFile().getPath());
            }
        }
        return res;
    }
    
    /**
	* Copy a Resource to the ~/.birdy folder
	* @param res Resource we want to copy
	* @throws DefaultException if Filemanager.copyFile fails
	* @see DefaultException
	* @see Resource
	* @see FileManager#copyFile(File,File)
	*/
    public void copyFilesToResource(Resource res) throws DefaultException{
        if (!res.isFile()){
			File f = new File(System.getProperty("user.dir"));
			
            File dir = new File(f.getPath() + PropertyManager.sep + res.getRelativePath());
            String outputDir = userHome + PropertyManager.sep + res.getRelativePath();
            
            if(dir.isDirectory())
            {
            	File[] filesToCopy = dir.listFiles();
            	for (int i = 0; i < filesToCopy.length; i++) {
            		if (filesToCopy[i].isFile()){
            			File dest = new File(outputDir + PropertyManager.sep + filesToCopy[i].getName());
            			FileManager.copyFile(filesToCopy[i], dest);
            		}
            	}
            }
        }
    }
    

    /**
	* Class which represent a resource File with its path
	*/
    private class Resource {
        
        private String relativePath;
        private boolean file;
        public boolean isFile() {
            return file;
        }
        public Resource(String relativePath, boolean file) {
            this.relativePath = relativePath;
            this.file = file;
        }
        
        public String toString(){
            return relativePath;
        }
        public String getRelativePath() {
            return relativePath;
        }
    }


	public void cleanTmp() 
	{
		File tmpDir = new File(PropertyManager.getInstance().getPathProperty("path_cover_tmp"));
		for(File tmpCover : tmpDir.listFiles())
		{
			if(!tmpCover.delete())
			{
				logger.debug("Error when deleting : " + tmpCover.getAbsolutePath());
			}
		}
	}
}
