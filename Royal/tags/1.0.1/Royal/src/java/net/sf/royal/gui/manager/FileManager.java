package net.sf.royal.gui.manager;

import java.io.File;
import java.io.InputStream;
import java.util.List;

import net.sf.royal.exception.RoyalException;
import net.sf.royal.exception.DefaultException;

public class FileManager {

    private FileManager() {
    }

    public static void copyFile(File src, File dest) throws DefaultException {
        java.io.FileInputStream sourceFile = null;
        java.io.FileOutputStream destinationFile = null;
        try {
            dest.createNewFile();
            sourceFile = new java.io.FileInputStream(src);
            destinationFile = new java.io.FileOutputStream(dest);
            byte buffer[] = new byte[512 * 1024];
            int nbLecture;
            while ((nbLecture = sourceFile.read(buffer)) != -1) {
                destinationFile.write(buffer, 0, nbLecture);
            }
        } catch (java.io.FileNotFoundException f) {
            throw new DefaultException(f, RoyalException.CONTINUE);
        } catch (java.io.IOException e) {
            throw new DefaultException(e, RoyalException.CONTINUE);
        } finally {
            try {
                sourceFile.close();
            } catch (Exception e) {
                throw new DefaultException(e, RoyalException.CONTINUE);
            }
            try {
                destinationFile.close();
            } catch (Exception e) {
                throw new DefaultException(e, RoyalException.CONTINUE);
            }
        }
    }
    
    public static void copyFile(InputStream sourceFile, File dest) throws DefaultException {
        java.io.FileOutputStream destinationFile = null;
        try {
            dest.createNewFile();
            destinationFile = new java.io.FileOutputStream(dest);
            byte buffer[] = new byte[512 * 1024];
            int nbLecture;
            while ((nbLecture = sourceFile.read(buffer)) != -1) {
                destinationFile.write(buffer, 0, nbLecture);
            }
        } catch (java.io.FileNotFoundException f) {
            throw new DefaultException(f, RoyalException.CONTINUE);
        } catch (java.io.IOException e) {
            throw new DefaultException(e, RoyalException.CONTINUE);
        } finally {
            try {
                sourceFile.close();
            } catch (Exception e) {
                throw new DefaultException(e, RoyalException.CONTINUE);
            }
            try {
                destinationFile.close();
            } catch (Exception e) {
                throw new DefaultException(e, RoyalException.CONTINUE);
            }
        }
    }
    
    /**
     * Put a file in the good path (if the file is already in the good path, do nothing)
     * @param selectedFile
     * @param path
     * @return
     * @throws DefaultException 
     */
    public static File putFile(File selectedFile, String path) throws DefaultException{
        if (selectedFile != null){
            File newFile = null;
            String fileToCompare = new File(path + "/" + selectedFile.getName()).getAbsolutePath();
            if (!fileToCompare.equals(selectedFile.getAbsolutePath())){
                newFile = new File(path + "/" + selectedFile.getName());
                FileManager.copyFile(selectedFile, newFile);
            } else {
                newFile = selectedFile;
            }
            return newFile;
            
        }
        return null;
    }
    
    public static void deleteFile(String path) throws DefaultException {
        if (path != null){
            File toDelete = new File(path);
            try {
                if (!toDelete.delete()){
                    throw new DefaultException(new RuntimeException("Cannot delete "+ path), RoyalException.CONTINUE);
                }
            } catch (SecurityException e){
                throw new DefaultException(e, RoyalException.CONTINUE);
            }
        }
    }
    
    public static void deleteFiles(List<String> pathList) throws DefaultException {
        for(int i=0;i<pathList.size();i++){
            FileManager.deleteFile(pathList.get(i));
        }
    }
}
