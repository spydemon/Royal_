package net.sf.royal.gui.manager;

import java.awt.Dimension;
import java.io.File;

import javax.swing.ImageIcon;

public class IconManager {

    private static String pathDirectory = PropertyManager.getInstance().getPathProperty("path_icon") + "/";
    
    /**
     * Retrieve the icon from its file name
     * @param fileName
     * @return
     */
    public static ImageIcon getIcon(String fileName){
    	if((new File(pathDirectory+fileName)).exists())
    	{
    		return new ImageIcon(pathDirectory + fileName);
    	}
    	else
    	{
    		System.err.println("Icon not found : " + pathDirectory + fileName);
    		return new ImageIcon();
    	}
    }
    
    /**
     * Get the new dimension
     * @param dim
     * @param maxDim
     * @return
     */
    public static Dimension getNewDimension(Dimension dim, int maxDim){
        float maxDimension = new Float(maxDim).floatValue();
        float width = new Float(dim.getWidth()).floatValue();
        float height = new Float(dim.getHeight()).floatValue();
        float div = 1;
        if (dim.width > dim.height){
            div = width; 
        } else {
            div = height;
        }
        float scale = maxDimension / div;
        
        float newWidth = width * scale;
        float newHeight = height * scale;
        
        return new Dimension(new Float(newWidth).intValue(), new Float(newHeight).intValue());
    }
}
