package net.sf.royal.gui.util;

import java.awt.Image;
import java.awt.Toolkit;

import javax.swing.ImageIcon;

import net.sf.royal.gui.manager.PropertyManager;

/**
  * @author bibounde, Soulou
  * Different methods to manipulate pictures with swing
  */
public class ImageHelper {
    /**
     * Get a scaled image for specific width and height
     * @param icon
     * @param width
     * @param height
     * @return
     */
    public static ImageIcon getScaledImage(ImageIcon icon, int width, int height){
        if (icon.getImage() == null){
            return null;
        }
        float scale = 0;
        float scaleFromX = 0;
        float scaleFromY = 0;
        float imgWidth = new Float(icon.getIconWidth()).floatValue();
        float imgHeight = new Float(icon.getIconHeight()).floatValue();
        
        scaleFromX = new Float(width).floatValue() / imgWidth;
        scaleFromY = new Float(height).floatValue() / imgHeight;
        
        
        
        if (new Float(imgWidth * scaleFromX).intValue() <= width && new Float(imgHeight * scaleFromX).intValue() <= height)
        {
            scale = scaleFromX;
        }
        else 
        {
            scale = scaleFromY;
        }     
        int newWidth =  new Float(imgWidth * scale).intValue()-5;
        int newHeight = new Float(imgHeight * scale).intValue()-5;
        
        
        if (PropertyManager.getInstance().getProperty("always_grow_image").equals("false"))
        {
            if (newWidth > icon.getIconWidth() || newHeight > icon.getIconHeight())
            {
                ImageIcon ii = new ImageIcon(icon.getImage().getScaledInstance(icon.getIconWidth(), icon.getIconHeight(), Image.SCALE_DEFAULT));
                ii.setDescription(icon.getDescription());
                return ii;
            }
        } 
        ImageIcon ii =  new ImageIcon(icon.getImage().getScaledInstance(newWidth, newHeight, Image.SCALE_DEFAULT));
        ii.setDescription(icon.getDescription());
        return ii;
    }

	/**
	  * @param path
	  * @param width
	  * @param height
	  * @return JLabel of an image of width x height
	  */
	public static ImageIcon getImageIcon(String path, int width, int height)
	{
		Image img = Toolkit.getDefaultToolkit().getImage(path);
		ImageIcon ic = new ImageIcon(img);
		ic.setDescription(path);
		return  getScaledImage(ic, width, height);
	}
	
	public static ImageIcon getImageIcon(String path)
	{
		Image img = Toolkit.getDefaultToolkit().getImage(path);
		ImageIcon ic = new ImageIcon(img);
		ic.setDescription(path);
		return ic;
	}
}
