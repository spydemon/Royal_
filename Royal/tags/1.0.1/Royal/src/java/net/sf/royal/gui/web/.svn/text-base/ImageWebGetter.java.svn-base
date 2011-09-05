package net.sf.royal.gui.web;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.charset.Charset;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;

import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.MessagePaneManager;
import net.sf.royal.gui.manager.PropertyManager;

import org.apache.log4j.Logger;

/**
 * From an URL, get an ImageIcon to print
 * @author Soulou
 */
public class ImageWebGetter 
{
	private static Logger logger = Logger.getLogger(ImageWebGetter.class);
	private static String tmpImagePath = PropertyManager.getInstance().getPathProperty("path_cover_tmp");
	
	private ImageWebGetter() {}
	
	public static ImageIcon getImageFromURL(String url)
	{
		String[] splitName = url.split("/");
		String name = "unknown";
		try
		{
			name = URLDecoder.decode(splitName[splitName.length-1], Charset.defaultCharset().displayName());
		} catch (UnsupportedEncodingException uee)
		{
			uee.printStackTrace();
			logger.error("Error when decoding image URL", uee);
		}
		if(name.indexOf('?') != -1)
		{
			splitName = name.split("\\?");
			name = splitName[0];
		}
		
		// We get the extension for ImageIO
		splitName = url.split("\\.");
		String typeExt = splitName[splitName.length-1];
		
		File tmpDir = new File(tmpImagePath);
		
		// We check if we have already the file in order to not download it twice
		for(File f : tmpDir.listFiles())
		{
			if(f.getName().equals(name))
			{
				ImageIcon ii = new ImageIcon(tmpImagePath + PropertyManager.sep + name);
				ii.setDescription(tmpImagePath + PropertyManager.sep + name);
				return ii;
			}
		}
		
		
		BufferedImage image = null;
		try
		{
			image = ImageIO.read(new URL(url));
			File fileImage = new File(tmpDir + PropertyManager.sep + name);
			ImageIO.write(image, typeExt, fileImage);
			logger.debug("Picture downloaded : " + fileImage.getName());
		}
		catch (MalformedURLException mue)
		{
			mue.printStackTrace();
		}
		catch (FileNotFoundException fnfe)
		{
			logger.debug("404 Error : " + url);
		}
		catch (IOException ioe)
		{
			ioe.printStackTrace();
			logger.error("Error when fetching : " + url, ioe);
			MessagePaneManager.showCheckErrorPane(LocaleManager.getInstance().getString("connection_error"));
		}
		
		if(image != null)
		{
			ImageIcon ii = new ImageIcon(image);
			ii.setDescription(tmpImagePath + PropertyManager.sep + name);
			return ii;
		}
		return new ImageIcon();
	}
}
