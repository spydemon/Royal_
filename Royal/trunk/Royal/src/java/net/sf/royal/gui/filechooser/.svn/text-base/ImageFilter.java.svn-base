package net.sf.royal.gui.filechooser;

import java.io.File;

import javax.swing.filechooser.FileFilter;

import net.sf.royal.gui.manager.LocaleManager;

public class ImageFilter extends FileFilter {

    static final String jpeg = "jpeg";
    final String jpg = "jpg";
    final String gif = "gif";
    final String png = "png";
    
    public boolean accept(File file) {
        if (file.isDirectory()) {
            return true;
        }
        
        String s = file.getName();
        int i = s.lastIndexOf('.');

        if (i > 0 &&  i < s.length() - 1) {
            String extension = s.substring(i+1).toLowerCase();
            if (gif.equals(extension) ||
                jpeg.equals(extension) ||
                png.equals(extension) ||
                jpg.equals(extension)) {
                    return true;
            } else {
                return false;
            }
        }
        return false;
    }

    public String getDescription() {
        return LocaleManager.getInstance().getString("file_img_description");
    }

}
