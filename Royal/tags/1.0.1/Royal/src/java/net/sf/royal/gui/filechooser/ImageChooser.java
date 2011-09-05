package net.sf.royal.gui.filechooser;

import java.io.File;

import javax.swing.JFileChooser;

public class ImageChooser extends JFileChooser {

    public ImageChooser(String defaultPath, String img){
        this.setCurrentDirectory(new File(defaultPath));
        if (img != null){
            this.setSelectedFile(new File(img));
        }
        this.setFileFilter(new ImageFilter());
        this.setAccessory(new ImagePreview(this));
    }
}
