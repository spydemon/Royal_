package net.sf.birdy.gui;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.ConvolveOp;
import java.awt.image.Kernel;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;

public class ImageProcess  {

    public static void main(String[] args){
        ImageIcon icon = new ImageIcon("resources/images/cover/belladone2.jpg");
        Image image = icon.getImage();
                image = new ImageIcon(image).getImage();
                /** On crée la nouvelle image */
                BufferedImage bufferedImage = new BufferedImage(
                                                      image.getWidth(null),
                                                      image.getHeight(null),
                                                      BufferedImage.TYPE_INT_RGB );
                Graphics g = bufferedImage.createGraphics();
                g.drawImage(image,0,0,null);
                g.dispose();
        
        
        float[] matrice = {
                (float) 0.0, (float) 0.0, (float) 0.0,(float) 0.0, (float) 0.1, (float) 0.1,
                (float) 0.1, (float) 0.1, (float) 0.1,(float) 0.1, (float) 0.1, (float) 0.1,
                (float) 0.1, (float) 0.1, (float) 0.1
                };
        BufferedImageOp op = new ConvolveOp(new Kernel(1,15,matrice));
        BufferedImage nouvelleImage = op.filter(bufferedImage, null);
        
        ImageIcon newIcon = new ImageIcon(nouvelleImage);
        
        JFrame frame = new JFrame();
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.getContentPane().add(new JLabel(newIcon));
        frame.pack();
        frame.setVisible(true);
    }
}
