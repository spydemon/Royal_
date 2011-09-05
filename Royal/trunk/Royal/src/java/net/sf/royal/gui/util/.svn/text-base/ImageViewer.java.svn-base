package net.sf.royal.gui.util;

import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.border.EtchedBorder;

/**
 * 
 * @author bibounde
 *
 */
public class ImageViewer extends JLabel{

    private ImageIcon image;
    private ImageDialog imageDialog;
    private Dimension dimension;
    private boolean focusState = Boolean.TRUE.booleanValue();
    
    public ImageViewer(Dimension defaultDimension){
        this(null, defaultDimension);
    }
    
    public ImageViewer(ImageIcon imageIcon, Dimension defaultDimension){
        super();
        this.image = imageIcon;
        this.dimension = defaultDimension;
        this.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
        this.setPreferredSize(this.dimension);
        this.initListener();
        this.imageDialog = new ImageDialog();
    }

    protected void paintComponent(Graphics g) {
        if (this.image != null && this.image.getImage() != null){
            ImageIcon imageIconToDraw = this.getScaledImage(this.getSize().width, this.getSize().height);
            int x = (this.getSize().width - imageIconToDraw.getIconWidth())/2;
            int y = (this.getSize().height - imageIconToDraw.getIconHeight())/2;
            g.drawImage(imageIconToDraw.getImage(),x,y,this);
        }
    }
    
    private ImageIcon getScaledImage(int width, int height){
        return ImageHelper.getScaledImage(this.image, width, height);
    }
    
    private void initListener()
    {
        this.addMouseListener(new MouseAdapter()
        {
            public void mouseReleased(MouseEvent e) 
            {
                if (e.getButton() == MouseEvent.BUTTON3 && !e.isPopupTrigger())
                {
                    if (image != null && image.getImage() != null && !image.getDescription().isEmpty()) // && !ImageViewer.this.isFocusState())
                    {
                        imageDialog.display();
                    }
                } 
            }
            public void mouseEntered(MouseEvent arg0)
            {
                if (image != null && image.getImage() != null){
                    ImageViewer.this.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
                }
            }
        });
    }
    
    /**
     * Get the path of the file
     * @return
     */
    public String getImageURL(){
        if (image == null){
            return null;
        }
        return image.getDescription();
    }
    
    
    
    private class ImageDialog extends JDialog {
        
        private JLabel label = new JLabel();
        
        public ImageDialog(){
           super();
           label.setIcon(image);
           this.getContentPane().add(label);
           this.setModal(Boolean.TRUE.booleanValue());
        }
        
        public void display(){
            this.pack();
            this.setLocationRelativeTo(null);
            this.setVisible(Boolean.TRUE.booleanValue());
        }

        /**
         * @return Returns the label.
         */
        public JLabel getLabel() {
            return label;
        }

        /**
         * @param label The label to set.
         */
        @SuppressWarnings("unused")
		public void setLabel(JLabel label) {
            this.label = label;
        }
    }

    public ImageDialog getImageDialog() {
        return imageDialog;
    }

    public void setImageDialog(ImageDialog imageDialog) {
        this.imageDialog = imageDialog;
    }

    /**
     * @return Returns the image.
     */
    public ImageIcon getImage() {
        return image;
    }

    /**
     * @param image The image to set.
     */
    public void setImage(ImageIcon image) {
        this.image = image;
        this.imageDialog.getLabel().setIcon(image);
        this.validate();
        this.repaint();
    }

    /**
     * @return Returns the focusState.
     */
    public boolean isFocusState() {
        return focusState;
    }

    /**
     * @param focusState The focusState to set.
     */
    public void setFocusState(boolean focusState) {
        this.focusState = focusState;
    }
}
