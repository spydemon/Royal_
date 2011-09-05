package net.sf.royal.splasher;

import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Window;

/**
  * @author bibounde
  * Window which is launch by Splasher.java
  * Load the different elements of the application
  * @see Splasher
  */
public class SplashWindow extends Window {

	/**
	  * Invoke the main method of the class given in argument
	  * @param className Name of the class
	  * @param args Args given to the main method
	  */
    public static void invokeMain(String className, String[] args) {
        try {
            Class.forName(className).getMethod("main",
                    new Class[] { String[].class }).invoke(null,
                    new Object[] { args });
        } catch (Exception e) {
            InternalError error = new InternalError("Failed to invoke main method");
            error.initCause(e);
            throw error;
        }
    }

	/**
	  Create the Image object of the splash screen
	  @param imageURL URL of the picture
	  @see URL
	  @see Image
	  @see Toolkit#createImage(URL)
	  */

	/**
	  * Instance of the SplashWindow
	  * We can use it by calling splash(Image), or use the constructor
	  * @see SplashWindow#splash(Image)
	  */
    private static SplashWindow instance;


	/**
	  * Create and Show the splash screen
	  * @param image Image of the splash screen
	  */
    public static void splash(Image image) {
        if (instance == null && image != null) {
            Frame f = new Frame();

            instance = new SplashWindow(f, image);
            instance.setVisible(Boolean.TRUE.booleanValue());

			// If we are in the AWTEvent thread and if there is only 1 processor
			// available to the JVM
            if (!EventQueue.isDispatchThread()
                    && Runtime.getRuntime().availableProcessors() == 1) {

				// Instance thread wait until notifyall() is called in paint() method
                synchronized (instance) {
                    while (!instance.paintCalled) {
                        try {

                            instance.wait();

                        } catch (InterruptedException e) {
                        }
                    }
                }
            }
        }
    }


	/**
	  * Image attribute of the splash
	  */
    private Image image;

	/**
	  * Main constructor
	  * @param parent Parent Frame
	  * @param image Image of the splash screen
	  * @see MediaTracker
	  */
    private SplashWindow(Frame parent, Image image) {
        super(parent);
        this.image = image;

		// Media manager (pictures / songs / videos etc.
        MediaTracker mt = new MediaTracker(this);
        mt.addImage(image, 0);
        try {
            mt.waitForID(0);
        } catch (InterruptedException ie) {
        }

		// The splash screen has the size of the picture
        this.setSize(new Dimension(this.image.getWidth(this), this.image
                .getHeight(this)));
        this.setLocationRelativeTo(null);
        this.setVisible(true);
    }


    private boolean paintCalled = false;

	/**
	  * The background image fills the frame so we 
      * don't need to clear the applet on repaints. 
      * Just call the paint method.
      * @param g Graphics
      * @see Graphics
      */
    public void update(Graphics g) {
        paint(g);
    }

	/**
	  * Draw the splash screen and notify
	  * @param g Graphics
	  * @see Graphics
	  */
    public void paint(Graphics g) {
        g.drawImage(image, 0, 0, this);
        g.drawString("BirDy 0.2.0", 10 ,g.getClipBounds().height - 10);
        // If we wait in splash() method, stop waiting now.
        if (!paintCalled) {
            paintCalled = true;
            synchronized (this) {
                notifyAll();
            }
        }
    }
}
