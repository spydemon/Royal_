package net.sf.birdy.docks;

import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.IOException;

import javax.swing.JFrame;

import net.sf.birdy.gui.docks.DockablePanel;
import net.sf.birdy.gui.docks.DockablePanelFactory;
import net.sf.birdy.gui.docks.Perspective;
import net.sf.birdy.gui.docks.PerspectiveFactory;

public class Dock {

    JFrame frame;
    Perspective perspective;
    
    public Dock(){
        //DockableContainerFactory.setFactory(new BirDyDockableContainerFactory());
        DockablePanel testPanel1 = DockablePanelFactory.createTestPanel("ok", "test1", true);
        DockablePanel testPanel2 = DockablePanelFactory.createTestPanel("cancel", "test2", false);
        DockablePanel testPanel3 = DockablePanelFactory.createTestPanel("back", "test3", true);
        
        DockablePanel[] panels = {testPanel1,testPanel2,testPanel3};
        
        perspective = PerspectiveFactory.createTestPerspective("toto.xml", panels);
        
        frame = new JFrame("Docks - Test");
        frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        frame.addWindowListener(new WindowListener(){
            public void windowActivated(WindowEvent arg0) {}
            public void windowClosed(WindowEvent arg0) {}

            public void windowClosing(WindowEvent arg0) {
                try {
                    perspective.save();
                } catch (IOException e) {
                    e.printStackTrace();
                }
                System.exit(0);
            }

            public void windowDeactivated(WindowEvent arg0) {}
            public void windowDeiconified(WindowEvent arg0) {}
            public void windowIconified(WindowEvent arg0) {}
            public void windowOpened(WindowEvent arg0) {}
            
        });
        frame.getContentPane().add(perspective);
        frame.pack();
    }
    
    
    /**
     * @return Returns the frame.
     */
    public JFrame getFrame() {
        return frame;
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        Dock dock = new Dock();
        dock.getFrame().setVisible(Boolean.TRUE.booleanValue());
    }

}
