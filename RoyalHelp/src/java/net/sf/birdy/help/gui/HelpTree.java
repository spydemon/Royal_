package net.sf.birdy.help.gui;

import javax.swing.JTree;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;

import net.sf.birdy.help.core.model.Help;

public class HelpTree extends JTree 
{    
    public HelpTree()
    {
        this.setHelp(HelperManager.getInstance().getHelp());
        this.setIcons();
    }
    
    public void setHelp(Help help)
    {
        ((DefaultTreeModel) this.getModel()).setRoot(help);
    }
    
    private void setIcons()
    {
        DefaultTreeCellRenderer renderer = new DefaultTreeCellRenderer();
        //renderer.setLeafIcon(new ImageIcon(ClassLoader.getSystemResource(Helper.ICON_PATH + "/" + "file_obj.gif")));
        //renderer.setClosedIcon(new ImageIcon(ClassLoader.getSystemResource(Helper.ICON_PATH + "/" + "toc_closed.gif")));
        //renderer.setOpenIcon(new ImageIcon(ClassLoader.getSystemResource(Helper.ICON_PATH + "/" + "toc_open.gif")));
        //renderer.setLeafIcon(new ImageIcon(Helper.ICON_PATH + "/" + "sync.gif"));
        //renderer.setClosedIcon(new ImageIcon(Helper.ICON_PATH + "/" + "sync.gif"));
        //renderer.setOpenIcon(new ImageIcon(Helper.ICON_PATH + "/" + "sync.gif"));
        this.setCellRenderer(renderer);
    }

}
