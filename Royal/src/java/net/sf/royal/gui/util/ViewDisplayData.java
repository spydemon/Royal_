package net.sf.royal.gui.util;

public class ViewDisplayData {

    private String title;
    private String iconPath;
    
    
    public ViewDisplayData(String title, String iconPath) {
        this.title = title;
        this.iconPath = iconPath;
    }
    /**
     * @return Returns the iconPath.
     */
    public String getIconPath() {
        return iconPath;
    }
    /**
     * @param iconPath The iconPath to set.
     */
    public void setIconPath(String iconPath) {
        this.iconPath = iconPath;
    }
    /**
     * @return Returns the title.
     */
    public String getTitle() {
        return title;
    }
    /**
     * @param title The title to set.
     */
    public void setTitle(String title) {
        this.title = title;
    }
    
    
}
