package net.sf.royal.gui.wizard;

public abstract class DownloadAction {

    private String title;
    private String key;
    
    public DownloadAction(String title, String key){
        this.title = title;
        this.key = key;
    }
    
    public abstract void doAction();

    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }
}
