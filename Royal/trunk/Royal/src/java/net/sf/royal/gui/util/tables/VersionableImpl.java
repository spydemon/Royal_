package net.sf.royal.gui.util.tables;

public class VersionableImpl implements Versionable {

    protected int version = Versionable.NONE;
    
    /**
     * @return Returns the state.
     */
    public int getVersion() {
        return version;
    }


    /**
     * @param version The state to set.
     */
    public void setVersion(int version) {
        this.version = version;
    }
}
