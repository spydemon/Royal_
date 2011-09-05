package net.sf.royal.gui.util.tables;

import net.sf.royal.datamodel.Color;
import net.sf.royal.datamodel.Illustration;
import net.sf.royal.datamodel.Scenario;
import net.sf.royal.datamodel.Work;

public class WorkObject extends VersionableImpl {
	public static final int ILLUSTRATION = 0;
	public static final int COLORATION = 1;
	public static final int SCENARIO = 2;
	
	private int type;
	private Long workId;
	private AuthorObject authorObject;
    private Long tekId;
	
	public WorkObject(){
	}
	
	public WorkObject(Work work){
		if (work != null){
            type = WorkObject.getType(work);
			
			workId = work.getId();
			authorObject = new AuthorObject(work.getAuthor());
		}
        this.tekId = work.getTekId();
	}	

	/**
	 * @return Returns the type.
	 */
	public int getType() {
		return type;
	}

	/**
	 * @param type The type to set.
	 */
	public void setType(int type) {
		this.type = type;
	}

	/**
	 * @return Returns the workId.
	 */
	public Long getWorkId() {
		return workId;
	}

	/**
	 * @param workId The workId to set.
	 */
	public void setWorkId(Long workId) {
		this.workId = workId;
	}
	
	/**
	 * @return Returns the authorObject.
	 */
	public AuthorObject getAuthorObject() {
		return authorObject;
	}

	/**
	 * @param authorObject The authorObject to set.
	 */
	public void setAuthorObject(AuthorObject authorObject) {
		this.authorObject = authorObject;
	}
    
    public String toString(){
        String res = "WorkObject : " +
                     "\n  + id : " + this.workId +
                     "\n  + type : " + this.type +
                     "\n  + author : " + this.authorObject.toString();
        return res;
    }
    
    public WorkObject getClone(){
        WorkObject res = new WorkObject();
        res.workId = this.getWorkId();
        res.setType(this.getType());
        res.setAuthorObject(this.getAuthorObject());
        res.setVersion(this.getVersion());
        return res;
    }

    public Long getTekId() {
        return tekId;
    }

    public void setTekId(Long tekId) {
        this.tekId = tekId;
    }
    
    public static int getType(Work work){
        if (work instanceof Illustration){
            return ILLUSTRATION;
        } else if (work instanceof Color){
            return COLORATION;
        } else if (work instanceof Scenario){
            return SCENARIO;
        }
        return -1;
    }
}
