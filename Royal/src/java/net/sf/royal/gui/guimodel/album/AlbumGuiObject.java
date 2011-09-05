package net.sf.royal.gui.guimodel.album;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import net.sf.royal.datamodel.Tome;
import net.sf.royal.datamodel.Work;
import net.sf.royal.gui.util.tables.VersionableImpl;


public class AlbumGuiObject extends VersionableImpl {

	private Long albumId;
	private String cover;
	private String title;
	private String tome;
	private Date registration;
	private String isbn;
	private String dimension;
	private int pageCount;
    private int copies;
	private boolean integral;
	private boolean original;
	private boolean special;
	private List<Work> works = new ArrayList<Work>();
	private SerieGuiObject serie;
	private EditorGuiObject editor;
	private CollectionGuiObject collection;
    private Long tekId;
	
	public AlbumGuiObject(Long albumId){
		this.albumId = albumId;
	}
	
	/**
	 * @return Returns the works.
	 */
	public List<Work> getWorks() {
		return works;
	}
	/**
	 * @param works The works to set.
	 */
	public void setWorks(List<Work> works) {
		this.works = works;
	}

	/**
	 * @return Returns the albumId.
	 */
	public Long getAlbumId() {
		return albumId;
	}

	/**
	 * @param albumId The albumId to set.
	 */
	public void setAlbumId(Long albumId) {
		this.albumId = albumId;
	}

	/**
	 * @return Returns the cover.
	 */
	public String getCover() {
		return cover;
	}

	/**
	 * @param cover The cover to set.
	 */
	public void setCover(String cover) {
		this.cover = cover;
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

	/**
	 * @return Returns the tome.
	 */
	public String getTome() {
		return tome;
	}

	/**
	 * @param tome The tome to set.
	 */
	public void setTome(Tome tome) {
        this.tome = "";
        if (tome.getRevision() != null){
            this.tome = String.valueOf(tome.getNumber()) + "." + tome.getRevision();
        } else {
            if (tome.getNumber() != 0){
                this.tome = String.valueOf(tome.getNumber());
            }
        }
	}
    
    public void setTome(String tome){
        this.tome = tome;
    }

	/**
	 * @return Returns the dimension.
	 */
	public String getDimension() {
		return dimension;
	}

	/**
	 * @param dimension The dimension to set.
	 */
	public void setDimension(String dimension) {
		this.dimension = dimension;
	}

	/**
	 * @return Returns the integral.
	 */
	public boolean isIntegral() {
		return integral;
	}

	/**
	 * @param integral The integral to set.
	 */
	public void setIntegral(boolean integral) {
		this.integral = integral;
	}

	/**
	 * @return Returns the isbn.
	 */
	public String getIsbn() {
		return isbn;
	}

	/**
	 * @param isbn The isbn to set.
	 */
	public void setIsbn(String isbn) {
		this.isbn = isbn;
	}

	/**
	 * @return Returns the original.
	 */
	public boolean isOriginal() {
		return original;
	}

	/**
	 * @param original The original to set.
	 */
	public void setOriginal(boolean original) {
		this.original = original;
	}

	/**
	 * @return Returns the pageCount.
	 */
	public int getPageCount() {
		return pageCount;
	}

	/**
	 * @param pageCount The pageCount to set.
	 */
	public void setPageCount(int pageCount) {
		this.pageCount = pageCount;
	}

	/**
	 * @return Returns the registration.
	 */
	public Date getRegistration() {
		return registration;
	}

	/**
	 * @param registration The registration to set.
	 */
	public void setRegistration(Date registration) {
		this.registration = registration;
	}

	/**
	 * @return Returns the special.
	 */
	public boolean isSpecial() {
		return special;
	}

	/**
	 * @param special The special to set.
	 */
	public void setSpecial(boolean special) {
		this.special = special;
	}
	
	/**
	 * @return Returns the serie.
	 */
	public SerieGuiObject getSerie() {
		return serie;
	}

	/**
	 * @param serie The serie to set.
	 */
	public void setSerie(SerieGuiObject serie) {
		this.serie = serie;
	}

	/**
	 * @return Returns the collection.
	 */
	public CollectionGuiObject getCollection() {
		return collection;
	}

	/**
	 * @param collection The collection to set.
	 */
	public void setCollection(CollectionGuiObject collection) {
		this.collection = collection;
	}

	/**
	 * @return Returns the editor.
	 */
	public EditorGuiObject getEditor() {
		return editor;
	}

	/**
	 * @param editor The editor to set.
	 */
	public void setEditor(EditorGuiObject editor) {
		this.editor = editor;
    }
    
    public Tome getDataModelTome(){
        Tome res = new Tome();
        if (tome != null && !tome.equals("")){
            String[] splitedTome = tome.split("\\.");
            if (splitedTome.length > 0){
                res.setNumber(Integer.parseInt(splitedTome[0]));}
            if (splitedTome.length > 1){
                res.setRevision(splitedTome[1]);
            }
        }
        return res;
    }

    /**
     * @return Returns the copies.
     */
    public int getCopies() {
        return copies;
    }

    /**
     * @param copies The copies to set.
     */
    public void setCopies(int copies) {
        this.copies = copies;
    }

    public Long getTekId() {
        return tekId;
    }

    public void setTekId(Long tekId) {
        this.tekId = tekId;
    }
}
