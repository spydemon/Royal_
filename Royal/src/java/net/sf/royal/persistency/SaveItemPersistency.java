package net.sf.royal.persistency;

import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import net.sf.royal.datamodel.Album;
import net.sf.royal.datamodel.AlbumDAO;
import net.sf.royal.datamodel.Author;
import net.sf.royal.datamodel.AuthorDAO;
import net.sf.royal.datamodel.Borrower;
import net.sf.royal.datamodel.BorrowerDAO;
import net.sf.royal.datamodel.Collection;
import net.sf.royal.datamodel.CommentedImage;
import net.sf.royal.datamodel.Dedication;
import net.sf.royal.datamodel.Editor;
import net.sf.royal.datamodel.EditorDAO;
import net.sf.royal.datamodel.HibernateUtil;
import net.sf.royal.datamodel.Loan;
import net.sf.royal.datamodel.LoanDAO;
import net.sf.royal.datamodel.PojoDAO;
import net.sf.royal.datamodel.Serie;
import net.sf.royal.datamodel.SerieDAO;
import net.sf.royal.datamodel.Type;
import net.sf.royal.datamodel.TypeDAO;
import net.sf.royal.datamodel.Work;
import net.sf.royal.datamodel.Bibliotheque;
import net.sf.royal.exception.PersistencyException;
import net.sf.royal.gui.guimodel.loan.BorrowerGuiObject;
import net.sf.royal.gui.guimodel.loan.LoanAlbumGuiObject;
import net.sf.royal.gui.util.tables.CommentedImageObject;
import net.sf.royal.gui.util.tables.Versionable;

import org.apache.log4j.Logger;

public class SaveItemPersistency
{
	private final static Logger logger = Logger.getLogger(SaveItemPersistency.class);

    private static PojoDAO albumDAO = new AlbumDAO();
    private static PojoDAO serieDAO = new SerieDAO();
    private static PojoDAO editorDAO = new EditorDAO();
    private static PojoDAO typeDAO = new TypeDAO();
    private static PojoDAO authorDAO = new AuthorDAO();
    private static PojoDAO borrowerDAO = new BorrowerDAO();
    private static PojoDAO loanDAO = new LoanDAO();
	
    public static void saveBibliotheque(Bibliotheque b) {
    	try{
    		HibernateUtil.beginTransaction();
    		logger.debug("Save " + b.getName() + " (" + b.getId() + ")");
    		albumDAO.save(b);     
    		HibernateUtil.commitTransaction();
    	} catch (PersistencyException e) {
        try {
            HibernateUtil.rollbackTransaction();
        } catch (PersistencyException e1) {
            e1.manageException();
        }
        PersistencyException pe = new PersistencyException(e, PersistencyException.CONTINUE);
        pe.manageException();
    }
    	
    }
    public static void saveAlbum(Album album) 
    {
        try{
            HibernateUtil.beginTransaction();
            logger.debug("Save " + album.getTitle() + " (" + album.getId() + ")");
            albumDAO.save(album);     
            HibernateUtil.commitTransaction();
        } catch (PersistencyException e) {
            try {
                HibernateUtil.rollbackTransaction();
            } catch (PersistencyException e1) {
                e1.manageException();
            }
            PersistencyException pe = new PersistencyException(e, PersistencyException.CONTINUE);
            pe.manageException();
        }
    }
        
    
    public static void saveAuthor(Author author){
        try {
            HibernateUtil.beginTransaction();
            logger.debug("Save " + author.toString() + " (" + author.getId() + ")");
            authorDAO.save(author);
            HibernateUtil.commitTransaction();
            
        } catch (PersistencyException e) {
            try {
                HibernateUtil.rollbackTransaction();
            } catch (PersistencyException e1) {
                e1.manageException();
            }
            PersistencyException pe = new PersistencyException(e, PersistencyException.CONTINUE);
            pe.manageException();
        }
    }

    public static void saveCollection(Collection collection){
        try{
          HibernateUtil.beginTransaction();
          editorDAO.save(collection);
          HibernateUtil.commitTransaction();
          
        } catch (PersistencyException e) {
            try {
                HibernateUtil.rollbackTransaction();
            } catch (PersistencyException e1) {
                e1.manageException();
            }
            PersistencyException pe = new PersistencyException(e, PersistencyException.CONTINUE);
            pe.manageException();
        }
    }
    
	/**
	  * Add a dedication to the albumID
	  * @param albimId
	  * @param location
	  * @param date
	  * @param description
	  * @param imageURL
	  */
    public static void saveDedication(Long albumID, String location, Date date, String description, String imageURL){
        Album album = null;
        try {
            album = (Album) albumDAO.findByPrimaryKey(albumID);
            Dedication dedication = album.getDedication();
            dedication.setLocation(location);
            dedication.setDate(date);
            dedication.setDescription(description);
            dedication.setImageURL(imageURL);
            HibernateUtil.beginTransaction();
            albumDAO.save(album);
            HibernateUtil.commitTransaction();
        } catch (PersistencyException e) {
            e.manageException();
        }
    }
    
    /**
     * Save the commented images of an Album
     * @param albumID with the album id we can get the serieId
     * @param commentedImageObjects List which contains ComentedImageObject
     */
    public static void saveAlbumCommentedImages(Long albumId, List<CommentedImageObject> commentedImageObjects){
        try {
            Album album = (Album) albumDAO.findByPrimaryKey(albumId);
            Object[] oldCommentedImages = album.getGallery().toArray();
            HashMap<CommentedImage, CommentedImageObject> ciToSyncId = new HashMap<CommentedImage, CommentedImageObject>();
            for(int i=0;i<commentedImageObjects.size();i++){
                CommentedImageObject ciObject = (CommentedImageObject) commentedImageObjects.get(i);
                if (ciObject.getVersion() == Versionable.NEW){
                    CommentedImage cImage = new CommentedImage();
                    PersistencyManager.setCommentedImage(cImage, ciObject);
                    ciToSyncId.put(cImage, ciObject);
                    cImage.setAlbum(album);
                    album.addCommentedImage(cImage);
                } else if (ciObject.getVersion() == Versionable.UPDATED){
                    PersistencyManager.updateCommentedImage(ciObject, oldCommentedImages);
                } else if (ciObject.getVersion() == Versionable.REMOVED){
                    PersistencyManager.deleteCommentedImages(ciObject, oldCommentedImages);
                }
            }
            
            HibernateUtil.beginTransaction();
            albumDAO.save(album);
            HibernateUtil.commitTransaction();
            Iterator<CommentedImage> iter = ciToSyncId.keySet().iterator();
            while (iter.hasNext()) {
                CommentedImage ci = (CommentedImage) iter.next();
                ((CommentedImageObject) ciToSyncId.get(ci)).setId(ci.getId());
            }
        } catch (PersistencyException e) {
            e.manageException();
        }
    }
        
    public static void saveEditor(Editor editor){
        try{
          HibernateUtil.beginTransaction();
          editorDAO.save(editor);
          HibernateUtil.commitTransaction();
          
        } catch (PersistencyException e) {
            try {
                HibernateUtil.rollbackTransaction();
            } catch (PersistencyException e1) {
                e1.manageException();
            }
            PersistencyException pe = new PersistencyException(e, PersistencyException.CONTINUE);
            pe.manageException();
        }
    }

    /**
     * Save or update the given Serie
     * @param serie we want to save
     */
    public static void saveSerie(Serie serie){
        try{
          HibernateUtil.beginTransaction();
          logger.debug("Save serie : " + serie.getName() + " (" + serie.getId() + ")");
          serieDAO.save(serie);
          HibernateUtil.commitTransaction();
          
        } catch (PersistencyException e) {
            try {
                HibernateUtil.rollbackTransaction();
            } catch (PersistencyException e1) {
                e1.manageException();
            }
            PersistencyException pe = new PersistencyException(e, PersistencyException.CONTINUE);
            pe.manageException();
        }
    }
       
    public static void saveType(Type type){
        try {
            HibernateUtil.beginTransaction();
            logger.debug("Save Type : " + type.getName() + " (" + type.getId() + ")");
            typeDAO.save(type);
            HibernateUtil.commitTransaction();
            
        } catch (PersistencyException e) {
            try {
                HibernateUtil.rollbackTransaction();
            } catch (PersistencyException e1) {
                e1.manageException();
            }
            PersistencyException pe = new PersistencyException(e, PersistencyException.CONTINUE);
            pe.manageException();
        }
    }
    
    public static void saveWork(Work w)
    {
    	if(w.getAuthor() == null || w.getAlbum() == null)
    	{
    		logger.error("Work w is not persistent");
    		return;
    	}
        try {
            HibernateUtil.beginTransaction();
            logger.debug("Save Work : " + w.getId() + 
            		" (Album : " + w.getAlbum().getId() + " - Author : " + w.getAuthor().getId() + " )");
            typeDAO.save(w);
            HibernateUtil.commitTransaction();
            
        } catch (PersistencyException e) {
            try {
                HibernateUtil.rollbackTransaction();
            } catch (PersistencyException e1) {
                e1.manageException();
            }
            PersistencyException pe = new PersistencyException(e, PersistencyException.CONTINUE);
            pe.manageException();
        }
    }
    
    public static void saveBorrowerGuiObject(BorrowerGuiObject borrower){
        try {
            Borrower res = null;
            if (borrower.getId() != null){
                res = (Borrower) borrowerDAO.findByPrimaryKey(borrower.getId());
            } else {
                res = new Borrower();
            }
            res.setCreation(borrower.getCreation());
            res.setEmail(borrower.getEmail());
            res.setFirstName(borrower.getFirstName());
            res.setName(borrower.getName());
            res.setPhone(borrower.getPhone());
            
            HibernateUtil.beginTransaction();
            borrowerDAO.save(res);
            HibernateUtil.commitTransaction();
            borrower.setId(res.getId());
        } catch (Exception e) {
            try {
                HibernateUtil.rollbackTransaction();
            } catch (PersistencyException e1) {
                e1.manageException();
            }
            PersistencyException pe = new PersistencyException(e, PersistencyException.CONTINUE);
            pe.manageException();
        }
    }
    
    public static void saveLoanAlbumGuiObjects(Long BorrowerID, List<LoanAlbumGuiObject> toAdd, List<LoanAlbumGuiObject> toRemove){
        try {
            HibernateUtil.beginTransaction();
            Borrower borrower = (Borrower) borrowerDAO.findByPrimaryKey(BorrowerID);
            for(int i=0;i<toAdd.size();i++){
                LoanAlbumGuiObject lago = toAdd.get(i);
                Loan loan = new Loan();
                loan.setBegin(lago.getLoanDate());
                loan.setMaxDays(lago.getMaxDays());
                loan.setPenaltyByDay(lago.getPenaltyByDay());
                loan.setPriceByDay(lago.getPriceByDay());
                loan.setBorrower(borrower);
                borrower.addLoan(loan);
                Album album = (Album) albumDAO.findByPrimaryKey(lago.getAlbumID());
                loan.setAlbum(album);
                borrowerDAO.save(borrower);
                loanDAO.save(loan);
                lago.setLoaned(true);
                lago.setLoanID(loan.getId());
            }
            for(int i=0;i<toRemove.size();i++){
                LoanAlbumGuiObject lago = (LoanAlbumGuiObject) toRemove.get(i);
                Loan loan = (Loan) loanDAO.findByPrimaryKey(lago.getLoanID());
                loan.setEnd(new Date());
                loanDAO.save(loan);
            }
            HibernateUtil.commitTransaction();
        } catch (Exception e) {
            try {
                HibernateUtil.rollbackTransaction();
            } catch (PersistencyException e1) {
                e1.manageException();
            }
            PersistencyException pe = new PersistencyException(e, PersistencyException.CONTINUE);
            pe.manageException();
        }
    }
}	
