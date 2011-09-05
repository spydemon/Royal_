package net.sf.birdy.persistency;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import junit.framework.TestCase;
import net.sf.birdy.datamodel.Album;
import net.sf.birdy.datamodel.AlbumDAO;
import net.sf.birdy.datamodel.Author;
import net.sf.birdy.datamodel.AuthorDAO;
import net.sf.birdy.datamodel.Collection;
import net.sf.birdy.datamodel.CollectionDAO;
import net.sf.birdy.datamodel.Color;
import net.sf.birdy.datamodel.CommentedImage;
import net.sf.birdy.datamodel.Dedication;
import net.sf.birdy.datamodel.Editor;
import net.sf.birdy.datamodel.EditorDAO;
import net.sf.birdy.datamodel.HibernateUtil;
import net.sf.birdy.datamodel.IllustrationDAO;
import net.sf.birdy.datamodel.Model;
import net.sf.birdy.datamodel.Scenario;
import net.sf.birdy.datamodel.Serie;
import net.sf.birdy.datamodel.SerieDAO;
import net.sf.birdy.datamodel.TestHelper;
import net.sf.birdy.datamodel.Type;
import net.sf.birdy.datamodel.TypeDAO;
import net.sf.birdy.datamodel.Work;
import net.sf.birdy.datamodel.WorkDAO;
import net.sf.birdy.exception.PersistencyException;
import net.sf.birdy.gui.guimodel.album.AlbumGuiObject;
import net.sf.birdy.gui.guimodel.album.CollectionGuiObject;
import net.sf.birdy.gui.guimodel.album.EditorGuiObject;
import net.sf.birdy.gui.guimodel.album.SerieGuiObject;
import net.sf.birdy.gui.guimodel.album.TypeGuiObject;
import net.sf.birdy.gui.manager.LocaleManager;
import net.sf.birdy.gui.util.tables.CommentedImageObject;
import net.sf.birdy.gui.util.tables.Versionable;
import net.sf.birdy.gui.util.tables.WorkObject;

public class PersistencyTest extends TestCase {

	public void testInitSessionHibernate(){
		try
		{
			HibernateUtil.initSessionFactory();
		} catch (Exception e) {
			e.printStackTrace();
		}		
	}

    public void testfindDedicationByAlbumID(){
        Serie serie = TestHelper.instance().createOneSerie();
        Album album = TestHelper.instance().createOneAlbum(serie);
        SerieDAO sdDAO = new SerieDAO();
        AlbumDAO alDAO = new AlbumDAO();
        Dedication dedication = TestHelper.instance().createOneDedication(album);
        try {
            HibernateUtil.beginTransaction();
            sdDAO.save(serie);
            alDAO.save(album);
        
            HibernateUtil.commitTransaction();
        } catch (PersistencyException e) {
            e.printStackTrace();
        }
        TestHelper.instance().compareTwoDedication(dedication, PersistencyManager.findDedicationByAlbumID(album.getId()));
    }
    
    public void testSaveDedication(){
        Serie serie = TestHelper.instance().createOneSerie();
        SerieDAO sdDAO = new SerieDAO();
        Album album = TestHelper.instance().createOneAlbum(serie);
        try {
            sdDAO.save(serie);
            AlbumDAO alDAO = new AlbumDAO();
            Dedication dedication = TestHelper.instance().createOneDedication(album);
            HibernateUtil.beginTransaction();
            alDAO.save(album);
        
            HibernateUtil.commitTransaction();
        
            String newLocation = "new location";
            Date newDate = new Date();
            String newDescription = "new description";
            String newImageURL = "new imageURL";
            PersistencyManager.saveDedication(album.getId(),newLocation, new Date(), newDescription, newImageURL);
            Dedication foundDedication = ((Album) alDAO.findByPrimaryKey(album.getId())).getDedication();
            assertNotNull("The dedication is null", foundDedication);
            assertTrue("The location is not the good one", foundDedication.getLocation().equals(newLocation));
            assertTrue("The description is not the good one", foundDedication.getDescription().equals(newDescription));
            assertTrue("The imageURL is not the good one", foundDedication.getImageURL().equals(newImageURL));
        } catch (PersistencyException e) {
            e.printStackTrace();
        }
    }
    
    public void testFindAlbumByID(){
        Serie serie = TestHelper.instance().createOneSerie();
        Album album = TestHelper.instance().createOneAlbum(serie);
        SerieDAO sdDAO = new SerieDAO();
        AlbumDAO alDAO = new AlbumDAO();
        try {
            HibernateUtil.beginTransaction();
            sdDAO.save(serie);
            alDAO.save(album);
        
            HibernateUtil.commitTransaction();
        } catch (PersistencyException e) {
            e.printStackTrace();
        }
        TestHelper.instance().compareTwoAlbum(album, PersistencyManager.findAlbumByID(album.getId()));
    }
    
    public void testSaveAlbumANDDedication(){
        Serie serie = TestHelper.instance().createOneSerie();
        Album album = TestHelper.instance().createOneAlbum(serie);
        SerieDAO sdDAO = new SerieDAO();
        AlbumDAO alDAO = new AlbumDAO();
        Dedication dedication = TestHelper.instance().createOneDedication(album);
        try{
            HibernateUtil.beginTransaction();
            sdDAO.save(serie);
            alDAO.save(album);
            HibernateUtil.commitTransaction();
        
            String newState = "new state";
            int newNote = 12;
            float newPrice = 19;
            Date newPurchaseDate = new Date();
            String newLocation = "new location";
            Date newDate = new Date();
            String newDescription = "new description";
            String newImageURL = "new imageURL";
            PersistencyManager.saveAlbum(album.getId(), newState, newNote, newPrice, newPurchaseDate, newLocation, newDate, newDescription, newImageURL);
            Album foundAlbum = (Album) alDAO.findByPrimaryKey(album.getId());
            assertNotNull("The album is null", foundAlbum);
            assertTrue("The state is not the good one", foundAlbum.getState().equals(newState));
            assertTrue("The price is not the good one", foundAlbum.getPrice() == newPrice);
            assertTrue("The note is not the good one", foundAlbum.getNote() == newNote);
            
            Dedication foundDedication = foundAlbum.getDedication();
            assertNotNull("The dedication is null", foundDedication);
            assertTrue("The location is not the good one", foundDedication.getLocation().equals(newLocation));
            assertTrue("The description is not the good one", foundDedication.getDescription().equals(newDescription));
            assertTrue("The imageURL is not the good one", foundDedication.getImageURL().equals(newImageURL));
        } catch (PersistencyException e) {
            e.printStackTrace();
        }
    }
    
    public void testfindCommentedImagesBySerieID(){
        Serie serie = TestHelper.instance().createOneSerie();
        SerieDAO serieDAO = new SerieDAO();
        CommentedImage ci1 = TestHelper.instance().createOneCommentedImage(serie, 1);
        CommentedImage ci3 = TestHelper.instance().createOneCommentedImage(serie, 3);
        CommentedImage ci2 = TestHelper.instance().createOneCommentedImage(serie, 2);
        try {
            HibernateUtil.beginTransaction();
            serieDAO.save(serie);
            HibernateUtil.commitTransaction();        
        } catch (PersistencyException e) {       
            e.printStackTrace();  
        }
        
        List foundCIs = PersistencyManager.findCommentedImagesBySerieID(serie.getId(), Boolean.TRUE.booleanValue());
        assertTrue("The number of commented images is not correct", foundCIs.size() == 3);
        assertTrue("The first commented images is not the correct one", ((Model) foundCIs.get(0)).getId().longValue() == ci1.getId().longValue());
        assertTrue("The second commented images is not the correct one", ((Model) foundCIs.get(1)).getId().longValue() == ci2.getId().longValue());
        assertTrue("The second commented images is not the correct one", ((Model) foundCIs.get(2)).getId().longValue() == ci3.getId().longValue());
    }
    
    public void testSaveSerieCommentedImages(){
        Serie serie = TestHelper.instance().createOneSerie();
        SerieDAO serieDAO = new SerieDAO();
        CommentedImage ci1 = TestHelper.instance().createOneCommentedImage(serie, 0);
        CommentedImage ci2 = TestHelper.instance().createOneCommentedImage(serie, 1);
        try{
            HibernateUtil.beginTransaction();
            serieDAO.save(serie);
            HibernateUtil.commitTransaction();   
        
            
            CommentedImageObject ciObjectNew = new CommentedImageObject(TestHelper.instance().createOneCommentedImage(serie,2));
            ciObjectNew.setVersion(Versionable.NEW);
            CommentedImageObject ciObjectUpdated = new CommentedImageObject(ci1);
            ciObjectUpdated.setPosition(24);
            ciObjectUpdated.setVersion(Versionable.UPDATED);
            CommentedImageObject ciObjectRemoved = new CommentedImageObject(ci2);
            ciObjectRemoved.setVersion(Versionable.REMOVED);
            
            List ciObjects = new ArrayList();
            ciObjects.add(ciObjectNew);
            ciObjects.add(ciObjectUpdated);
            ciObjects.add(ciObjectRemoved);
            
            PersistencyManager.saveSerieCommentedImages(serie.getId(), ciObjects);
            
            Serie foundSerie = (Serie) serieDAO.findByPrimaryKey(serie.getId());
            assertNotNull("The serie is not found", foundSerie);
            Object[] list = foundSerie.getGallery().toArray();
            assertTrue("The new commented image is not correct", isCommentedImageInArray(list, ciObjectNew));
            assertTrue("The updated commented image is not correct", isCommentedImageInArray(list, ciObjectUpdated));
            assertFalse("The removed commented image is still in the db", isCommentedImageInArray(list, ciObjectRemoved));
        
        } catch (PersistencyException e) {     
            e.printStackTrace(); 
        }
    }
    
    private boolean isCommentedImageInArray(Object[] list, CommentedImageObject ciObject){
        for(int i=0;i<list.length;i++){
            CommentedImage cImage = (CommentedImage) list[i];
            if (cImage.getComment().equals(ciObject.getComment())
                    && cImage.getHalign() == ciObject.getHalign()
                    && cImage.getImageURL().equals(ciObject.getImageURL())
                    && cImage.getPosition() == ciObject.getPosition()
                    && cImage.getValign().equals(ciObject.getValign())){
                return Boolean.TRUE.booleanValue();
            }
        }
        return Boolean.FALSE.booleanValue();
    }
    
    public void testfindAGO(){
    		Serie serie = TestHelper.instance().createOneSerie();
    		SerieDAO sdao = new SerieDAO();
    		Editor editor = TestHelper.instance().createOneEditor();
    		EditorDAO edao = new EditorDAO();
    		Collection collection = TestHelper.instance().createOneCollection(editor, Boolean.FALSE.booleanValue());
    		Album album = TestHelper.instance().createOneAlbum(serie, collection);
    		Author author = TestHelper.instance().createOneAuthor();
    		AuthorDAO adao = new AuthorDAO();
    		Work work = TestHelper.instance().createOneIllustration(author, album);
    		WorkDAO wdao = new IllustrationDAO();
    		try{
        		HibernateUtil.beginTransaction();
        		edao.save(editor);
        		sdao.save(serie);
        		adao.save(author);
        		wdao.save(work);
        		HibernateUtil.commitTransaction();   
        } catch (PersistencyException e) {             
            e.printStackTrace();         
        }
    		
    		assertNotNull("Album object has no id", album.getId());
    		AlbumGuiObject ago = PersistencyManager.findAGO(album.getId());
    		
    		assertTrue("The ago is not correct (id)", ago.getAlbumId().longValue() == album.getId().longValue());
    		assertTrue("The ago is not correct (cover)", ago.getCover().equals(album.getCover()));
    		assertTrue("The ago is not correct (dimension)", ago.getDimension().equals(album.getDimension()));
    		assertTrue("The ago is not correct (isbn)", ago.getIsbn().equals(album.getIsbn()));
    		assertTrue("The ago is not correct (page count)", ago.getPageCount() == album.getPageCount());
    		assertTrue("The ago is not correct (title)", ago.getTitle().equals(album.getTitle()));
    		assertTrue("The ago is not correct (tome)", ago.getTome().equals(album.getTome().getNumber() + "." + album.getTome().getRevision()));
    		
    		SerieGuiObject sgo = ago.getSerie();
    		assertTrue("The sgo is no correct (id)", sgo.getId().longValue() == serie.getId().longValue());
    		assertTrue("The sgo is not correct (name)", sgo.getName().equals(serie.getName()));
    		
    		CollectionGuiObject cgo = ago.getCollection();
    		assertTrue("The cgo is no correct (id)", cgo.getId().longValue() == collection.getId().longValue());
    		assertTrue("The cgo is not correct (name)", cgo.getName().equals(collection.getName()));
    		
    		EditorGuiObject ego = ago.getEditor();
    		assertTrue("The ego is no correct (id)", ego.getId().longValue() == editor.getId().longValue());
    		assertTrue("The ego is not correct (name)", ego.getName().equals(editor.getName()));
    		
    		List works = ago.getWorks();
    		assertTrue("There is no work linked with ago", works.size() == 1);
    		WorkObject workObject = (WorkObject) ago.getWorks().get(0);
    		assertTrue("The workObject is not correct (id)", workObject.getWorkId().longValue() == work.getId().longValue());
    		
    		assertTrue("The workObject is not correct (author id)", workObject.getAuthorObject().getId().longValue() == author.getId().longValue());
    		assertTrue("The workObject is not correct (author first name)", workObject.getAuthorObject().getFirstName().equals(author.getFirstName()));
    		assertTrue("The workObject is not correct (author name)", workObject.getAuthorObject().getName().equals(author.getName()));
    		assertTrue("The workObject is not correct (author nickname)", workObject.getAuthorObject().getNickName().equals(author.getNickName()));
    }
    
    public void testFindEditorGuiObject(){
        Editor editor1 = TestHelper.instance().createOneEditor();
        Editor editor2 = TestHelper.instance().createOneEditor();
        EditorDAO dao = new EditorDAO();
        try{
        HibernateUtil.beginTransaction();
        dao.save(editor1);
        dao.save(editor2);
        HibernateUtil.commitTransaction();         
        } catch (PersistencyException e) {             
            e.printStackTrace();         
        }
        
        List editors = PersistencyManager.findEditorGuiObjects();
        for(int i=1;i<editors.size();i++){
            EditorGuiObject ed1 = (EditorGuiObject) editors.get(i - 1);
            EditorGuiObject ed2 = (EditorGuiObject) editors.get(i);
            if(ed1.getName().compareTo(ed2.getName()) > 0){
                fail("The editors are not sorted");
            }
        }
    }
    
    public void testFindCollectionGuiObjects(){
        Editor editor = TestHelper.instance().createOneEditor();
        EditorDAO editorDAO = new EditorDAO();
        Collection collection = TestHelper.instance().createOneCollection(editor, Boolean.TRUE.booleanValue());
        try{
            HibernateUtil.beginTransaction();
            editorDAO.save(editor);
            HibernateUtil.commitTransaction();         
        } catch (PersistencyException e) {             
            e.printStackTrace();         
        }
        List collectionGO = PersistencyManager.findCollectionGuiObjects(editor.getId());
        boolean isOneHead = Boolean.FALSE.booleanValue();
        for(int i=0;i<collectionGO.size();i++){
            CollectionGuiObject coll = (CollectionGuiObject) collectionGO.get(i);
            if (coll.isHead()){
                isOneHead = Boolean.TRUE.booleanValue();
            } else {
                assertTrue("The collectionGO is not the correct one (id)", coll.getId().longValue() == collection.getId().longValue());
                assertTrue("The collectionGO is not the correct one (comment)", coll.getComment().equals(collection.getDescription()));
                assertTrue("The collectionGO is not the correct one (name)", coll.getName().equals(collection.getName()));
                assertTrue("The collectionGO is not the correct one (web)", coll.getWeb().equals(collection.getWeb()));
            }
        }
        assertTrue("There is no head collection in the editor", isOneHead);
    }
    
    public void testSaveAlbumGuiObjectSerieUpdated(){
        Serie s1 = TestHelper.instance().createOneSerie();
        Serie s2 = TestHelper.instance().createOneSerie();
        Editor ed = TestHelper.instance().createOneEditor();
        Collection col = TestHelper.instance().createOneCollection(ed, Boolean.TRUE.booleanValue());
        Album album = TestHelper.instance().createOneAlbum(s1);
        col.addAlbum(album);
        album.setCollection(col);
        
        SerieDAO sdao = new SerieDAO();
        EditorDAO edao = new EditorDAO();
        CollectionDAO cdao = new CollectionDAO();
        
        try{
            HibernateUtil.beginTransaction();
            edao.save(ed);
            cdao.save(col);
            sdao.save(s1);
            sdao.save(s2);
            
            HibernateUtil.commitTransaction();         
        
            AlbumGuiObject ago = PersistencyManager.findAGO(album.getId());
            SerieGuiObject sgo = new SerieGuiObject(s2);
            sgo.setVersion(Versionable.UPDATED);
            ago.setSerie(sgo);
            
            PersistencyManager.saveAlbumGuiObject(ago);
            
            AlbumDAO adao = new AlbumDAO();
            Album foundAlbum = (Album) adao.findByPrimaryKey(album.getId());
            assertTrue("The serie is not the good one", foundAlbum.getSerie().getId().longValue() == s2.getId().longValue());
            Serie foundSerie = (Serie) sdao.findByPrimaryKey(s1.getId());
            assertTrue("The serie 1 is still linked to an album", foundSerie.getAlbums().isEmpty());
            foundSerie = (Serie) sdao.findByPrimaryKey(s2.getId());
            assertTrue("The serie 2 is not linked to the album", !foundSerie.getAlbums().isEmpty());
        } catch (PersistencyException e) {             
            e.printStackTrace();         
        }
    }
    
    public void testSaveAlbumGuiObjectSerieNewTypeNew(){
        Serie s1 = TestHelper.instance().createOneSerie();
        Serie s2 = TestHelper.instance().createOneSerie();
        Type t2 = TestHelper.instance().createOneType();
        Editor ed = TestHelper.instance().createOneEditor();
        Collection col = TestHelper.instance().createOneCollection(ed, Boolean.TRUE.booleanValue());
        Album album = TestHelper.instance().createOneAlbum(s1);
        col.addAlbum(album);
        album.setCollection(col);
        
        SerieDAO sdao = new SerieDAO();
        EditorDAO edao = new EditorDAO();
        CollectionDAO cdao = new CollectionDAO();
        
        try{
            HibernateUtil.beginTransaction();
            edao.save(ed);
            cdao.save(col);
            sdao.save(s1);
            HibernateUtil.commitTransaction();         
        
            AlbumGuiObject ago = PersistencyManager.findAGO(album.getId());
            SerieGuiObject sgo = new SerieGuiObject(s2);
            sgo.setVersion(Versionable.NEW);
            TypeGuiObject tgo = new TypeGuiObject(t2);
            tgo.setVersion(Versionable.NEW);
            sgo.setType(tgo);
            ago.setSerie(sgo);
            
            PersistencyManager.saveAlbumGuiObject(ago);
            
            AlbumDAO adao = new AlbumDAO();
            Album foundAlbum = (Album) adao.findByPrimaryKey(album.getId());
            Serie foundSerie = (Serie) sdao.findByPrimaryKey(foundAlbum.getSerie().getId());
            assertNotNull("There is no type linked to the serie", foundSerie.getType());
        } catch (PersistencyException e) {             
            e.printStackTrace();         
        }
    }
    
    public void testSaveAlbumGuiObjectSerieNewTypeUpdated(){
        Serie s1 = TestHelper.instance().createOneSerie();
        Serie s2 = TestHelper.instance().createOneSerie();
        Type t2 = TestHelper.instance().createOneType();
        Editor ed = TestHelper.instance().createOneEditor();
        Collection col = TestHelper.instance().createOneCollection(ed, Boolean.TRUE.booleanValue());
        Album album = TestHelper.instance().createOneAlbum(s1);
        col.addAlbum(album);
        album.setCollection(col);
        
        SerieDAO sdao = new SerieDAO();
        EditorDAO edao = new EditorDAO();
        CollectionDAO cdao = new CollectionDAO();
        TypeDAO tdao = new TypeDAO();
        
        try{
            HibernateUtil.beginTransaction();
            edao.save(ed);
            cdao.save(col);
            sdao.save(s1);
            tdao.save(t2);
            HibernateUtil.commitTransaction();         
        
        
            AlbumGuiObject ago = PersistencyManager.findAGO(album.getId());
            SerieGuiObject sgo = new SerieGuiObject(s2);
            sgo.setVersion(Versionable.NEW);
            TypeGuiObject tgo = new TypeGuiObject(t2);
            tgo.setVersion(Versionable.UPDATED);
            sgo.setType(tgo);
            ago.setSerie(sgo);
            
            PersistencyManager.saveAlbumGuiObject(ago);
            
            AlbumDAO adao = new AlbumDAO();
            Album foundAlbum = (Album) adao.findByPrimaryKey(album.getId());
            Serie foundSerie = (Serie) sdao.findByPrimaryKey(foundAlbum.getSerie().getId());
            assertNotNull("There is no type linked to the serie", foundSerie.getType());
        } catch (PersistencyException e) {             
            e.printStackTrace();         
        }
    }
    
    public void testSaveAlbumGuiObjectCollectionNew(){
        Serie s = TestHelper.instance().createOneSerie();
        Editor ed = TestHelper.instance().createOneEditor();
        Collection col1 = TestHelper.instance().createOneCollection(ed, Boolean.TRUE.booleanValue());
        Collection col2 = TestHelper.instance().createOneCollection(ed, Boolean.FALSE.booleanValue());
        Album album = TestHelper.instance().createOneAlbum(s);
        col1.addAlbum(album);
        album.setCollection(col1);
        
        SerieDAO sdao = new SerieDAO();
        EditorDAO edao = new EditorDAO();
        CollectionDAO cdao = new CollectionDAO();
        
        try{
        
            HibernateUtil.beginTransaction();
            edao.save(ed);
            cdao.save(col1);
            sdao.save(s);
            HibernateUtil.commitTransaction();         
            
            AlbumGuiObject ago = PersistencyManager.findAGO(album.getId());
            CollectionGuiObject cgo = new CollectionGuiObject(col2);
            cgo.setVersion(Versionable.NEW);
            ago.setCollection(cgo);
            
            PersistencyManager.saveAlbumGuiObject(ago);
            
            AlbumDAO adao = new AlbumDAO();
            Album foundAlbum = (Album) adao.findByPrimaryKey(album.getId());
            assertTrue("The collection is not the good one (name)", foundAlbum.getCollection().getName().equals(cgo.getName()));
            assertTrue("The collection is not the good one (web)", foundAlbum.getCollection().getWeb().equals(cgo.getWeb()));
            assertTrue("The collection is not the good one (description)", foundAlbum.getCollection().getDescription().equals(cgo.getComment()));
        } catch (PersistencyException e) {             
            e.printStackTrace();         
        }
    }
    
    public void testSaveAlbumGuiObjectCollectionNewEditorUpdated(){
        Serie s = TestHelper.instance().createOneSerie();
        Editor ed1 = TestHelper.instance().createOneEditor();
        Editor ed2 = TestHelper.instance().createOneEditor();
        Collection col1 = TestHelper.instance().createOneCollection(ed1, Boolean.TRUE.booleanValue());
        Collection col2 = TestHelper.instance().createOneCollection(ed2, Boolean.FALSE.booleanValue());
        Album album = TestHelper.instance().createOneAlbum(s);
        col1.addAlbum(album);
        album.setCollection(col1);
        
        SerieDAO sdao = new SerieDAO();
        EditorDAO edao = new EditorDAO();
        CollectionDAO cdao = new CollectionDAO();
        
        try{
            HibernateUtil.beginTransaction();
            edao.save(ed1);
            edao.save(ed2);
            cdao.save(col1);
            sdao.save(s);
            HibernateUtil.commitTransaction();         
            
            
            AlbumGuiObject ago = PersistencyManager.findAGO(album.getId());
            CollectionGuiObject cgo = new CollectionGuiObject(col2);
            cgo.setVersion(Versionable.NEW);
            ago.setCollection(cgo);
            EditorGuiObject ego = new EditorGuiObject(ed2);
            ego.setVersion(Versionable.UPDATED);
            ago.setEditor(ego);
            
            PersistencyManager.saveAlbumGuiObject(ago);
            
            AlbumDAO adao = new AlbumDAO();
            Album foundAlbum = (Album) adao.findByPrimaryKey(album.getId());
            assertTrue("The collection is not the good one (name)", foundAlbum.getCollection().getName().equals(cgo.getName()));
            assertTrue("The collection is not the good one (web)", foundAlbum.getCollection().getWeb().equals(cgo.getWeb()));
            assertTrue("The collection is not the good one (description)", foundAlbum.getCollection().getDescription().equals(cgo.getComment()));
            
            assertTrue("The editor is not the good one (name)", foundAlbum.getCollection().getEditor().getName().equals(ego.getName()));
            assertTrue("The editor is not the good one (name)", foundAlbum.getCollection().getEditor().getWeb().equals(ego.getWeb()));
            assertTrue("The editor is not the good one (name)", foundAlbum.getCollection().getEditor().getDescription().equals(ego.getComment()));
        } catch (PersistencyException e) {             
            e.printStackTrace();         
        }
    }
    
    public void testSaveAlbumGuiObjectCollectionNewEditorNew(){
        Serie s = TestHelper.instance().createOneSerie();
        Editor ed1 = TestHelper.instance().createOneEditor();
        Editor ed2 = TestHelper.instance().createOneEditor();
        Collection col1 = TestHelper.instance().createOneCollection(ed1, Boolean.TRUE.booleanValue());
        Collection col2 = TestHelper.instance().createOneCollection(ed2, Boolean.FALSE.booleanValue());
        Album album = TestHelper.instance().createOneAlbum(s);
        col1.addAlbum(album);
        album.setCollection(col1);
        
        SerieDAO sdao = new SerieDAO();
        EditorDAO edao = new EditorDAO();
        CollectionDAO cdao = new CollectionDAO();
        
        try{
            HibernateUtil.beginTransaction();
            edao.save(ed1);
            cdao.save(col1);
            sdao.save(s);
            HibernateUtil.commitTransaction();         
            
            
            AlbumGuiObject ago = PersistencyManager.findAGO(album.getId());
            CollectionGuiObject cgo = new CollectionGuiObject(col2);
            cgo.setVersion(Versionable.NEW);
            ago.setCollection(cgo);
            EditorGuiObject ego = new EditorGuiObject(ed2);
            ego.setVersion(Versionable.NEW);
            ago.setEditor(ego);
            
            PersistencyManager.saveAlbumGuiObject(ago);
            
            AlbumDAO adao = new AlbumDAO();
            Album foundAlbum = (Album) adao.findByPrimaryKey(album.getId());
            assertTrue("The collection is not the good one (name)", foundAlbum.getCollection().getName().equals(cgo.getName()));
            assertTrue("The collection is not the good one (web)", foundAlbum.getCollection().getWeb().equals(cgo.getWeb()));
            assertTrue("The collection is not the good one (description)", foundAlbum.getCollection().getDescription().equals(cgo.getComment()));
            
            assertTrue("The collection number is incorrect", foundAlbum.getCollection().getEditor().getCollections().size() == 2);
            assertTrue("The editor is not the good one (name)", foundAlbum.getCollection().getEditor().getName().equals(ego.getName()));
            assertTrue("The editor is not the good one (name)", foundAlbum.getCollection().getEditor().getWeb().equals(ego.getWeb()));
            assertTrue("The editor is not the good one (name)", foundAlbum.getCollection().getEditor().getDescription().equals(ego.getComment()));
        } catch (PersistencyException e) {             
            e.printStackTrace();         
        }
    }
    
    public void testSaveAlbumGuiObjectCollectionUpdatedEditorUpdated(){
        Serie s = TestHelper.instance().createOneSerie();
        Editor ed1 = TestHelper.instance().createOneEditor();
        Editor ed2 = TestHelper.instance().createOneEditor();
        Collection col1 = TestHelper.instance().createOneCollection(ed1, Boolean.TRUE.booleanValue());
        Collection col2 = TestHelper.instance().createOneCollection(ed2, Boolean.FALSE.booleanValue());
        Album album = TestHelper.instance().createOneAlbum(s);
        col1.addAlbum(album);
        album.setCollection(col1);
        
        SerieDAO sdao = new SerieDAO();
        EditorDAO edao = new EditorDAO();
        CollectionDAO cdao = new CollectionDAO();
        
        try{
            HibernateUtil.beginTransaction();
            edao.save(ed1);
            edao.save(ed2);
            cdao.save(col1);
            cdao.save(col2);
            sdao.save(s);
            HibernateUtil.commitTransaction();         
            
            
            AlbumGuiObject ago = PersistencyManager.findAGO(album.getId());
            CollectionGuiObject cgo = new CollectionGuiObject(col2);
            cgo.setVersion(Versionable.UPDATED);
            ago.setCollection(cgo);
            EditorGuiObject ego = new EditorGuiObject(ed2);
            ego.setVersion(Versionable.UPDATED);
            ago.setEditor(ego);
            
            PersistencyManager.saveAlbumGuiObject(ago);
            
            AlbumDAO adao = new AlbumDAO();
            Album foundAlbum = (Album) adao.findByPrimaryKey(album.getId());
            assertTrue("The collection is not the good one (name)", foundAlbum.getCollection().getName().equals(cgo.getName()));
            assertTrue("The collection is not the good one (web)", foundAlbum.getCollection().getWeb().equals(cgo.getWeb()));
            assertTrue("The collection is not the good one (description)", foundAlbum.getCollection().getDescription().equals(cgo.getComment()));
            
            assertTrue("The editor is not the good one (name)", foundAlbum.getCollection().getEditor().getName().equals(ego.getName()));
            assertTrue("The editor is not the good one (name)", foundAlbum.getCollection().getEditor().getWeb().equals(ego.getWeb()));
            assertTrue("The editor is not the good one (name)", foundAlbum.getCollection().getEditor().getDescription().equals(ego.getComment()));
        } catch (PersistencyException e) {             
            e.printStackTrace();         
        }
    }
    
    public void testSaveAlbumGuiObjectCollectionUpdated(){
        Serie s = TestHelper.instance().createOneSerie();
        Editor ed1 = TestHelper.instance().createOneEditor();
        Editor ed2 = TestHelper.instance().createOneEditor();
        Collection col1 = TestHelper.instance().createOneCollection(ed1, Boolean.TRUE.booleanValue());
        Collection col2 = TestHelper.instance().createOneCollection(ed2, Boolean.FALSE.booleanValue());
        Album album = TestHelper.instance().createOneAlbum(s);
        col1.addAlbum(album);
        album.setCollection(col1);
        
        SerieDAO sdao = new SerieDAO();
        EditorDAO edao = new EditorDAO();
        CollectionDAO cdao = new CollectionDAO();
        
        try{
            HibernateUtil.beginTransaction();
            edao.save(ed1);
            edao.save(ed2);
            cdao.save(col1);
            cdao.save(col2);
            sdao.save(s);
            HibernateUtil.commitTransaction();         
            
            
            AlbumGuiObject ago = PersistencyManager.findAGO(album.getId());
            CollectionGuiObject cgo = new CollectionGuiObject(col2);
            cgo.setVersion(Versionable.UPDATED);
            ago.setCollection(cgo);
            
            PersistencyManager.saveAlbumGuiObject(ago);
            
            AlbumDAO adao = new AlbumDAO();
            Album foundAlbum = (Album) adao.findByPrimaryKey(album.getId());
            assertTrue("The collection is not the good one (name)", foundAlbum.getCollection().getName().equals(cgo.getName()));
            assertTrue("The collection is not the good one (web)", foundAlbum.getCollection().getWeb().equals(cgo.getWeb()));
            assertTrue("The collection is not the good one (description)", foundAlbum.getCollection().getDescription().equals(cgo.getComment()));
        } catch (PersistencyException e) {             
            e.printStackTrace();         
        }
    }
    
    public void testSaveAlbumGuiObjectWorkDeleted(){
        Serie s = TestHelper.instance().createOneSerie();
        Editor ed1 = TestHelper.instance().createOneEditor();
        Collection col1 = TestHelper.instance().createOneCollection(ed1, Boolean.TRUE.booleanValue());
        Album album = TestHelper.instance().createOneAlbum(s);
        col1.addAlbum(album);
        album.setCollection(col1);
        Author a1 = TestHelper.instance().createOneAuthor();
        Work w1 = TestHelper.instance().createOneIllustration(a1, album);
        
        
        
        SerieDAO sdao = new SerieDAO();
        EditorDAO edao = new EditorDAO();
        CollectionDAO cdao = new CollectionDAO();
        WorkDAO wdao = new WorkDAO();
        AuthorDAO audao = new AuthorDAO();
        
        try{
            HibernateUtil.beginTransaction();
            edao.save(ed1);
            cdao.save(col1);
            sdao.save(s);
            audao.save(a1);
            wdao.save(w1);
            HibernateUtil.commitTransaction();         
            
            
            AlbumDAO adao = new AlbumDAO();
            Album foundAlbum = (Album) adao.findByPrimaryKey(album.getId());
            assertTrue("There is no work linked to the album", !foundAlbum.getWorks().isEmpty());
            
            AlbumGuiObject ago = PersistencyManager.findAGO(album.getId());
            WorkObject wo = (WorkObject) ago.getWorks().get(0);
            wo.setVersion(Versionable.REMOVED);
            
            PersistencyManager.saveAlbumGuiObject(ago);
            
            
            foundAlbum = (Album) adao.findByPrimaryKey(album.getId());
            assertTrue("There is still one work linked to the album", foundAlbum.getWorks().isEmpty());
        } catch (PersistencyException e) {             
            e.printStackTrace();         
        }
    }
    
    public void testSaveAlbumGuiObjectWorkUpdated(){
        Serie s = TestHelper.instance().createOneSerie();
        Editor ed1 = TestHelper.instance().createOneEditor();
        Collection col1 = TestHelper.instance().createOneCollection(ed1, Boolean.TRUE.booleanValue());
        Album album = TestHelper.instance().createOneAlbum(s);
        col1.addAlbum(album);
        album.setCollection(col1);
        Author a1 = TestHelper.instance().createOneAuthor();
        Work w1 = TestHelper.instance().createOneIllustration(a1, album);
        
        
        SerieDAO sdao = new SerieDAO();
        EditorDAO edao = new EditorDAO();
        CollectionDAO cdao = new CollectionDAO();
        WorkDAO wdao = new WorkDAO();
        AuthorDAO audao = new AuthorDAO();
      
        try{
            
            HibernateUtil.beginTransaction();
            edao.save(ed1);
            cdao.save(col1);
            sdao.save(s);
            audao.save(a1);
            wdao.save(w1);
            HibernateUtil.commitTransaction();         
            
            
            AlbumDAO adao = new AlbumDAO();
            Album foundAlbum = (Album) adao.findByPrimaryKey(album.getId());
            assertTrue("There is no work linked to the album", !foundAlbum.getWorks().isEmpty());
            
            AlbumGuiObject ago = PersistencyManager.findAGO(album.getId());
            WorkObject wo = (WorkObject) ago.getWorks().get(0);
            wo.setType(WorkObject.SCENARIO);
            wo.setVersion(Versionable.UPDATED);
            
            PersistencyManager.saveAlbumGuiObject(ago);
            
            
            foundAlbum = (Album) adao.findByPrimaryKey(album.getId());
            assertTrue("There is no work linked to the album", !foundAlbum.getWorks().isEmpty());
            assertTrue("This is not the good work ;-)", foundAlbum.getWorks().toArray()[0] instanceof Scenario);
        } catch (PersistencyException e) {             
            e.printStackTrace();         
        }
    }
    
    public void testSaveAlbumGuiObjectWorkNewAuthorUpdated(){
        Serie s = TestHelper.instance().createOneSerie();
        Editor ed1 = TestHelper.instance().createOneEditor();
        Collection col1 = TestHelper.instance().createOneCollection(ed1, Boolean.TRUE.booleanValue());
        Album album = TestHelper.instance().createOneAlbum(s);
        col1.addAlbum(album);
        album.setCollection(col1);
        Author a1 = TestHelper.instance().createOneAuthor();
        Author a2 = TestHelper.instance().createOneAuthor();
        Work w1 = TestHelper.instance().createOneIllustration(a1, album);
        
        
        SerieDAO sdao = new SerieDAO();
        EditorDAO edao = new EditorDAO();
        CollectionDAO cdao = new CollectionDAO();
        WorkDAO wdao = new WorkDAO();
        AuthorDAO audao = new AuthorDAO();
        
        try{
            HibernateUtil.beginTransaction();
            edao.save(ed1);
            cdao.save(col1);
            sdao.save(s);
            audao.save(a1);
            audao.save(a2);
            wdao.save(w1);
            HibernateUtil.commitTransaction();         
            
            
            AlbumDAO adao = new AlbumDAO();
            Album foundAlbum = (Album) adao.findByPrimaryKey(album.getId());
            assertTrue("There is no work linked to the album", !foundAlbum.getWorks().isEmpty());
            
            AlbumGuiObject ago = PersistencyManager.findAGO(album.getId());
            
            Work w2 = TestHelper.instance().createOneColor((Author) audao.findByPrimaryKey(a2.getId()), album);
            WorkObject wo = new WorkObject(w2);
            wo.setVersion(Versionable.NEW);
            ago.getWorks().add(wo);
            PersistencyManager.saveAlbumGuiObject(ago);
            
            
            foundAlbum = (Album) adao.findByPrimaryKey(album.getId());
            assertTrue("There is no work linked to the album", !foundAlbum.getWorks().isEmpty());
            Object[] foundWorks = foundAlbum.getWorks().toArray();
            boolean ok = Boolean.FALSE.booleanValue();
            for (int i = 0; i < foundWorks.length; i++) {
                Work w = (Work) foundWorks[i];      
                if (w instanceof Color && w.getAuthor().getId().longValue() == a2.getId().longValue()){
                    ok = Boolean.TRUE.booleanValue();
                }
            }
            assertTrue("This is not the good work", ok);
        } catch (PersistencyException e) {             
            e.printStackTrace();         
        }
    }
    
    public void testSaveAlbumGuiObjectWorkNewAuthorNew(){
        Serie s = TestHelper.instance().createOneSerie();
        Editor ed1 = TestHelper.instance().createOneEditor();
        Collection col1 = TestHelper.instance().createOneCollection(ed1, Boolean.TRUE.booleanValue());
        Album album = TestHelper.instance().createOneAlbum(s);
        col1.addAlbum(album);
        album.setCollection(col1);
        Author a1 = TestHelper.instance().createOneAuthor();
        Author a2 = TestHelper.instance().createOneAuthor();
        Work w1 = TestHelper.instance().createOneIllustration(a1, album);
        
        
        SerieDAO sdao = new SerieDAO();
        EditorDAO edao = new EditorDAO();
        CollectionDAO cdao = new CollectionDAO();
        WorkDAO wdao = new WorkDAO();
        AuthorDAO audao = new AuthorDAO();
        
        try{
            HibernateUtil.beginTransaction();
            edao.save(ed1);
            cdao.save(col1);
            sdao.save(s);
            audao.save(a1);
            wdao.save(w1);
            HibernateUtil.commitTransaction();         
            
            AlbumDAO adao = new AlbumDAO();
            Album foundAlbum = (Album) adao.findByPrimaryKey(album.getId());
            assertTrue("There is no work linked to the album", !foundAlbum.getWorks().isEmpty());
            
            AlbumGuiObject ago = PersistencyManager.findAGO(album.getId());
            
            Work w2 = TestHelper.instance().createOneColor(a2, album);
            WorkObject wo = new WorkObject(w2);
            wo.setVersion(Versionable.NEW);
            wo.getAuthorObject().setVersion(Versionable.NEW);
            ago.getWorks().add(wo);
            PersistencyManager.saveAlbumGuiObject(ago);
            
            
            foundAlbum = (Album) adao.findByPrimaryKey(album.getId());
            assertTrue("There is no work linked to the album", !foundAlbum.getWorks().isEmpty());
            Object[] foundWorks = foundAlbum.getWorks().toArray();
            boolean ok = Boolean.FALSE.booleanValue();
            for (int i = 0; i < foundWorks.length; i++) {
                Work w = (Work) foundWorks[i];
                
                if (w instanceof Color && w.getAuthor().getId() != null){
                    ok = Boolean.TRUE.booleanValue();
                }
            }
            assertTrue("This is not the good work", ok);
        } catch (PersistencyException e) {             
            e.printStackTrace();         
        }
    }
    
    public void testRemoveAlbumGuiObject(){
        Serie s = TestHelper.instance().createOneSerie();
        Editor ed1 = TestHelper.instance().createOneEditor();
        Collection col1 = TestHelper.instance().createOneCollection(ed1, Boolean.TRUE.booleanValue());
        Album album = TestHelper.instance().createOneAlbum(s);
        col1.addAlbum(album);
        album.setCollection(col1);
        Author a1 = TestHelper.instance().createOneAuthor();
        Work w1 = TestHelper.instance().createOneIllustration(a1, album);
        
        SerieDAO sdao = new SerieDAO();
        EditorDAO edao = new EditorDAO();
        CollectionDAO cdao = new CollectionDAO();
        WorkDAO wdao = new WorkDAO();
        AuthorDAO audao = new AuthorDAO();
        
        try{
            HibernateUtil.beginTransaction();
            edao.save(ed1);
            cdao.save(col1);
            sdao.save(s);
            audao.save(a1);
            wdao.save(w1);
            HibernateUtil.commitTransaction();         
            
            
            AlbumGuiObject ago = PersistencyManager.findAGO(album.getId());
            
            PersistencyManager.removeAlbumGuiObject(ago);
            
            Serie foundSerie = (Serie) sdao.findByPrimaryKey(s.getId());
            assertTrue("Serie is still linked with removed album", foundSerie.getAlbums().size() + 1 == s.getAlbums().size());
            Collection foundCollection = (Collection) cdao.findByPrimaryKey(col1.getId());
            assertTrue("Collection is still linked with removed album", foundCollection.getAlbums().size() + 1 == col1.getAlbums().size());
            assertNull("Work is still in the db", wdao.findByPrimaryKey(w1.getId()));
            Author foundAuthor = (Author) audao.findByPrimaryKey(a1.getId());
            assertTrue("Work is still linked with removed work", foundAuthor.getWorks().size() + 1 == a1.getWorks().size());
        
        } catch (PersistencyException e) {             
            e.printStackTrace();         
        }
    }
    
    protected void setUp() throws Exception {
        Locale.getDefault().setDefault(LocaleManager.instance.loadLocale());
    }
    
    
    
    
    
    
    
    
    
    
    
    
}
