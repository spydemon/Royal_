package net.sf.birdy.datamodel;

import net.sf.birdy.exception.PersistencyException;
import junit.framework.TestCase;

public class InsertionTest extends TestCase {

    public void testInsertEditor(){
        Editor editor = TestHelper.instance().createOneEditor();
        EditorDAO dao = new EditorDAO();
        TestHelper.instance().createOneCollection(editor, Boolean.TRUE.booleanValue());
        try {
            HibernateUtil.beginTransaction();
            dao.save(editor);
            
            HibernateUtil.commitTransaction();
        } catch (PersistencyException e) {
            e.printStackTrace();
        }
        this.inDataBaseTest(editor, dao);
        this.findInDataBaseTest(editor, dao);
    }
    
    public void testInsertCollection(){
        Editor editor = TestHelper.instance().createOneEditor();
        Collection collection = TestHelper.instance().createOneCollection(editor, Boolean.FALSE.booleanValue());
        EditorDAO edDAO = new EditorDAO();
        CollectionDAO colDAO = new CollectionDAO();
        try {
            HibernateUtil.beginTransaction();
            edDAO.save(editor);
            colDAO.save(collection);
        
            HibernateUtil.commitTransaction();
        } catch (PersistencyException e) {
            e.printStackTrace();
        }
        this.inDataBaseTest(editor, edDAO);
        this.inDataBaseTest(collection, colDAO);
        this.findInDataBaseTest(collection, colDAO);
    }
    
    public void testInsertionAuthor(){
        Author author = TestHelper.instance().createOneAuthor();
        AuthorDAO dao = new AuthorDAO();
        try {
            HibernateUtil.beginTransaction();
            dao.save(author);
        
            HibernateUtil.commitTransaction();
        } catch (PersistencyException e) {
            e.printStackTrace();
        }
        this.inDataBaseTest(author, dao);
        this.findInDataBaseTest(author, dao);
    }
    
    public void testInsertionColor(){
        Author author = TestHelper.instance().createOneAuthor();
        AuthorDAO aDAO = new AuthorDAO();
        Serie serie = TestHelper.instance().createOneSerie();
        SerieDAO sDAO = new SerieDAO();
        Album album = TestHelper.instance().createOneAlbum(serie);
        Work work = TestHelper.instance().createOneColor(author, album);
        WorkDAO wDAO = new ColorDAO();
        try{
            HibernateUtil.beginTransaction();
            aDAO.save(author);
            sDAO.save(serie);
            wDAO.save(work);
            HibernateUtil.commitTransaction();
        } catch (PersistencyException e) {
            e.printStackTrace();
        }
        this.inDataBaseTest(author, aDAO);
        this.inDataBaseTest(work, wDAO);
        this.findInDataBaseTest(work, wDAO);
    }
    
    public void testInsertionIllustration(){
        Author author = TestHelper.instance().createOneAuthor();
        AuthorDAO aDAO = new AuthorDAO();
        Serie serie = TestHelper.instance().createOneSerie();
        SerieDAO sDAO = new SerieDAO();
        Album album = TestHelper.instance().createOneAlbum(serie);
        Work work = TestHelper.instance().createOneIllustration(author, album);
        WorkDAO wDAO = new IllustrationDAO();
        try {
            HibernateUtil.beginTransaction();
            sDAO.save(serie);
            aDAO.save(author);
            wDAO.save(work);
        
            HibernateUtil.commitTransaction();
        } catch (PersistencyException e) {
            e.printStackTrace();
        }
        this.inDataBaseTest(author, aDAO);
        this.inDataBaseTest(work, wDAO);
        this.findInDataBaseTest(work, wDAO);
    }
    
    public void testInsertionScenario(){
        Author author = TestHelper.instance().createOneAuthor();
        AuthorDAO aDAO = new AuthorDAO();
        Serie serie = TestHelper.instance().createOneSerie();
        SerieDAO sDAO = new SerieDAO();
        Album album = TestHelper.instance().createOneAlbum(serie);
        Work work = TestHelper.instance().createOneScenario(author, album);
        WorkDAO wDAO = new ScenarioDAO();
        try {
            HibernateUtil.beginTransaction();
            sDAO.save(serie);
            aDAO.save(author);
            wDAO.save(work);
        
            HibernateUtil.commitTransaction();
        } catch (PersistencyException e) {
            e.printStackTrace();
        }
        this.inDataBaseTest(author, aDAO);
        this.inDataBaseTest(work, wDAO);
        this.findInDataBaseTest(work, wDAO);
    }
    
    public void testInsertionAlbum(){
        Serie serie = TestHelper.instance().createOneSerie();
        Album album = TestHelper.instance().createOneAlbum(serie);
        SerieDAO sDAO = new SerieDAO();
        AlbumDAO alDAO = new AlbumDAO();
        try {
            HibernateUtil.beginTransaction();
            sDAO.save(serie);
            alDAO.save(album);
        
            HibernateUtil.commitTransaction();
        } catch (PersistencyException e) {
            e.printStackTrace();
        }
        this.inDataBaseTest(album, alDAO);
        this.findInDataBaseTest(album, alDAO);
    }
    
    public void testInsertionAlbumWithCollection(){
    		Editor editor = TestHelper.instance().createOneEditor();
        Collection collection = TestHelper.instance().createOneCollection(editor, Boolean.FALSE.booleanValue());
        EditorDAO edDAO = new EditorDAO();
        Serie serie = TestHelper.instance().createOneSerie();
        Album album = TestHelper.instance().createOneAlbum(serie,collection);
        SerieDAO sDAO = new SerieDAO();
        try {
            HibernateUtil.beginTransaction();
            edDAO.save(editor);
            sDAO.save(serie);
            HibernateUtil.commitTransaction();
        
            AlbumDAO adao = new AlbumDAO();
            Album foundAlbum = (Album) adao.findByPrimaryKey(album.getId());
            assertTrue("The album has no Collection", foundAlbum.getCollection().getId().longValue() == collection.getId().longValue());
            CollectionDAO cdao = new CollectionDAO();
            Collection foundCollection = (Collection) cdao.findByPrimaryKey(collection.getId());
        } catch (PersistencyException e) {
            e.printStackTrace();
        }
    }
    
    public void testInsertionDedication(){
        Serie serie = TestHelper.instance().createOneSerie();
        Album album = TestHelper.instance().createOneAlbum(serie);
        SerieDAO sDAO = new SerieDAO();
        AlbumDAO alDAO = new AlbumDAO();
        Dedication dedication = TestHelper.instance().createOneDedication(album);
        try {
            HibernateUtil.beginTransaction();
            sDAO.save(serie);
            alDAO.save(album);

            HibernateUtil.commitTransaction();
        
            this.inDataBaseTest(album, alDAO);
            Album foundAlbum = (Album) alDAO.findByPrimaryKey(album.getId());
            assertNotNull("The Album has no dedication", foundAlbum.getDedication());
            TestHelper.instance().compareTwoDedication(album.getDedication(), foundAlbum.getDedication());
        } catch (PersistencyException e) {
            e.printStackTrace();
        }
    }
    
    public void testInsertionSerie(){
        Serie serie = TestHelper.instance().createOneSerie();
        SerieDAO dao = new SerieDAO();
        try{
            HibernateUtil.beginTransaction();
            dao.save(serie);
      
            HibernateUtil.commitTransaction();
        } catch (PersistencyException e) {
            e.printStackTrace();
        }
        this.inDataBaseTest(serie, dao);
        this.findInDataBaseTest(serie, dao);
    }
    
    public void testInsertionCommentedImage(){
        Serie serie = TestHelper.instance().createOneSerie();
        SerieDAO sdao = new SerieDAO();
        CommentedImage commentedImage = TestHelper.instance().createOneCommentedImage(serie, 1);
        CommentedImageDAO cidao = new CommentedImageDAO();
        try{
            HibernateUtil.beginTransaction();
            sdao.save(serie);
            cidao.save(commentedImage);
        
            HibernateUtil.commitTransaction();
            this.inDataBaseTest(commentedImage, cidao);
            this.findInDataBaseTest(commentedImage, cidao);
            Serie foundSerie = (Serie) sdao.findByPrimaryKey(serie.getId());
            assertTrue("There is no commented image in the serie", !foundSerie.getGallery().isEmpty());
        } catch (PersistencyException e) {
            e.printStackTrace();
        }
    }
    
    public void testInsertionType(){
        Type type = TestHelper.instance().createOneType();
        TypeDAO dao = new TypeDAO();
        try{
            HibernateUtil.beginTransaction();
            dao.save(type);
        
            HibernateUtil.commitTransaction();
        
            this.inDataBaseTest(type, dao);
            this.findInDataBaseTest(type, dao);
            
            Serie serie = TestHelper.instance().createOneSerie();
            Type foundType = (Type) dao.findByPrimaryKey(type.getId());
            serie.setType(foundType);
            foundType.addSerie(serie);
            SerieDAO sdao = new SerieDAO();
            HibernateUtil.beginTransaction();
            dao.save(foundType);
            sdao.save(serie);
        
            HibernateUtil.commitTransaction();
            
            Serie foundSerie = (Serie) sdao.findByPrimaryKey(serie.getId());
            assertNotNull("The serie doesn't contain type", foundSerie.getType());
        } catch (PersistencyException e) {
            e.printStackTrace();
        }
    }
    
    private void inDataBaseTest(Model model, PojoDAO dao){
        assertNotNull("The id " + model.getClass().toString() + " is null", model.getId());
    }
    
    private void findInDataBaseTest(Model model, PojoDAO dao){
        Model foundModel = null;
        try{
            foundModel = (Model) dao.findByPrimaryKey(model.getId());
        } catch (PersistencyException e) {
            e.printStackTrace();
        }
        assertNotNull(model.getClass().toString() + " is not in th DB", foundModel);
        assertTrue("The version of " + foundModel.getClass().toString() + " is not the correct one", model.getId().longValue() == foundModel.getId().longValue());
        if ((model != null && foundModel == null) || (model == null && foundModel != null)){
            fail("One " + model.getClass() + " is null but the second isn't");
        }
        if (model instanceof Editor){
            TestHelper.instance().compareTwoEditors((Editor) model, (Editor) foundModel);
        } else if (model instanceof Collection){
            TestHelper.instance().compareTwoCollections((Collection) model, (Collection) foundModel); 
        } else if (model instanceof Author){
            TestHelper.instance().compareTwoAuthors((Author) model, (Author) foundModel);
        } else if (model instanceof Work){
            TestHelper.instance().compareTwoWorks((Work) model, (Work) foundModel);
        } else if (model instanceof Album){
            TestHelper.instance().compareTwoAlbum((Album) model, (Album) foundModel);
        } else if (model instanceof Serie){
            TestHelper.instance().compareTwoSeries((Serie) model, (Serie) foundModel);
        } else if (model instanceof CommentedImage){
            TestHelper.instance().compareTwoCommentedImages((CommentedImage) model, (CommentedImage) foundModel);
        } else if (model instanceof Type){
            TestHelper.instance().compareTwoTypes((Type) model, (Type) foundModel);
        }
    }
}
