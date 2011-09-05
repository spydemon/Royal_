package net.sf.birdy.datamodel;

import java.util.Date;
import org.hibernate.Session;

import junit.framework.TestCase;

public class TestHelper extends TestCase{

    private int increment = 0;
    
    private static TestHelper instance = new TestHelper();
    
    private TestHelper(){}
    

    public Editor createOneEditor(){
        Editor res = new Editor();
        res.setVersion(new Long(increment++));
        increment("Editor");
        res.setName("eName - " + increment++);
        res.setWeb("eWeb - " + increment++);
        res.setDescription("eDes - " + increment++);
        return res;
    }
    
    public Collection createOneCollection(Editor editor, boolean head){
        Collection res = new Collection();
        res.setVersion(new Long(increment++));
        if (head){
            res.setName("");
            res.setWeb("");
            res.setDescription("");
        } else {
            res.setName("cName - " + increment++);
            res.setWeb("cWeb - " + increment++);
            res.setDescription("cDes - " + increment++);
        }
        
        res.setHead(head);
        res.setEditor(editor);
        editor.addCollection(res);
        return res;
        
    }
    
    public Author createOneAuthor(){
        Author res = new Author();
        res.setVersion(new Long(increment++));
        res.setName("aName - " + increment++);
        res.setWeb("aWeb - " + increment++);
        res.setStory("aStory - " + increment++);
        res.setFirstName("aFirstName - " + increment++);
        res.setNickName("aNickName - " + increment++);
        res.setBirth(new Date());
        res.setDeath(new Date());
        res.setPhoto("photo - " + increment++);
        return res;
    }
    
    public Work createOneColor(Author author, Album album){
        Work res = new Color();
        res.setVersion(new Long(increment++));
        author.addWork(res);
        res.setAuthor(author);
        album.addWork(res);
        res.setAlbum(album);
        return res;
    }
    
    public Work createOneScenario(Author author, Album album){
        Work res = new Scenario();
        res.setVersion(new Long(increment++));
        author.addWork(res);
        res.setAuthor(author);
        album.addWork(res);
        res.setAlbum(album);
        return res;
    }
    
    public Work createOneIllustration(Author author, Album album){
        Work res = new Illustration();
        res.setVersion(new Long(increment++));
        author.addWork(res);
        res.setAuthor(author);
        album.addWork(res);
        res.setAlbum(album);
        return res;
    }
    
    public Album createOneAlbum(Serie serie, Collection collection){
    		Album res = createOneAlbum(serie);
	    	if (collection != null){
	    		collection.addAlbum(res);
	    		res.setCollection(collection);
	    }
	    	return res;
    }
    
    public Album createOneAlbum(Serie serie){
        Album res = new Album();
        serie.addAlbum(res);
        res.setSerie(serie);
        res.setVersion(new Long(increment++));
        res.setTitle("title - " + increment++);
        res.setRegistration(new Date());
        res.setCover("cover - " + increment++);
        res.setNote(increment++ % 10);
        res.setPrice(new Float(increment++).floatValue());
        res.setState("state - " + increment++);
        res.setDimension(increment++ + " x " + increment++);
        res.setIntegral(Boolean.FALSE.booleanValue());
        res.setOriginal(Boolean.TRUE.booleanValue());
        res.setPageCount(increment++);
        res.setComment("comment - " + increment++);
        res.setIsbn("isbn - " + increment++);
        res.setCopies(increment++);
        res.setSpecial(Boolean.FALSE.booleanValue());
        res.setPurchaseDate(new Date());
        Tome tome = new Tome();
        tome.setNumber(increment++);
        tome.setRevision("a");
        res.setTome(tome);
        return res;
    }
    
    public Dedication createOneDedication(Album album){
        Dedication res = new Dedication();
        res.setDate(new Date());
        res.setImageURL("imageURL  - " + increment++);
        res.setLocation("location - " + increment++);
        res.setDescription("description - " + increment++);
        album.setDedication(res);
        res.setAlbum(album);
        return res;
    }
    
    public Dedication createOneDedication(Album album, String imageURL){
        Dedication res = this.createOneDedication(album);
        res.setImageURL(imageURL);
        return res;
    }
    
    public Serie createOneSerie(){
        Serie res = new Serie();
        res.setVersion(new Long(increment++));
        TestHelper.instance().increment("Serie");
        res.setName("sName - " + increment++);
        res.setWeb("sWeb - " + increment++);
        res.setDescription("sDesc - " + increment++);
        res.setClosed(Boolean.FALSE.booleanValue());
        res.setOneShot(Boolean.FALSE.booleanValue());
        return res;
    }
    
    public CommentedImage createOneCommentedImage(Serie serie, int position){
        CommentedImage res = new CommentedImage();
        res.setVersion(new Long(increment++));
        //res.setImageURL("ciURL - " + increment++);
        res.setImageURL("resources/images/gallery/ileautresorpl.JPG");
        res.setComment("ciComment - " + increment++);
        res.setHalign(increment++ % 2);
        res.setValign("middle");
        res.setPosition(position);
        serie.addCommentedImage(res);
        res.setSerie(serie);
        return res;
    }
    
    public Type createOneType(){
        Type res = new Type();
        res.setName("tname - " + increment++);
        return res;
    }
    
    public Borrower createOneBorrower(){
        Borrower res = new Borrower();
        res.setVersion(new Long(increment++));
        res.setCreation(new Date());
        res.setFirstName("firstname - " + increment++);
        res.setPhone("phone - " + increment++);
        res.setName("name - " + increment++);
        return res;
    }
    
    public Loan createOneLoan(Borrower borrower, Album album, boolean closed){
        Loan res = new Loan();
        res.setVersion(new Long(increment++));
        res.setBegin(new Date());
        res.setMaxDays(increment++);
        res.setPenaltyByDay(increment++);
        res.setPriceByDay(increment++);
        if (closed){
            res.setEnd(new Date());
        }
        res.setAlbum(album);
        res.setBorrower(borrower);
        borrower.addLoan(res);
        return res;
    }
    
    public void compareTwoAlbum(Album al1, Album al2){
        assertTrue("The Album is not the good one (title)", al1.getTitle().equals(al2.getTitle()));
        assertTrue("The Album is not the good one (cover)", al1.getCover().equals(al2.getCover()));
        assertTrue("The Album is not the good one (note)", al1.getNote() == al2.getNote());
        assertTrue("The Album is not the good one (price)", al1.getPrice() == al2.getPrice());
        assertTrue("The Album is not the good one (state)", al1.getState().equals(al2.getState()));
        assertTrue("The Album is not the good one (dimension)", al1.getDimension().equals(al2.getDimension()));
        assertTrue("The Album is not the good one (integral)", al1.isIntegral() == al2.isIntegral());
        assertTrue("The Album is not the good one (original)", al1.isOriginal() == al2.isOriginal());
        assertTrue("The Album is not the good one (pageCount)", al1.getPageCount() == al2.getPageCount());
        assertTrue("The Album is not the good one (comment)", al1.getComment().equals(al2.getComment()));
        assertTrue("The Album is not the good one (isbn)", al1.getIsbn().equals(al2.getIsbn()));
        assertTrue("The Album is not the good one (copies)", al1.getCopies() == al2.getCopies());
        assertTrue("The Album is not the good one (special)", al1.isSpecial()== al2.isSpecial());
        assertTrue("The Album is not the good one (tome number)", al1.getTome().getNumber() == al2.getTome().getNumber());
        assertTrue("The Album is not the good one (tome revision)", al1.getTome().getRevision().equals(al2.getTome().getRevision()));
    }

    public void compareTwoAuthors(Author aut1, Author aut2){
        assertTrue("The Author is not the good one (name)", aut1.getName().equals(aut2.getName()));
        assertTrue("The Author is not the good one (firstName)", aut1.getFirstName().equals(aut2.getFirstName()));
        assertTrue("The Author is not the good one (nickName)", aut1.getNickName().equals(aut2.getNickName()));
        assertTrue("The Author is not the good one (story)", aut1.getStory().equals(aut2.getStory()));
        assertTrue("The Author is not the good one (web)", aut1.getWeb().equals(aut2.getWeb()));
        assertTrue("The Author is not the good one (photo)", aut1.getPhoto().equals(aut2.getPhoto()));
    }

    public void compareTwoCollections(Collection col1, Collection col2){
        assertTrue("The Collection is not the good one (name)", col1.getName().equals(col2.getName()));
        assertTrue("The Collection is not the good one (web)", col1.getWeb().equals(col2.getWeb()));
        assertTrue("The Collection is not the good one (description)", col1.getDescription().equals(col2.getDescription()));
        assertTrue("The Collection is not the good one (head)", col1.isHead() == col2.isHead());
    }

    public void compareTwoDedication(Dedication ded1, Dedication ded2){
        assertTrue("The Dedication is not the good one (location)", ded1.getLocation().equals(ded2.getLocation()));
        assertTrue("The Dedication is not the good one (imageURL)", ded1.getImageURL().equals(ded2.getImageURL()));
        assertTrue("The Dedication is not the good one (description)", ded1.getDescription().equals(ded2.getDescription()));
    }

    public void compareTwoEditors(Editor ed1, Editor ed2){
        assertTrue("The Editor is not the good one (name)", ed1.getName().equals(ed2.getName()));
        assertTrue("The Editor is not the good one (web)", ed1.getWeb().equals(ed2.getWeb()));
        assertTrue("The Editor is not the good one (description)", ed1.getDescription().equals(ed2.getDescription()));
    }

    public void compareTwoWorks(Work w1, Work w2){
        assertTrue("The Work is not the good one (instance of)", w1.getClass().equals(w2.getClass()));
    }
    
    public void compareTwoSeries(Serie s1, Serie s2){
        assertTrue("The Serie is not the good one (name)", s1.getName().equals(s2.getName()));
        assertTrue("The Serie is not the good one (close)", s1.isClosed() == s2.isClosed());
        assertTrue("The Serie is not the good one (one shot)", s1.isOneShot() == s2.isOneShot());
        assertTrue("The Serie is not the good one (description)", s1.getDescription().equals(s2.getDescription()));
        assertTrue("The Serie is not the good one (web)", s1.getWeb().equals(s2.getWeb()));
    }
    
    public void compareTwoCommentedImages(CommentedImage ci1, CommentedImage ci2){
        assertTrue("The CommentedImage is not the good one (imageURL)", ci1.getImageURL().equals(ci2.getImageURL()));
        assertTrue("The CommentedImage is not the good one (comment)", ci1.getComment().equals(ci2.getComment()));
        assertTrue("The CommentedImage is not the good one (valign)", ci1.getValign().equals(ci2.getValign()));
        assertTrue("The CommentedImage is not the good one (valign)", ci1.getHalign() == ci2.getHalign());
        assertTrue("The CommentedImage is not the good one (position)", ci1.getPosition() == ci2.getPosition());
    }
    
    public void compareTwoTypes(Type t1, Type t2){
        assertTrue("The Type is not the good one (name)", t1.getName().equals(t2.getName()));
    }
    
    public static TestHelper instance() {
        return instance;
    }
    
    private void increment(String clazz){
		try
		{
			HibernateUtil.initSessionFactory();
		} catch (Exception e) {
			e.printStackTrace();
		}		
		Session s = HibernateUtil.currentSession();
        Integer i = (Integer) s.createQuery("select count(*) from " + clazz).uniqueResult();
        if (i != null){
            increment += i.intValue();
        } else {
            increment ++;
        }
        
    }
}
