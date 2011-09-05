package net.sf.birdy.gui.guimodel;

import java.util.Date;
import java.util.Locale;

import javax.swing.JFrame;
import javax.swing.UIManager;

import net.sf.birdy.datamodel.Album;
import net.sf.birdy.datamodel.AlbumDAO;
import net.sf.birdy.datamodel.Author;
import net.sf.birdy.datamodel.AuthorDAO;
import net.sf.birdy.datamodel.Borrower;
import net.sf.birdy.datamodel.BorrowerDAO;
import net.sf.birdy.datamodel.Collection;
import net.sf.birdy.datamodel.Editor;
import net.sf.birdy.datamodel.EditorDAO;
import net.sf.birdy.datamodel.HibernateUtil;
import net.sf.birdy.datamodel.IllustrationDAO;
import net.sf.birdy.datamodel.Loan;
import net.sf.birdy.datamodel.Serie;
import net.sf.birdy.datamodel.SerieDAO;
import net.sf.birdy.datamodel.TestHelper;
import net.sf.birdy.datamodel.Work;
import net.sf.birdy.datamodel.WorkDAO;
import net.sf.birdy.exception.PersistencyException;
import net.sf.birdy.gui.docks.DockablePanel;
import net.sf.birdy.gui.docks.DockablePanelFactory;
import net.sf.birdy.gui.manager.LocaleManager;
import net.sf.birdy.gui.manager.PropertyManager;
import net.sf.birdy.persistency.PersistencyManager;

import com.jgoodies.plaf.plastic.Plastic3DLookAndFeel;
import com.jgoodies.plaf.plastic.theme.ExperienceBlue;
import com.vlsolutions.swing.docking.DockingDesktop;
import com.vlsolutions.swing.docking.DockingPreferences;

public class TestModelisable {
    
    public static final String PERSONAL_INFO = "personal_info";
    public static final String GALLERY = "gallery";
    public static final String ALBUM = "album";
    public static final String ALBUM_TABLE = "albumtable";
    public static final String SERIE_THUMBNAIL = "serie_thumbnail";
    public static final String SERIE = "serie";
    public static final String SERIE_TABLE = "serie_table";
    public static final String UPLOAD = "upload";
    public static final String DOWNLOAD = "download";
    public static final String LOAN_TABLE = "loan_table";
    public static final String BORROWER = "borrower";
    public static final String BORROWER_TABLE = "borrower_table";
    public static final String PROPERTY_DISPLAY = "property_display";
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        Locale.getDefault().setDefault(LocaleManager.instance.loadLocale());
        Plastic3DLookAndFeel.setCurrentTheme(new ExperienceBlue());
        try {
            UIManager.setLookAndFeel(new Plastic3DLookAndFeel());
        } catch (Exception e) {}
        JFrame frame = new JFrame();
        DockingDesktop desk = new DockingDesktop();
        DockingPreferences.setDottedDesktopStyle();
        
        //chooseTest(PERSONAL_INFO, desk);
        //chooseTest(GALLERY, desk);
        //chooseTest(ALBUM, desk);
        //chooseTest(ALBUM_TABLE, desk);
        //chooseTest(SERIE_THUMBNAIL, desk);
        //chooseTest(SERIE, desk);
        //chooseTest(SERIE_TABLE, desk);
        //chooseTest(UPLOAD, desk);
        //chooseTest(DOWNLOAD, desk);
        //chooseTest(LOAN_TABLE, desk);
        //chooseTest(BORROWER, desk);
        //chooseTest(BORROWER_TABLE, desk);
        chooseTest(PROPERTY_DISPLAY, desk);
        
        frame.getContentPane().add(desk);
        
        frame.setTitle("Test - Modelisable");
        frame.pack();
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setVisible(Boolean.TRUE.booleanValue());
    }
    
    public static void chooseTest(String s, DockingDesktop desk){
        if (s.equals(PERSONAL_INFO)){
            DockablePanel dockablePanel = DockablePanelFactory.createDPPersonalInfo();
            Serie serie = TestHelper.instance().createOneSerie();
            Album album = TestHelper.instance().createOneAlbum(serie);
            TestHelper.instance().createOneDedication(album, "resources/images/dedication/belladone_12b.JPG");
            SerieDAO sdao = new SerieDAO();
            AlbumDAO dao = new AlbumDAO();
            try {
                HibernateUtil.beginTransaction();
                sdao.save(serie);
                dao.save(album);
            
                HibernateUtil.commitTransaction();
            } catch (PersistencyException e) {
                e.printStackTrace();
            }
            
            dockablePanel.getModelisable().getGuiModel().setRelatedObject(album.getId());
            dockablePanel.getModelisable().getGuiModel().updateValues();
            
            desk.addDockable(dockablePanel);
        } else if (s.equals(GALLERY)){
            DockablePanel dockablePanel = DockablePanelFactory.createDPAlbumGallery();
            Serie serie = TestHelper.instance().createOneSerie();
            SerieDAO serieDAO = new SerieDAO();
            TestHelper.instance().createOneCommentedImage(serie, 0);
            TestHelper.instance().createOneCommentedImage(serie, 1);
            try {
                HibernateUtil.beginTransaction();
                serieDAO.save(serie);
            
                HibernateUtil.commitTransaction();
            } catch (PersistencyException e) {
                e.printStackTrace();
            }
            
            dockablePanel.getModelisable().getGuiModel().setRelatedObject(serie.getId());
            dockablePanel.getModelisable().getGuiModel().updateValues();
            
            desk.addDockable(dockablePanel);
        } else if (s.equals(ALBUM)){
        		DockablePanel dockablePanel = DockablePanelFactory.createDPAlbum();
            
            	Serie serie = TestHelper.instance().createOneSerie();
	    		SerieDAO sdao = new SerieDAO();
	    		Editor editor = TestHelper.instance().createOneEditor();
	    		EditorDAO edao = new EditorDAO();
	    		Collection collection = TestHelper.instance().createOneCollection(editor, Boolean.TRUE.booleanValue());
	    		Album album = TestHelper.instance().createOneAlbum(serie, collection);
                album.setCover(PropertyManager.getInstance().getSystemProperty("path_cover") + "/" + "ouleregardneportepascouv01.jpg");
	    		Author author = TestHelper.instance().createOneAuthor();
	    		AuthorDAO adao = new AuthorDAO();
	    		Work work = TestHelper.instance().createOneIllustration(author, album);
	    		WorkDAO wdao = new IllustrationDAO();
             try {
        	    		HibernateUtil.beginTransaction();
        	    		edao.save(editor);
        	    		sdao.save(serie);
        	    		adao.save(author);
        	    		wdao.save(work);
	    		
                    HibernateUtil.commitTransaction();
                } catch (PersistencyException e) {
                    e.printStackTrace();
                }
            
	    		dockablePanel.getModelisable().getGuiModel().setRelatedObject(album.getId());
	    		dockablePanel.getModelisable().getGuiModel().updateValues();
	    		desk.addDockable(dockablePanel);
        } else if (s.equals(ALBUM_TABLE)){
            DockablePanel dockablePanel = DockablePanelFactory.createDPAlbumTable();
            
            Serie serie = TestHelper.instance().createOneSerie();
            SerieDAO sdao = new SerieDAO();
            Editor editor = TestHelper.instance().createOneEditor();
            EditorDAO edao = new EditorDAO();
            Collection collection = TestHelper.instance().createOneCollection(editor, Boolean.FALSE.booleanValue());
            Album album = TestHelper.instance().createOneAlbum(serie, collection);
            album.setCover(PropertyManager.getInstance().getSystemProperty("path_cover") + "/" + "ouleregardneportepascouv01.jpg");
            Author author = TestHelper.instance().createOneAuthor();
            AuthorDAO adao = new AuthorDAO();
            Work work = TestHelper.instance().createOneIllustration(author, album);
            WorkDAO wdao = new IllustrationDAO();
            try {
                HibernateUtil.beginTransaction();
                edao.save(editor);
                sdao.save(serie);
                adao.save(author);
                wdao.save(work);
            
                HibernateUtil.commitTransaction();
            } catch (PersistencyException e) {
                e.printStackTrace();
            }
        
            dockablePanel.getModelisable().getGuiModel().setRelatedObject(PersistencyManager.findAlbumGuiObjects());
            dockablePanel.getModelisable().getGuiModel().updateValues();
            desk.addDockable(dockablePanel);
        } else if(s.equals(SERIE_THUMBNAIL)){
            DockablePanel dockablePanel = DockablePanelFactory.createDPSerieThumb();
            
            dockablePanel.getModelisable().getGuiModel().setRelatedObject(new Long(405));
            dockablePanel.getModelisable().getGuiModel().updateValues();
            desk.addDockable(dockablePanel);
        } else if(s.equals(SERIE)){
            DockablePanel dockablePanel = DockablePanelFactory.createDPSerie();
            
            dockablePanel.getModelisable().getGuiModel().setRelatedObject(new Long(405));
            dockablePanel.getModelisable().getGuiModel().updateValues();
            desk.addDockable(dockablePanel);
        } else if(s.equals(SERIE_TABLE)){
            DockablePanel dockablePanel = DockablePanelFactory.createDPSerieTable();
            dockablePanel.getModelisable().getGuiModel().updateValues();
            desk.addDockable(dockablePanel);
        } else if(s.equals(UPLOAD)){
            DockablePanel dockablePanel = DockablePanelFactory.createDPAlbumUpload();
            dockablePanel.getModelisable().getGuiModel().updateValues();
            desk.addDockable(dockablePanel);
        } else if(s.equals(DOWNLOAD)){
            DockablePanel dockablePanel = DockablePanelFactory.createDPAlbumDownload();
            dockablePanel.getModelisable().getGuiModel().updateValues();
            desk.addDockable(dockablePanel);
        } else if(s.equals(LOAN_TABLE)){
            DockablePanel dockablePanel = DockablePanelFactory.createDPLoanAlbumTable();
            dockablePanel.getModelisable().getGuiModel().updateValues();
            desk.addDockable(dockablePanel);
        } else if(s.equals(BORROWER)){
            Borrower bo = new Borrower();
            bo.setCreation(new Date());
            bo.setEmail("c.oukhemanou@free.fr");
            bo.setFirstName("Clément");
            bo.setName("Oukhemanou");
            bo.setPhone("05-56-56-34-12");
            HibernateUtil.beginTransaction();
            BorrowerDAO dao = new BorrowerDAO();
            try {
                dao.save(bo);
                HibernateUtil.commitTransaction();
            } catch (PersistencyException e) {
                e.printStackTrace();
            }
            DockablePanel dockablePanel = DockablePanelFactory.createDPBorrower();
            dockablePanel.getModelisable().getGuiModel().setRelatedObject(bo.getId());
            dockablePanel.getModelisable().getGuiModel().updateValues();
            desk.addDockable(dockablePanel);
        } else if(s.equals(BORROWER_TABLE)){
            Borrower bo = new Borrower();
            bo.setCreation(new Date());
            bo.setEmail("c.oukhemanou@free.fr");
            bo.setFirstName("Clément");
            bo.setName("Oukhemanou");
            bo.setPhone("05-56-56-34-12");
            
            Loan loan = new Loan();
            loan.setMaxDays(2);
            loan.setBegin(new Date(new Date().getTime() - (86400000 * 15)));
            loan.setBorrower(bo);
            Serie serie = TestHelper.instance().createOneSerie();
            Album album = TestHelper.instance().createOneAlbum(serie);
            loan.setAlbum(album);
            bo.addLoan(loan);
            HibernateUtil.beginTransaction();
            BorrowerDAO dao = new BorrowerDAO();
            try {
                dao.save(serie);
                dao.save(album);
                dao.save(bo);
                dao.save(loan);
                HibernateUtil.commitTransaction();
            } catch (PersistencyException e) {
                e.printStackTrace();
            }
            DockablePanel dockablePanel = DockablePanelFactory.createDPBorrowerTable();
            desk.addDockable(dockablePanel);
        } else if(s.equals(PROPERTY_DISPLAY)){
            DockablePanel dockablePanel = DockablePanelFactory.createDPPropertyDisplay();
            dockablePanel.getModelisable().getGuiModel().updateValues();
            desk.addDockable(dockablePanel);
        }
    }
}
