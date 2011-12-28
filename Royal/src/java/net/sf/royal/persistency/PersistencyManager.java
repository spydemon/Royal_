package net.sf.royal.persistency;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;

import net.sf.royal.datamodel.Album;
import net.sf.royal.datamodel.AlbumDAO;
import net.sf.royal.datamodel.Author;
import net.sf.royal.datamodel.AuthorDAO;
import net.sf.royal.datamodel.Borrower;
import net.sf.royal.datamodel.BorrowerDAO;
import net.sf.royal.datamodel.Bibliotheque;
import net.sf.royal.datamodel.Collection;
import net.sf.royal.datamodel.CollectionDAO;
import net.sf.royal.datamodel.CommentedImage;
import net.sf.royal.datamodel.CommentedImageDAO;
import net.sf.royal.datamodel.Dedication;
import net.sf.royal.datamodel.Editor;
import net.sf.royal.datamodel.EditorDAO;
import net.sf.royal.datamodel.HibernateUtil;
import net.sf.royal.datamodel.Loan;
import net.sf.royal.datamodel.Model;
import net.sf.royal.datamodel.PojoDAO;
import net.sf.royal.datamodel.Serie;
import net.sf.royal.datamodel.SerieDAO;
import net.sf.royal.datamodel.Type;
import net.sf.royal.datamodel.TypeDAO;
import net.sf.royal.datamodel.Work;
import net.sf.royal.datamodel.WorkDAO;
import net.sf.royal.exception.PersistencyException;
import net.sf.royal.gui.guimodel.album.AlbumGuiObject;
import net.sf.royal.gui.guimodel.album.CollectionGuiObject;
import net.sf.royal.gui.guimodel.album.EditorGuiObject;
import net.sf.royal.gui.guimodel.album.SerieGuiObject;
import net.sf.royal.gui.guimodel.loan.BorrowerGuiObject;
import net.sf.royal.gui.guimodel.loan.LoanAlbumGuiObject;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.util.graph.DistributionValue;
import net.sf.royal.gui.util.tables.CommentedImageObject;

import org.apache.log4j.Logger;
import org.hibernate.Query;

/**
 * @author bibounde
 * @author Soulou Manager of the persistency, using hibernate library
 * @author Kevin Hagner
 */

public class PersistencyManager {

	public final static boolean SORTED = true;
	public final static boolean UNSORTED = false;

	private final static String FIELD_NAME = "TABLE_NAME";

	private final static Logger logger = Logger
			.getLogger(PersistencyManager.class);

	private static PojoDAO albumDAO = new AlbumDAO();
	private static PojoDAO commentedImageDAO = new CommentedImageDAO();
	private static PojoDAO serieDAO = new SerieDAO();
	private static PojoDAO editorDAO = new EditorDAO();
	private static PojoDAO typeDAO = new TypeDAO();
	private static PojoDAO collectionDAO = new CollectionDAO();
	private static PojoDAO authorDAO = new AuthorDAO();
	private static PojoDAO workDAO = new WorkDAO();
	private static PojoDAO borrowerDAO = new BorrowerDAO();

	/**
	 * Returns the Dedication corresponding to the given ID
	 * 
	 * @return Dedication
	 * @param album
	 *            ID of the working Album
	 * @see Dedication
	 */
	public static Dedication findDedicationByAlbumID(Long albumID) {
		Album album = null;
		try {
			album = (Album) albumDAO.findByPrimaryKey(albumID);
			return album.getDedication();
		} catch (PersistencyException e) {
			e.manageException();
		}
		return null;
	}

	/**
	 * Returns the Album corresponding to the given ID
	 * 
	 * @param album
	 *            ID
	 * @return Album
	 */
	public static Album findAlbumByID(Long albumID) {
		try {
			return (Album) albumDAO.findByPrimaryKey(albumID);
		} catch (PersistencyException e) {
			e.manageException();
		}
		return null;
	}

	/**
	 * Returns the Serie corresponding to the given ID
	 * 
	 * @param album
	 *            ID
	 * @return Serie
	 */
	public static Serie findSerieByID(Long serieID) {
		try {
			return (Serie) serieDAO.findByPrimaryKey(serieID);
		} catch (PersistencyException e) {
			e.manageException();
		}
		return null;
	}

	/**
	 * Returns the Type corresponding to the given ID
	 * 
	 * @param album
	 *            ID
	 * @return Type
	 */
	public static Type findTypeByID(Long typeID) {
		try {
			return (Type) typeDAO.findByPrimaryKey(typeID);
		} catch (PersistencyException e) {
			e.manageException();
		}
		return null;
	}

	/**
	 * Returns the Editor corresponding to the given ID
	 * 
	 * @param album
	 *            ID
	 * @return Editor
	 */
	public static Editor findEditorByID(Long editorID) {
		try {
			return (Editor) editorDAO.findByPrimaryKey(editorID);
		} catch (PersistencyException e) {
			e.manageException();
		}
		return null;
	}

	/**
	 * Returns the Collecton corresponding to the given ID
	 * 
	 * @param album
	 *            ID
	 * @return Collection
	 */
	public static Collection findCollectionByID(Long collectionID) {
		try {
			return (Collection) collectionDAO.findByPrimaryKey(collectionID);
		} catch (PersistencyException e) {
			e.manageException();
		}
		return null;
	}

	/**
	 * Returns the Author corresponding to the given ID
	 * 
	 * @param album
	 *            ID
	 * @return Author
	 */
	public static Author findAuthorByID(Long authorID) {
		try {
			return (Author) authorDAO.findByPrimaryKey(authorID);
		} catch (PersistencyException e) {
			e.manageException();
		}
		return null;
	}

	/**
	 * Returns the Author corresponding to the given workID
	 * 
	 * @param workID
	 * @return Author
	 */
	public static Author findAuthorByWorkID(Long workID) {
		try {
			String query = "select w.author from Work as w where w.id=:id";
			Author author = (Author) HibernateUtil.currentSession()
					.createQuery(query).setParameter("id", workID)
					.uniqueResult();
			return author;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	/**
	 * Returns the Work corresponding to the given workID
	 * 
	 * @param workID
	 * @return Work
	 */
	public static Work findWorkByID(Long workID) {
		try {
			return (Work) workDAO.findByPrimaryKey(workID);
		} catch (PersistencyException e) {
			e.manageException();
		}
		return null;
	}

	/**
	 * Find the commented image of an album
	 * 
	 * @param albumID
	 * @param sorted
	 *            if true the results are sorted by position
	 * @return List
	 */
	@SuppressWarnings("unchecked")
	public static List<CommentedImage> findCommentedImagesByAlbumID(
			Long albumID, boolean sorted) {
		String sort = "";
		if (sorted) {
			sort = " order by ci.position asc";
		}
		try {
			Query query = HibernateUtil
					.currentSession()
					.createQuery(
							"from CommentedImage ci where ci.album.id=:id"
									+ sort).setLong("id", albumID.longValue());
			return (List<CommentedImage>) query.list();
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	/**
	 * @param ciID
	 * @return CommentedImage
	 */
	public static CommentedImage findCommentedImagesByID(Long ciID) {
		try {
			Query query = HibernateUtil.currentSession()
					.createQuery("from CommentedImage ci where ci.id=:id")
					.setLong("id", ciID);
			return (CommentedImage) query.uniqueResult();
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	/**
	 * Set the value of commented image from a commented image object
	 * 
	 * @param ci
	 * @param ciObject
	 */
	public static void setCommentedImage(CommentedImage cImage,
			CommentedImageObject ciObject) {
		cImage.setComment(ciObject.getComment());
		cImage.setHalign(ciObject.getHalign());
		cImage.setImageURL(ciObject.getImageURL());
		cImage.setPosition(ciObject.getPosition());
		cImage.setValign(ciObject.getValign());
	}

	/**
	 * Update an old commentedImage with the values of new CommentedImageObject.
	 * 
	 * @param coteObject
	 * @param oldCotes
	 */
	public static void updateCommentedImage(CommentedImageObject ciObject,
			Object[] oldCommentedImages) {
		for (int i = 0; i < oldCommentedImages.length; i++) {
			CommentedImage oldCommentedimage = (CommentedImage) oldCommentedImages[i];
			if (oldCommentedimage.getId().longValue() == ciObject.getId()
					.longValue()) {
				PersistencyManager.setCommentedImage(oldCommentedimage,
						ciObject);
			}
		}
	}

	/**
	 * Delete Commented Image of a Serie. Use delete-orphan
	 * 
	 * @param ciObjectToDelete
	 * @param commentedImages
	 *            CommentedImages which are in the session.
	 */
	public static void deleteCommentedImages(
			CommentedImageObject ciObjectToDelete, Object[] commentedImages) {
		for (int i = 0; i < commentedImages.length; i++) {
			CommentedImage ci = (CommentedImage) commentedImages[i];
			if (ci.getId().longValue() == ciObjectToDelete.getId().longValue()) {
				ci.getAlbum().removeCommentedImage(ci);
			}
		}
	}

	/**
	 * Find AlbumGUIObject in DB from albumId
	 * 
	 * @param albumId
	 * @return
	 */
	public static AlbumGuiObject findAGO(Long albumId) {
		Album album;
		try {
			album = (Album) albumDAO.findByPrimaryKey(albumId);
			AlbumGuiObject albumGuiObject = new AlbumGuiObject(albumId);

			Work[] works = (Work[]) album.getWorks().toArray();
			for (int i = 0; i < works.length; i++) {
				albumGuiObject.getWorks().add(works[i]);
			}
			albumGuiObject.setSerie(new SerieGuiObject(album.getSerie()));
			albumGuiObject.setCollection(new CollectionGuiObject(album
					.getCollection().getId(), album.getCollection().getName(),
					album.getCollection().isHead(), album.getCollection()
							.getWeb(), album.getCollection().getDescription()));
			Editor editor = album.getCollection().getEditor();
			albumGuiObject.setEditor(new EditorGuiObject(editor.getId(), editor
					.getName(), editor.getDescription(), editor.getWeb()));

			albumGuiObject.setCover(album.getCover());
			albumGuiObject.setTitle(album.getTitle());
			albumGuiObject.setTome(album.getTome());
			albumGuiObject.setRegistration(album.getRegistration());
			albumGuiObject.setIsbn(album.getIsbn());
			albumGuiObject.setPageCount(album.getPageCount());
			albumGuiObject.setDimension(album.getDimension());
			albumGuiObject.setOriginal(album.isOriginal());
			albumGuiObject.setIntegral(album.isIntegral());
			albumGuiObject.setSpecial(album.isSpecial());
			albumGuiObject.setCopies(album.getCopies());
			albumGuiObject.setTekId(album.getTekId());

			return albumGuiObject;
		} catch (PersistencyException e) {
			e.manageException();
		}
		return null;

	}

	/**
	 * Get a list which contains CollectionGuiObject for one Editor
	 * 
	 * @param editorId
	 * @return
	 */
	public static List<Collection> findCollectionByEditor(Long editorId) {
		if (editorId == null) {
			return new ArrayList<Collection>();
		}
		try {
			String query = "select c from Collection c inner join c.editor e where e.id=:id  order by c.name asc";
			@SuppressWarnings("unchecked")
			List<Collection> collections = HibernateUtil.currentSession()
					.createQuery(query).setParameter("id", editorId).list();
			return collections;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	public static List<Bibliotheque> findLibs() {
		String query = "FROM Bibliotheque ORDER BY id";
		List<Bibliotheque> libs = (List<Bibliotheque>) HibernateUtil.currentSession().createQuery(query).list();
		return libs;
	}
	
	/**
	 * Get a list which contains AuthorObjects order by nickName, name and
	 * firstName
	 * 
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static List<Author> findAuthors() {
		String query = "from Author a order by a.nickName+a.name+a.firstName";// ,a.name,a.firstName
																				// asc";
		List<Author> authors = (List<Author>) HibernateUtil.currentSession()
				.createQuery(query).list();
		return authors;
	}

	/**
	 * Get a list which contains all Types
	 * 
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static List<Type> findTypes() {
		String query = "from Type t order by t.name asc";
		try {
			List<Type> types = (List<Type>) HibernateUtil.currentSession()
					.createQuery(query).list();
			return types;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	/**
	 * Find the head Collection of an Editor
	 * 
	 * @param editor
	 * @return A Collection
	 */
	public static Collection findHeadCollection(Editor editor) {
		if (editor == null || editor.getId() == null) {
			throw new IllegalArgumentException(
					"The Editor or its id must not be null");
		}
		try {
			String query = "from Collection c where c.editor.id=:id and c.head=:head";
			Collection res = (Collection) HibernateUtil.currentSession()
					.createQuery(query)
					.setLong("id", editor.getId().longValue())
					.setBoolean("head", Boolean.TRUE.booleanValue())
					.uniqueResult();
			return res;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	/**
	 * Find all the editors in the database
	 * 
	 * @return The list of all editors
	 */
	@SuppressWarnings("unchecked")
	public static List<Editor> findEditors() {
		String query = "from Editor e order by e.name";
		List<Editor> res = null;
		try {
			res = (List<Editor>) HibernateUtil.currentSession()
					.createQuery(query).list();
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return res;
	}

	/**
	 * Get all the collections of the database
	 * 
	 * @return The list of the collections
	 */
	@SuppressWarnings("unchecked")
	public static List<Collection> findCollections() {
		String query = "from Collection e order by e.name";
		List<Collection> res = null;
		try {
			res = (List<Collection>) HibernateUtil.currentSession()
					.createQuery(query).list();
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return res;
	}

	/**
	 * Find all the album in DB
	 * 
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static List<Album> findAlbums() {
		try {
			// String query = "from Album a order by a.title asc";
			String query = "from Album a order by a.title asc";
			ArrayList<Album> albums = (ArrayList<Album>) HibernateUtil
					.currentSession().createQuery(query).list();
			return albums;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	/**
	 * Remove an Album which must represents a persistent album
	 * 
	 * @param album
	 */
	public static void removeAlbum(Album album) {
		if (album.getId() == null)
			throw new IllegalArgumentException("The Album must be persistent");
		try {
			HibernateUtil.beginTransaction();

			Serie s = album.getSerie();
			if (s != null) {
				s.removeAlbum(album);
				serieDAO.save(s);
			}
			Collection c = album.getCollection();
			if (c != null) {
				c.removeAlbum(album);
				collectionDAO.save(c);
			}
			Set<Work> works = album.getWorks();
			logger.debug("Delete works of album : " + album.getId());
			for (Work w : works) {
				Author au = w.getAuthor();
				au.removeWork(w);
				authorDAO.save(au);
				workDAO.delete(w);
			}

			Set<CommentedImage> gallery = album.getGallery();
			logger.debug("Delete commentedImage of album : " + album.getId());
			for (CommentedImage ci : gallery) {
				gallery.remove(ci);
				commentedImageDAO.delete(ci);
			}

			Album a = (Album) albumDAO.findByPrimaryKey(album.getId());
			logger.debug("Delete Album " + album.getId());
			albumDAO.delete(a);
			HibernateUtil.commitTransaction();
		} catch (PersistencyException e) {
			e.manageException();
		}
	}

	public static int findCountAlbums() {
		try {
			Integer count = (Integer) HibernateUtil.currentSession()
					.createQuery("select count(*) from Album").uniqueResult();
			if (count != null) {
				return count.intValue();
			}
		} catch (Exception e) {
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.FATAL);
			pe.manageException();
		}
		return 0;
	}

	public static int findCountSeries() {
		try {
			Integer count = (Integer) HibernateUtil.currentSession()
					.createQuery("select count(*) from Serie").uniqueResult();
			if (count != null) {
				return count.intValue();
			}
		} catch (Exception e) {
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.FATAL);
			pe.manageException();
		}
		return 0;

	}

	/**
	 * Find all the Serie in the DB
	 * 
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static List<Serie> findSeries() {
		try {
			String query = "from Serie s order by s.name";
			ArrayList<Serie> series = (ArrayList<Serie>) HibernateUtil
					.currentSession().createQuery(query).list();
			return series;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	/**
	 * Find all the Serie in the DB using the ID of an author
	 * 
	 * @param id
	 *            The id of the author
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static List<Serie> findSeriesByAuthorID(long id) {
		try {
			String query = "from Serie where id in (select distinct w.album.serie.id from Work w where w.author.id=:id)";
			ArrayList<Serie> series = (ArrayList<Serie>) HibernateUtil
					.currentSession().createQuery(query).setParameter("id", id)
					.list();
			return series;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	/**
	 * Find all the AuthorObject of a Serie
	 * 
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static List<Author> findAuthorsBySerie(Long serieId) {
		try {
			String query = "select distinct aut from Work w inner join w.author aut where w.album.serie.id=:id order by aut.nickName, aut.name, aut.firstName";
			List<Author> authors = (List<Author>) HibernateUtil
					.currentSession().createQuery(query)
					.setParameter("id", serieId).list();
			return authors;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	/**
	 * Find all the EditorObject of a Serie
	 * 
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static List<Editor> findEditorObjects(Long serieId) {
		try {
			String query = "select distinct col.editor from Album alb inner join alb.collection col where alb.serie.id=:id order by col.editor.name";
			List<Editor> editors = (List<Editor>) HibernateUtil
					.currentSession().createQuery(query)
					.setParameter("id", serieId).list();

			return editors;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	public static void removeSerie(Serie serie) {
		if (serie == null)
			throw new IllegalArgumentException("The Serie must be persistent");
		try {
			HibernateUtil.beginTransaction();
			for (Album album : (Set<Album>) serie.getAlbums()) {
				Album a = (Album) albumDAO.findByPrimaryKey(album.getId());
				a.setSerie(null);
				logger.debug("Remove serie \"" + serie.getName()
						+ "\" of album : " + a.getTitle() + " (" + a.getId()
						+ ")");
				albumDAO.save(a);
			}

			Type type = serie.getType();
			if (type != null) {
				Type t = (Type) typeDAO.findByPrimaryKey(type.getId());
				t.removeSerie(serie);
				logger.debug("Remove serie \"" + serie.getName()
						+ "\" to set of type : " + t.getName() + " ("
						+ t.getId() + ")");
				typeDAO.save(t);
			}

			// Serie s = (Serie)serieDAO.findByPrimaryKey(serie.getId());
			logger.debug("Delete serie : " + serie.getName() + " ("
					+ serie.getId() + ")");
			serieDAO.delete(serie);
			HibernateUtil.commitTransaction();

		} catch (PersistencyException e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
	}

	/**
	 * Delete the type t from the database
	 * 
	 * @param t
	 *            Type to delete
	 */
	public static void removeType(Type type) {
		if (type == null) {
			throw new IllegalArgumentException("The Serie must be persistent");
		}
		try {
			HibernateUtil.beginTransaction();
			for (Serie s : (Set<Serie>) type.getSeries()) {
				s.setType(null);
				serieDAO.save(s);
			}
			Type t = (Type) typeDAO.findByPrimaryKey(type.getId());
			typeDAO.delete(t);

			HibernateUtil.commitTransaction();
		} catch (PersistencyException e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
	}

	/**
	 * Delete the work w from the database
	 * 
	 * @param w
	 *            Work to delete
	 */
	public static void removeWork(Work work) {
		if (work == null) {
			throw new IllegalArgumentException("The work must be persistent");
		}
		try {
			HibernateUtil.beginTransaction();
			workDAO.delete(work);
			HibernateUtil.commitTransaction();
		} catch (PersistencyException e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
	}

	public static Long findTekId(Model model) {
		if (model.getId() != null) {
			String query = "select m.tekId from " + model.getClass().getName()
					+ " m where m.id=:id";
			try {
				return (Long) HibernateUtil.currentSession().createQuery(query)
						.setParameter("id", model.getId()).uniqueResult();
			} catch (Exception e) {
				try {
					HibernateUtil.rollbackTransaction();
				} catch (PersistencyException e1) {
					e1.manageException();
				}
				PersistencyException pe = new PersistencyException(e,
						PersistencyException.CONTINUE);
				pe.manageException();
			}
		}
		return null;
	}

	/**
	 * Find all albums for a Serie
	 * 
	 * @return List<Album>
	 */
	@SuppressWarnings("unchecked")
	public static List<Album> findAlbumsBySerieID(long id) {
		try {
			String query = "from Album a where a.serie.id=:id order by a.tome.number, a.tome.revision asc";
			ArrayList<Album> albums = (ArrayList<Album>) HibernateUtil
					.currentSession().createQuery(query).setParameter("id", id)
					.list();
			return albums;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	public static List<Album> findAlbumsAuthorNull() {
		try {
			String query = "from Album a where a.id not in (select album.id from Work) order by a.title asc";
			ArrayList<Album> albums = (ArrayList<Album>) HibernateUtil
					.currentSession().createQuery(query).list();
			return albums;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	public static List<Album> findAlbumsSerieNull() {
		try {
			String query = "from Album a where a.serie=null order by a.title asc";
			ArrayList<Album> albums = (ArrayList<Album>) HibernateUtil
					.currentSession().createQuery(query).list();
			return albums;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	public static List<Album> findAlbumsTypeNull() {
		try {
			String query = "from Album a where a.serie.type is null order by a.title asc";
			ArrayList<Album> albums = (ArrayList<Album>) HibernateUtil
					.currentSession().createQuery(query).list();
			return albums;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	public static List<Serie> findSeriesTypeNull() {
		try {
			String query = "from Serie s where s.type is null order by s.name asc";
			ArrayList<Serie> series = (ArrayList<Serie>) HibernateUtil
					.currentSession().createQuery(query).list();
			return series;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	/**
	 * Find all albums for a Author
	 * 
	 * @return List<Album>
	 */
	@SuppressWarnings("unchecked")
	public static List<Album> findAlbumsByAuthorID(long id) {
		try {
			String query = "from Album where id in (select w.album from Work w where w.author.id=:id) order by title asc";
			ArrayList<Album> albums = (ArrayList<Album>) HibernateUtil
					.currentSession().createQuery(query).setParameter("id", id)
					.list();
			return albums;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	public static Album findAlbumByTekID(Long tekId) {
		if (tekId != null) {
			String query = "from Album a where a.tekId=:id";
			try {
				return (Album) HibernateUtil.currentSession()
						.createQuery(query).setParameter("id", tekId)
						.uniqueResult();
			} catch (Exception e) {
				try {
					HibernateUtil.rollbackTransaction();
				} catch (PersistencyException e1) {
					e1.manageException();
				}
				PersistencyException pe = new PersistencyException(e,
						PersistencyException.CONTINUE);
				pe.manageException();
			}
		}
		return null;
	}

	public static Serie findSerieByTekID(Long tekId) {
		if (tekId != null) {
			String query = "from Serie s where s.tekId=:id";
			try {
				return (Serie) HibernateUtil.currentSession()
						.createQuery(query).setParameter("id", tekId)
						.uniqueResult();
			} catch (Exception e) {
				try {
					HibernateUtil.rollbackTransaction();
				} catch (PersistencyException e1) {
					e1.manageException();
				}
				PersistencyException pe = new PersistencyException(e,
						PersistencyException.CONTINUE);
				pe.manageException();
			}
		}
		return null;
	}

	public static Type findTypeByTekID(Long tekId) {
		if (tekId != null) {
			String query = "from Type t where t.tekId=:id";
			try {
				return (Type) HibernateUtil.currentSession().createQuery(query)
						.setParameter("id", tekId).uniqueResult();
			} catch (Exception e) {
				try {
					HibernateUtil.rollbackTransaction();
				} catch (PersistencyException e1) {
					e1.manageException();
				}
				PersistencyException pe = new PersistencyException(e,
						PersistencyException.CONTINUE);
				pe.manageException();
			}
		}
		return null;
	}

	public static Author findAuthorByTekID(Long tekId) {
		if (tekId != null) {
			String query = "from Author a where a.tekId=:id";
			try {
				return (Author) HibernateUtil.currentSession()
						.createQuery(query).setParameter("id", tekId)
						.uniqueResult();
			} catch (Exception e) {
				try {
					HibernateUtil.rollbackTransaction();
				} catch (PersistencyException e1) {
					e1.manageException();
				}
				PersistencyException pe = new PersistencyException(e,
						PersistencyException.CONTINUE);
				pe.manageException();
			}
		}
		return null;
	}

	public static Collection findCollectionByTekID(Long tekId) {
		if (tekId != null) {
			String query = "from Collection c where c.tekId=:id";
			try {
				return (Collection) HibernateUtil.currentSession()
						.createQuery(query).setParameter("id", tekId)
						.uniqueResult();
			} catch (Exception e) {
				try {
					HibernateUtil.rollbackTransaction();
				} catch (PersistencyException e1) {
					e1.manageException();
				}
				PersistencyException pe = new PersistencyException(e,
						PersistencyException.CONTINUE);
				pe.manageException();
			}
		}
		return null;
	}

	public static Editor findEditorByTekID(Long tekId) {
		if (tekId != null) {
			String query = "from Editor e where e.tekId=:id";
			try {
				return (Editor) HibernateUtil.currentSession()
						.createQuery(query).setParameter("id", tekId)
						.uniqueResult();
			} catch (Exception e) {
				try {
					HibernateUtil.rollbackTransaction();
				} catch (PersistencyException e1) {
					e1.manageException();
				}
				PersistencyException pe = new PersistencyException(e,
						PersistencyException.CONTINUE);
				pe.manageException();
			}
		}
		return null;
	}

	public static Work findWorkByTekID(Long tekId) {
		if (tekId != null) {
			String query = "from Work w where w.tekId=:id";
			try {
				return (Work) HibernateUtil.currentSession().createQuery(query)
						.setParameter("id", tekId).uniqueResult();
			} catch (Exception e) {
				try {
					HibernateUtil.rollbackTransaction();
				} catch (PersistencyException e1) {
					e1.manageException();
				}
				PersistencyException pe = new PersistencyException(e,
						PersistencyException.CONTINUE);
				pe.manageException();
			}
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	public static List<DistributionValue> findDistributionValues(Long borrowerID) {
		try {
			String sQuery = "select count(*) from Loan l where l.borrower.id=:id";
			Integer total = (Integer) HibernateUtil.currentSession()
					.createQuery(sQuery).setParameter("id", borrowerID)
					.uniqueResult();

			sQuery = "select distinct l.album.serie.id from Loan l where l.borrower.id=:id";
			List<Long> serieIDs = HibernateUtil.currentSession()
					.createQuery(sQuery).setParameter("id", borrowerID).list();
			List<DistributionValue> res = new ArrayList<DistributionValue>();
			DistributionValue nullValue = null;
			for (int i = 0; i < serieIDs.size(); i++) {
				Long serieID = serieIDs.get(i);
				sQuery = "select count(*) from Loan l inner join l.album alb where alb.serie.id=:id and l.borrower.id=:bid";
				Integer count = (Integer) HibernateUtil.currentSession()
						.createQuery(sQuery).setParameter("id", serieID)
						.setParameter("bid", borrowerID).uniqueResult();
				double value = (count.doubleValue() * 100)
						/ total.doubleValue();
				Type type = ((Serie) serieDAO.findByPrimaryKey(serieID))
						.getType();
				if (type == null) {
					if (nullValue == null) {
						nullValue = new DistributionValue(LocaleManager
								.getInstance().getString("null_type"), 0.00);
					}
					nullValue.setValue(nullValue.getValue() + value);
				} else {
					res.add(new DistributionValue(type.getName(), value));
				}
			}
			if (nullValue != null) {
				res.add(nullValue);
			}
			return res;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	/**
	 * Get the number of loans for a borrower
	 * 
	 * @param borrowerID
	 *            The id of the borrower
	 * @return The number of loans
	 */
	public static int findLoanTotal(Long borrowerID) {
		try {
			String sQuery = "select count(*) from Loan l where l.borrower.id=:id";
			Integer total = (Integer) HibernateUtil.currentSession()
					.createQuery(sQuery).setParameter("id", borrowerID)
					.uniqueResult();
			return total.intValue();
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return 0;
	}

	@SuppressWarnings("unchecked")
	public static float findFineTotal(Long borrowerID) {
		try {
			String sQuery = "from Loan l where l.borrower.id=:id";
			List<Loan> list = HibernateUtil.currentSession()
					.createQuery(sQuery).setParameter("id", borrowerID).list();
			float res = 0;
			for (int i = 0; i < list.size(); i++) {
				Loan loan = list.get(i);

				Date dateMax = new Date(loan.getBegin().getTime()
						+ (loan.getMaxDays() * 86400000));

				Date ref = null;
				if (loan.getEnd() != null) {
					ref = loan.getEnd();
				} else {
					ref = new Date();
				}

				if (!dateMax.after(ref)) {
					int days = new Float(
							(ref.getTime() - dateMax.getTime()) / 86400000)
							.intValue();
					res += days * loan.getPriceByDay();
				}
			}
			return res;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return 0;
	}

	@SuppressWarnings("unchecked")
	public static float findPenaltyTotal(Long borrowerID) {
		try {
			String sQuery = "from Loan l where l.borrower.id=:id";
			List<Loan> list = HibernateUtil.currentSession()
					.createQuery(sQuery).setParameter("id", borrowerID).list();
			float res = 0;
			for (int i = 0; i < list.size(); i++) {
				Loan loan = list.get(i);

				Date dateMax = new Date(loan.getBegin().getTime()
						+ (loan.getMaxDays() * 86400000));

				Date ref = null;
				if (loan.getEnd() != null) {
					ref = loan.getEnd();
				} else {
					ref = new Date();
				}

				if (!dateMax.after(ref)) {
					int days = new Float(
							(ref.getTime() - dateMax.getTime()) / 86400000)
							.intValue();
					res += days * loan.getPenaltyByDay();
				}
			}
			return res;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return 0;
	}

	@SuppressWarnings("unchecked")
	public static int findDelayTotal(Long borrowerID) {
		try {
			String sQuery = "from Loan l where l.borrower.id=:id";
			List<Loan> list = HibernateUtil.currentSession()
					.createQuery(sQuery).setParameter("id", borrowerID).list();
			int res = 0;
			for (int i = 0; i < list.size(); i++) {
				Loan loan = list.get(i);

				Date dateMax = new Date(loan.getBegin().getTime()
						+ (loan.getMaxDays() * 86400000));

				Date ref = null;
				if (loan.getEnd() != null) {
					ref = loan.getEnd();
				} else {
					ref = new Date();
				}

				if (!dateMax.after(ref)) {
					int days = new Float(
							(ref.getTime() - dateMax.getTime()) / 86400000)
							.intValue();
					res += days;
				}
			}
			return res;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return 0;
	}

	public static void delLib (Bibliotheque b) {
		String hqlDelete = "delete Bibliotheque where id = :id";
		HibernateUtil.currentSession().createQuery(hqlDelete).setParameter("id", b.getId()).executeUpdate();
		System.out.println("Delete bibli");
	}
	
	@SuppressWarnings("unchecked")
	public static float findLoanAverage(Long borrowerID) {
		try {
			String sQuery = "select distinct l.album from Loan l where l.borrower.id=:id";
			List<Album> list = (List<Album>) HibernateUtil.currentSession()
					.createQuery(sQuery).setParameter("id", borrowerID).list();
			int total = 0;
			for (int i = 0; i < list.size(); i++) {
				Album album = list.get(i);
				sQuery = "select count(*) from Loan l where l.borrower.id=:id and l.album.id=:aid";
				Integer count = (Integer) HibernateUtil.currentSession()
						.createQuery(sQuery).setParameter("id", borrowerID)
						.setParameter("aid", album.getId()).uniqueResult();
				total += count.intValue();
			}
			if (list.size() != 0) {
				return new Float(total).floatValue()
						/ new Float(list.size()).floatValue();
			}

		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return 0;
	}

	@SuppressWarnings("unchecked")
	public static List<String> findLocalizedLoanYears(Long borrowerID) {
		try {
			String sQuery = "from Loan l where l.borrower.id=:id order by l.begin desc";
			List<Loan> list = (List<Loan>) HibernateUtil.currentSession()
					.createQuery(sQuery).setParameter("id", borrowerID).list();
			List<String> res = new ArrayList<String>();
			for (int i = 0; i < list.size(); i++) {
				Loan loan = (Loan) list.get(i);
				String year = LocaleManager.getInstance().getYearDate(
						loan.getBegin());
				if (!res.contains(year)) {
					res.add(year);
				}
			}
			return res;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	/**
	 * Returns the number of borrows for a year
	 * 
	 * @param borrowerID
	 *            , year
	 * @return
	 */
	public static int[] getLoanNumberForOneYear(Long borrowerID, String year) {
		try {
			Date jan = LocaleManager.getInstance().getMonthDate("01-" + year);
			Date jan2 = LocaleManager.getInstance()
					.getMonthDate(
							"01-"
									+ String.valueOf(Integer.valueOf(year)
											.intValue() + 1));
			Date feb = LocaleManager.getInstance().getMonthDate("02-" + year);
			Date mar = LocaleManager.getInstance().getMonthDate("03-" + year);
			Date avr = LocaleManager.getInstance().getMonthDate("04-" + year);
			Date may = LocaleManager.getInstance().getMonthDate("05-" + year);
			Date jun = LocaleManager.getInstance().getMonthDate("06-" + year);
			Date jul = LocaleManager.getInstance().getMonthDate("07-" + year);
			Date aug = LocaleManager.getInstance().getMonthDate("08-" + year);
			Date sep = LocaleManager.getInstance().getMonthDate("09-" + year);
			Date oct = LocaleManager.getInstance().getMonthDate("10-" + year);
			Date nov = LocaleManager.getInstance().getMonthDate("11-" + year);
			Date dec = LocaleManager.getInstance().getMonthDate("12-" + year);

			int[] res = new int[12];
			res[0] = getLoanNumberBetweenDates(jan, feb, borrowerID);
			res[1] = getLoanNumberBetweenDates(feb, mar, borrowerID);
			res[2] = getLoanNumberBetweenDates(mar, avr, borrowerID);
			res[3] = getLoanNumberBetweenDates(avr, may, borrowerID);
			res[4] = getLoanNumberBetweenDates(may, jun, borrowerID);
			res[5] = getLoanNumberBetweenDates(jun, jul, borrowerID);
			res[6] = getLoanNumberBetweenDates(jul, aug, borrowerID);
			res[7] = getLoanNumberBetweenDates(aug, sep, borrowerID);
			res[8] = getLoanNumberBetweenDates(sep, oct, borrowerID);
			res[9] = getLoanNumberBetweenDates(oct, nov, borrowerID);
			res[10] = getLoanNumberBetweenDates(nov, dec, borrowerID);
			res[11] = getLoanNumberBetweenDates(dec, jan2, borrowerID);

			return res;

		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	/**
	 * Returns the number of borrowing between two dates for one borrower
	 * 
	 * @param start
	 *            , stop, borrowerID
	 * @return
	 */
	public static int getLoanNumberBetweenDates(Date start, Date stop,
			Long borrowerID) {
		try {
			String sQuery = "select count(*) from Loan l where l.borrower.id=:id and l.begin!=:stop and l.begin between :start and :stop";
			Integer res = (Integer) HibernateUtil.currentSession()
					.createQuery(sQuery).setParameter("id", borrowerID)
					.setParameter("start", start).setParameter("stop", stop)
					.uniqueResult();
			return res.intValue();
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return 0;
	}

	public static double[] getMaxDaysForOneYear(Long borrowerID, String year) {
		try {
			Date jan = LocaleManager.getInstance().getMonthDate("01-" + year);
			Date jan2 = LocaleManager.getInstance()
					.getMonthDate(
							"01-"
									+ String.valueOf(Integer.valueOf(year)
											.intValue() + 1));
			Date feb = LocaleManager.getInstance().getMonthDate("02-" + year);
			Date mar = LocaleManager.getInstance().getMonthDate("03-" + year);
			Date avr = LocaleManager.getInstance().getMonthDate("04-" + year);
			Date may = LocaleManager.getInstance().getMonthDate("05-" + year);
			Date jun = LocaleManager.getInstance().getMonthDate("06-" + year);
			Date jul = LocaleManager.getInstance().getMonthDate("07-" + year);
			Date aug = LocaleManager.getInstance().getMonthDate("08-" + year);
			Date sep = LocaleManager.getInstance().getMonthDate("09-" + year);
			Date oct = LocaleManager.getInstance().getMonthDate("10-" + year);
			Date nov = LocaleManager.getInstance().getMonthDate("11-" + year);
			Date dec = LocaleManager.getInstance().getMonthDate("12-" + year);

			double[] res = new double[12];
			res[0] = getMaxDaysBetweenDates(jan, feb, borrowerID);
			res[1] = getMaxDaysBetweenDates(feb, mar, borrowerID);
			res[2] = getMaxDaysBetweenDates(mar, avr, borrowerID);
			res[3] = getMaxDaysBetweenDates(avr, may, borrowerID);
			res[4] = getMaxDaysBetweenDates(may, jun, borrowerID);
			res[5] = getMaxDaysBetweenDates(jun, jul, borrowerID);
			res[6] = getMaxDaysBetweenDates(jul, aug, borrowerID);
			res[7] = getMaxDaysBetweenDates(aug, sep, borrowerID);
			res[8] = getMaxDaysBetweenDates(sep, oct, borrowerID);
			res[9] = getMaxDaysBetweenDates(oct, nov, borrowerID);
			res[10] = getMaxDaysBetweenDates(nov, dec, borrowerID);
			res[11] = getMaxDaysBetweenDates(dec, jan2, borrowerID);

			return res;

		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	/**
	 * Returns the number of maximum days between the date of beginning and date
	 * of end of the borrow
	 * 
	 * @param start
	 *            , stop, borrowerID
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static double getMaxDaysBetweenDates(Date start, Date stop,
			Long borrowerID) {
		try {
			String sQuery = "from Loan l where l.borrower.id=:id and l.end is not null and l.begin < :stop";
			List<Loan> loans = (List<Loan>) HibernateUtil.currentSession()
					.createQuery(sQuery).setParameter("id", borrowerID)
					.setParameter("stop", stop).list();
			long startTime = start.getTime();
			long stopTime = stop.getTime();
			long longRes = 0;
			for (int i = 0; i < loans.size(); i++) {
				Loan loan = (Loan) loans.get(i);
				long beginTime = loan.getBegin().getTime();
				long maxTime = beginTime + (loan.getMaxDays() * 86400000);

				long endRes;
				if (maxTime > startTime) {
					if (maxTime > stopTime) {
						endRes = stopTime;
					} else {
						endRes = maxTime;
					}
					longRes += endRes - startTime;
				}
			}
			double res = longRes / 86400000;
			return res;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return 0;
	}

	public static double[] getEffectiveDaysForOneYear(Long borrowerID,
			String year) {
		try {
			Date jan = LocaleManager.getInstance().getMonthDate("01-" + year);
			Date jan2 = LocaleManager.getInstance()
					.getMonthDate(
							"01-"
									+ String.valueOf(Integer.valueOf(year)
											.intValue() + 1)); // ?????
			Date feb = LocaleManager.getInstance().getMonthDate("02-" + year);
			Date mar = LocaleManager.getInstance().getMonthDate("03-" + year);
			Date avr = LocaleManager.getInstance().getMonthDate("04-" + year);
			Date may = LocaleManager.getInstance().getMonthDate("05-" + year);
			Date jun = LocaleManager.getInstance().getMonthDate("06-" + year);
			Date jul = LocaleManager.getInstance().getMonthDate("07-" + year);
			Date aug = LocaleManager.getInstance().getMonthDate("08-" + year);
			Date sep = LocaleManager.getInstance().getMonthDate("09-" + year);
			Date oct = LocaleManager.getInstance().getMonthDate("10-" + year);
			Date nov = LocaleManager.getInstance().getMonthDate("11-" + year);
			Date dec = LocaleManager.getInstance().getMonthDate("12-" + year);

			double[] res = new double[12];
			res[0] = getEffectiveDaysBetweenDates(jan, feb, borrowerID);
			res[1] = getEffectiveDaysBetweenDates(feb, mar, borrowerID);
			res[2] = getEffectiveDaysBetweenDates(mar, avr, borrowerID);
			res[3] = getEffectiveDaysBetweenDates(avr, may, borrowerID);
			res[4] = getEffectiveDaysBetweenDates(may, jun, borrowerID);
			res[5] = getEffectiveDaysBetweenDates(jun, jul, borrowerID);
			res[6] = getEffectiveDaysBetweenDates(jul, aug, borrowerID);
			res[7] = getEffectiveDaysBetweenDates(aug, sep, borrowerID);
			res[8] = getEffectiveDaysBetweenDates(sep, oct, borrowerID);
			res[9] = getEffectiveDaysBetweenDates(oct, nov, borrowerID);
			res[10] = getEffectiveDaysBetweenDates(nov, dec, borrowerID);
			res[11] = getEffectiveDaysBetweenDates(dec, jan2, borrowerID);

			return res;

		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	/**
	 * Returns the number of effective days between the date of beginning and
	 * date of end of the borrow
	 * 
	 * @param start
	 *            , stop, borrowerID
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static double getEffectiveDaysBetweenDates(Date start, Date stop,
			Long borrowerID) {
		try {
			String sQuery = "from Loan l where l.borrower.id=:id and l.end is not null and l.begin < :stop";
			List<Loan> loans = (List<Loan>) HibernateUtil.currentSession()
					.createQuery(sQuery).setParameter("id", borrowerID)
					.setParameter("stop", stop).list();
			long startTime = start.getTime();
			long stopTime = stop.getTime();
			long longRes = 0;
			for (int i = 0; i < loans.size(); i++) {
				Loan loan = (Loan) loans.get(i);
				long endTime = loan.getEnd().getTime();

				long endRes;
				if (endTime > startTime) {
					if (endTime > stopTime) {
						endRes = stopTime;
					} else {
						endRes = endTime;
					}
					longRes += endRes - startTime;
				}
			}
			double res = longRes / 86400000;
			System.out.println("Between " + start + " and " + stop + " : "
					+ res);
			return res;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return 0;
	}

	@SuppressWarnings("unchecked")
	public static List<Borrower> findBorrowers() {
		try {
			String sQuery = "from Borrower b order by b.name, b.firstName";
			List<Borrower> borrowers = (List<Borrower>) HibernateUtil
					.currentSession().createQuery(sQuery).list();
			return borrowers;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	public static List<Loan> findLoansByAlbum(Long id) {
		String query = "from Loan l where l.album.id=:id";
		List<Loan> res = new ArrayList<Loan>();
		try {
			res = (List<Loan>) HibernateUtil.currentSession()
					.createQuery(query).setParameter("id", id).list();
			return res;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return res;
	}

	public static void removeBorrowerGuiObject(BorrowerGuiObject bgo) {
		try {
			Borrower borrower = (Borrower) borrowerDAO.findByPrimaryKey(bgo
					.getId());
			HibernateUtil.beginTransaction();
			borrowerDAO.delete(borrower);
			HibernateUtil.commitTransaction();
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
	}

	@SuppressWarnings("unchecked")
	public static boolean isBorrowerInLate(Long id) {
		try {
			String sQuery = "from Loan l where l.borrower.id=:id";
			List<Loan> loans = (List<Loan>) HibernateUtil.currentSession()
					.createQuery(sQuery).setParameter("id", id).list();
			for (int i = 0; i < loans.size(); i++) {
				Loan loan = loans.get(i);
				Long limitTime = loan.getBegin().getTime()
						+ (86400000 * loan.getMaxDays());
				Date limitDate = new Date(limitTime);
				if (new Date().after(limitDate)) {
					return true;
				}

			}
			return false;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return false;
	}

	@SuppressWarnings("unchecked")
	public static List<LoanAlbumGuiObject> findLoanAlbumGuiObjects() {
		try {
			String sQuery = "from Album a order by a.serie.name, a.tome.number, a.tome.revision asc";
			List<Album> albums = (List<Album>) HibernateUtil.currentSession()
					.createQuery(sQuery).list();
			List<LoanAlbumGuiObject> res = new ArrayList<LoanAlbumGuiObject>();
			for (int i = 0; i < albums.size(); i++) {
				Album album = (Album) albums.get(i);
				LoanAlbumGuiObject lago = new LoanAlbumGuiObject();
				lago.setAlbumID(album.getId());
				lago.setImageURL(album.getCover());
				String label = "";
				if (album.getTome() != null) {
					label += album.getTome().getNumber();
					if (album.getTome().getRevision() != null
							&& !album.getTome().getRevision().equals("")) {
						label += "." + album.getTome().getRevision();
					}
				}
				lago.setLabel(label + ". " + album.getTitle());
				lago.setSerieName(album.getSerie().getName());
				sQuery = "from Loan l where l.album.id=:id and l.end is not null";
				List<Loan> loans = (List<Loan>) HibernateUtil.currentSession()
						.createQuery(sQuery).setParameter("id", album.getId())
						.list();
				if (loans.size() == 1) {
					Loan loan = (Loan) loans.get(0);
					lago.setBorrowerLabel(loan.getBorrower().getName());
					lago.setLoanDate(loan.getBegin());
					lago.setLoaned(true);
					lago.setLoanID(loan.getId());
					lago.setPenaltyByDay(loan.getPenaltyByDay());
					lago.setPriceByDay(loan.getPriceByDay());
					lago.setMaxDays(loan.getMaxDays());
				}
				res.add(lago);
			}
			return res;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	public static List<LoanAlbumGuiObject> findLoanAlbumGuiObjects(
			Long borrowerID, boolean getClosed) {
		try {

			StringBuffer sQuery = new StringBuffer(
					"from Loan l where l.borrower.id=:id");
			if (!getClosed) {
				sQuery.append(" and l.end is null");
			}
			sQuery.append(" order by l.begin asc");
			List<Loan> loans = HibernateUtil.currentSession()
					.createQuery(sQuery.toString())
					.setParameter("id", borrowerID).list();
			List<LoanAlbumGuiObject> res = new ArrayList<LoanAlbumGuiObject>();
			for (int i = 0; i < loans.size(); i++) {
				LoanAlbumGuiObject lago = new LoanAlbumGuiObject();
				Loan loan = (Loan) loans.get(i);
				lago.setBorrowerLabel(loan.getBorrower().getName());
				lago.setLoanDate(loan.getBegin());
				lago.setLoaned(true);
				lago.setLoanID(loan.getId());
				lago.setPenaltyByDay(loan.getPenaltyByDay());
				lago.setPriceByDay(loan.getPriceByDay());
				lago.setMaxDays(loan.getMaxDays());

				Album album = loan.getAlbum();

				lago.setAlbumID(album.getId());
				lago.setImageURL(album.getCover());
				String label = "";
				if (album.getTome() != null) {
					label += album.getTome().getNumber();
					if (album.getTome().getRevision() != null
							&& !album.getTome().getRevision().equals("")) {
						label += "." + album.getTome().getRevision();
					}
				}
				lago.setLabel(label + ". " + album.getTitle());
				lago.setSerieName(album.getSerie().getName());
				res.add(lago);
			}
			return res;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	public static List<LoanAlbumGuiObject> findLoanAlbumGuiObjects(
			Long[] albumIDsToExcept) {
		try {
			StringBuffer sQuery = new StringBuffer(
					"select alb from Loan l right outer join l.album alb where (l is null or l.end is not null) ");
			if (albumIDsToExcept.length > 0) {
				sQuery.append("and alb.id not in (");
				for (int i = 0; i < albumIDsToExcept.length; i++) {
					if (i != 0) {
						sQuery.append(", ");
					}
					sQuery.append(":id" + i);
				}
				sQuery.append(")");
			}
			sQuery.append(" order by alb.serie.name, alb.tome.number, alb.tome.revision asc");
			System.out.println(sQuery);
			Query query = HibernateUtil.currentSession().createQuery(
					sQuery.toString());
			for (int i = 0; i < albumIDsToExcept.length; i++) {
				query.setParameter("id" + i, albumIDsToExcept[i]);
			}

			List<Album> albums = (List<Album>) query.list();
			List<LoanAlbumGuiObject> res = new ArrayList<LoanAlbumGuiObject>();
			for (int i = 0; i < albums.size(); i++) {
				Album album = (Album) albums.get(i);
				LoanAlbumGuiObject lago = new LoanAlbumGuiObject();
				lago.setAlbumID(album.getId());
				lago.setImageURL(album.getCover());
				String label = "";
				if (album.getTome() != null) {
					label += album.getTome().getNumber();
					if (album.getTome().getRevision() != null
							&& !album.getTome().getRevision().equals("")) {
						label += "." + album.getTome().getRevision();
					}
				}
				lago.setLabel(label + ". " + album.getTitle());
				lago.setSerieName(album.getSerie().getName());
				lago.setLoaned(false);
				res.add(lago);
			}
			return res;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	public static Borrower findBorrowerByID(Long id) {
		try {
			Borrower borrower = (Borrower) borrowerDAO.findByPrimaryKey(id);
			return borrower;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
		return null;
	}

	/**
	 * Look at the database metadata and return the table names
	 * 
	 * @return List<String> of the different table names
	 */
	public static ArrayList<String> getTableNames() throws SQLException {
		ArrayList<String> l = new ArrayList<String>();
		Connection connecDB = HibernateUtil.currentSession().connection();
		if (connecDB != null) {
			String[] filter = new String[1];
			filter[0] = "TABLE";
			DatabaseMetaData meta = connecDB.getMetaData();
			ResultSet rs = meta.getTables(null, null, null, filter);

			while (rs.next())
				l.add(rs.getString(FIELD_NAME));
		}
		connecDB.close();
		return l;
	}

	public static void removeAuthor(Author au) {
		if (au.getId() == null)
			throw new IllegalArgumentException("The author must be persistent.");

		au = PersistencyManager.findAuthorByID(au.getId());
		ArrayList<Work> worksToDelete = findWorksByAuthorID(au.getId());
		try {
			HibernateUtil.beginTransaction();
			HibernateUtil.currentSession().flush();
			for (Work w : worksToDelete) {
				logger.debug("Delete Work : " + w.getId() + " ( "
						+ w.getAlbum() + " - " + w.getAuthor() + ")");
				workDAO.delete(w);
			}
			logger.debug("Delete Author : " + au.getId() + " ( " + au + " )");
			authorDAO.delete(au);
			HibernateUtil.commitTransaction();
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}
	}

	private static ArrayList<Work> findWorksByAuthorID(Long id) {
		try {
			String query = "from Work where author.id=:id";
			@SuppressWarnings("unchecked")
			ArrayList<Work> works = (ArrayList<Work>) HibernateUtil
					.currentSession().createQuery(query).setParameter("id", id)
					.list();
			return works;
		} catch (Exception e) {
			try {
				HibernateUtil.rollbackTransaction();
			} catch (PersistencyException e1) {
				e1.manageException();
			}
			PersistencyException pe = new PersistencyException(e,
					PersistencyException.CONTINUE);
			pe.manageException();
		}

		return null;
	}
}
