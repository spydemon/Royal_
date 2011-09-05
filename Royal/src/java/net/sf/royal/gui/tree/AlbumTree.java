package net.sf.royal.gui.tree;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

import javax.swing.JPopupMenu;
import javax.swing.JTree;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

import net.sf.royal.datamodel.Album;
import net.sf.royal.datamodel.Author;
import net.sf.royal.datamodel.Serie;
import net.sf.royal.datamodel.Type;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.ShortcutManager;
import net.sf.royal.gui.pane.MainPane;
import net.sf.royal.gui.pane.MainToolBar;
import net.sf.royal.persistency.PersistencyManager;

/**
 * @author Soulou
 * Class creating a Tree with albums data
 */
public class AlbumTree extends JTree
{
	/* Constants */
	public static final int SORT_ALBUM = 0x00000000;
	public static final int SORT_SERIE = 0x00000001;
	public static final int SORT_AUTHOR = 0x0000010;
	public static final int SORT_TYPE = 0x00000100;

	/* Fields */
	private static AlbumTree instance;
	
	private DefaultMutableTreeNode nRoot;
	private TreeModel tmModel;

	private List<Serie> listSerie = null;
	private List<Album> listAlbum = null;
	private List<Author> listAuthor = null;
	private List<Type> listType = null;

	private int currentFlags;
	
	/* Constructors */
	private AlbumTree()
	{
		this.setRootVisible(false);
		this.setCellRenderer(new AlbumCellRenderer());
		
		this.initListeners();
	}
	
	/**
	 * Creation of the different listeners apply to the AlbumTree;
	 */
	public void initListeners()
	{
		this.addTreeSelectionListener(new TreeSelectionListener() 
		{
			public void valueChanged(TreeSelectionEvent e) 
			{
				DefaultMutableTreeNode node = (DefaultMutableTreeNode)
				       AlbumTree.this.getLastSelectedPathComponent();
				if(node == null)
				{
					MainToolBar.getInstance().activeTools(null,false);
				}
				else
				{
					MainPane.getInstance().updateInfoPane(node.getUserObject());
					MainToolBar.getInstance().activeTools(node.getUserObject(),true);
				}
			}	
		});
		
		this.addKeyListener(new KeyListener() 
		{
			@Override
			public void keyTyped(KeyEvent ke) {}	
			@Override
			public void keyReleased(KeyEvent ke) {}
			@Override
			public void keyPressed(KeyEvent ke) 
			{
				// If the delete key has been pressed
				if(ke.getKeyChar() == ShortcutManager.DELETE_)
				{
					DefaultMutableTreeNode nodeToDel = 
						(DefaultMutableTreeNode)AlbumTree.this.getLastSelectedPathComponent();
					DeleteActionListener dal = new DeleteActionListener(nodeToDel.getUserObject());
					dal.actionPerformed(new ActionEvent(nodeToDel,0,"Delete"));
				}
			}
		});
		
		this.addMouseListener(new MouseAdapter() 
		{	
			public void showContextualMenu(MouseEvent e)
			{
				int x = e.getX();
				int y = e.getY();
				JTree tree = (JTree)e.getSource();
				
				TreePath path = tree.getPathForLocation(x, y);
                if (path == null)
                        return; 
                tree.setSelectionPath(path);
                DefaultMutableTreeNode node = (DefaultMutableTreeNode)path.getLastPathComponent();
                
                JPopupMenu menu = null;
                if(node.getUserObject() instanceof Album)
                {
                	Album a =(Album)node.getUserObject();
                	menu = new AlbumMenu(a);
                }
                else if(node.getUserObject() instanceof Serie)
                {
                	Serie s = (Serie)node.getUserObject();
                	menu = new SerieMenu(s);
                }
                else if(node.getUserObject() instanceof Author)
                {
                	Author au = (Author)node.getUserObject();
                	menu = new AuthorMenu(au);
                }
                else if(node.getUserObject() instanceof Type)
                {
                	Type t = (Type)node.getUserObject();
                	menu = new TypeMenu(t);
                }
				menu.show(tree, x, y);
			}
			
			
			@Override
			public void mousePressed(MouseEvent e)
			{
				if(e.getButton() == MouseEvent.BUTTON3)
				{
					if(e.isPopupTrigger())
					{
						showContextualMenu(e);
					}
				}
			}
		});
		
	}

	/**
	 * Modify the node structure de sort the different albums.
	 * @param flags Flags using to define the sort type
	 * @see AlbumTree#SORT_ALBUM
	 * @see AlbumTree#SORT_AUTHOR
	 * @see AlbumTree#SORT_SERIE
	 * @see AlbumTree#SORT_TYPE
	 */
	public void sortAlbums(int flags)
	{
		this.currentFlags = flags;
		this.nRoot = new DefaultMutableTreeNode(LocaleManager.getInstance().getString("collection"));

		DefaultMutableTreeNode tmpAlbum, tmpNode;

		if(this.currentFlags == SORT_ALBUM) 
		{
			this.listAlbum = PersistencyManager.findAlbums();
			for(Album album : this.listAlbum) 
			{
				tmpAlbum = new DefaultMutableTreeNode(album);
				this.nRoot.add(tmpAlbum);
			}
		}
		else if(this.currentFlags == SORT_SERIE) 
		{
			/* Albums without serie */
			tmpNode = nullSerieNode();
			if(!tmpNode.isLeaf())
				this.nRoot.add(tmpNode);
			
			/* The others */
			this.listSerie = PersistencyManager.findSeries();
			for(Serie serie : this.listSerie)
				this.nRoot.add(serieNode(serie));
		}
		else if(this.currentFlags == SORT_AUTHOR) 
		{
			/* Albums without author */
			tmpNode = nullAuthorNode();
			if(!tmpNode.isLeaf())
				this.nRoot.add(tmpNode);
			
			this.listAuthor = PersistencyManager.findAuthors();
			for(Author author : this.listAuthor)
				this.nRoot.add(authorNode(author));
		}
		else if(this.currentFlags == SORT_TYPE) 
		{
			/* Albums without type */
			tmpNode = nullTypeNode();
			if(!tmpNode.isLeaf())
				this.nRoot.add(tmpNode);
			
			/* Album with type */
			this.listType = PersistencyManager.findTypes();
			for(Type type : this.listType)
				this.nRoot.add(this.typeNode(type));
		}
		else if(this.currentFlags == (SORT_TYPE | SORT_SERIE)) 
		{
			/* Albums without type */
			tmpNode = nullTypeSerieNode();
			if(!tmpNode.isLeaf())
				this.nRoot.add(tmpNode);
			
			this.listType = PersistencyManager.findTypes();
			for(Type type : this.listType)
				this.nRoot.add(this.typeSerieNode(type));
		}
		else if(this.currentFlags == (SORT_AUTHOR | SORT_SERIE)) 
		{
			this.listAuthor = PersistencyManager.findAuthors();
			for(Author author : this.listAuthor)
				this.nRoot.add(authorSerieNode(author));
		}

		this.tmModel = new DefaultTreeModel(this.nRoot);
		this.setLargeModel(true);
		this.setModel(this.tmModel);
	}

	/**
	 * Give back a serie node with all its album child nodes
	 * @return
	 */
	private DefaultMutableTreeNode serieNode(Serie s)
	{
		DefaultMutableTreeNode tmpSerie = new DefaultMutableTreeNode(s);
		// We use HQL queries in order to have the albums order by tome number
		List<Album> albums = PersistencyManager.findAlbumsBySerieID(s.getId());
		for(Album albumToAdd : albums)
			tmpSerie.add(new DefaultMutableTreeNode(albumToAdd));
		return tmpSerie;
	}

	/**
	 * @return an author node with its Albums
	 */
	private DefaultMutableTreeNode authorNode(Author a)
	{
		DefaultMutableTreeNode tmpAuthor = new DefaultMutableTreeNode(a);
		
		this.listAlbum = PersistencyManager.findAlbumsByAuthorID(a.getId());
		for(Album album : this.listAlbum)
			tmpAuthor.add(new DefaultMutableTreeNode(album));
		return tmpAuthor;
	}

	/**
	 * @return an author node with its series
	 */
	private DefaultMutableTreeNode authorSerieNode(Author a)
	{
		DefaultMutableTreeNode tmpAuthor = new DefaultMutableTreeNode(a);
		
		this.listSerie = PersistencyManager.findSeriesByAuthorID(a.getId());
		for(Serie serie : this.listSerie)
			tmpAuthor.add(serieNode(serie));
		return tmpAuthor;
	}

	/**
	  * @return a Type-Album Node
	  */
	private DefaultMutableTreeNode typeNode(Type t)
	{
		DefaultMutableTreeNode tmpType = new DefaultMutableTreeNode(t);
		for(Serie serie : (Iterable<Serie>)t.getSeries())
			for(Album album : (Iterable<Album>)serie.getAlbums())
				tmpType.add(new DefaultMutableTreeNode(album));
		return tmpType;
	}

	/**
	  * @return a Type-Album Node
	  */
	private DefaultMutableTreeNode typeSerieNode(Type t)
	{
		DefaultMutableTreeNode tmpType = new DefaultMutableTreeNode(t);
		for(Serie serie : (Iterable<Serie>)t.getSeries())
			tmpType.add(serieNode(serie));
		return tmpType;
	}
	
	/**
	 * @return the node containing all albums without Author
	 */
	private DefaultMutableTreeNode nullAuthorNode()
	{
		DefaultMutableTreeNode nullAuthorNode = new DefaultMutableTreeNode(LocaleManager.getInstance().getString("no_author"));
		this.listAlbum = PersistencyManager.findAlbumsAuthorNull();
		for(Album album : this.listAlbum)
			nullAuthorNode.add(new DefaultMutableTreeNode(album));
		return nullAuthorNode;	
	}
	
	/**
	 * @return the node containing all albums without Serie
	 */
	private DefaultMutableTreeNode nullSerieNode()
	{
		DefaultMutableTreeNode nullSerieNode = new DefaultMutableTreeNode(LocaleManager.getInstance().getString("no_serie"));
		this.listAlbum = PersistencyManager.findAlbumsSerieNull();
		for(Album album : this.listAlbum)
			nullSerieNode.add(new DefaultMutableTreeNode(album));
		return nullSerieNode;
	}
	
	/**
	 * @return the node containing all albums without Type;
	 */
	private DefaultMutableTreeNode nullTypeNode()
	{
		DefaultMutableTreeNode nullTypeNode = new DefaultMutableTreeNode(LocaleManager.getInstance().getString("no_type"));
		this.listAlbum = PersistencyManager.findAlbumsTypeNull();
		for(Album album : this.listAlbum)
			nullTypeNode.add(new DefaultMutableTreeNode(album));
		return nullTypeNode;
	}
	
	/**
	 * @return the node containing all series without Type;
	 */
	private DefaultMutableTreeNode nullTypeSerieNode()
	{
		DefaultMutableTreeNode nullTypeNode = new DefaultMutableTreeNode(LocaleManager.getInstance().getString("no_type"));
		this.listSerie = PersistencyManager.findSeriesTypeNull();
		for(Serie serie : this.listSerie)
			nullTypeNode.add(new DefaultMutableTreeNode(serie));
		return nullTypeNode;
	}
	
	/**
	 * @return the static instance of the class
	 * Initialize if it wasn't done before
	 */
	public static AlbumTree getInstance()
	{
		if(instance == null)
			instance = new AlbumTree();
		return instance;
	}
	
	/**
	 * @return the root of the tree
	 */
	public static DefaultMutableTreeNode getRoot()
	{
		return instance.nRoot;
	}
	
	public TreeModel doBackupTreeModel()
	{
		DefaultMutableTreeNode root = (DefaultMutableTreeNode)this.getModel().getRoot();
		DefaultMutableTreeNode newroot = cloneNodes(root);
		DefaultTreeModel dtm = new DefaultTreeModel(newroot);
		return dtm;
	}
	
	private DefaultMutableTreeNode cloneNodes(DefaultMutableTreeNode root)
	{
		if(root.isLeaf())
		{
			return (DefaultMutableTreeNode)root.clone();
		}
		
		ArrayList<DefaultMutableTreeNode> listChildren = new ArrayList<DefaultMutableTreeNode>();
		DefaultMutableTreeNode newRoot = (DefaultMutableTreeNode)root.clone(); 
		for(Enumeration<?> e = root.children(); e.hasMoreElements();)
			listChildren.add((DefaultMutableTreeNode)e.nextElement());
		
		for(DefaultMutableTreeNode node : listChildren)
		{
			newRoot.add(cloneNodes(node));
		}
		
		return newRoot;
	}
	
	public int getCurrentFlags()
	{
		return this.currentFlags;
	}
}
