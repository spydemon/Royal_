package net.sf.royal.gui.tree;

import java.awt.Color;
import java.awt.Component;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeCellRenderer;

import net.sf.royal.datamodel.Album;
import net.sf.royal.datamodel.Author;
import net.sf.royal.datamodel.Serie;
import net.sf.royal.datamodel.Type;
import net.sf.royal.gui.manager.IconManager;

public class AlbumCellRenderer implements TreeCellRenderer 
{	
	
	private JLabel nameLabel;

	private JLabel iconLabel;
	
	private JPanel renderer;

	private DefaultTreeCellRenderer defaultRenderer = new DefaultTreeCellRenderer();

	private Color backgroundSelectionColor;

	private Color backgroundNonSelectionColor;

	public AlbumCellRenderer() 
	{
	    renderer = new JPanel();
	    nameLabel = new JLabel();
	    iconLabel = new JLabel();
	    renderer.add(iconLabel);
	    renderer.add(nameLabel);
	    backgroundSelectionColor = defaultRenderer
	        .getBackgroundSelectionColor();
	    backgroundNonSelectionColor = defaultRenderer
	        .getBackgroundNonSelectionColor();
	}

	public Component getTreeCellRendererComponent(JTree tree, Object value,
	    boolean selected, boolean expanded, boolean leaf, int row,
	    boolean hasFocus) 
	{
	    Component returnValue = null;
	    if ((value != null) && (value instanceof DefaultMutableTreeNode)) 
	    {
	        Object userObject = ((DefaultMutableTreeNode) value)
	            .getUserObject();
	        if (userObject instanceof Album) 
	        {
	        	Album album = (Album)userObject;
	        	iconLabel.setIcon(IconManager.getIcon("album.png"));
	    		if(album.getTome() != null && album.getTome().getNumber() != 0)
	    		{
	    			nameLabel.setText("(" + album.getTome().getNumber() + ") " + album.getTitle());
	    		}
	    		else
	    		{
	    			nameLabel.setText(album.getTitle());
	    		}
	        }
	        else if(userObject instanceof Serie)
	        {
	        	Serie serie = (Serie)userObject;
	        	iconLabel.setIcon(IconManager.getIcon("serie.png"));
	        	nameLabel.setText(serie.getName());
	        }
	        else if(userObject instanceof Type)
	        {
	        	Type t = (Type)userObject;
	        	iconLabel.setIcon(IconManager.getIcon("type.png"));
	        	nameLabel.setText(t.getName());	
	        }
	        else if(userObject instanceof Author)
	        {
	        	boolean closeParenthesis = false;
	        	boolean hasNickname = false;
	        	boolean hasName = false;
	        	Author author = (Author)userObject;
	        	StringBuilder strAuthor = new StringBuilder();
	        	iconLabel.setIcon(IconManager.getIcon("author.png"));
	        	
	        	String nickname = author.getNickName();
	        	String firstname = author.getFirstName();
	        	String name = author.getName();
	        	if(nickname != null && !nickname.isEmpty())
	        	{
	        		strAuthor.append(nickname);
	        		hasNickname = true;
	        	}
	        	if(name != null && !name.isEmpty())
	        	{	
	        		if(hasNickname)
	        		{
	        			strAuthor.append(" (");
		        		closeParenthesis = true;
	        		}
	        		strAuthor.append(name);
	        		hasName = true;
	        	}
	        	if(firstname != null && !firstname.isEmpty())
	        	{
	        		if(closeParenthesis || hasName)
	        		{
	        			strAuthor.append(" ");
	        		}
	        		else if(hasNickname)
	        		{
	        			strAuthor.append(" (");
	        			closeParenthesis = true;
	        		}
	        		strAuthor.append(firstname);
	        	}
	        	if(closeParenthesis)
	        	{
	        		strAuthor.append(")");
	        	}
	        	nameLabel.setText(strAuthor.toString());
	        }
	        else
	        {
	        	iconLabel.setIcon(IconManager.getIcon("help.gif"));
	        	nameLabel.setText(userObject.toString());
	        }
	        renderer.setEnabled(tree.isEnabled());
	        returnValue = renderer;
        	if (selected) 
        	{
        		renderer.setBackground(backgroundSelectionColor);
        	} 
        	else 
        	{
        		renderer.setBackground(backgroundNonSelectionColor);
        	}
	    }
	    if (returnValue == null) 
	    {
	    	returnValue = defaultRenderer.getTreeCellRendererComponent(tree,
	            value, selected, expanded, leaf, row, hasFocus);
	    }
	    return returnValue;
	}
}
