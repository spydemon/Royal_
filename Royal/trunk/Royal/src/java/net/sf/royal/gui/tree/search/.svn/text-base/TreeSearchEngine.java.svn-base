package net.sf.royal.gui.tree.search;

import java.util.ArrayList;
import java.util.Enumeration;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

import net.sf.royal.gui.tree.AlbumTree;

/**
 *  @author Soulou
 *  Main algorithm to research something in the AlbumTree 
 */
public class TreeSearchEngine
{
	/**
	 * New TreeModel, will replace the full one.
	 */
	public static DefaultTreeModel treeModel;
	/**
	 * Pattern to find
	 */
	public static String nameToSearch;
	/**
	 * List of the paths to expand after creating the new TreeModel.
	 */
	public static ArrayList<TreePath> pathsToExpand = new ArrayList<TreePath>();
	
	/**
	 * Search for a string in the Tree.
	 * @param name Pattern to find
	 */
	public static void search(String name)
	{
		/* Delete previous search */
		pathsToExpand.clear();
		/* Define new pattern */
		nameToSearch = name.toLowerCase();
		
		/* Create the root of our new TreeModel */
		DefaultMutableTreeNode newRoot = new DefaultMutableTreeNode("Collection");
		treeModel = new DefaultTreeModel(newRoot);
		
		/* Get the current root node */
		ArrayList<DefaultMutableTreeNode> listChildren = new ArrayList<DefaultMutableTreeNode>();
		DefaultMutableTreeNode oldRoot = (DefaultMutableTreeNode)AlbumTree.getInstance().getModel().getRoot();
		for(Enumeration<?> e = oldRoot.children(); e.hasMoreElements();)
			listChildren.add((DefaultMutableTreeNode)e.nextElement());
		
		/* Search in the current root node */
		SearchInNode(listChildren);
		
		/* After searching we set the model to our JTree
		 * Which will be displayed.
		 */
		AlbumTree.getInstance().setModel(treeModel);
		
		/* And we expand the different paths we have listed */
		for(TreePath tp : pathsToExpand)
		{
			// DEBUG - System.out.println("Expand : " + tp.toString());
			AlbumTree.getInstance().expandPath(tp);
		}
	}
		
	public static void SearchInNode(ArrayList<DefaultMutableTreeNode> listChildren)
	{		
		// DEBUG - System.out.println("Searching in : " + searchNode.getUserObject().toString());
		
		/* Temporary Nodes */
		DefaultMutableTreeNode parentClone = null;
		DefaultMutableTreeNode grandParent = null;
		DefaultMutableTreeNode grandParentClone = null;
		
		/* We browse all the child nodes the the node given by argument */
		for(DefaultMutableTreeNode tmpTn : listChildren)
		{
			// DEBUG -- System.out.println("Browsing : " + tmpTn.getUserObject().toString());
			
			/* If we find the pattern in his name */
			if(tmpTn.getUserObject().toString().toLowerCase().contains(nameToSearch))
			{
				DefaultMutableTreeNode parent = (DefaultMutableTreeNode)tmpTn.getParent();
				/* If it is a first level node, we juste add it */
				if(parent == AlbumTree.getInstance().getModel().getRoot())
				{
					// DEBUG -- System.out.println("Add : " + tmpTn.getUserObject().toString());
					((DefaultMutableTreeNode)treeModel.getRoot()).add(tmpTn);
				}
				/* For a second level node, we duplicate its parent
				 * and we notice it to expand in order to show at the
				 * user that the research match the child node
				 */
				else if (parent.getParent() == AlbumTree.getInstance().getModel().getRoot())
				{
					// DEBUG -- System.out.println("Because of : " + tmpTn.getUserObject().toString());
					// DEBUG -- System.out.println("Add the father node : " + parent.getUserObject().toString());
					
					/* We only create one parent node the next nodes will be added normally */
					if(parentClone == null)
					{
						parentClone = (DefaultMutableTreeNode)parent.clone();
						parentClone.removeAllChildren();
					}
					parentClone.add(tmpTn);
					((DefaultMutableTreeNode)treeModel.getRoot()).add(parentClone);
					
					TreePath tpToAdd = new TreePath(
									treeModel.getRoot()).pathByAddingChild(parentClone);
					if(pathsToExpand.indexOf(tpToAdd) < 0)
						pathsToExpand.add(tpToAdd);
				}
				/* Third level node, as the second one, but with the grand-father */
				else
				{
					grandParent = (DefaultMutableTreeNode)parent.getParent();
					// DEBUG -- System.out.println("Because of : " + tmpTn.getUserObject().toString());
					// DEBUG -- System.out.println("Add the father node : " + parent.getUserObject().toString());
					// DEBUG -- System.out.println("Add the grand-father node : " + grandParent.getUserObject().toString());
					
					if(grandParentClone == null)
					{
						grandParentClone = (DefaultMutableTreeNode)grandParent.clone();
						grandParentClone.removeAllChildren();
					}
					if(parentClone == null)
					{
						parentClone = (DefaultMutableTreeNode)parent.clone();
						parentClone.removeAllChildren();
					}
					
					grandParentClone.add(parentClone);
					parentClone.add(tmpTn);
					
					((DefaultMutableTreeNode)treeModel.getRoot()).add(grandParentClone);
					
					TreePath tpToAdd = new TreePath(
							treeModel.getRoot()).pathByAddingChild(grandParentClone)
							.pathByAddingChild(parentClone);
					if(pathsToExpand.indexOf(tpToAdd) < 0)
						pathsToExpand.add(tpToAdd);
				}
			}
			/* If the pattern doesn't match and if the node has child(ren)
			 * We look at it
			 */
			else if(!tmpTn.isLeaf())
			{
				ArrayList<DefaultMutableTreeNode> newListChildren = new ArrayList<DefaultMutableTreeNode>();
				for(Enumeration<?> e = tmpTn.children(); e.hasMoreElements();)
					newListChildren.add((DefaultMutableTreeNode)e.nextElement());
				SearchInNode(newListChildren);
			}
		}
	}
}
