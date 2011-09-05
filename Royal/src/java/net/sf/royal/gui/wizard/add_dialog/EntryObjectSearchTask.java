package net.sf.royal.gui.wizard.add_dialog;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.RejectedExecutionException;

import javax.swing.DefaultComboBoxModel;
import javax.swing.SwingWorker;



/**
 * Searching thread that publish the EntryPaneObjects fitting a prefix.<br/>
 * Usage : <br/>
 * <ul>
 * <li>Create a new instance with a list of EntryPaneObject and a prefix</li>
 * <li>Launch the research thanks to #start()</li>
 * <li>If you want to stop the research, use {@link #interrupt()}, which will stop the process
 * as soon as possible.</li>
 * <li>Each EntryPaneObject matching the prefix will be published.<br/>
 *  This action will invoke all PropertiesChangeListener added to this instance.</li>
 * <li>At the end of the process, the result will be published and be available through the method
 * {@link #get()}.</li>
 * </ul>
 * You can also use {@link #setRessource(List)} and {@link #setPrefix(String)} to change the 
 * data of the EntryObjectSearcher. BUT you can only do it when no research is being done.<br/>
 * You can use {@link #isWorking()} to know if a work is being done.
 * @author Steven Nguyen
 *
 */
public class EntryObjectSearchTask extends SwingWorker<List<EntryPaneObject>, EntryPaneObject>
{
// FIELDS
	/**
	 * Initial list of EntryPaneObject 
	 */
	private List<EntryPaneObject> initList;
	
	/**
	 * If true, the ObjectSearcher is not working
	 */
	private boolean working = false;
	
	/**
	 * The prefix
	 */
	private String prefix;
	
	/**
	 * The combobox completed by the EntryObjectSearcTask
	 */
	private DefaultComboBoxModel cbm;
	
	private JEntryDocument doc;
	
// CONSTRUCTOR
	/**
	 * Create a new ObjectSearcher with the list of elements and
	 * the prefix to test.
	 * @param ressource The list of element to test
	 * @param prefix The prefix to find in the elements
	 */
	public EntryObjectSearchTask(List<EntryPaneObject> ressource, 
								 String prefix, 
								 DefaultComboBoxModel cbm,
								 JEntryDocument  doc)
	{
		super();
		this.initList = ressource;
		this.prefix = prefix;
		this.working = false;
		this.cbm= cbm;
		this.doc = doc;
	}
	
	@Override
	protected List<EntryPaneObject> doInBackground() throws Exception {
		List<EntryPaneObject> res = new ArrayList<EntryPaneObject>();
		String[] tmp; /* array of String of the EntryPaneObject to use 
		               * for research */
		int i = 0; /* for the loop in tmp */
		boolean ok = false; /* for stopping the loop in tmp */
		//System.out.println("doInBackground en cours");
		//System.out.println("Il y a " + this.initList.size() + " élements dans la liste");
		//System.out.println("On utilise "+ this.prefix + " comme préfix");
		for(EntryPaneObject epo : this.initList)
		{
			
			if(this.working)
			{
				ok = false;
				tmp = epo.getName();
				for(i=0; i<tmp.length && !ok; i++)
				{
					//System.out.println("test de : \"" + tmp[i] + "\"");
					if(tmp[i].toLowerCase().startsWith(this.prefix.toLowerCase()))
					{
						this.publish(epo);
						res.add(epo);
						ok = true;
					}//if
				}//for
			}//if
		}//for
		this.working = false;
		//this.doc.setOK(true);
		return res;
	}
	
	@Override
	protected void process(List<EntryPaneObject> list)
	{
		//System.out.println("On a process un nouvel élement");
		//System.out.println("Il y a " + list.size() + " éléments processed");
		for(EntryPaneObject e : list)
		{
			this.cbm.addElement(e);
			//System.out.println("Element : " + e.toString());
		}
		//this.doc.setOK(true);
	}
	
	@Override
	protected void done()
	{
		this.doc.setOK();
	}
	
	/**
	 * Start the work
	 */
	public void start(DefaultComboBoxModel cbm)
	{
		//System.out.println("On start une nouvelle tache de recherche");
		this.cbm = cbm;
		this.working = true;
		this.execute();
	}
	
	/**
	 * Get the state of the Searcher
	 * @return true if a research is being done
	 */
	public boolean isWorking()
	{
		return this.working;
	}
	
	/**
	 * Interrupt the work as soon as possible
	 */
	public void interrupt()
	{
		this.working = false;
	}
	
	/**
	 * Set the list of ressource used for research.
	 * The thread must be not working
	 * @param ressource The new list of ressource
	 * @see #isWorking()
	 * @see #interrupt()
	 */
	public void setRessource(List<EntryPaneObject> ressource)
	{
		if(this.working)
			throw new RejectedExecutionException("Changing ressource while" +
			"the research is being done");
		else
		{
			this.initList = ressource;
		}
	}
	
	/**
	 * Change the prefix. Can be done only when no work is being done.
	 * @param prefix The new prefix
	 * @see #isWorking()
	 * @see #interrupt()
	 */
	public void setPrefix(String prefix)
	{
		if(this.working)
			throw new RejectedExecutionException("Changing prefix while" +
					"the research is being done");
		else
		{
			this.prefix = prefix;
		}
	}
}