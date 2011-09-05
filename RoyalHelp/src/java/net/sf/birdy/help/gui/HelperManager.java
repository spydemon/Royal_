package net.sf.birdy.help.gui;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

import net.sf.birdy.help.core.locale.LocaleManager;
import net.sf.birdy.help.core.lucene.Listable;
import net.sf.birdy.help.core.lucene.TreeIndexer;
import net.sf.birdy.help.core.lucene.TreeSearcher;
import net.sf.birdy.help.core.model.Help;
import net.sf.birdy.help.core.model.Model;
import net.sf.birdy.help.core.model.ModelImpl;

public class HelperManager 
{
    private URL BASE_IMG = null;

    private static HelperManager instance = new HelperManager();

    private MainPanel mainPanel;
    private Help help;
    
    private int DEPTH = 20;
    private List<ModelImpl> next = new ArrayList<ModelImpl>();
    private List<ModelImpl> previous = new ArrayList<ModelImpl>();
    private ModelImpl currentModel = null;

    private boolean indexDone;

    private String directory = null;

    /**
	 * Constructor for HelperManager
	 * In order to get an instance use the method getInstance
	 */
    private HelperManager(){
        BASE_IMG = this.getClass().getClassLoader().getResource("images");
    }
    
	/** 
	 * Set the Locale to be used in the help.<br/>
	 * Available locale are defined by constants in LocaleManager.
	 * @see net.sf.birdy.help.core.locale.LocaleManager
	 *
	 * @param locale The selected locale
	 */
    public void setLocale(String locale){
        LocaleManager.getInstance().setLocale(locale);
    }

	/**
	 *	Get an instance of HelperManager
	 *	@return An instance of HelperManager
	 */
    public static HelperManager getInstance() {
        return instance;
    }

    public MainPanel getMainPanel() {
        return mainPanel;
    }

    public void setMainPanel(MainPanel mainPanel) {
        this.mainPanel = mainPanel;
    }

    public Help getHelp() {
        return help;
    }

    public void setHelp(Help help) {
        this.help = help;
    }
    
    public void displayPage(ModelImpl model){
        try {
            //this.currentModel = model;
            this.mainPanel.getNext().setEnabled(!this.next.isEmpty());
            this.mainPanel.getPrevious().setEnabled(!this.previous.isEmpty());

            this.mainPanel.displayPage(model);
            
        } catch (IOException e) {
            //Do nothing
            e.printStackTrace();
        }
    }
    public void displayPage(ModelImpl model, boolean increment){
        if (increment){
            this.next.clear();
            if (currentModel != null){
                if (this.previous.size() >= DEPTH){
                    this.previous.remove(0);
                }
                this.previous.add(currentModel);
            }
            this.currentModel = model;
        }
        
        this.displayPage(model);
    }
    
    
    /**
     * Displays the next help page
     */
    public void displayNext(){
        if (!this.next.isEmpty()){
            ModelImpl toDisplay = (ModelImpl) this.next.get(this.next.size() - 1);
            this.next.remove(toDisplay);
            if (this.previous.size() >= DEPTH){
                this.previous.remove(0);
            }
            this.previous.add(this.currentModel);
            this.currentModel = toDisplay;
            System.out.println("Next : " + this.next);
            this.displayPage(toDisplay);
        }
    }
    
    
    /**
     * Displays the previous help page
     */
    public void displayPrevious(){
        if (!this.previous.isEmpty()){
            ModelImpl toDisplay = (ModelImpl) this.previous.get(this.previous.size() - 1);
            this.previous.remove(toDisplay);
            if (this.next.size() >= DEPTH){
                this.next.remove(0);
            }
            this.next.add(this.currentModel);
            this.currentModel = toDisplay;
            this.displayPage(toDisplay);
        }
    }
    
    public void displayHome(){
        this.displayPage(this.help, Boolean.TRUE.booleanValue());
    }
    
    public void gotoLink(String link){
        String[] html = link.split("/");
        String keyhtml = html[html.length - 1];
        String key = keyhtml.substring(0, keyhtml.length() - 5);
        Model model = this.help.findModel(key);
        this.displayPage((ModelImpl) model, Boolean.TRUE.booleanValue());
    }
    
    /**
     * Searches in the help for the specified word
     * @param word The word to find
     * @return The list of sections of the help matching the word
     */
    public List<Listable> searchForWord(String word){
        this.indexFiles();
        Map<String, Listable> map = TreeSearcher.search(word, this.directory);
        List<Listable> values = new ArrayList<Listable>();
        for(Object o : map.values())
        {
        	System.out.println("Listable" + o);
        	values.add((Listable)o);
        }
        
        Collections.sort(values, new Comparator<Listable>(){

            public int compare(Listable o1, Listable o2) {
                Listable l1 = (Listable) o1;
                Listable l2 = (Listable) o2;
                return 1- (Integer.parseInt(l1.getScore()) - Integer.parseInt(l2.getScore()));
            }
            
        });
        System.out.println(values);
        return values;
    }
    
    private void indexFiles(){
        if (!indexDone){
            try {
                TreeIndexer.index(this.help, this.directory);
                this.indexDone = Boolean.TRUE.booleanValue();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
    
    public void highlightWords(String[] words){
        for(int i=0;i<words.length;i++){
            TextUtil.getInstance().highlight(this.mainPanel.getPage(), words[i]);
        }
    }
    
    public void cleanHighlight(){
        TextUtil.removeHighlights(this.mainPanel.getPage());
    }
    
    public Model getModel(Listable listable){
        return this.help.findModel(listable.getKey());
    }

    /**
     * Get the help directory
     * @return The directory
     */
    public String getDirectory() {
        return directory;
    }

    /**
     * Set the help directory
     * @param directory The help directory
     */
    public void setDirectory(String directory) {
        this.directory = directory;
    }
    
    public URL getImageBase(){
        return BASE_IMG;
    }
}
