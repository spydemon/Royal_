package net.sf.birdy.help.gui;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.net.URL;
import java.util.List;

import javax.swing.DefaultListModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkEvent.EventType;
import javax.swing.event.HyperlinkListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.StyleSheet;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import net.sf.birdy.help.core.locale.LocaleManager;
import net.sf.birdy.help.core.lucene.Listable;
import net.sf.birdy.help.core.model.ModelImpl;

public class MainPanel extends JPanel {

    public static final int SEARCH = 0;
    public static final int TREE = 1;
    
    private HelpTree tree;
    private JList list;
    private JScrollPane treePane;
    private JEditorPane page;
    
    private JButton previous;
    private JButton next;
    private JButton home;
    private JButton print;
    
    
    private JTextField searchField;
    private JLabel searchLabel;
    private JProgressBar bar;
    
    private int type = TREE;
    private boolean treeSelectionFlag = Boolean.FALSE.booleanValue();
    
    private boolean canHighlight = Boolean.FALSE.booleanValue();
    private String helpCss = "help.css";
    
    public MainPanel(){
        this.initLayout();
        this.initListener();
    }
    
    
	/**
	 * Set the listeners of the components of the MainPanel
	 */
    private void initListener() {
        this.tree.addTreeSelectionListener(new TreeSelectionListener(){

            public void valueChanged(TreeSelectionEvent e) {
                if (e.isAddedPath() && treeSelectionFlag){
                    ModelImpl model = (ModelImpl) tree.getLastSelectedPathComponent();
                    if (model != null){
                        HelperManager.getInstance().displayPage(model, Boolean.TRUE.booleanValue());
                    }
                }
                treeSelectionFlag = Boolean.TRUE.booleanValue();
            }
            
        });
        
        this.previous.addActionListener(new ActionListener(){

            public void actionPerformed(ActionEvent e) {
                disableHighlight();
                HelperManager.getInstance().displayPrevious();
            }
            
        });
        
        this.next.addActionListener(new ActionListener(){

            public void actionPerformed(ActionEvent e) {
                disableHighlight();
                HelperManager.getInstance().displayNext();
            }
            
        });
        
        this.home.addActionListener(new ActionListener(){

            public void actionPerformed(ActionEvent e) {
                if (type != TREE){
                    type = TREE;
                    updateLeftPane();
                }
                disableHighlight();
                HelperManager.getInstance().displayHome();
            }
            
        });
        
        this.print.addActionListener(new ActionListener(){

            public void actionPerformed(ActionEvent e) {
                //TODO
            }
            
        });
        
        page.addHyperlinkListener(new HyperlinkListener(){

            public void hyperlinkUpdate(HyperlinkEvent e) {
                if (e.getEventType() == EventType.ACTIVATED){
                    HelperManager.getInstance().gotoLink(e.getURL().getPath());
                }
            }
            
        });
        
        page.addPropertyChangeListener("page", new PropertyChangeListener(){

            public void propertyChange(PropertyChangeEvent evt) {
                if (canHighlight){
                    highlight();
                }
            }
            
        });
        
        searchField.addActionListener(new ActionListener(){

            public void actionPerformed(ActionEvent e) {
                if (!searchField.equals("")){
                    canHighlight = Boolean.TRUE.booleanValue();
                    type = SEARCH;
                    Search search = new Search(bar, searchField.getText());
                    search.search();
                }
                
            }
            
        });
        
        list.addListSelectionListener(new ListSelectionListener(){

            public void valueChanged(ListSelectionEvent e) {
                
                if (!e.getValueIsAdjusting() && !list.isSelectionEmpty()){
                    Listable listable = (Listable) list.getSelectedValue();
                    ModelImpl model = (ModelImpl) HelperManager.getInstance().getModel(listable);
                    HelperManager.getInstance().cleanHighlight();
                    HelperManager.getInstance().displayPage(model, true);
                }
            }
            
        });
    }

	
	/**
	 * Add components and set the layout of the MainPanel
	 */
    private void initLayout() 
    {	
        this.setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.insets = new Insets(5,5,5,5);
        
        this.list = new JList(new DefaultListModel());
        this.list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        
        this.tree = new HelpTree();
        this.tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        treePane = new JScrollPane(this.tree);
        treePane.setMinimumSize(new Dimension(200, 300));
        
        this.page = new JEditorPane();
        this.page.setContentType("text/html;charset=utf-8");
        this.page.setEditable(Boolean.FALSE.booleanValue());
        StyleSheet css = ((HTMLEditorKit) page.getEditorKit()).getStyleSheet();
        css.importStyleSheet(this.getClass().getClassLoader().getResource(helpCss));
        
        
        //Due to a SUN bug, ne to set the complete URL of background Image.
        StringBuffer chipRule = new StringBuffer();
        chipRule.append("li {font-family:         Verdana;");
        chipRule.append("font-size:           12pt;");
        chipRule.append("color:               black;");
        chipRule.append("padding-bottom:      5px;");
        chipRule.append("list-style-type:     none;");
        //chipRule.append("background-image:    url(" + HelperManager.getInstance().getImageBase() + "/chip.gif);");
        chipRule.append("background-repeat:   no-repeat;");
        chipRule.append("background-position: 0% 40%;");
        chipRule.append("padding-left:        15px;");
        chipRule.append("}");
        
        StringBuffer noteRule = new StringBuffer();
        noteRule.append(".note {text-indent:      0px;");
        noteRule.append("font-family:         Verdana;");
        noteRule.append("font-size:           12pt;");
        noteRule.append("color:               black;");
        //noteRule.append("background-image:    url(" + HelperManager.getInstance().getImageBase() + "/warning.gif);");
        noteRule.append("background-repeat:   no-repeat;");
        noteRule.append("background-position: 5px 13px;");
        noteRule.append("background-color:    #f5f5f5;");
        noteRule.append("padding-left:        20px;");
        noteRule.append("padding-right:       15px;");
        noteRule.append("padding-top:         10px;");
        noteRule.append("padding-bottom:      10px;");
        noteRule.append("margin-left:         70px;");
        noteRule.append("margin-right:        70px;");
        noteRule.append("border-style:        solid;");
        noteRule.append("border-width:        1px;");
        noteRule.append("border-color:        black;");
        noteRule.append("border-padding:      40px;");
        noteRule.append("}");
        
        css.addRule(chipRule.toString()); 
        css.addRule(noteRule.toString());

        JScrollPane pagePane =  new JScrollPane(this.page);
        pagePane.setMinimumSize(new Dimension(200, 300));
        
        JSplitPane pane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, treePane, pagePane);
        pane.setDividerLocation(0.30);
        
        this.add(pane, gbc);
        
        try
        {
        //this.previous = new JButton(new ImageIcon(ClassLoader.getSystemResource(Helper.ICON_PATH + "/" + "back_enabled.gif")));
        //this.previous.setDisabledIcon(new ImageIcon(ClassLoader.getSystemResource(Helper.ICON_PATH + "/" + "back_disabled.gif")));
        this.previous = new JButton(new ImageIcon(Helper.ICON_PATH + "/" + "previous_enabled.gif"));
        this.previous.setDisabledIcon(new ImageIcon(Helper.ICON_PATH + "/" + "previous_disabled.gif"));
        this.previous.setEnabled(Boolean.FALSE.booleanValue());
        
        //this.next = new JButton(new ImageIcon(ClassLoader.getSystemResource(Helper.ICON_PATH + "/" + "next_enabled.gif")));
        //this.next.setDisabledIcon(new ImageIcon(ClassLoader.getSystemResource(Helper.ICON_PATH + "/" + "next_disabled.gif")));
        //this.next.setEnabled(Boolean.FALSE.booleanValue());
        this.next = new JButton(new ImageIcon(Helper.ICON_PATH + "/" + "next_enabled.gif"));
        this.next.setDisabledIcon(new ImageIcon(Helper.ICON_PATH + "/" + "next_disabled.gif"));
        this.next.setEnabled(Boolean.FALSE.booleanValue());
        
        //this.home = new JButton(new ImageIcon(ClassLoader.getSystemResource(Helper.ICON_PATH + "/" + "home.gif")));
        this.home = new JButton(new ImageIcon(Helper.ICON_PATH + "/" + "original.gif"));
        
        //this.print = new JButton(new ImageIcon(ClassLoader.getSystemResource(Helper.ICON_PATH + "/" + "print.gif")));
        this.print = new JButton(new ImageIcon(Helper.ICON_PATH + "/" + "original.gif"));
        //this.print.setDisabledIcon(new ImageIcon(ClassLoader.getSystemResource(Helper.ICON_PATH + "/" + "print_disabled.gif")));
        this.print.setDisabledIcon(new ImageIcon(Helper.ICON_PATH + "/" + "original.gif"));
        this.print.setEnabled(Boolean.FALSE.booleanValue());
        
        this.searchField = new JTextField(15);
        //this.searchLabel = new JLabel(new ImageIcon(ClassLoader.getSystemResource(Helper.ICON_PATH + "/" + "search.gif")));
        this.searchLabel = new JLabel(new ImageIcon(Helper.ICON_PATH + "/" + "original.gif"));
        }
        catch(Exception e)
        {
        	System.out.println(e.getMessage());
        }
        
        this.bar = new JProgressBar();
        this.bar.setIndeterminate(Boolean.FALSE.booleanValue());
        this.bar.setPreferredSize(new Dimension(30, 10));
    }


	/**
	 * Get the progress bar of the panel
	 */
    public JProgressBar getBar() {
        return bar;
    }

	
	/**
	 * Get the button to get home back
	 */
    public JButton getHome() {
        return home;
    }


	
    public JList getList() {
        return list;
    }


	/**
	 * Get the button used to move to the next page
	 */
    public JButton getNext() {
        return next;
    }


    public JEditorPane getPage() {
        return page;
    }


	/**
	 * Get the button used to move to the previous page
	 */
    public JButton getPrevious() {
        return previous;
    }


	/**
	 * Get the button used to print the help page
	 */
    public JButton getPrint() {
        return print;
    }


	/**
	 * Get the JTextField of search section of the panel (top right)
	 */
    public JTextField getSearchField() {
        return searchField;
    }


	/**
	 * Get the JLabel of the search section of the Panel (top right)
	 */
    public JLabel getSearchLabel() {
        return searchLabel;
    }


	/**
	 * Get the help Tree of the panel (left)
	 */
    public HelpTree getTree() {
        return tree;
    }


	/** 
	 * Get the JScrollPane containing the HelpTree (left)
	 */
    public JScrollPane getTreePane() {
        return treePane;
    }


    public int getType() {
        return type;
    }
    
	
	
    public void displayPage(ModelImpl model) throws IOException{
        URL url = this.getClass().getClassLoader().getResource(LocaleManager.getInstance().getLocale() + "/" + model.getHtmlPath());
        TreeNode[] pathToExpand = model.getPath();
        if (url == null){
            ModelImpl realModel = HelperManager.getInstance().getHelp();
            url = this.getClass().getClassLoader().getResource(LocaleManager.getInstance().getLocale() + "/" + realModel.getHtmlPath());
            pathToExpand = realModel.getPath();
        }
        
        //The page is already loaded and we must highlight
        if (url.equals(this.page.getPage()) && canHighlight){
            highlight();
        }
        this.page.setPage(url);
        
        this.treeSelectionFlag = Boolean.FALSE.booleanValue();
        this.tree.setSelectionPath(new TreePath(pathToExpand));
        
        
    }
    
    private void displaySearchPanel(){
        this.treePane.setViewportView(this.list);
    }
    
    private void displayTreePanel(){
        System.out.println("hé ho");
        this.treePane.setViewportView(this.tree);
    }
    
    public void updateLeftPane(){
        if (this.type == TREE){
            this.displayTreePanel();
        } else {
            this.displaySearchPanel();
        }
    }
    
    public void searchForWord(String word){
        ((DefaultListModel) this.list.getModel()).clear();
        HelperManager hm = HelperManager.getInstance();
        hm.setDirectory("ressources/help");
        List<Listable> results = hm.searchForWord(word);
        for (int i=0;i<results.size();i++){
            ((DefaultListModel) this.list.getModel()).addElement(results.get(i));
        }
        this.type = SEARCH;
        this.updateLeftPane();
    }
    
    private void disableHighlight(){
        this.canHighlight = Boolean.FALSE.booleanValue();
        HelperManager.getInstance().cleanHighlight();
    }
    
	
	/**
	 * Highlight all occurency of searched words
	 */
    private void highlight(){
        String[] words = searchField.getText().split(" ");
        HelperManager.getInstance().highlightWords(words);
    }
    
    private class Search implements Runnable {
        
        private JProgressBar bar;
        private String word;
        
        public Search(JProgressBar bar, String word){
            this.bar = bar;
            this.word = word;
        }

        public void run() {
            bar.setIndeterminate(Boolean.TRUE.booleanValue());
            MainPanel.this.searchForWord(word);
            bar.setIndeterminate(Boolean.FALSE.booleanValue());
        }
        
        public void search(){
            new Thread(this).start();
        }
        
    }
   
}



