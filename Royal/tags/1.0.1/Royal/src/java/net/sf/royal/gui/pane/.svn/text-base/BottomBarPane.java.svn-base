package net.sf.royal.gui.pane;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.JLabel;
import javax.swing.JPanel;

import net.sf.royal.gui.manager.IconManager;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.pane.bottom_bar.WorkPanel;
import net.sf.royal.persistency.PersistencyManager;

public class BottomBarPane extends JPanel {

	private static BottomBarPane instance;
	
	private int nbAlbums = 0;
	private int nbSeries = 0;
    private JLabel albumLabel = null;
    private JLabel serieLabel = null;
	private WorkPanel wp = null;
    
    private BottomBarPane(){

        super();
        this.setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 0;
        gbc.weighty = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(5,5,5,5);
        
        this.albumLabel = new JLabel();
        this.albumLabel.setIcon(IconManager.getIcon("album.png"));
        
        this.add(this.albumLabel, gbc);
        
        gbc.gridx++;
        
        this.serieLabel = new JLabel();
        this.serieLabel.setIcon(IconManager.getIcon("serie.png"));
        
        this.add(this.serieLabel, gbc);
        
        gbc.gridx++;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.EAST;
		this.wp = new WorkPanel();
        this.add(this.wp, gbc);
        
        this.setPreferredSize(new Dimension(30,35));
        
        this.refreshStats();
    }
    
    public void refreshStats(){
        this.nbAlbums = PersistencyManager.findCountAlbums();
        albumLabel.setText(this.nbAlbums + " " + LocaleManager.getInstance().getString("albums"));
        this.nbSeries = PersistencyManager.findCountSeries();
        serieLabel.setText(this.nbSeries + " " + LocaleManager.getInstance().getString("series"));
    }
    
    public void addAlbum()
    {
    	this.nbAlbums++;
    	albumLabel.setText(this.nbAlbums + " " + LocaleManager.getInstance().getString("albums"));
    }
    public void rmAlbum()
    {
    	this.nbAlbums--;
    	albumLabel.setText(this.nbAlbums + " " + LocaleManager.getInstance().getString("albums"));
    }
    public void addSerie()
    {
    	this.nbSeries++;
        serieLabel.setText(this.nbSeries + " " + LocaleManager.getInstance().getString("series"));
    }
    public void rmSerie()
    {
    	this.nbSeries--;
        serieLabel.setText(this.nbSeries + " " + LocaleManager.getInstance().getString("series"));
    }
    
    public static BottomBarPane getInstance()
    {
    	if(instance == null)
    	{
    		instance = new BottomBarPane();
    	}
    	return instance;
    }
    
	public static void setInstance(BottomBarPane in)
	{
		instance = in;
	}
}
