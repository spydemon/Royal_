package net.sf.royal.datamodel;

import java.util.HashSet;
import java.util.Set;

public class Type extends ModelImpl {

    private String name;
    
    private Set<Serie> series = new HashSet<Serie>();

    /**
     * @return Returns the name.
     */
    public String getName() {
        return name;
    }

    /**
     * @param name The name to set.
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return Returns the series.
     */
    public Set<Serie> getSeries() {
        return series;
    }

    /**
     * @param series The series to set.
     */
    public void setSeries(Set<Serie> series) {
        this.series = series;
    }
    
    public void addSerie(Serie serie){
        this.series.add(serie);
    }
    
    public void removeSerie(Serie serie){
        this.series.remove(serie);
    }

	@Override
	public String toString() {
		return this.getName();
	}
}
