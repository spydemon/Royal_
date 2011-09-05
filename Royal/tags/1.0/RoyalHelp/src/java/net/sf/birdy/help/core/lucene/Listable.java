package net.sf.birdy.help.core.lucene;

public class Listable {

    private String key;
    private String display;
    private String score;
    
    public Listable(String key, String display, String score) {
        this.key = key;
        this.display = display;
        this.score = score;
    }
    public String getDisplay() {
        return display;
    }
    public void setDisplay(String display) {
        this.display = display;
    }
    public String getKey() {
        return key;
    }
    public void setKey(String key) {
        this.key = key;
    }
    public String getScore() {
        return score;
    }
    public void setScore(String score) {
        this.score = score;
    }
    
    public String toString(){
        return this.score + " % - " + this.display;
    }
}
