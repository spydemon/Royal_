package net.sf.birdy.help.core.model;

import javax.swing.tree.DefaultMutableTreeNode;

import net.sf.birdy.help.core.locale.LocaleManager;

public abstract class ModelImpl extends DefaultMutableTreeNode implements Model, TreeChild {

    private String name;
    private String fr;
    private String en;
    private Model father;
    private String htmlPath;
    
    public String getEn() {
        return en;
    }
    public void setEn(String en) {
        this.en = en;
    }
    public String getFr() {
        return fr;
    }
    public void setFr(String fr) {
        this.fr = fr;
    }
    public String getName() {
        return name;
    }
    public void setName(String name) {
        this.name = name;
    }

    public Model getFather() {
        return father;
    }
    public void setFather(Model father) {
        this.father = father;
    }

    public String toString(){
        String locale = LocaleManager.getInstance().getLocale();
        if (locale.equals(LocaleManager.FR)){
            return this.getFr();
        } else if (locale.equals(LocaleManager.EN)){
            return this.getEn();
        }
        return this.getFr();
    }
    public String getHtmlPath() {
        return htmlPath;
    }
    public void setHtmlPath(String path) {
        this.htmlPath = path;
    }
    public abstract Model findModel(String key) ;
    
    
}
