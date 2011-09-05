package net.sf.birdy.help.core.model;


public interface Model{

    public String getEn();
    
    public void setEn(String en);
    
    public String getFr(); 
    
    public void setFr(String fr);
    
    public String getName();
    
    public void setName(String name);
    
    public Model getFather();
    
    public void setFather(Model father);
    
    public String getHtmlPath();
    public void setHtmlPath(String path);
    
    public Model findModel(String key);
    
}
