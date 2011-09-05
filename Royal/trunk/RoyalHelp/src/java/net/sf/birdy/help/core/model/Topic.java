package net.sf.birdy.help.core.model;

import java.util.ArrayList;
import java.util.List;

public class Topic extends ModelImpl {
    
    private List<ModelImpl> childs = new ArrayList<ModelImpl>();
    
    public void addChild(ModelImpl model){
        this.childs.add(model);
        this.add(model);
        model.setFather(this);
    }
    
    public void removeChild(ModelImpl model){
        model.setFather(null);
        this.childs.remove(model);
        this.remove(model);
    }
    
    public void addTopic(Topic topic){
        this.addChild(topic);
    }
    
    public void addArticle(Article article){
        this.addChild(article);
    }

    public List<ModelImpl> getChilds() {
        return childs;
    }

    public void setChilds(List<ModelImpl> childs) {
        this.childs = childs;
    }

    public Model findModel(String key) {
        if (this.getName().equals(key)){
            return this;
        }
        for(int i=0;i<this.getChilds().size();i++){
            Model model = (Model) this.getChilds().get(i);
            Model found = model.findModel(key);
            if (found != null){
                return found;
            }
        }
        
        return null;
    }
}
