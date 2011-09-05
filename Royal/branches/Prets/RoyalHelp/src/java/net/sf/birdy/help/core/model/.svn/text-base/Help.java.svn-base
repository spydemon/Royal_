package net.sf.birdy.help.core.model;

import java.util.ArrayList;
import java.util.List;

public class Help extends ModelImpl {

    private List<Topic> topics = new ArrayList<Topic>();
    
    public void addTopic(Topic topic){
        this.topics.add(topic);
        this.add(topic);
    }
    
    public void removeTopic(Topic topic){
        this.topics.add(topic);
        this.remove(topic);
    }

    public List<Topic> getTopics() {
        return topics;
    }

    public void setTopics(List<Topic> topics) {
        this.topics = topics;
    }

    public Model findModel(String key) {
        if (this.getName().equals(key)){
            return this;
        }
        for(int i=0;i<this.getTopics().size();i++){
            Model model = (Model) this.getTopics().get(i);
            Model found = model.findModel(key);
            if (found != null){
                return found;
            }
        }
        return null;
    }
}
