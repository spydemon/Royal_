package net.sf.birdy.help.core.model;

import java.io.IOException;

import org.apache.commons.digester.Digester;
import org.xml.sax.SAXException;

public class Populator {

    private String fileToParse;

    public Populator(String fileToParse){
        this.fileToParse = fileToParse;
    }
    
    private Help readXML() throws IOException, SAXException{
        
        Digester d = new Digester();
        
        d.addObjectCreate("help", Help.class);
        d.addSetProperties("help");
        d.addObjectCreate("help/topic", Topic.class);
        d.addSetProperties("help/topic");
        
        d.addObjectCreate("help/topic/topic", Topic.class);
        d.addSetProperties("help/topic/topic");
        
        
        d.addObjectCreate("help/topic/article", Article.class);
        d.addSetProperties("help/topic/article");
        
        
        d.addObjectCreate("help/topic/topic/article", Article.class);
        d.addSetProperties("help/topic/topic/article");
        
        d.addSetNext("help/topic", "addTopic");
        d.addSetNext("help/topic/topic", "addTopic");
        d.addSetNext("help/topic/article", "addArticle");
        d.addSetNext("help/topic/topic/article", "addArticle");
        
        Help res = (Help) d.parse(getClass().getClassLoader().getResourceAsStream(fileToParse));
        
        return res;
    }
    
    public Help populate() throws IOException, SAXException{
        Help res = this.readXML();
        this.createHTMLPaths(res);
        return res;
    }
    
    private void createHTMLPaths(Help help){
        help.setHtmlPath(help.getName() + ".html");
        
        for(int i=0;i<help.getTopics().size();i++){
            Topic topic = (Topic) help.getTopics().get(i); 
            String pathdir = topic.getName();
            this.createTopicPath(topic, pathdir);
            
        }
        
    }
    
    private void createTopicPath(Topic topic, String pathdir){
        topic.setHtmlPath(pathdir + "/" + topic.getName() + ".html");
        
        for(int i=0;i<topic.getChilds().size();i++){
            ModelImpl child = (ModelImpl) topic.getChilds().get(i);
            if (child instanceof Topic){
                String newPathdir = pathdir + "/" + child.getName();
                this.createTopicPath((Topic) child, newPathdir);
            } else {
                this.createArticlePath((Article) child, pathdir);
            }
        }
    }
    
    private void createArticlePath(Article article, String pathdir){
        article.setHtmlPath(pathdir + "/" + article.getName() + ".html");
    }
}
