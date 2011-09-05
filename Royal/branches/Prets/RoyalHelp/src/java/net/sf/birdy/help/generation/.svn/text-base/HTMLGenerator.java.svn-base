package net.sf.birdy.help.generation;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

import net.sf.birdy.help.core.model.Article;
import net.sf.birdy.help.core.model.Help;
import net.sf.birdy.help.core.model.Model;
import net.sf.birdy.help.core.model.Populator;
import net.sf.birdy.help.core.model.Topic;

import org.xml.sax.SAXException;

import estexte.TextProcessor;

public class HTMLGenerator {
    
    private String srcFolder = "";
    private String outputFolder = "";
    private Help help = null;
    private String[] languages;
    
    
    public HTMLGenerator(String srcFolder, String outputFolder, Help help, String[] languages){
        this.srcFolder = srcFolder;
        this.outputFolder = outputFolder;
        this.help = help;
        this.languages = languages;
    }
    
    private void generateHTMLFile(File src, File output) throws IOException{
        TextProcessor processor = new TextProcessor(new FileInputStream(src), src.getParent());
        processor.process();
        processor.write(new FileOutputStream(output));
    }
    
    public void execute() throws IOException{
        //Create index.html file
        for(int i=0; i<this.languages.length;i++){
            new File(this.outputFolder + "/" + this.languages[i]).mkdir();
            File src = new File(srcFolder + "/" + this.languages[i] + "/" + "index.txt");
            if (src.exists()){
                File out = new File(outputFolder + "/" + this.languages[i] + "/" + "index.html");
                this.generateHTMLFile(src, out);
            }
        }
        
        for(int i=0;i<help.getTopics().size();i++){
            Topic topic = (Topic) help.getTopics().get(i);
            this.performTopic(topic, null);
        }
    }
    
    private void performTopic(Topic topic, String fatherPath) throws IOException{
        String topicPath = null;
        if (fatherPath != null){
            topicPath = fatherPath + "/" + topic.getName();
        } else {
            topicPath = topic.getName();
        }
        for(int i=0; i<this.languages.length;i++){
            File src = new File(srcFolder + "/" + this.languages[i] + "/" + topicPath + "/" + topic.getName() + ".txt");
            if (src.exists()){
                new File(this.outputFolder + "/" + this.languages[i] + "/" + topicPath).mkdir();
                File out = new File(outputFolder + "/" + this.languages[i] + "/" + topicPath + "/" + topic.getName() + ".html");
                this.generateHTMLFile(src, out);
            }
        }
        for (int i=0;i<topic.getChilds().size();i++){
            Model child = (Model) topic.getChilds().get(i);
            if (child instanceof Topic){
                this.performTopic((Topic) child, topicPath);
            } else {
                this.performArticle((Article) child, topicPath);
            }
        }
        
        
    }
    
    private void performArticle(Article article, String fatherPath) throws IOException{
        for(int i=0; i<this.languages.length;i++){
            File src = new File(srcFolder + "/" + this.languages[i] + "/" + fatherPath + "/" + article.getName() + ".txt");
            if (src.exists()){
                File out = new File(outputFolder + "/" + this.languages[i] + "/" + fatherPath + "/" + article.getName() + ".html");
                this.generateHTMLFile(src, out);
            }
        }
    }
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        
        Populator populator = new Populator("help.xml");
        try {
            Help help = populator.populate();
            String[] languages = {"fr", "en"};
            HTMLGenerator generator = new HTMLGenerator("src/wiki",
                                                        "build/site",
                                                        help,
                                                        languages);
            generator.execute();
            
        } catch (IOException e1) {
            e1.printStackTrace();
        } catch (SAXException e1) {
            e1.printStackTrace();
        }
        
        
    }

    public String getOutputFolder() {
        return outputFolder;
    }

}