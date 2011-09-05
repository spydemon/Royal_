package net.sf.birdy.help.core.lucene;

import java.io.File;
import java.io.IOException;

import net.sf.birdy.help.core.model.Article;
import net.sf.birdy.help.core.model.Help;
import net.sf.birdy.help.core.model.Model;
import net.sf.birdy.help.core.model.Topic;

import org.apache.lucene.analysis.standard.StandardAnalyzer;
// Pour utilisation avec le constructeur de StandardAnalyzer
import org.apache.lucene.util.Version; 
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.store.FSDirectory;

public class TreeIndexer {

    public static void index(Help help, String directory) throws IOException{
        
        createFileIndex(directory, help);
        
        for(int i=0;i<help.getTopics().size();i++){
            performTopic((Topic) help.getTopics().get(i), directory);
        }
        
    }
    
    private static void performTopic(Topic topic, String directory) throws IOException{
        createFileIndex(directory, topic);
        for(int i=0;i<topic.getChilds().size();i++){
            Model child = (Model) topic.getChilds().get(i);
            if (child instanceof Topic){
                performTopic((Topic) child, directory);
            } else if (child instanceof Article){
                createFileIndex(directory, child);
            }
        }
        
    }
    
    private static void createFileIndex(String directory, Model model) throws IOException{
        IndexWriter writer = null;
        boolean create;
        File f = new File(directory);
        if (f.exists() && f.isDirectory()) {
          create = false;
        } else {
          create = true;
        }
        
        writer = new IndexWriter(FSDirectory.open(new File(directory)), new  StandardAnalyzer(Version.LUCENE_30), create, IndexWriter.MaxFieldLength.UNLIMITED);
        writer.addDocument(TreeDocument.getDocument(model));
        writer.optimize();
        
        if (null != writer) {
            writer.close();
        }
    }
}
