package net.sf.birdy.help.core.lucene;

import java.io.InputStreamReader;


import net.sf.birdy.help.core.locale.LocaleManager;
import net.sf.birdy.help.core.model.Model;

import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;

public class TreeDocument {
    
    public static final String KEY_KEY = "key";
    public static final String KEY_NAME = "name";
    public static final String KEY_CONTENT = "content";

    public static Document getDocument(Model model){
        Document doc = new Document();
        
        doc.add(new Field(KEY_KEY, model.getName(), Field.Store.YES, Field.Index.NOT_ANALYZED));
        doc.add(new Field(KEY_NAME,  model.toString(),  Field.Store.YES, Field.Index.NOT_ANALYZED));
        String filePath = LocaleManager.getInstance().getLocale() + "/" +model.getHtmlPath();
        System.out.println(filePath);
        doc.add(new Field(KEY_CONTENT, new InputStreamReader(TreeDocument.class.getClassLoader().getResourceAsStream(filePath))));
        
        return doc;
    }
}
