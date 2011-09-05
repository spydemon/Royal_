package net.sf.birdy.help.core.lucene;

import net.sf.birdy.help.core.lucene.Listable;

import java.io.File;
import java.text.DecimalFormat;
import java.util.HashMap;
import java.util.Map;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.queryParser.QueryParser;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.Searcher;
import org.apache.lucene.search.Sort;
import org.apache.lucene.search.TopFieldDocs;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.Version;

public class TreeSearcher {
    
    public static Map<String,Listable> search(String sQuery, String directory){
        
        Map<String, Listable> results = new HashMap<String, Listable>();
        
        try {
            Searcher searcher = new IndexSearcher(FSDirectory.open(new File(directory))); // Il faut remplacer le string par un org.apache.lucene.store.Directory
            Analyzer analyzer = new StandardAnalyzer(Version.LUCENE_30);
            
            QueryParser parser = new QueryParser(Version.LUCENE_30, TreeDocument.KEY_CONTENT, analyzer);
            Query query= parser.parse(sQuery);

            //Hits hits = searcher.search(query,Integer.valueOf("150"), null, Sort.RELEVANCE);
            TopFieldDocs hits = searcher.search(query, null, 150, Sort.RELEVANCE);
            
            int start = 0;
            //final int HITS_PER_PAGE = 10;
            //int end = Math.min(HITS_PER_PAGE, hits.totalHits);
            
            results = new HashMap<String, Listable>(); 
	
            //if (hits.length() > 0) {
            if (hits.totalHits > 0)
            {
            	System.out.println(hits.totalHits);
                DecimalFormat format = new DecimalFormat("##");
                for (int i = start; i < hits.totalHits; i++) {
                    //Document doc = hits.doc(i);
                    //String key = doc.get(TreeDocument.KEY_KEY);
                	System.out.println(i);
                	String key = hits.fields[i].getField();
                    if (!results.containsKey(key))
                    {
                        String score = format.format(hits.scoreDocs[i].score * 100);
                        //String display = doc.get(TreeDocument.KEY_NAME);
                        String display = hits.fields[i].toString();
                        Listable listable = new Listable(key, display, score);
                        results.put(key, listable);
                    }
                }
            } else {
                return null;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return results;
    }
}
