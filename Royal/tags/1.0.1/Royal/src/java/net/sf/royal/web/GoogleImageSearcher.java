package net.sf.royal.web;

import java.awt.BorderLayout;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.net.UnknownHostException;
import java.nio.charset.Charset;
import java.util.ArrayList;

import net.sf.json.JSONArray;
import net.sf.json.JSONObject;
import net.sf.royal.gui.manager.LocaleManager;
import net.sf.royal.gui.manager.MessagePaneManager;
import net.sf.royal.gui.manager.PropertyManager;

//For tests in main()
import javax.swing.JFrame;
import javax.swing.JTextArea;
// --


/**
 * @author Soulou
 * Class to search for image URL on the web in order to automatically
 * fill the cover when the user ask to do a search.
 */
public class GoogleImageSearcher 
{
	// Google API Image Search need a key for the application
	private static String api_key = PropertyManager.getInstance().getProperty("google_api_key");
	private static String ggUrl = PropertyManager.getInstance().getProperty("google_url");
	private static String refererWebsite = PropertyManager.getInstance().getProperty("royal_website");
	
	private String search;
	private ArrayList<String> resultURLs;
	
	public GoogleImageSearcher(String search)
	{
		this.search = search;
		this.resultURLs = new ArrayList<String>();
	}
	
	public void executeSearch()
	{
		StringBuilder builder = null;
		// Mandatory option in URL, q : query, key : application key
		try
		{
			URL searchURL = new URL(ggUrl + "&key=" + api_key + "&q=" + URLEncoder.encode(this.search, Charset.defaultCharset()
					.toString()));
			URLConnection searchConnection = searchURL.openConnection();
			searchConnection.addRequestProperty("Referer", refererWebsite);
		
			String line;
			builder = new StringBuilder();
			BufferedReader reader = new BufferedReader(new InputStreamReader(searchConnection.getInputStream()));
			while((line = reader.readLine()) != null)
				builder.append(line);
			
		} catch (MalformedURLException mue)
		{
			mue.printStackTrace();
		} catch (UnknownHostException uhe)
		{
			uhe.printStackTrace();
			MessagePaneManager.showCheckErrorPane(LocaleManager.getInstance().getString("connection_error"));
		} catch (IOException ioe)
		{
			ioe.printStackTrace();
		}
		
		
		JSONObject json = JSONObject.fromObject(builder.toString());
		JSONObject jsonResponseData = json.getJSONObject("responseData");
		JSONArray jsonResults = jsonResponseData.getJSONArray("results");
		JSONObject jsonCurrentResult;
		
		for(int i = 0; i < jsonResults.size(); i++)
		{
			jsonCurrentResult = jsonResults.getJSONObject(i);
			resultURLs.add(jsonCurrentResult.getString("url"));
		}
	}
	
	public ArrayList<String> getResults()
	{
		return this.resultURLs;
	}
	
	/* To test the class only */
	public static void main(String[] args)
	{
		if(args.length != 1)
			return;
		
		JFrame jf = new JFrame();
		
		JTextArea jta = new JTextArea();
		jf.add(jta, BorderLayout.CENTER);
		
		GoogleImageSearcher gis = new GoogleImageSearcher(args[0]);
		gis.executeSearch();
		
		StringBuilder sb = new StringBuilder();
		for(String s : gis.resultURLs)
			sb.append(s).append('\n');
		
		jta.setText(sb.toString());
		
		jf.pack();
		jf.setVisible(true);
		jf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}
}