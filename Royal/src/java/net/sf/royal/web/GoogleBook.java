package net.sf.royal.web;

import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.List;
import javax.imageio.ImageIO;
import com.google.api.client.http.javanet.NetHttpTransport;
import com.google.api.client.json.jackson.JacksonFactory;
import com.google.api.services.books.Books;
import com.google.api.services.books.Books.Volumes;
import com.google.api.services.books.model.Volume;
import com.google.api.services.books.model.VolumeVolumeInfo;
import com.google.api.services.books.model.VolumeVolumeInfoImageLinks;

import net.sf.royal.util.ISBN;
import net.sf.royal.web.ConnectionProblemException;




public class GoogleBook {
	
	private static Books gbooks = null;
	private String isbn;
	private Volume volume = null;
	private Volumes.List vl = null;
	private Volumes.Get vg = null;
	private String volumeid;
	private VolumeVolumeInfo vvi = null;
	private VolumeVolumeInfoImageLinks vviil = null;
	
	public GoogleBook(ISBN isbn){
		if(gbooks == null ){
			gbooks = new Books(new NetHttpTransport(),new  JacksonFactory());
		}
		this.isbn = isbn.toString();
		this.vl = gbooks.volumes.list(this.isbn+"+isbn:");
	}
	
	public void execute() throws ConnectionProblemException, ComicNotFoundException{
		List<Volume> lv = null;
		try {
			lv = this.vl.execute().getItems();
		} catch (IOException e) {
			throw new ConnectionProblemException();
		}
		if (lv == null){ 
			throw new ComicNotFoundException();
		}
		this.volumeid = lv.get(0).getId();
		this.vg = gbooks.volumes.get(this.volumeid);
		try {
			
			this.volume = this.vg.execute();
		} catch (IOException e) {
			throw new ConnectionProblemException();
		}
		
		this.vvi = this.volume.getVolumeInfo();
		this.vviil = this.vvi.getImageLinks();
	}
	
	public VolumeVolumeInfo getVolumeInfo(){
		return this.vvi;
	}
	
	public VolumeVolumeInfoImageLinks getVolumeImageLinks(){
		return this.vviil;
	}
	
	public Volume getVolume(){
		return this.volume;
	}
	
	public BufferedImage getCoverImage() throws ConnectionProblemException{
		String imagelink = null;
		try{
			imagelink = this.vviil.getThumbnail();
		} catch(NullPointerException e){}
		BufferedImage cover = null;
		if(imagelink != null){
			URL imageURL;
			try {
				imageURL = new URL(imagelink);
				URLConnection urlConn = imageURL.openConnection();
				urlConn.setRequestProperty("User-Agent", "" );
				urlConn.connect();
				InputStream urlStream = urlConn.getInputStream();
				cover = ImageIO.read(urlStream);
			} catch (MalformedURLException e) {
					//TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IOException e) {
					//TODO Auto-generated catch block
				throw new ConnectionProblemException(e.getMessage());
			}
		}
		return cover;
	}
}
