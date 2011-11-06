package net.sf.royal.mail;
import net.sf.royal.util.ISBN;

public class EmailisbnLine {
	private ISBN isbn;
	private String title;
	
	public EmailisbnLine(ISBN i, String t){
		this.isbn = i;
		this.title = t;
	}
	
	public ISBN getIsbn(){
		return this.isbn;
	}
	public String getTitle(){
		return this.title;
	}
}
