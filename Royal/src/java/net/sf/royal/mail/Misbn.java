package net.sf.royal.mail;
import java.io.IOException;
import java.util.ArrayList;
import javax.mail.Message;
import javax.mail.MessagingException;

import net.sf.royal.util.ISBN;
import net.sf.royal.util.InvalidStandardIDException;

public class Misbn extends Mail {

	public Misbn(int protocol) {
		super(protocol);
		// TODO Auto-generated constructor stub
	}
	
	public ArrayList<Emailisbn> getIsbnBySubject(String text, int mailreaded) throws MessagingException, IOException{
		ArrayList<Emailisbn> alei = new ArrayList<Emailisbn>();
		Message[] mgs = this.getMessagesBySubject(text, mailreaded);
		for(int i =0; i<mgs.length; i++){
			Emailisbn ei = new Emailisbn(mgs[i]);
			String[] lines = Mail.getLines(mgs[i]);
			for(int j = 0; j<lines.length;j++){
				String[] l  = lines[j].split("\\|", 2);
				if(l.length > 0){
					String title = "";
					ISBN isbn;
					try {
						isbn = new ISBN(l[0]);
						if(l.length > 1){
							title = l[1];
						}
						ei.addIsbnLine(new EmailisbnLine(isbn,title));
					} catch (InvalidStandardIDException e) {}
				}
			}
			alei.add(ei);
		}
		return alei;
	}

}
