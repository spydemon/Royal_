package net.sf.royal.mail;

import java.text.DateFormat;
import java.util.ArrayList;

import javax.mail.Flags.Flag;
import javax.mail.Message;
import javax.mail.MessagingException;

public class Emailisbn {

	private ArrayList<EmailisbnLine> lisbn = new ArrayList<EmailisbnLine>();
	private Message msg;
	public Emailisbn(Message m){
		this.msg = m;
	}
	
	public void addIsbnLine(EmailisbnLine eil){
		this.lisbn.add(eil);
	}
	
	public void deleteMessage() throws MessagingException{
		this.msg.setFlag(Flag.DELETED, true);
	}
	
	public ArrayList<EmailisbnLine> getEmailisbnLine(){
		return this.lisbn;
	}
	
	public Message getMessage(){
		return this.msg;
	}
	
	@Override
	public String toString(){
		try {
			return DateFormat.getInstance().format(this.msg.getReceivedDate()).toString();
		} catch (MessagingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return "ISBN";
	}
}