package net.sf.royal.mail;
import com.sun.mail.pop3.POP3SSLStore;
import com.sun.mail.imap.IMAPSSLStore;

import java.io.IOException;
import java.util.Properties;

import javax.mail.BodyPart;
import javax.mail.Flags.Flag;
import javax.mail.Folder;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Store;
import javax.mail.URLName;
import javax.mail.internet.MimeMultipart;
import javax.mail.search.SubjectTerm;

public class Mail {
    
    private Session session = null;
    private Store store = null;
    private String username, password;
    private Folder folder;
    public static final int IMAP = 1;
    public static final int POP3 = 2;
    private int protocol;
    private String hostname = null;
    
    public Mail(int protocol) {
        switch(protocol){
        case 1 : this.protocol = 1;
        break;
        case 2 : this.protocol = 2;
        break;
        }
    }
    
    public void setUserPass(String username, String password, String host) {
        this.username = username;
        this.password = password;
        this.hostname = host;
    }
    
    public void connect() throws Exception {
        if(this.protocol == Mail.IMAP){
        	Properties props = System.getProperties();
        	this.session = Session.getDefaultInstance(props, null);
            URLName url = new URLName("imap", this.hostname, 993, "",
                    this.username, this.password);
        	this.store = new IMAPSSLStore(session, url);
        	this.store.connect();
        }
        else if (this.protocol == Mail.POP3){
            String SSL_FACTORY = "javax.net.ssl.SSLSocketFactory";
            
            Properties pop3Props = new Properties();
            
            pop3Props.setProperty("mail.pop3.socketFactory.class", SSL_FACTORY);
            pop3Props.setProperty("mail.pop3.socketFactory.fallback", "false");
            pop3Props.setProperty("mail.pop3.port",  "995");
            pop3Props.setProperty("mail.pop3.socketFactory.port", "995");
            
            URLName url = new URLName("pop3", this.hostname, 995, "",
                    this.username, this.password);
            
            this.session = Session.getInstance(pop3Props, null);
            this.store = new POP3SSLStore(session, url);
            this.store.connect();
        }   
    }
    
    public void openFolder(String folderName) throws Exception {
        
        // Open the Folder
        folder = store.getDefaultFolder().getFolder(folderName);
        
        if (folder == null) {
            throw new Exception("Invalid folder");
        }
        
        // try to open read/write and if that fails try read-only
        try {
            
            folder.open(Folder.READ_WRITE);
            
        } catch (MessagingException ex) {
            
            folder.open(Folder.READ_ONLY);
            
        }
    }
    
    public void closeFolder() throws Exception {
        folder.close(false);
    }
    
    public void disconnect() throws Exception {
        store.close();
    }
    
    public Message[] getMessages(int mailreaded) throws MessagingException {
    	int nbmessage = folder.getMessageCount();
    	int start = nbmessage- mailreaded;
    	return folder.getMessages(start, nbmessage);
    }
     
    public Message[] getMessagesBySubject(String text, int mailreaded) throws MessagingException{
    	int nbmessage = folder.getMessageCount();
    	int start = nbmessage- mailreaded;
    	if(nbmessage <= mailreaded){
    		start = 1;
    	}
    	Message[] msgs = folder.search(new SubjectTerm(text), folder.getMessages(start, nbmessage));
    	return msgs;
    }   
    
	public static String getContent(Message t) throws IOException, MessagingException{
		String res ="";
		if ( t.getContent() instanceof MimeMultipart){
			MimeMultipart mmp = (MimeMultipart) t.getContent();
			BodyPart bp = mmp.getBodyPart(0);
			res = bp.getContent().toString();	
		}
		else if (t.getContent() instanceof String){
			res = (String) t.getContent();
		}
		return res;
	}
	
	public static String[] getLines(Message t) throws IOException, MessagingException{
		String fulllines = getContent(t);
		return fulllines.split("\n");
	}
	
	public static void deleteMessage(Message t) throws MessagingException{
		t.setFlag(Flag.DELETED, true);
	}
    
}

