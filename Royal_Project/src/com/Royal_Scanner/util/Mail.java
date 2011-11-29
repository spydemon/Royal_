package com.Royal_Scanner.util;

import java.util.Date; 
import java.util.Properties; 
import javax.activation.CommandMap; 
import javax.activation.DataHandler; 
import javax.activation.DataSource; 
import javax.activation.FileDataSource; 
import javax.activation.MailcapCommandMap; 
import javax.mail.BodyPart; 
import javax.mail.Multipart; 
import javax.mail.PasswordAuthentication; 
import javax.mail.Session; 
import javax.mail.Transport; 
import javax.mail.internet.InternetAddress; 
import javax.mail.internet.MimeBodyPart; 
import javax.mail.internet.MimeMessage; 
import javax.mail.internet.MimeMultipart; 

public class Mail extends javax.mail.Authenticator{
	private String strUser; 
	  private String strPass; 
	 
	  private String[] strArrTo; 
	  private String strFrom; 
	 
	  private String strPort; 
	  private String strSport; 
	 
	  private String strHost; 
	 
	  private String strSubject; 
	  private String strBody; 
	  
	  private boolean boolAuth; 
	   
	  private boolean boolDebuggable; 
	 
	  private Multipart multipart; 
	 
	 
	  public Mail() { 
	    strHost = "smtp.gmail.com"; // default smtp server 
	    strPort = "465"; // default smtp port 
	    strSport = "465"; // default socketfactory port 

		  
	    strUser = ""; // username 
	    strPass = ""; // password 
	    strFrom = ""; // email sent from 
	    strSubject = ""; // email subject 
	    strBody = ""; // email body 
	 
	    boolDebuggable = false; // debug mode on or off - default off 
	    boolAuth = true; // smtp authentication - default on 
	 
	    multipart = new MimeMultipart(); 
	 
	    // There is something wrong with MailCap, javamail can not find a handler for the multipart/mixed part, so this bit needs to be added. 
	    MailcapCommandMap mc = (MailcapCommandMap) CommandMap.getDefaultCommandMap(); 
	    mc.addMailcap("text/html;; x-java-content-handler=com.sun.mail.handlers.text_html"); 
	    mc.addMailcap("text/xml;; x-java-content-handler=com.sun.mail.handlers.text_xml"); 
	    mc.addMailcap("text/plain;; x-java-content-handler=com.sun.mail.handlers.text_plain"); 
	    mc.addMailcap("multipart/*;; x-java-content-handler=com.sun.mail.handlers.multipart_mixed"); 
	    mc.addMailcap("message/rfc822;; x-java-content-handler=com.sun.mail.handlers.message_rfc822"); 
	    CommandMap.setDefaultCommandMap(mc); 
	  } 
	 
	  public boolean send() throws Exception { 
	    Properties props = _setProperties(); 
	 
	    if(!strUser.equals("") && !strPass.equals("") && strArrTo.length > 0 && !strFrom.equals("") && !strSubject.equals("") && !strBody.equals("")) { 
	      Session session = Session.getInstance(props, this); 
	 
	      MimeMessage msg = new MimeMessage(session); 
	 
	      msg.setFrom(new InternetAddress(strFrom)); 
	       
	      InternetAddress[] addressTo = new InternetAddress[strArrTo.length]; 
	      for (int i = 0; i < strArrTo.length; i++) { 
	        addressTo[i] = new InternetAddress(strArrTo[i]); 
	      } 
	        msg.setRecipients(MimeMessage.RecipientType.TO, addressTo); 
	 
	      msg.setSubject(strSubject); 
	      msg.setSentDate(new Date()); 
	 
	      // setup message body 
	      BodyPart messageBodyPart = new MimeBodyPart(); 
	      messageBodyPart.setText(strBody); 
	      multipart.addBodyPart(messageBodyPart); 
	 
	      // Put parts in message 
	      msg.setContent(multipart); 
	 
	      // send email 
	      Transport.send(msg); 
	 
	      return true; 
	    } else { 
	      return false; 
	    } 
	  } 
	 
	  public void addAttachment(String filename) throws Exception { 
	    BodyPart messageBodyPart = new MimeBodyPart(); 
	    DataSource source = new FileDataSource(filename); 
	    messageBodyPart.setDataHandler(new DataHandler(source)); 
	    messageBodyPart.setFileName(filename); 
	 
	    multipart.addBodyPart(messageBodyPart); 
	  } 
	 
	  @Override 
	  public PasswordAuthentication getPasswordAuthentication() { 
	    return new PasswordAuthentication(strUser, strPass); 
	  } 
	 
	  private Properties _setProperties() { 
	    Properties props = new Properties(); 
	 
	    props.put("mail.smtp.host", strHost); 
	 
	    if(boolDebuggable) { 
	      props.put("mail.debug", "true"); 
	    } 
	 
	    if(boolAuth) { 
	      props.put("mail.smtp.auth", "true"); 
	    } 
	 
	    props.put("mail.smtp.port", strPort); 
	    props.put("mail.smtp.socketFactory.port", strSport); 
	    props.put("mail.smtp.socketFactory.class", "javax.net.ssl.SSLSocketFactory"); 
	    props.put("mail.smtp.socketFactory.fallback", "false"); 
	 
	    return props; 
	  } 
	 
	  // the getters and setters 
	  public String getBody() { 
	    return strBody; 
	  } 
	
	  public void setUser(String strUser){
		  this.strUser = strUser;
	  }
	  
	  public void setPass(String strPass){
		  this.strPass = strPass;
	  }
	  
	  public void setBody(String strBody) { 
	    this.strBody = strBody; 
	  } 
	 
	  public void setTo(String[] strArrTo) {
		  this.strArrTo = strArrTo;
	  }
	  
	  public void setTo(String strTo){
		  String[] ArrTo = {strTo}; 
		  this.strArrTo = ArrTo;
	  }

	  public void setFrom(String strFrom) {
		  this.strFrom = strFrom;
	  }

	  public void setSubject(String strSubject) {
		  this.strSubject = strSubject;
	  } 
	  
	  public void setPort(String strPort){
		  this.strPort = strPort;
	  }
	  
	  public void setSport(String strSport){
		  this.strSport = strSport;
	  }
	  
	  public void setHost(String strHost){
		  this.strHost = strHost;
	  }
	} 