package com.Royal_Scanner;

import com.Royal_Scanner.util.Base64Utils;
import com.Royal_Scanner.util.Mail;

import android.app.Activity;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
//import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
//import android.widget.Spinner;
import android.widget.Toast;

public class SetupMailActivity extends Activity{

	public SharedPreferences spEmail;
	public String strEmail;
	public String strPassword;
	public String strNewEmail;
	public String strNewPassword;
    public EditText etInputEmail;
	public EditText etInputPassword;
	public Button btnSetupMail;
    
    
	@Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.setupmail);
        
        //recuperation de la preference de l'adresse email d'envoi
        spEmail  = getSharedPreferences("PrefEmail", 0);
	    strEmail= spEmail.getString("email", "");
	    strPassword= spEmail.getString("password", "");
	    
	    //décodage du mots de passe
        try {
			strPassword = Base64Utils.decode(strPassword);
		} catch (Exception e) {
			strPassword="";
		}
       
	    //recuperation des element de setupmail.xml
        btnSetupMail = (Button) this.findViewById(R.id.btnSetupMail);
		etInputEmail = (EditText)findViewById(R.id.etInputEmail);
		etInputPassword = (EditText)findViewById(R.id.etInputPassword);
		
		//Mise en place des anciennes valeurs
		etInputEmail.setText(strEmail);
		etInputPassword.setText(strPassword);
		
        //mise en place des listener
        btnSetupMail.setOnClickListener(btnSetupListener);
	}
	
	public OnClickListener btnSetupListener = new Button.OnClickListener() 
	{   
		public void onClick(View v) 
		{   		
			strNewEmail = etInputEmail.getText().toString();
			strNewPassword = etInputPassword.getText().toString();
			
			if(!strNewEmail.equals("") && !strNewPassword.equals(""))
			{
				//on desactive le boutton d'envoi 
		    	btnSetupMail.setClickable(false);
				
				try { 
			    	
					//configuration de l'email
					Mail mail = new Mail(); 
			    	mail.setUser(strNewEmail);
			    	mail.setPass(strNewPassword);
				    mail.setTo(strNewEmail); 
				    mail.setFrom("Royal_Scanner"); 
				    mail.setSubject("Email Test"); 
				    mail.setBody(getString(R.string.setup_email_test_mail)); 
			        
				    if(mail.send()) {
			        	  
				    	  //enregistrement adresse mail
					      SharedPreferences.Editor spEmailEditor = spEmail.edit();
					      spEmailEditor.putString("email", strNewEmail);
					      spEmailEditor.putString("password", Base64Utils.encode(strNewPassword));
					      spEmailEditor.commit();
				          
					      //affichage du message info
					      Toast.makeText(SetupMailActivity.this, getString(R.string.setup_email_send_ok) , Toast.LENGTH_LONG).show(); 
					      
					      // On termine l'Activity en cours
					      SetupMailActivity.this.finish();
			        } 
			        else { 
					      //affichage du message info
			        	  Toast.makeText(SetupMailActivity.this, getString(R.string.setup_email_send_not_ok), Toast.LENGTH_LONG).show(); 
			        	
			        	  //on reactive le boutton d'envoi 
					      btnSetupMail.setClickable(true);
			        } 
			      } 
			      catch(Exception e) {
				      //affichage du message info
			    	  Toast.makeText(SetupMailActivity.this, getString(R.string.setup_email_send_not_ok), Toast.LENGTH_LONG).show(); 
			    	  
			    	  //on reactive le boutton d'envoi 
				      btnSetupMail.setClickable(true);
			      } 
			}
			else
			{
			      //affichage du message info
				  Toast.makeText(SetupMailActivity.this, getString(R.string.setup_email_no_info), Toast.LENGTH_LONG).show(); 
			}
		}
	};
}