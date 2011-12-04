package com.Royal_Scanner;

import java.util.ArrayList;

import android.app.Activity;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.TextView;
import android.widget.Toast;
import com.Royal_Scanner.util.*;;

public class ScanPageActivity extends Activity {
	
	public String strBarCode;
	public String strEmail;
	public String strPassword;
	public boolean boolScanMode;
	public ArrayList<String> strArrListIsbn;
	public String strListIsbn;
	public ISBN isbn;
	public Button btnSend;
	public Button btnScan;
	
	@Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.scan);
            
        // Récupération des parametres
		Bundle extras = getIntent().getExtras();
		if (extras != null) {
			
		// On récupere notre parametre String
		strBarCode = (String) extras.get("barCode");
		
		// On récupere notre parametre boolean
		boolScanMode = (boolean) extras.getBoolean("scanMode");
		
		//On récupere notre parametre ArrayList
		strArrListIsbn = (ArrayList<String>) extras.getStringArrayList("listIsbn");
		
		//on initialise la strListIsbn
		strListIsbn="";
		
		// Récupération du TextView du layout courant
		TextView tvResult = (TextView) this.findViewById(R.id.tvScanResult);
		btnScan = (Button) this.findViewById(R.id.btnScan);
		btnSend = (Button) this.findViewById(R.id.btnSendIsbn);
		
		try {
			isbn= new ISBN(strBarCode) ;		
			boolean isbn13=true;
			
			// On affecte la valeur
			tvResult.setText(strBarCode);
			if(boolScanMode==false)
			{
				strArrListIsbn.add(isbn.toString(isbn13));
				if(isbn13==false){strArrListIsbn.add(isbn.toString());}
				btnScan.setVisibility(View.INVISIBLE);			
				btnSend.setVisibility(View.VISIBLE);	
				btnSend.setOnClickListener(btMailListener);
			}
			else
			{
				strArrListIsbn.add(isbn.toString(isbn13));
				if(isbn13==false){strArrListIsbn.add(isbn.toString());}
				btnSend.setVisibility(View.VISIBLE);
				btnSend.setOnClickListener(btMailListener);
				btnScan.setVisibility(View.VISIBLE);			
				btnScan.setText(getString(R.string.btnScan_reScan));
				btnScan.setOnClickListener(btaddScanListener);
			}
		} catch (InvalidStandardIDException e) {
			// On signal l'erreur
			tvResult.setText(getString(R.string.scan_error));
			btnSend.setVisibility(View.INVISIBLE);			
			btnScan.setVisibility(View.VISIBLE);			
			btnScan.setText(getString(R.string.btnScan_reScan));
			btnScan.setOnClickListener(btReScanListener);
		}	
	}
		
		
}

	
	public OnClickListener btaddScanListener = new Button.OnClickListener() 
	{   
		public void onClick(View v) 
		{   
			// Initialisation du scan
			Intent intentScan = new Intent("com.Royal_Scanner.SCAN");
			startActivityForResult(intentScan, 0);
		}
	};	
	
	public OnClickListener btReScanListener = new Button.OnClickListener() 
	{   
		public void onClick(View v) 
		{   
			// Initialisation du scan
			Intent intentScan = new Intent("com.Royal_Scanner.SCAN");
			startActivityForResult(intentScan, 0);
		}
	};	
	
	public OnClickListener btMailListener = new Button.OnClickListener() 
	{   
	    public void onClick(View v) { 
	        	    	
	    	//recuperation de la preference de l'adresse email d'envoi
		    SharedPreferences spEmail  = getSharedPreferences("PrefEmail", 0);
		    strEmail= spEmail.getString("email", "");
		    strPassword= spEmail.getString("password", "");
	       
		    if(strEmail.equals("")){
		      //affichage du message info
		      Toast.makeText(ScanPageActivity.this, getString(R.string.sendIsbn_noEmail), Toast.LENGTH_LONG).show(); 
  	
  	          // Nouveau Intent configue mail
  	          Intent intentSetupEmail = new Intent(ScanPageActivity.this,SetupMailActivity.class);
  	          
  	          // On lance l'Activity de scan
  	          startActivity(intentSetupEmail);
	        }
	        else{
	        	
	        	//on desactive le boutton d'envoi 
		    	btnSend.setClickable(false);

	        	try { 
		          //décodage du mots de passe
	           	  strPassword = Base64Utils.decode(strPassword);
	  	    	  Mail mail = new Mail(); 
	  	    	  mail.setUser(strEmail);
	  	    	  mail.setPass(strPassword);
	  		      mail.setTo(strEmail); 
	  		      mail.setFrom("Royal_Scanner"); 
	  		      mail.setSubject(Md5.encode(strEmail)); 
	  		      for(int i=0; i<strArrListIsbn.size() ; i++){
	  		          strListIsbn += strArrListIsbn.get(i);
	  		    	  strListIsbn += "\n";
	  		      }
	  		      mail.setBody(strListIsbn); 
	  	          if(mail.send()) {
	  	              
				      //affichage du message info
	  	        	  Toast.makeText(ScanPageActivity.this, getString(R.string.email_sent), Toast.LENGTH_LONG).show(); 
		  	          
					  // On termine l'Activity en cours
		  			  ScanPageActivity.this.finish();
	  	          } 
	  	          else { 
				      //affichage du message info
	  	              Toast.makeText(ScanPageActivity.this, getString(R.string.email_not_sent), Toast.LENGTH_LONG).show(); 
	  	          } 
	  	        } 
	  	        catch(Exception e) {
				      //affichage du message info
	  	              Toast.makeText(ScanPageActivity.this, getString(R.string.email_not_sent), Toast.LENGTH_LONG).show(); 	      
	  	        }
	        }
	      } 
	 }; 
	
	// gestion du scan
			
				public void onActivityResult(int requestCode, int resultCode, Intent intentScan) {
					String contents;
					if (requestCode == 0) {
						      if (resultCode == RESULT_OK) {
						         contents = intentScan.getStringExtra("SCAN_RESULT");
						      						         
						         	// Envoi du resultat et affichage de la page details
									Intent intentScanResult = new Intent(ScanPageActivity.this,ScanPageActivity.class);
									intentScanResult.putExtra("barCode", contents);
									intentScanResult.putExtra("scanMode", boolScanMode);
									intentScanResult.putExtra("listIsbn", strArrListIsbn);
									
									// On lance l'Activity de scan
									startActivity(intentScanResult);
									
									// On termine l'Activity en cours
									ScanPageActivity.this.finish();
						      }
					}
					
				
			}
		    
}	