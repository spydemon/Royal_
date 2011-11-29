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

public class ScanPageActivity extends Activity {
	
	public String strBareCode;
	public String strEmail;
	public String strPassword;
//	public String strServeur;
//	public String strProtocol;
	public boolean boolScanMode;
	public ArrayList<String> strArrListIsbn;

	@Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.scan);
        
        //recuperation de la preference de l'adresse email d'envoi
	    SharedPreferences spEmail  = getSharedPreferences("PrefEmail", 0);
	    strEmail= spEmail.getString("email", "");
	    strPassword= spEmail.getString("password", "pvide");
//	    strServeur = spEmail.getString("serveur", "svide");
//	    strProtocol = spEmail.getString("protocol", "protovide");
	    
	    
        // Récupération des parametres
		Bundle extras = getIntent().getExtras();
		if (extras != null) {
			
		// On récupere notre parametre String
		strBareCode = (String) extras.get("bareCode");
		
		// On récupere notre parametre boolean
		boolScanMode = (boolean) extras.getBoolean("ScanMode");
		
		//On récupere notre parametre ArrayList
		strArrListIsbn = (ArrayList<String>) extras.getStringArrayList("listIsbn");
		
		// Récupération du TextView du layout courant
		TextView tvResult = (TextView) this.findViewById(R.id.tvScanResult);
		Button btnScan = (Button) this.findViewById(R.id.btnScan);
		Button btnSend = (Button) this.findViewById(R.id.btnEnvoi);
		
		int debutISBN = Integer.parseInt(strBareCode.substring(0, 3));
		//verification du code barre
		if(debutISBN==978 || debutISBN==979)
		{
		// On affecte la valeur
			tvResult.setText(strBareCode);
			if(boolScanMode==false)
			{
			strArrListIsbn.add(strBareCode);
			btnScan.setVisibility(View.INVISIBLE);			
			btnSend.setVisibility(View.VISIBLE);	
			btnSend.setText("Envoyer le code ISBN à Royal_");
			btnSend.setOnClickListener(btMailListener);
			}
			else
			{
			strArrListIsbn.add(strBareCode);
			btnSend.setVisibility(View.VISIBLE);
			btnSend.setText("Envoyer le code ISBN à Royal_");
			btnSend.setOnClickListener(btMailListener);
			btnScan.setVisibility(View.VISIBLE);			
			btnScan.setText("Effectuer un nouveau scan");
			btnScan.setOnClickListener(btAddScanListener);
			}
		}
		else
		{
		// On signal l'erreur
			tvResult.setText("Votre code barre ne correspond pas à une bande déssinée");
			btnSend.setVisibility(View.INVISIBLE);			
			btnScan.setVisibility(View.VISIBLE);			
			btnScan.setText("Effectuer un nouveau scanner");
			btnScan.setOnClickListener(btReScanListener);
		}
		
	}
		
		
}

	
	public OnClickListener btAddScanListener = new Button.OnClickListener() 
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
	      try { 
	    	Mail mail = new Mail(); 
	    	mail.setUser(strEmail);
	    	mail.setPass(strPassword);
		    mail.setTo(strEmail); 
		    mail.setFrom("Royal_Scanner"); 
		    mail.setSubject("This is an email sent using Royal_Scanner."); 
		    mail.setBody(strArrListIsbn.toString()); 
		    //mail.setPort(strPort);
		    //mail.setSport(strPort);
		    //mail.setHost(strServeur);
	        if(mail.send()) {
	          Toast.makeText(ScanPageActivity.this, "Email envoyé.", Toast.LENGTH_LONG).show(); 
	        } 
	        else { 
	          Toast.makeText(ScanPageActivity.this, "Erreur lors de l'envoi de l'email.", Toast.LENGTH_LONG).show(); 
	        } 
			ScanPageActivity.this.finish();
	      } 
	      catch(Exception e) {} 
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
									intentScanResult.putExtra("bareCode", contents);
									intentScanResult.putExtra("ScanMode", boolScanMode);
									intentScanResult.putExtra("listIsbn", strArrListIsbn);
									startActivity(intentScanResult);
									ScanPageActivity.this.finish();

						      }
					}
					
				
			}
		    
}	