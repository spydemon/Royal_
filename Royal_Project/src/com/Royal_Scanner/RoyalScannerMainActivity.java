package com.Royal_Scanner;

import java.util.ArrayList;

import com.Royal_Scanner.RoyalScannerMainActivity;
import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.animation.TranslateAnimation;
import android.widget.Button;

public class RoyalScannerMainActivity extends Activity {
    /** Main de l'application */
	
	public String strAdresseEmail; 
	public boolean boolScanMode;
	public ArrayList<String> strArrListIsbn = new ArrayList<String>();

	
	public OnClickListener btnScanListener = new Button.OnClickListener() 
	{   
		public void onClick(View v) 
		{
			boolScanMode = false;
			
			// Initialisation du scan 
			Intent intentScan = new Intent("com.Royal_Scanner.SCAN");
			
			// On lance l'Activity de scan
			startActivityForResult(intentScan, 0);	
		}
	};
	
	public OnClickListener btnScanListListener = new Button.OnClickListener() 
	{   
		public void onClick(View v) 
		{   	
			boolScanMode = true;
			
			// Initialisation du scan
			Intent intentScan = new Intent("com.Royal_Scanner.SCAN");
			
			// On lance l'Activity de scan
			startActivityForResult(intentScan, 0);			
		}
	};
	
	public OnClickListener btnSetupEmailListener = new Button.OnClickListener() 
	{   
		public void onClick(View v) 
		{   
					      
			// Initialisation de la configuration de l'email
			Intent intentSetupEmail = new Intent(RoyalScannerMainActivity.this, SetupMailActivity.class);

			// On lance l'Activity de configuration de l'email
			startActivity(intentSetupEmail);
		}
	};	
	
	public OnClickListener btnAboutListener = new Button.OnClickListener() 
	{   
		public void onClick(View v) 
		{   
			// Initialisation du bouton a propos
			Intent intentAbout = new Intent(RoyalScannerMainActivity.this, AboutActivity.class);

			// On lance l'Activity a propos
			startActivity(intentAbout);
		}
	};
	
	@Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);

        //recuperation de la preference de l'adresse email d'envoi
	    SharedPreferences spAdresseEmail  = getSharedPreferences("PrefEmail", 0);
	    strAdresseEmail= spAdresseEmail.getString("email", "");
        
        
        //Récupération du bouton "Account" du layout courant
        Button btnScan = (Button) this.findViewById(R.id.btnScan);
        Button btnScanList = (Button) this.findViewById(R.id.btnScanList);
        Button btnSetupEmail = (Button) this.findViewById(R.id.btnSetupMail);
        Button btnAbout = (Button) this.findViewById(R.id.btnAbout);
        
        // On définit l'evenement click sur le bouton
        btnScan.setOnClickListener(btnScanListener);
        btnScanList.setOnClickListener(btnScanListListener);
        btnSetupEmail.setOnClickListener(btnSetupEmailListener);
        btnAbout.setOnClickListener(btnAboutListener);
        
        // Gestion de l'animation
        TranslateAnimation trans1 = new TranslateAnimation(0, 0, -300, 0);
        trans1.setStartOffset(320);
        trans1.setFillAfter(true);
        trans1.setDuration(1000);
        btnScan.startAnimation(trans1);
        btnScanList.startAnimation(trans1);
        btnSetupEmail.startAnimation(trans1);
        btnAbout.startAnimation(trans1);
        
	}

	@Override
	public void onBackPressed() {
		
		// action on back button pressed.
		AlertDialog adQuit = new AlertDialog.Builder(this).create();  
	    adQuit.setMessage(getString(R.string.adQuit_message));  
	    adQuit.setButton(getString(R.string.btnNo), new DialogInterface.OnClickListener() {  
	      public void onClick(DialogInterface dialog, int which) {  
	 	     return;
	    } });   
	    adQuit.setButton2(getString(R.string.btnYes), new DialogInterface.OnClickListener() {  
		   public void onClick(DialogInterface dialog, int which) {  
		 	  finish();
		} });
	    adQuit.show();
	}
	
	// gestion du scan
	public void onActivityResult(int requestCode, int resultCode, Intent intentscan) {
		String contents;
		if (requestCode == 0) {
			      if (resultCode == RESULT_OK) {
			         contents = intentscan.getStringExtra("SCAN_RESULT");
//			         String format = intentscan.getStringExtra("SCAN_RESULT_FORMAT");
//			         Handle successful scan  
			         
			            // Envoi du resultat et affichage de la page details
						Intent intentcb = new Intent(RoyalScannerMainActivity.this,ScanPageActivity.class);
						intentcb.putExtra("barCode", contents);
						intentcb.putExtra("scanMode", boolScanMode);
						intentcb.putExtra("listIsbn", strArrListIsbn);
						startActivity(intentcb);
			      }
		}
		
	}
}
	