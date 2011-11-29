package com.Royal_Scanner;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.RelativeLayout;

public class FirstPageActivity extends Activity {
    
	
	public OnClickListener btnAboutListener = new OnClickListener() 
	{   
		public void onClick(View v) 
		{   
			// Initialisation du bouton a propos
			
			// Nouveau Intent
			Intent intentRoyalMainPage = new Intent(FirstPageActivity.this, RoyalScannerMainActivity.class);

			// On lance l'Activity
			startActivity(intentRoyalMainPage);
			
			// On termine l'Activity en cours
			FirstPageActivity.this.finish();
		}
	};
	
	/** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.firstscreen);
        
        RelativeLayout rl = (RelativeLayout) this.findViewById(R.id.rLfirtstscreen);
    	
    	rl.setOnClickListener(btnAboutListener);
    }
}