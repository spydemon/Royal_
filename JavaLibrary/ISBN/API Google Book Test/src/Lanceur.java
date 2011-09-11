import java.awt.BorderLayout;
import java.awt.Image;
import java.util.Scanner;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;
import com.google.api.services.books.model.VolumeVolumeInfo;
import com.openly.info.ISBN;
import com.openly.info.InvalidStandardIDException;


public class Lanceur {
	
	public static void main(String [] args){
		System.out.print("Entrez votre ISBN (exemple : 9782012101449) : ");
		Scanner sc = new Scanner(System.in);
		String i = sc.next();
		try {
			ISBN isbn = new ISBN(i);
			GoogleBook gb = new GoogleBook(isbn);
			try {
				gb.execute();
			} catch (GoogleBookException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			VolumeVolumeInfo volumeinfo = gb.getVolumeInfo();
			System.out.println(volumeinfo.getTitle());
			if(volumeinfo.getDescription() != null){
				System.out.println(volumeinfo.getDescription());
			}
			Image cover;
			try {
				cover = gb.get_CoverImage();
				if(cover != null){
					JFrame frame = new JFrame();
					JLabel label = new JLabel(new ImageIcon(cover));
					frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
					frame.getContentPane().add(label, BorderLayout.CENTER);
					frame.pack();
					frame.setVisible(true);
				}
			} catch (ConnectionProblemException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
		} catch (InvalidStandardIDException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
	}			
}
