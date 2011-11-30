package net.sf.royal.util;
import javax.mail.internet.MimeUtility;
import java.io.*;
import java.util.zip.Deflater;
import java.util.zip.Inflater;

public class Base64Utils {
    public static String encode(String pass) throws Exception {
        ByteArrayOutputStream baos;
        OutputStream b64os;
        byte[] result = pass.getBytes();
        // ENCODING TWICE WITH BASE64
        baos = new ByteArrayOutputStream();
        b64os = MimeUtility.encode(baos, "base64");
        b64os.write(result);
        b64os.close();
        result = baos.toByteArray();  	
        // REVERSING THE ENCODE-STRING
        return new String(result);

     }
     
     public static String decode(String decoder) throws Exception {
    	// REVERSING THE DECOMPRESSED STRING
    	 byte[] b = decoder.getBytes();
         ByteArrayInputStream bais = new ByteArrayInputStream(b);
         InputStream b64is = MimeUtility.decode(bais, "base64");
         byte[] tmp = new byte[b.length];
         int n = b64is.read(tmp);
         byte[] res = new byte[n];
         System.arraycopy(tmp, 0, res, 0, n);
         return new String(res);
     }  

    public static void main(String[] args) throws Exception {

        String test = "plop !";
        String encode = encode(test);
        System.out.println(encode);
        System.out.println(decode(encode));
    }

}