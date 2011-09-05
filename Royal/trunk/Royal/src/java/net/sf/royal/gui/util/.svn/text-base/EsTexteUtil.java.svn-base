package net.sf.royal.gui.util;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import estexte.TextProcessor;

public class EsTexteUtil {

    public static String getHTML(String estexte) {
        if (estexte == null){
            return null;
        }
        ByteArrayInputStream bais = new ByteArrayInputStream(estexte.getBytes());
        TextProcessor processor = new TextProcessor(bais,".");
        try {
            processor.process();
            return processor.content();
        } catch (IOException e) {
            e.printStackTrace();
            return "EStexte error : " + e.getMessage();
        }
    }
}
