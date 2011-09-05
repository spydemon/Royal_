package net.sf.royal.gui.util;

import javax.swing.text.PlainDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.AttributeSet;

public class DigitDocument extends PlainDocument
{
	private int maxLength = Integer.MAX_VALUE;
	@Override
	public void insertString(int offs, String str, AttributeSet a) throws BadLocationException
	{
		if(str != null && this.getLength() < this.maxLength)
		{
            char c;
            int posV;

			for(int i = 0; i < str.length(); i++)
            {
                posV = this.getText(0,this.getLength()).indexOf(',');
                c = str.charAt(i);
                if((c == '.' || c == ',') && posV == -1)
                    super.insertString(offs,Character.toString(','),a);
                else if(Character.isDigit(c) && (this.getLength()-posV != 3 || posV == -1))
                    super.insertString(offs,Character.toString(c),a);
                offs++;
            }
		}
	}
	
	public void setMaxLength(int i)
	{
		this.maxLength = i;
	}
}
