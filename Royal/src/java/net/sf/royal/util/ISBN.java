package net.sf.royal.util;


/**
 * ISBN wraps a String interpreted as an ISBN. It checks the checksum, too. supports both 10 digit and 13 digit ISBN
 * @author Eric Hellman
 *
 *  Copyright 2000-2005  by Openly Informatics, Inc.
 *  http://www.openly.com/
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free
 *  Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 *  MA 02111-1307, USA
 *@version 
 *<ul>
 *<li>2005-10-25 checkDigit13 and checkDigit were getting it completely wrong! Added a constructor with a parameter that sets whether or not the class tries to fix the isbn.
 *</ul>
 *
 */
public class ISBN implements StandardID {
	private static final String NUMVALUES = "0123456789X";

	private String value;
	private String value13;
	
	/**
	* the constructor requires a valid isbn string; dashes and spaces are ignored.
	*/
	public ISBN(String isbn)
		throws InvalidStandardIDException{
		init (isbn,false);
	}
	/**
	* the constructor requires a valid isbn string; dashes and spaces are ignored.
	*@param fix if true, constructor will try to fix bad check digits, otherwise will throw exception for bad isbn
	*/
	public ISBN(String isbn, boolean fix)
		throws InvalidStandardIDException{
		init(isbn,fix);
	}
	
	private void init(String isbn, boolean fix)
		throws InvalidStandardIDException{	
		if (isbn.length()<10) throw new InvalidStandardIDException("ISBN length<10");
		isbn=isbn.toUpperCase().trim();
		
		//remove all but numbers, X
		char[] theChars =isbn.toCharArray();
		int i; 
		int val=0;
		StringBuffer sb= new StringBuffer();
		for ( i=0; i<theChars.length; i++){
			val=NUMVALUES.indexOf(theChars[i]);
			if (val >=0 ){
				//valid character
				sb.append(theChars[i]);
			} else {
				//invalid character
				if (!fix && theChars[i]!=' ' &&theChars[i]!='-') throw new InvalidStandardIDException("invalid char");
			}
		}
		isbn=sb.toString();

		theChars =isbn.toCharArray();
		if (theChars.length<9) throw new InvalidStandardIDException("ISBN length<9");
		if (theChars.length>13) throw new InvalidStandardIDException("ISBN length>13");
		if (theChars.length==11) throw new InvalidStandardIDException("ISBN length=11");
		if (theChars.length==10) {
			//10 digit isbn
			int checksum=0;
			int weight=10;
			//compute checksum
			val=0;
			for ( i=0; weight>0 ; i++){
				val=NUMVALUES.indexOf(theChars[i]);
				if (val >=0 ){
					//valid character
					if (val==10 && weight!=1) throw new InvalidStandardIDException("X in a bad place");

					checksum=checksum+weight*val;
					weight--;
				} else {
					//invalid char
					// should never happen
					throw new InvalidStandardIDException("invalid char");
				}
			}
			if ((checksum%11)!=0){
				//bad checksum
				if (fix){
					value =isbn.substring(0,9)+checkDigit(isbn.substring(0,9));
				} else 
					throw new InvalidStandardIDException("bad checksum-10");
			} else 
				value=isbn;
				
			//make 13 digit isbn
			value13="978"+value.substring(0,9)+checkDigit13("978"+value.substring(0,9));
			
			
		} else if (theChars.length==13) {
			if (!isbn.startsWith("978") && !isbn.startsWith("979") ){
				throw new InvalidStandardIDException("13 digit isbn must start with 978 or 979");
			}
			int checksum13=0;
			int weight13=1;
			//compute checksum
			val=0;
			for ( i=0; i<13; i++){
				val=NUMVALUES.indexOf(theChars[i]);
				if (val >=0 ){
					//valid character
					if (val==10 ) throw new InvalidStandardIDException("X not valid in ISBN 13");
					checksum13=checksum13+weight13*val;
					weight13=(weight13+2)%4;
				} else {
					//invalid char
					//should never occur
					throw new InvalidStandardIDException("invalid char");
				}
			}
			if ((checksum13%10)!=0) {
				//bad checksum
				if (fix){
					value13 =isbn.substring(0,12)+checkDigit13(isbn.substring(0,12));
				} else 
					throw new InvalidStandardIDException("bad checksum-13");
			} else {
				value13=isbn;
			}
			if (value13.startsWith("978")){
				//make 10 digit version
				
				value =value13.substring(3,12)+checkDigit(value13.substring(3,12));
			} else {
				value=null;
			}
		} else if  (theChars.length==9) {
			if (fix){
				value =isbn.substring(0,9)+checkDigit(isbn.substring(0,9));
				value13="978"+value.substring(0,9)+checkDigit13("978"+value.substring(0,9));
			} else 
				throw new InvalidStandardIDException("9-digit isbn");
		} else if  (theChars.length==12) {
			if (!isbn.startsWith("978") && !isbn.startsWith("979") ){
				throw new InvalidStandardIDException("13 digit isbn must start with 978 or 979");
			}
			if (fix){
				value =isbn.substring(3,12)+checkDigit(isbn.substring(3,12));
				value13=value.substring(0,12)+checkDigit13(value.substring(0,12));
			} else 
				throw new InvalidStandardIDException("12-digit isbn");
		} 
			
			
	}
	
	/** return 10-digit ISBN unless 979
	*/
	public String toString(){
		return toString(false);
	}
	
	/** return 13-digit ISBN unless 979 or isbn13 is false
	*/
	public String toString(boolean isbn13){
		if( isbn13 || value==null){
			return value13;
		} else {
		// try to return isbn-10
			return value;
		}
	}
	/** returns a isbn checkdigit for the first 9 digits in a string
	*/
	public static String checkDigit(String isbn){

		char[] theChars =isbn.toCharArray();
		int checksum=0;
		int weight=10;
		//clean the string, compute checksum
		int i; int val;
		for ( i=0; (i<theChars.length) && (weight>1 ); i++){
			val=NUMVALUES.indexOf(theChars[i]);
			if (val >=0 ){
				//valid character
				if (val<10){ //not a dash
					checksum=checksum+weight*val;
					weight--;
				}
			} 
		}
		if (checksum%11==0) return "0";
		return NUMVALUES.substring(11-checksum%11,12-checksum%11);
	}
	
	/** returns a isbn checkdigit for the first 12 digits in a string
	*/
	public static String checkDigit13(String isbn){

		char[] theChars =isbn.toCharArray();
		int checksum13=0;
		int weight13=1;
		//clean the string, compute checksum
		int i; int val;
		for ( i=0; (i<theChars.length && i<12); i++){
			val=NUMVALUES.indexOf(theChars[i]);
			if (val >=0 ){
				//valid character
				if (val<10){ //not a dash
					checksum13=checksum13+weight13*val;
					weight13=(weight13+2)%4;
				}
			} 
		}
		if (checksum13%10==0) return "0";
		return NUMVALUES.substring(10-checksum13%10,11-checksum13%10);
	}
	
	/**
	* @return "ISBN"
	*/
	public String IDName(){
		return "ISBN";
	}


    /** test for equality
    */
    public boolean equals(ISBN isbn)
    {
        return (value13.equals(isbn.value13));
    }
    
    /** test Strings for equality
    */
    public boolean equals(String isbnString)
    {
        try
        {
            ISBN isbn = new ISBN(isbnString);
            return equals(isbn);
        }
        catch (InvalidStandardIDException e)
        {
            return false;
        }
    }
    
    public static void main(String[] args)  {	
		if(args.length==0){
			//help message
			System.out.println("command line: java com.openly.info.ISBN [ISBN to normalize or correct]");
		} else if (!args[0].startsWith("t")){
			//normalize an isbn
			ISBN test=null;
			try{
				test = new ISBN(args[0],true);
				System.out.println(test.toString() );
			}catch(Exception e){
				System.out.println(e);
			}
			
		} else {
			//test mode
			System.out.println("doing code test");
		
			//test code
			System.out.print(" testing 0-8436-1072-7 (good): ");
			try{
				ISBN good= new ISBN("0-8436-1072-7");
				System.out.println(good );
				System.out.print(" testing 0-8436-1072-7 (13): ");
				System.out.println(good.toString(true) );
				ISBN good13=new ISBN(good.toString(true) );
				System.out.println("13->10:" );
				System.out.println(good13 );
				System.out.println(good13.toString(true) );
			}catch(Exception e){
				System.out.println("bad");
			}
			System.out.print(" testing 0-8436-1072-X (bad): ");
			try{
				System.out.println(new ISBN("0-8436-1072-X"));
			}catch(Exception e){
				System.out.println("bad");
			}
			System.out.print(" testing 0-8436-1077-X (bad): ");
			try{
				System.out.println(new ISBN("0-8436-1077-X"));
			}catch(Exception e){
				System.out.println("bad");
			}
			System.out.print(" testing 0-8436-x072-7 (bad): ");
			try{
				System.out.println(new ISBN("0-8436-x072-7"));
			}catch(Exception e){
				System.out.println("bad");
			}
			System.out.print(" testing  0940016737 (good): ");
			try{
				ISBN good= new ISBN(" 0940016737");
				System.out.println(good );
				System.out.print(" testing 0940016737 (13): ");
				System.out.println(good.toString(true) );
			
			}catch(Exception e){
				System.out.println("bad");
			}
			System.out.print(" testing  x0843610727 (bad): ");
			try{
				System.out.println(new ISBN("x0843610727"));
			}catch(Exception e){
				System.out.println("bad");
			}
			System.out.print(" testing  08436107278 (bad): ");
			try{
				System.out.println(new ISBN("08436107278"));
			}catch(Exception e){
				System.out.println("bad");
			}
			System.out.print(" testing 9780901 69066 1 (good): ");
			try{
				ISBN good= new ISBN("9780901 69066 1");
				System.out.println(good );
				System.out.print(" testing 9780901 69066 1 (13): ");
				System.out.println(good.toString(true) );
			}catch(Exception e){
				System.out.println(e);
			}
			System.out.print(" testing  9790901 69066 1 (bad): ");
			try{
				System.out.println(new ISBN("9790901 69066 1"));
			}catch(Exception e){
				System.out.println("bad");
			}
			System.out.print(" testing 978 1565 923928 (good): ");
			try{
				ISBN good= new ISBN("978 1565 923928");
				System.out.println(good );
				System.out.print(" testing  978 1565 923928 (13): ");
				System.out.println(good.toString(true) );
			}catch(Exception e){
				System.out.println(e);
			}

		}
	}


}

