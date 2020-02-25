package dev.pacwomen.utils;

import java.io.IOException;
import java.io.BufferedReader;
import java.io.FileReader;

/**
 * Utilities to be used if needed for the rest of the classes. 
 */
public class Utils {

    /**
     * Loads a file as a string. Requires a path to the file. 
     * @param path, a path to a file
     * @return String
     */
    public static String loadFileAsString(String path){
	StringBuilder builder = new StringBuilder();
	try{
	    BufferedReader br = new BufferedReader(new FileReader(path));
	    String line;
	    while((line = br.readLine()) != null)
		builder.append(line + "\n");

	    br.close();

	}catch(IOException e){
	    e.printStackTrace();
	}

	return builder.toString();
    }

    /**
     * Parses a string number to an int number
     * @param number, a string that is a number
     * @return int, the number as a int
     */
    public static int parseInt(String number){
	try{
	    return Integer.parseInt(number);
	}catch(NumberFormatException e){
	    e.printStackTrace();
	    return 0;
	}
    }
	
}
