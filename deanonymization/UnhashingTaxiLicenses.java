import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Scanner;
import java.util.ArrayList;
import java.util.HashMap;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

/**
 * Hashing Assignment:
 * Determine which hashing algorithm was used for each value
   Determine what inputs to the algorithm were used to compute each hash.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
public class UnhashingTaxiLicenses
{
	 static HashMap<String, String> hashMedallions = new HashMap();
	 static HashMap<String, String> sixDigitHackLicenses = new HashMap();
	 static HashMap<String, String> sevenDigitHackLicenses = new HashMap();
	 
    /**
     * 
     * NOTE: UNCOMMENT firstTimeSetUp() to set up large hashMap files 
     * 
     * 
     */
     public static void main (String[] args)throws NoSuchAlgorithmException, FileNotFoundException
     {
         //firstTimeSetUp();
    	 
    	 System.out.println("Deserializing");
         hashMedallions = (HashMap) deserializeData(hashMedallions, hashMedallions.getClass(), "medallions.exclude");
    	 sixDigitHackLicenses = (HashMap) deserializeData(sixDigitHackLicenses, sixDigitHackLicenses.getClass(), "hackLicenses6.exclude");
    	 sevenDigitHackLicenses = (HashMap) deserializeData(sevenDigitHackLicenses, sevenDigitHackLicenses.getClass(), "hackLicenses7.exclude");
    	 
    	 
         //EXAMPLES
         String medallionHash = "B12DB2625EBD5A2EBECBCD25AC089928";
         System.out.println(medallionHash + " matches to " + getMedallion(medallionHash));
        
         String licenseHash = "0BB6EDE86969525491B06031E8460F41"; 
         System.out.println(licenseHash + " mapped to " + getLicense(licenseHash));
         
         String license = "2E42";
         System.out.println("license " + license + " hashed to " + MD5(license));  
         
         
         String hackLicense = "493092";
         String hash = MD5(hackLicense);
         System.out.println(toUpperCase(hash) + " " + hackLicense);
           
           
         String ex = "275572";
         String x = MD5(ex);
         System.out.println(toUpperCase(x));
           
         ex = "5418523";
         x = MD5(ex);
         System.out.println(toUpperCase(x));
           
     }
    
     /**
      * Generates table of medallions and licenses with corresponding hash and saves them to large files used to load the data
      * NOTE: the .exclude extension should be added to .gitignore file to prevent large file from being stored to cloud storage 
      * CAVEAT: allow 6-10 minutes for set up. If you run out of memory, run each method call one at a time
      * @throws NoSuchAlgorithmException
      */
     public static void firstTimeSetUp() throws NoSuchAlgorithmException
     {
    	 setUpMedallions();
         setUp6DigitHackLicenses();
         setUp7DigitHackLicenses();
    	 
     }
     
     private static void setUpMedallions() throws NoSuchAlgorithmException
     {
    	 System.out.println("Generating Medallions");
    	 String[] allMedallions = Generate_Licenses.generateMedallionAll(); //currently does not generate 6 digit medallions - commented out TODO: uncomment
    	 System.out.println("Hashing Medallions");
         String[] allHashes = MD5(allMedallions);
         System.out.println("Creating hashmap");
         HashMap<String, String> medallionsHM = generateHashMap(allHashes, allMedallions);
         System.out.println("serializing");
         serializeData(medallionsHM, medallionsHM.getClass(), "medallions.exclude"); 
     }
     
     private static void setUp6DigitHackLicenses() throws NoSuchAlgorithmException
     {
    	 System.out.println("Generating 6 digit hack licenses");
         String[] hackLicenses6 = Generate_Licenses.generate6DigitHackLicenses();
         System.out.println("Hashing");
         String[] hackLicenseHashes = MD5(hackLicenses6);
         System.out.println("Generating hashmap");
         HashMap<String, String> hackLicenses6HM = generateHashMap(hackLicenseHashes, hackLicenses6);
         System.out.println("serializing");
         serializeData(hackLicenses6HM,hackLicenses6HM.getClass(), "hackLicenses6.exclude"); 
     }
     
     private static void setUp7DigitHackLicenses() throws NoSuchAlgorithmException
     {
    	 System.out.println("Generating 7 digit hack licenses");
         String[] hackLicenses7 = Generate_Licenses.generate7DigitHackLicenses();
         System.out.println("Hashing");
         String[] hackLicenseHashes = MD5(hackLicenses7);
         System.out.println("Generating hashmap");
         HashMap<String, String> hackLicenses7HM = generateHashMap(hackLicenseHashes, hackLicenses7);
         System.out.println("serializing");
         serializeData(hackLicenses7HM,hackLicenses7HM.getClass(), "hackLicenses7.exclude");
    	 
     }
     
     public static String getMedallion(String hash)
     {
    	 return (String) hashMedallions.get(hash);
    	
     }
     
     public static String getLicense(String hash)
     {
    	 String license = (String) sixDigitHackLicenses.get(hash);
    	 if(license != null)
    	 {
    		 return license; 
    	 }
    	 
    	 return (String) sevenDigitHackLicenses.get(hash);
    	 
     }
     public static HashMap<String, String> generateHashMap(String[] keys, String[] values)
     {
    	 int size = keys.length;
    	 HashMap<String, String> hm = new HashMap();
    	 
    	 for(int i=0; i<size; i++)
    	 {
    		 
    		 hm.put(toUpperCase(keys[i]), values[i]);
    	 }
    	 return hm;
    	 
     }
     
     //hashes in our dataset are all upper case, so we do the same for 
     public static String toUpperCase(String hash)
     {
    	 
    	 int size = hash.length();
    	 String newHash = "";
    	 for (int i =0; i<size; i++)
    	 {
    		 char character = hash.charAt(i);
    		 if(isNumber(character))
    		 {
    			 newHash = newHash + character;
    		 }
    		 else
    		 {
    			 String letter = "" + character;
    			 letter = letter.toUpperCase();
    			 newHash = newHash + letter;
    		 }
    		 
    	 }
    	 return newHash;
    	 
     }
     
     private static boolean isNumber(char c)
     {
    	 for(int i =0; i<Generate_Licenses.DIGITS.length; i++)
    	 {
    		 if(Generate_Licenses.DIGITS[i] == c)
    		 {
    			 return true;
    		 }
    	 }
    	 return false;
     }
    
     
     /**
      * @return hexadecimal hash String 
      */
     public static String MD5(String str)throws NoSuchAlgorithmException
     {
    	 
            MessageDigest md = MessageDigest.getInstance("MD5");
            md.reset();
            byte[] buffer = str.getBytes();
            
            md.update(buffer);
            byte[] digest = md.digest();
          
            //http://stackoverflow.com/questions/6120657/how-to-generate-a-unique-hash-code-for-string-input-in-android
            //converts byte array to hexadecimal string
            String hexStr = "";
            for (int i = 0; i < digest.length; i++) {
                hexStr +=  Integer.toString( ( digest[i] & 0xff ) + 0x100, 16).substring( 1 );
            }
            
            String hash = hexStr;
          
          return hash;
         
     }
     
     /**
      * @return String array of hexadecimal hashes 
      */
     public static String[] MD5(String[] licenses)throws NoSuchAlgorithmException
     {
     
    	 int size = licenses.length;
    	 String[] hashes = new String[size];
         MessageDigest md = MessageDigest.getInstance("MD5");
         int hashIndex =0;
         
         
         for(int j=0; j<licenses.length; j++)
         {
            hashes[hashIndex] = MD5(licenses[j]);
            hashIndex++;
          }
          
          return hashes;
       
         
     }
     
    
      public static void serializeData(Object object, Class<?> cls, String filepath)
      { 
    	  try
          {
             FileOutputStream fileOut = new FileOutputStream(filepath);
             ObjectOutputStream out = new ObjectOutputStream(fileOut);
             out.writeObject(object);
             out.close();
             fileOut.close();
             System.out.println("Serialized data is saved as " + filepath);
          }
    	  catch(IOException i)
          {
              i.printStackTrace();
          }
    	  
    	  
      }
      
      public static Object deserializeData(Object input,Class<?> cls, String filepath)
      {
    	  try
          {
             FileInputStream fileIn = new FileInputStream(filepath);
             ObjectInputStream in = new ObjectInputStream(fileIn);
             input = in.readObject();
             in.close();
             fileIn.close();
          }
    	  
    	  catch(IOException i)
          {
             i.printStackTrace();
             return null;
          }
    	  
    	  catch(ClassNotFoundException c)
          {
             System.out.println("Class not found");
             c.printStackTrace();
             return null;
          }
    	  
    	  return input;
    	  
      }
    		  
     
     /**
      * Previously used to read in dictionary from file and generate formatted list to construct array
      * 
      */
      public static void readInWords() throws FileNotFoundException
     {
         Scanner reader = new Scanner(new File("hash_dictionary.txt")); 
       ArrayList<String> words = new ArrayList<String>();
       int size =0;
       //array[]
       while(reader.hasNext())
       {
         words.add(reader.nextLine());
         size++;
       }
       System.out.println("size: " + size);
       int i =0;
       System.out.print("{");
       for(i=0; i<words.size()-1; i++)
       {
        System.out.print("\"" + words.get(i) + "\",");   
       }
       System.out.print("\"" + words.get(i) + "\"" + "}");
       System.out.println();
     }
}
