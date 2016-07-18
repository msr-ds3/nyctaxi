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
 * Description: Deanonymize NYC's Taxi and Limousine Company's hack license and medallion data
 * The program generates all possible medallion and hack license numbers, and find their corresponding hash using the MD5 algorithm
 * NOTE: The hashes in the dataset are capitalized, so we return capitalized hashes in this program
 * 
 * @author Jai Punjwani
 * @version 1.0
 */
public class UnhashingTaxiLicenses
{
	 static HashMap<String, String> hashMedallions = new HashMap();
	 static HashMap<String, String> medallions1 = new HashMap();
	 static HashMap<String, String> medallions2 = new HashMap();
	 static HashMap<String, String> sixDigitHackLicenses = new HashMap();
	 static HashMap<String, String> sevenDigitHackLicenses = new HashMap();
	 
    
     public static void main (String[] args)throws NoSuchAlgorithmException, FileNotFoundException
     {
         setUp(); 
    	 
    	 System.out.println("Deserializing. Allow approximately 3-4 minutes");
         //hashMedallions = (HashMap) deserializeData(hashMedallions, hashMedallions.getClass(), "medallions.exclude");
         medallions1 = (HashMap) deserializeData(medallions1, medallions1.getClass(), "medallions1.exclude");
         medallions1 = (HashMap) deserializeData(medallions2, medallions2.getClass(), "medallions2.exclude");
         
    	 sixDigitHackLicenses = (HashMap) deserializeData(sixDigitHackLicenses, sixDigitHackLicenses.getClass(), "hackLicenses6.exclude");
    	 sevenDigitHackLicenses = (HashMap) deserializeData(sevenDigitHackLicenses, sevenDigitHackLicenses.getClass(), "hackLicenses7.exclude");
    	 
    	 
         //EXAMPLES
         String medallionHash = "B12DB2625EBD5A2EBECBCD25AC089928";
         System.out.println(medallionHash + " matches to " + getMedallion(medallionHash));
        
         String licenseHash = "0BB6EDE86969525491B06031E8460F41"; 
         System.out.println(licenseHash + " mapped to " + getLicense(licenseHash));
         
         licenseHash = "CFCD208495D565EF66E7DFF9F98764DA";
         System.out.println(licenseHash + " mapped to " + getLicense(licenseHash));
         
         String license = "2E42";
         System.out.println("license " + license + " hashed to " + MD5(license));  
         
         
         String hackLicense = "493092";
         String hash = MD5(hackLicense);
         System.out.println(hash + " " + hackLicense);
         
           
         String ex = "275572";
         hash = MD5(ex);
         System.out.println(hash);
           
         ex = "5418523";
         hash = MD5(ex);
         System.out.println(hash);
           
     }
    
     /**
      * Generates table of medallions and licenses with corresponding hash and saves them to large files used to load the data
      * NOTE: the .exclude extension should be added to .gitignore file to prevent large file from being stored to GitHub 
      * CAVEAT: allow 6-10 minutes for set up. If you run out of memory, run each method call one at a time
      * @throws NoSuchAlgorithmException
      */
     public static boolean setUp() throws NoSuchAlgorithmException
     {
    	 
    	 System.out.println("Setting up. Allow up to 10 minutes");
    	 
    	 if(!(new File("medallions1.exclude").exists() && new File("medallions2.exclude").exists()))
    	 {
    		 setUpMedallions(); 
    	 }
    	 
    	 if(!(new File("hackLicenses6.exclude").exists()))
    	 {
         setUp6DigitHackLicenses();
    	 }
    	 
    	 if(!(new File("hackLicenses7.exclude").exists()))
    	 {
         setUp7DigitHackLicenses();
    	 }
         
    	 System.out.println("Files created/loaded, setup complete");
    	 return true;
    	 
     }
     
     private static void setUpMedallions() throws NoSuchAlgorithmException
     {
    	 /*
    	 System.out.println("Generating Medallions");
    	 String[] allMedallions = Generate_Licenses.generateMedallionAll(); //currently does not generate 6 digit medallions - commented out TODO: uncomment
    	 System.out.println("Hashing Medallions");
         String[] allHashes = MD5(allMedallions);
         System.out.println("Creating hashmap");
         HashMap<String, String> medallionsHM = generateHashMap(allHashes, allMedallions);
         System.out.println("serializing first half");
         serializeData(medallionsHM, medallionsHM.getClass(), "medallions.exclude");
         */
    	 
    	 String[][] allMedallions = Generate_Licenses.generateMedallionAll(2);
    	 for(int arr =0; arr<allMedallions.length; arr++)
    	 {
    		 String[] medallions = allMedallions[arr];
    		 System.out.println("Hashing Medallions part " + (arr + 1));
             String[] hashes = MD5(medallions);
             System.out.println("Creating hashmap part " + (arr+1));
             HashMap<String, String> medallionsHM = generateHashMap(hashes, medallions);
             System.out.println("serializing part " + (arr +1));
             serializeData(medallionsHM, medallionsHM.getClass(), "medallions" + (arr+1) + ".exclude");
    	 }
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
    	 String medallion =  (String) medallions1.get(hash);
    	 if(medallion!= null)
    	 {
    		 return medallion;
    	 }
    	 
    	 return (String) medallions2.get(hash);
    	
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
      * @return UPPER CASE hexadecimal hash String 
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
          
          return toUpperCase(hash);
         
     }
     
     /**
      * @return String array of UPPER CASE hexadecimal hashes 
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
    		  
}
