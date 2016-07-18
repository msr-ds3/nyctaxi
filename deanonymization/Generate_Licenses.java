/**
 * Description: Generates all possible NYC TLC yellow taxi medallion numbers, as well as driver hack licenses
 * 
 * L = letter ; N = number
 * Medallion formats: NLNN ; LLNNN ; LLLNNN
 * Hack_license format: NNNNNN
 * 
 * NOTE: as of version 1.0, this only returns 4 and 5 character medallions
 * 
 * @author Jai Punjwani
 * @version 1.0
 */

public class Generate_Licenses {
	
	
	public static final char[] DIGITS = {'0','1','2','3','4','5','6','7','8','9'};
	public static final char[] LETTERS = {'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T',
		'U','V','W','X','Y','Z'};
	
	/**
	 * @return array of all licenses in format: NLNN
	 */
	public static String[] generateMedallion1()
	{	
		int numSize = DIGITS.length;
		int totalSize = numSize * LETTERS.length * numSize * numSize;
		String[] licenses = new String[totalSize];
		int licenseIndex =0;
		
		for(int num1 =0; num1<numSize; num1++)
		{
			for(int letter2 =0; letter2 < LETTERS.length; letter2++)
			{
				for(int num3 =0; num3<numSize; num3++)
				{
					for(int num4 =0; num4<numSize; num4++)
					{
						String license = "" + DIGITS[num1] + LETTERS[letter2] + DIGITS[num3] + DIGITS[num4];
						licenses[licenseIndex] = license;
						licenseIndex++;
					}
				}
			}
		}
		
		return licenses;
		
	}
	
	
	/**
	 * 
	 * @return array of all licenses in format LLNNN
	 */
	public static String[] generateMedallion2()
	{
		int totalSize = LETTERS.length * LETTERS.length * DIGITS.length * DIGITS.length * DIGITS.length;
		String[] licenses = new String[totalSize];
		int licenseIndex =0;    
		for(int letter1=0; letter1<LETTERS.length; letter1++)
		{
			for(int letter2=0; letter2<LETTERS.length; letter2++)
			{
				for(int num3 =0; num3<DIGITS.length; num3++)
				{
					for(int num4=0; num4<DIGITS.length; num4++)
					{
						for(int num5=0; num5<DIGITS.length; num5++)
						{
							String license = "" + LETTERS[letter1] + LETTERS[letter2] + DIGITS[num3] + DIGITS[num4] + DIGITS[num5];
							licenses[licenseIndex] = license;
							licenseIndex++;
						}
					}
				}
			}
			
		}
		
		return licenses;
		
	}
	
	/**
	 * 
	 * @return array of all licenses in format LLLNNN
	 */
	public static String[] generateMedallion3()
	{
		int totalSize = LETTERS.length * LETTERS.length * LETTERS.length * DIGITS.length * DIGITS.length * DIGITS.length;
		String[] licenses = new String[totalSize];
		int licenseIndex =0;    
		for(int letter1=0; letter1<LETTERS.length; letter1++)
		{
			for(int letter2=0; letter2<LETTERS.length; letter2++)
			{
				for(int letter3 =0; letter3<LETTERS.length; letter3++)
				{
					for(int num4 =0; num4<DIGITS.length; num4++)
					{
						for(int num5=0; num5<DIGITS.length; num5++)
						{
							for(int num6=0; num6<DIGITS.length; num6++)
							{
								String license = "" + LETTERS[letter1] + LETTERS[letter2] + LETTERS[letter3] + DIGITS[num4] + DIGITS[num5] + DIGITS[num6];
								licenses[licenseIndex] = license;
								licenseIndex++;
							}
						}
					}
				}
				
			}
			
		}
		
		return licenses;
		
	}
	
	public static String[] generateMedallionAll()
	{
		String[] format1 = generateMedallion1();
		String[] format2 = generateMedallion2();
		String[] format3 = generateMedallion3();
		
		int format1Len = format1.length;
		int format2Len = format2.length;
		int format3Len = format3.length;
		int length = format1Len+format2Len+format3Len;
		
		String[] allMedallions = new String[length];
		int allMedallionsIndex =0;
		
		for(int i =0; i<format1Len; i++)
		{
			allMedallions[allMedallionsIndex] = format1[i];
			allMedallionsIndex++;
		}
		
		for(int i =0; i<format2Len; i++)
		{
			allMedallions[allMedallionsIndex] = format2[i];
			allMedallionsIndex++;
		}
		
		
		for(int i =0; i<format3Len; i++)
		{
			allMedallions[allMedallionsIndex] = format3[i];
			allMedallionsIndex++;
		}
		
		return allMedallions;
		
	}
	
	/**
	 * 
	 * @param medallionArrays - number of arrays to split medallions into
	 */
	public static String[][] generateMedallionAll(int numArrays)
	{
		String[] medallions = generateMedallionAll();
		int medallionSize = medallions.length;
		int medallionIndex =0;
		int arrSize = (medallions.length/numArrays) + 1;
		String[][] medallionArrays = new String[numArrays][arrSize];
		
		outerloop:
		for(int array =0; array<numArrays; array++)
		{
			for(int i =0; i<medallionArrays[array].length; i++)
			{
				medallionArrays[array][i] = medallions[medallionIndex];
				medallionIndex++;
				if(medallionIndex == medallionSize)
				{
					break outerloop;
				}
			}
			
		}
		
		return medallionArrays;
		
	}
	
	
	/**
	 * @return array of possible hack licenses - NOTE: 6-digit and 7-digit possible
	 */
	public static String[] generateHackLicenses()
	{
		String[] sixDigitLicenses = generate6DigitHackLicenses();
		String[] sevenDigitLicenses = generate7DigitHackLicenses();
		int totalLength = sixDigitLicenses.length + sevenDigitLicenses.length;
		
		String[] hackLicenses = new String[totalLength];
		int licenseIndex =0;
		
		for(int i=0; i<sixDigitLicenses.length; i++)
		{
			hackLicenses[licenseIndex] = sixDigitLicenses[i];
			licenseIndex++;
		}
		
		for(int i=0; i<sevenDigitLicenses.length; i++)
		{
			hackLicenses[licenseIndex] = sevenDigitLicenses[i];
			licenseIndex++;
		}
		
		return hackLicenses;
	}
	
	/**
	 * 
	 * @return string array of all possible 6 digit license numbers
	 */
	public static String[] generate6DigitHackLicenses()
	{
		int numLength = DIGITS.length;
		int totalSize = numLength * numLength * numLength * numLength * numLength * numLength;
		String[] hackLicenses = new String[totalSize];
		int licenseIndex = 0;
		
		for(int num1 =0; num1<numLength; num1++)
		{
			for(int num2 =0; num2<numLength; num2++)
			{
				for(int num3=0; num3<numLength; num3++)
				{
					for(int num4=0; num4<numLength; num4++)
					{
						for(int num5=0; num5<numLength; num5++)
						{
							for(int num6=0; num6<numLength; num6++)
							{
									String license = "" + DIGITS[num1] + DIGITS[num2] + DIGITS[num3] + DIGITS[num4] + DIGITS[num5] + DIGITS[num6];
									hackLicenses[licenseIndex] = license;
									licenseIndex++;
							
							}
						}
					}
				}
			}
		}
		
		return hackLicenses;
	}
	
	/**
	 * 
	 * @return string array of all possible 7 digit license numbers
	 */
	public static String[] generate7DigitHackLicenses()
	{
		int numLength = DIGITS.length;
		int totalSize = numLength * numLength * numLength * numLength * numLength * numLength *numLength;
		String[] hackLicenses = new String[totalSize];
		int licenseIndex = 0;
		
		for(int num1 =0; num1<numLength; num1++)
		{
			for(int num2 =0; num2<numLength; num2++)
			{
				for(int num3=0; num3<numLength; num3++)
				{
					for(int num4=0; num4<numLength; num4++)
					{
						for(int num5=0; num5<numLength; num5++)
						{
							for(int num6=0; num6<numLength; num6++)
							{
								for(int num7 =0; num7<numLength; num7++)
								{
									String license = "" + DIGITS[num1] + DIGITS[num2] + DIGITS[num3] + DIGITS[num4] + DIGITS[num5] + DIGITS[num6] + DIGITS[num7];
									hackLicenses[licenseIndex] = license;
									licenseIndex++;
								}
								
							}
						}
					}
				}
			}
		}
		
		return hackLicenses;
	}
	
	
	
	private static void generate_letters()
	{
		String letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
		String s = "";
		for(int i = 0; i<letters.length(); i++)
		{
			s = s + "'" + letters.charAt(i) + "',";
		}
		System.out.println(s);
	}

}
