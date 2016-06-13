||Date: April 2016 
|| Author: Jasmien Cels
|| Purpose: Miranda Exercises from Thompson Chapter Two


sales :: num -> num
maxi n m = n ,if n >= m
	= m , otherwise
allEqual a b = (a=b) 

||Define a function to find the week in which maximum sales occured during weeks 0 to n. 
sales 0 = 0 
sales 1 = 0
sales 2 = 200
sales 3 = 300
sales 4 = 200
sales 5 = 0
sales 6 = 90 

|| This function return the maxSales

maxSales n 
	= sales 0 ,if n=0
	= maxi (maxSales (n-1)) (sales n) ,otherwise

||This function return the week where the sales were highest

maxSalesWeek n 
	= 0 ,if n=0
	= maxi (maxSalesWeek (n-1)) (n) ,otherwise

||Define a function to find a week in which there are zero sales in weeks 0 to n
||if no week has zero sales, it should return n+1

zeroSales n
	= (n+1) ,if (sales 0 ~= 0) & (sales n ~= 0) & (sales(n-1) ~= 0) 
	= 0 ,if (sales 0 = 0)
	= n ,if (sales n = 0) 
	= zeroSales (n-1) ,otherwise

|| Define a function that returns the number of weeks during weeks 0 to n that had zero sales.

zeroSalesCount n
	=  0 ,if n=0 & (sales 0 ~= 0)
	=  1 , if n =0 & (sales 0 = 0)
	= zeroSalesCount (n-1) + 1 , if (sales n = 0) 
	= zeroSalesCount (n-1) ,otherwise

||Define a function (using the above code)which returns the number of weeks that returned s. 

sSalesCount n s 
	= 0 ,if n = 0 & (sales 0 ~=s)
	= 1 ,if n = 0 & (sales 0 = s)
	= sSalesCount (n-1) s + 1 , if (sales n = s) 
	= sSalesCount (n-1) s ,otherwise


||Write a function definition fib so that fib n is the nth number in the sequence 

fib 0 =0
fib 1 = 1
fib (n+2) = fib n + fib (n+1)

||Function k to the power of n

power::num -> num -> num 

power k n 
	= 1 ,if (n=0) 
	= k ,if (n=1) 
	= k * power k (n-1) ,otherwise

|| Define a factorial function using pattern matching 

factorial :: num -> num 

factorial 0 = 1
factorial n = n * factorial (n-1)

|| Can also be written as: 

facc :: num -> num 

facc 0 = 1
facc (n+1) = facc n  * (n+1) 

|| Function to check if any of the sales week 0 to n had zero sales 

isZeroWeek :: num -> bool 

isZeroWeek n = (sales n = 0) 

zeroInPeriod :: num -> bool 
zeroInPeriod 0 = (sales 0 = 0) 
zeroInPeriod n = zeroInPeriod (n-1) \/ (sales n = 0) 


||Give a definition of the nAnd function, which returns True except when the two arguments are both true

nAnd :: bool -> bool -> bool 

nAnd x y = ~(x & y )

||Give a function definition for allZeroPeriod so that allZeroPeriod n tests whether the sales for every week in the period 0 to n are zero 

allZeroPeriod :: num -> bool 

allZeroPeriod 0 = (sales 0 = 0) 
allZeroPeriod n = (sales n =0) & allZeroPeriod (n-1)

|| Give a function definition for isAbovePeriod, which returns True if at least one of the sales in week 0 to n exceeds the value target. 

isAbovePeriod :: num -> num -> bool  

isAbovePeriod target 0 = (sales 0 > target) 
isAbovePeriod target n = isAbovePeriod target (n-1) \/ (sales n > target)

|| Give a function definition for allAbovePeriod, which checks whether sales exceed the target in all weeks

allAbovePeriod :: num -> num -> bool 

allAbovePeriod target 0 = (sales 0 > target)
allAbovePeriod target n = allAbovePeriod target (n-1) & (sales n > target)

||Give a function definition for numEqualMax so that n m and p are euqal to the maximum of the three 

max3 :: num -> num -> num -> num
max3 a b c = a, if (a >= b) & (a >= c) 
	   = b, if (b >= a) & (b >= c) 
	   = c, otherwise

numEqualMax :: num -> num -> num -> num 

numEqualMax a b c = 3 ,if (a = b=c)
		  = 2, if ((a =b) \/ (a=c) \/ (b=c))
		  = 1, otherwise 

|| Define a function to convert small letters to capital letters. 

offset = code 'A' - code 'a'
capital :: char -> char 
capital l = decode (code l + offset)

|| Define a function that converts a char representation of a number to a number and leave all other values alone 

charToNum :: char -> num 
charToNum n = 0, if (code n <=47) \/ (code n >= 58)
charToNum n = code n - 48, otherwise

|| Define a function which takes three strings and returns a single string which when printed shows the three strings on separate lines 

string == [char]

onThreeLines :: [char] ->  [char] ->  [char] ->  [char]
onThreeLines a b c =  a ++ "\n" ++ b ++ "\n" ++ c


|| Give a function definition which takes a string and a number and returns the string joined to itself n times. If n is 0 , an empty string is returned

duplicate :: [char] -> num -> [char]

duplicate str 0 = " " 
duplicate str n = str ++ duplicate str (n-1) 

|| Give a function definition makes forms a string of n spaces

makeSpaces :: num -> string

makeSpaces 0 = ""
makeSpaces n = makeSpaces (n-1) ++ " " 

||Using the previous answer, give a function that puts n spaces before a string 

pushRight ::num -> string -> string 

pushRight n str = makeSpaces n ++ str

|| Write a function definition for averageSales n, which returns the average value of sales 0 to sales n 

avg ::  num -> num -> num
avg n m = (n+m) /2

averageSales :: num -> num 

averageSales 0 = sales 0 
averageSales n = avg (averageSales (n-1)) (sales n)

||Define the function salesExceed val n, which returns the number of weeks in the period 0 to n in which sales exceed val 

salesExceed:: num -> num -> num 

salesExceed val 0 = 1, if  (sales 0 > val)
salesExceed val 0 = 0, if (sales 0 <= val)
salesExceed val n = 1 + salesExceed val (n-1), if (sales n > val) 
salesExceed val n = salesExceed val (n-1), otherwise 


|| Define a function that returns the number of weeks that is greater than the average

aboveAverageSales :: num -> num 

aboveAverageSales 0 = salesExceed (averageSales 0) 0 
aboveAverageSales n = salesExceed (averageSales n) n 

|| Write an application to print out a table with each week, the average and the total 

|| First create the general function to print the entire table 

printTable:: num -> string

printTable n = heading ++ printWeeks n ++ printAverage n ++ printTotal n 

heading :: string || has immediate definition 
heading = "	Week 		Sales\n"

printWeeks:: num -> string 
printWeeks 0 = printWeek 0 
printWeeks n = printWeeks n ++ printWeek (n-1) 


printWeek :: num -> string
printWeek n = rPrintNum n ++ rPrintNum (sales n) ++ "\n"

offset2:: num 
offset2 = 10 

rPrintNum :: num -> string
rPrintNum n = rjustify offset2 (shownum n) 

printAverage :: num -> string 
printAverage n = "\n     Average " ++ rPrintFloat (averageSales n)

rPrintFloat :: num -> string 
rPrintFloat n = rjustify offset2 (showfloat 2 n) 

totalSales:: num -> num 
totalSales 0 = sales 0
totalSales n = totalSales (n-1) + sales n 

printTotal :: num -> string 
printTotal n = "\n       Total " ++ rPrintNum (totalSales n) 


|| Define a function minMax which returns the min and the max of three numbers 

minMax :: num -> num -> num -> (num,num) 

max4::num -> num -> num
max4 x y = x, if x >y 
	= y, otherwise 

min4:: num -> num -> num 
min4 a b = a, if a < b 
	= b, otherwise 

minMax a b c = ( (max4(max4 a b) c) , (min4(min4 a b) c))

|| Define a function that returns the max of two numbers, together with the number of times it occurs 
count :: num -> num -> num 
count n b = 1, if n > b \/ b > n  
	  = 2, otherwise 

maxOccurs :: num -> num -> (num, num) 

maxOccurs a b = ( (max4 a b) , (count a b ) )

|| Define the function which puts the elements of a triple into ascending order. 

middle:: num -> num -> num -> num 
middle a b c = a, if b> a> c \/ c>a>b
	     = b, if a> b> c \/ c>b>a
	     = c, otherwise

orderTriple:: (num, num, num ) -> (num, num, num) 

orderTriple (a, b, c) = ( max4(max4 a b) c, (middle a b c), (min4(min4 a b) c) )

||Define the sum of squares function using the where clause 
sumSquares:: num -> num -> num 
sumSquares n m 
 = sqN + sqM ||Spacing here is important for some reason  
	where 
	sqN = n*n 
	sqM = m*m
|| Define the function cjustify n str which gives a string on length n by adding spaces to both ends of str to centre in the answer. Need to consider what to do when the length of str is greater than n and when is n is not a positive integer 

cjustify2 :: num -> string -> string 

cjustify2 0 str = str
cjustify2 n str = spcN n ++ str ++ spcN n 
			where 
			spcN 0 = " "
			spcN n = " " ++ spcN (n-1)

|| Define the function for stars the prints out n stars 

stars :: num -> string 

stars 0 = ""
stars n = "*" ++ stars (n-1) 


