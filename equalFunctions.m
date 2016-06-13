|| Date: April 2016
|| Author: Jasmien Cels
|| Purpose: Miranda Exercises from Thompson Book Chapter One


|| Give a definition for allFourEquals which uses the function allEqual
allEqual:: num -> num -> num -> bool
allEqual a b c = (a=b) & (b = c)

allFourEqual:: num -> num -> num -> num -> bool
allFourEqual a b c d = (allEqual a b c) & (c=d) 

|| Give the definitions for these functions which count how many of their arguments are equal 

howManyEqual :: num -> num -> num -> num
howManyEqual a b c = 3 , if (a=b) & (b=c)
		  = 2 , if (a=b) \/ (b = c) \/ (a=c)
		  = 0 , otherwise
 
||Give a definition of a function which gives the value True if the three numbers are all different

allDifferent:: num -> num -> num -> bool 
allDifferent a b c = (a ~= b) & (b ~= c) & (a ~= c)

|| Give a definition of howManyEqual which uses the functions allEqual and allDifferent 

howManyEqual2 a b c = 3 , if (allEqual a b c)
		   = 0, if (allDifferent a b c) 
		   = 2, otherwise 

|| Give a definition of a function which returns its argument to the power four. Give the definition which uses the function square. 

square:: num -> num 
square x = x*x

fourPower :: num -> num 
fourPower x = (square x) * (square x) 
