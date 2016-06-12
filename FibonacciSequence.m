|| Date: April 11th 2016
|| Purpose: Miranda Exercises — Lists and Higher Ordered Functions
|| Source: Programming with Miranda — Clack, Myers, and Poon
|| Author: Jasmien Cels


|| Function that returns the element at the nth specific place in the list
get_nth any [] = error "error, function applied to empty list"
get_nth 1 (front : any) = front
get_nth n (front : rest) = get_nth (n-1) rest

|| Function that returns the Fibonnacci Sequence
fibs = fibonacci 1 1
fibonacci n1 n2 = n1 : fibonacci n2 (n1+n2) 

