#12.1 Write a program to read through the file and sum all the numbers. 
#

#Importing the regular expression library.
import re

print re.findall("\S+?@\S+", "From stephen.marquard@uct.ac.za Sat Jan  5 09:14:16 2008")

#asking for the name
fname = raw_input("Enter file name: ")
if len(fname) < 1 : fname = "regex_sum_190669.txt"
fname = "/Users/zzddfge/Desktop/Compartida/Python/regex_sum_190669.txt"

#Open file and loop for reading every line
fh = open(fname)
suma = 0
for line in fh:
   linea = line.rstrip()
   numeros = re.findall("[0-9]+", linea)
   if len(numeros) > 0:
      for numero in numeros:
         n = int(numero)
         suma = suma + n
   
print suma   
