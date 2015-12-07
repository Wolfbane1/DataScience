#5.2 Write a program that repeatedly prompts a user for integer numbers until the user enters 'done'. Once 'done' is entered, 
#print out the largest and smallest of the numbers. If the user enters anything other than a valid number catch it with a 
#try/except and put out an appropriate message and ignore the number. Enter the numbers from the book for problem 5.1 and 
#Match the desired output as shown.

minimum = None
maximum = None
salir = False
while not salir :
   entrada = raw_input("Enter a number: ")
   if entrada == "done" : 
      salir = True
   else:
      try:
         num = int(entrada)
      except:
         print "Invalid input"
         continue
        
      print num
      if minimum is None:
         minimum = num
         maximum = num
      else:
         if minimum > num:
            minimum = num
         if maximum < num:
            maximum = num

print "Maximum", maximum
print "Minimum", minimum