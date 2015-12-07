#3.3 Write a program to prompt for a score between 0.0 and 1.0. If the score is out of range, print an error. If the score is between 0.0 and 1.0, print a grade using the following table:
#Score Grade
#>= 0.9 A
#>= 0.8 B
#>= 0.7 C
#>= 0.6 D
#< 0.6 F
#If the user enters a value out of range, print a suitable error message and exit. For the test, enter a score of 0.85.
score = raw_input("Enter a score between 0.0 and 1.0: ")
try :
   s = float(score)
except :
   s = -1
message = "A"
if s >= 0.0 and s <= 1.0:
   if s < 0.6:
      message = "F"
   elif s < 0.7:
      message = "D"
   elif s < 0.8:
      message = "C"
   elif s < 0.9:
      message = "B"
else:
   message = "Error, score out of range or not numeric"

print message