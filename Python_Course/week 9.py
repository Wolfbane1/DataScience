# Use words.txt as the file name
#Ejercicio 1
#fname = raw_input("Enter file name: ")
#fh = open(fname)
#for line in fh: 
#   print line.rstrip().upper()
   
#Ejercicio 2   
# Use the file name mbox-short.txt as the file name
fname = raw_input("Enter file name: ")
fname = "/Users/zzddfge/Desktop/Compartida/Python/mbox-short.txt"
fh = open(fname)
numElementos = 0
suma = 0.0
for line in fh:
    if not line.startswith("X-DSPAM-Confidence:") : continue
#   Count these lines and extract the floating point values from each of the lines and compute the average of those values and 
#   produce an output as shown below.
    a = line.split()
    numElementos = numElementos + 1
    suma = suma + float(a[1])
        
print "Average spam confidence:", suma/numElementos