

fname = raw_input("Enter file name: ")
if len(fname) < 1 : fname = "mbox-short.txt"
fname = "/Users/zzddfge/Desktop/Compartida/Python/mbox-short.txt"

fh = open(fname)
count = 0
fh = open(fname)
for line in fh:
   palabras = line.rstrip().split()
   if len(palabras) > 0 and palabras[0] == "From":
      print palabras[1]
      count = count + 1
print "There were", count, "lines in the file with From as the first word"
