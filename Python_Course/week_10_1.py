#9.4 Write a program to read through the mbox-short.txt and figure out who has the sent the greatest number of mail messages. 
#The program looks for 'From ' lines and takes the second word of those lines as the person who sent the mail. The program 
#creates a Python dictionary that maps the sender's mail address to a count of the number of times they appear in the file. 
#
#After the dictionary is produced, the program reads through the dictionary using a maximum loop to find the most prolific 
#committer.
fname = raw_input("Enter file name: ")
if len(fname) < 1 : fname = "mbox-short.txt"
fname = "/Users/zzddfge/Desktop/Compartida/Python/mbox-short.txt"

fh = open(fname)
#count = 0
fh = open(fname)
direcciones = dict()
contador = 0
for line in fh:
   palabras = line.rstrip().split()
   if len(palabras) > 0 and palabras[0] == "From":
      contador = direcciones.get(palabras[1],0)+1
      direcciones[palabras[1]] = contador
#      print palabras[1]
#      count = count + 1

cadena_maxima = ""
valor_maximo = -10
for clave in direcciones:
   valor = direcciones.get(clave, -10)
   if valor > valor_maximo :
      cadena_maxima = clave
      valor_maximo = valor
print cadena_maxima, valor_maximo