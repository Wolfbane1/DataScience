#10.2 Write a program to read through the mbox-short.txt and figure out the distribution by hour of the day for each of the 
#messages. 
#You can pull the hour out from the 'From ' line by finding the time and then splitting the string a second time 
#using a colon.
#From stephen.marquard@uct.ac.za Sat Jan  5 09:14:16 2008
#Once you have accumulated the counts for each hour, print out the counts, sorted by hour as shown below.##
#
fname = raw_input("Enter file name: ")
if len(fname) < 1 : fname = "mbox-short.txt"
fname = "/Users/zzddfge/Desktop/Compartida/Python/mbox-short.txt"

fh = open(fname)
fh = open(fname)
direcciones = dict()
contador = 0
for line in fh:
   palabras = line.rstrip().split()
   if len(palabras) > 0 and palabras[0] == "From":
      contador = direcciones.get(palabras[5].split(":")[0],0)+1
      direcciones[palabras[5].split(":")[0]] = contador
      #print palabras[5], palabras[5].split(":")[0]
#      count = count + 1

lst = list()
for clave, valor in direcciones.items():
    lst.append( (clave, valor) )
lst.sort()

for clave, valor in lst[:10] :
    print clave, valor
#cadena_maxima = ""
#valor_maximo = -10
#for clave in direcciones:
#   valor = direcciones.get(clave, -10)
#   if valor > valor_maximo :
#      cadena_maxima = clave
#      valor_maximo = valor
#print cadena_maxima, valor_maximo