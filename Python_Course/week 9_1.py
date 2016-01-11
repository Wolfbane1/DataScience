
def encuentraPalabra(lst, word):
   encontrado = False;
   i = 0
   while not encontrado and i <= len(lst)-1:
 #     print "encuentraPalabra:", lst[i], word
      if lst[i] == word:
         encontrado = True;
      i = i + 1 
   return encontrado


fname = raw_input("Enter file name: ")
fname = "/Users/zzddfge/Desktop/Compartida/Python/romeo.txt"
fh = open(fname)
lst = list()
for line in fh:
   words = line.split()

   for i in range(len(words)):
#      print "Main: ", words[i]
      if len(lst) == 0 or  not encuentraPalabra(lst, words[i]):
#         print "Main - Anado", words[i]
         lst.append(words[i])
   
lst.sort()
print lst

