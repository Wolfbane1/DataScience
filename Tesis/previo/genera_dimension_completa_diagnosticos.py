import os
import re

nivel_anterior = ""
nivel_3_cod = ""
nivel_3_des = ""
nivel_4_cod = ""
nivel_4_des = ""
nivel_4 = "NO"
escribe = "NO"
cad = " Codigo final Consultar correspondencia entre clasificaciones "
l = len(cad)+7

def generaCSVFichero(dirName, fname, fh_out):
   fh = open(os.path.join(dirName, fname))
   for line in fh:
      nivel_4 = "NO"
      nivel_3 = "NO"
      escribe = "NO"
      linea = line.rstrip()
      if re.search("^[0-9]+;", linea):
         nivel_anterior = linea
      else:
         if re.search("^[0-9]+[.][0-9]+\s", linea):
           if re.search("^[0-9]+[.]([0-9]{1})\s", linea): 
              nivel_3_cod = linea[0:5]
              nivel_3_des = linea[6:].replace(cad, "")
              nivel_3 = "SI"
           else:
              nivel_4_cod = linea[0:6]
              nivel_4_des = linea[7:].replace(cad, "")
              nivel_4 = "SI"
           
         if nivel_4 == "SI":
            res = "%s;%s;%s;%s;%s\n" % (nivel_4_cod, nivel_4_des, nivel_3_cod, nivel_3_des, nivel_anterior)
            escribe = "SI"
         elif nivel_3 == "SI":
            res = "%s;%s;%s;%s;%s\n" % (nivel_3_cod, nivel_3_des, nivel_3_cod, nivel_3_des, nivel_anterior)
            escribe = "SI"
         
         if escribe == "SI":
            print(res)
            fh_out.write(res)
   
   fh.close()


dir = "/Users/zzddfge/Dropbox/Tesis/Varios/DATOS_CIE9/Diagnosticos"

print dir

for dirName, subdirList, fileList in os.walk(dir):
   print('Directorio encontrado: %s' % dirName)
   
   #Abrimos el fichero de salida
   f_out = os.path.join(dirName, "dimensionDiagnosticos.csv")
   print('Fichero de Salida: %s' % f_out)
   fh_out = open(f_out, 'a')
   fh_out.write("CODIGO;DESCRIPCION;CODIGO_N3;DESCRIPCION_N3;CODIGO_N2;DESCRIPCION_N2;CODIGO_N1;DESCRIPCION_N1;CAP;TIPO\n")
   
   for fname in fileList:
      if fname.startswith("dimensionDiagnosticos.txt"):
         print('\tProcesando fichero %s' % fname)   
         generaCSVFichero(dirName, fname, fh_out)
   
   fh_out.close()
