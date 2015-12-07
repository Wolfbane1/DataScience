str1 = "Hello"
str2 = 'there'
bob = str1 + str2
print bob


x = '40'
y = int(x) + 2
print y

x = 'From marquard@uct.ac.za'
print x[8]

x = 'From marquard@uct.ac.za'
print x[14:17]

for letter in 'banana' :
    print letter
    
    
print len('banana')*7

greet = 'Hello Bob'
print greet.upper()

print dir(greet)

data = 'From stephen.marquard@uct.ac.za Sat Jan  5 09:14:16 2008'
pos = data.find('.')
print data[pos:pos+3]

text = "X-DSPAM-Confidence:    0.8475";
pos = text.find('0')
print text[pos:]


fh = open("/Users/zzddfge/Desktop/Compartida/Python/mbox.txt", "r")

countFrom = 0
countLinea = 0
posFin = 1
for line in fh:
    countLinea = countLinea + 1
    if line.find('From') >= 0:
       print line[:len(line)-1]
       countFrom = countFrom + 1
    
print countFrom,"Lineas FROM"
print countLinea, "Lineas Totales"