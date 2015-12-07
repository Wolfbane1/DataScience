hrs = raw_input("Enter Hours:")
h = float(hrs)
rate = raw_input("Enter Rate:")
r = float(rate)
pay = 0
if h > 40:
    pay = (h-40)*r*1.5
    
pay = pay + r * 40
print pay