def computepay(h,r):
    pay = 0
    if h > 40:
       pay = (h-40)*r*1.5
    pay = pay + r * 40
    return pay

hrs = raw_input("Enter Hours:")
h = float(hrs)
rate = raw_input("Enter rate:")
r = float(rate)
p = computepay(h,r)
print "Pay",p