# Average UK Rainfall (mm) for 1910 by month
# http://www.metoffice.gov.uk/climate/uk/datasets
rainfall = (('JAN',111.4),
            ('FEB',126.1),
            ('MAR', 49.9),
            ('APR', 95.3),
            ('MAY', 71.8),
            ('JUN', 70.2),
            ('JUL', 97.1),
            ('AUG',140.2),
            ('SEP', 27.0),
            ('OCT', 89.4),
            ('NOV',128.4),
            ('DEC',142.2),
           )

# (1) Use a list comprehension to create a list of month,rainfall tuples where
# the amount of rain was greater than 100 mm.
rain_over_100 = [(month,rain) for (month,rain) in rainfall if rain > 100]
print("Months and rainfall values when the amount of rain was greater than 100mm:")
print(rain_over_100)

 
# (2) Use a list comprehension to create a list of just month names where the
# amount of rain was less than 50 mm. 
rain_less_than_50 = [(month,rain) for (month,rain) in rainfall if rain < 50]
print("Months and rainfall values when the amount of rain was less than 50mm:")
print(rain_less_than_50)

# (3) Now do (1) and (2) using conventional loops (you can choose to do 
# this before 1 and 2 !). 
#(1)with conventional loop
rain_over_100 = []
rain_less_than_50 = []

for month,rain in rainfall:
    if rain > 100:
        rain_over_100.append((month,rain))
print("Months and rainfall values when the amount of rain was greater than 100mm:")
print(rain_over_100)

for month,rain in rainfall:
    if rain < 50:
        rain_less_than_50.append((month,rain))
print("Months and rainfall values when the amount of rain was less than 50mm:")
print(rain_less_than_50)

# A good example output is:
#
# Step #1:
# Months and rainfall values when the amount of rain was greater than 100mm:
# [('JAN', 111.4), ('FEB', 126.1), ('AUG', 140.2), ('NOV', 128.4), ('DEC', 142.2)]
# ... etc.

