birds = ( ('Passerculus sandwichensis','Savannah sparrow',18.7),
          ('Delichon urbica','House martin',19),
          ('Junco phaeonotus','Yellow-eyed junco',19.5),
          ('Junco hyemalis','Dark-eyed junco',19.6),
          ('Tachycineata bicolor','Tree swallow',20.2),
         )

#(1) Write three separate list comprehensions that create three different
# lists containing the latin names, common names and mean body masses for
# each species in birds, respectively. 

# (2) Now do the same using conventional loops (you can choose to do this 
# before 1 !). 

# A nice example out out is:
# Step #1:
# Latin names:
# ['Passerculus sandwichensis', 'Delichon urbica', 'Junco phaeonotus', 'Junco hyemalis', 'Tachycineata bicolor']
# ... etc.

#list comprehensions 
latin_names = [bird[0] for bird in birds]
common_names = [bird[1] for bird in birds]
mean_body_mass = [bird[2] for bird in birds]

print("Latin names:")
print(latin_names)
print("Common names:")
print(common_names)
print("Mean body mass:")
print(mean_body_mass)

#conventional loops
latin_names = []
common_names = []
mean_body_mass = []

for bird in birds:
    latin_names.append(bird[0])
for bird in birds:
    common_names.append(bird[1])
for bird in birds:
    mean_body_mass.append(bird[2])

print("Latin names:")
print(latin_names)
print("Common names:")
print(common_names)
print("Mean body mass:")
print(mean_body_mass)
