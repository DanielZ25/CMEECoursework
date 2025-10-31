import csv
import sys
import doctest

#Define function
def is_an_oak(name):
    """ Returns True if name is starts with 'quercus' 
    >>> is_an_oak('Quercus')
    True

    >>> is_an_oak('Quercus robur')
    True

    >>> is_an_oak('quercus')
    True

    >>> is_an_oak('Fagus sylvatica')
    False

    >>> is_an_oak('Quercuss')
    True

    >>> is_an_oak('Quercs')
    True

    >>> is_an_oak('Pinus')
    False

    >>> is_an_oak('')
    False

    >>> is_an_oak('QUERCUS ALBA')
    True
    """

    if not name:
        return False
    
    name_lower = name.lower().strip()
    genus = name_lower.split()[0] if ' ' in name_lower else name_lower

    if genus.startswith('quercus'):
        return True
    
    if genus.startswith ('q') and 5 <= len(genus) <= 9:
        target = 'quercus'
        matches = sum(1 for i, char in enumerate(genus) if i < len(target) and char == target[i])
        if matches >=5:
            return True
    return False


def main(argv): 
    f = open('../data/TestOaksData.csv','r')
    g = open('../data/JustOaksData.csv','w')
    taxa = csv.reader(f)
    csvwrite = csv.writer(g)
    csvwrite.writerow(['Genus', 'Species'])
    next(taxa)

    for row in taxa:
        print(row)
        print ("The genus is: ") 
        print(row[0] + '\n')
        if is_an_oak(row[0]):
            print('FOUND AN OAK!\n')
            csvwrite.writerow([row[0], row[1]])    

    return 0
    
if (__name__ == "__main__"):
    status = main(sys.argv)

    print("\nRunning doctests...")
    doctest.testmod()