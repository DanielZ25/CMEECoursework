import csv
# A function that computes a score by returning the number of matches starting
# from arbitrary startpoint (chosen by user)
def calculate_score(s1, s2, l1, l2, startpoint):
    matched = "" # to hold string displaying alignements
    score = 0
    for i in range(l2):
        if (i + startpoint) < l1:
            if s1[i + startpoint] == s2[i]: # if the bases match
                matched = matched + "*"
                score = score + 1
            else:
                matched = matched + "-"

    # some formatted output
    print("." * startpoint + matched)           
    print("." * startpoint + s2)
    print(s1)
    print(score) 
    print(" ")

    return score

# Test the function with some example starting points:
# calculate_score(s1, s2, l1, l2, 0)
# calculate_score(s1, s2, l1, l2, 1)
# calculate_score(s1, s2, l1, l2, 5)

def main():
    input_file = "../data/sequences.csv"
    output_file = "../results/alignment_results.txt"

    with open(input_file, 'r') as csvfile:
        reader = csv.reader(csvfile)
        next(reader)
        row = next(reader)
        seq1 = row[0]
        seq2 = row[1]
    
    l1 = len(seq1)
    l2 = len(seq2)
    if l1 >= l2:
        s1 = seq1
        s2 = seq2
    else:
        s1 = seq2
        s2 = seq1
        l1, l2 = l2, l1 # swap the two lengths

    


# now try to find the best match (highest score) for the two sequences
    my_best_align = None
    my_best_score = -1

    for i in range(l1): # Note that you just take the last alignment with the highest score
        z = calculate_score(s1, s2, l1, l2, i)
        if z > my_best_score:
            my_best_align = "." * i + s2 # think about what this is doing!
            my_best_score = z 
    print(my_best_align)
    print(s1)
    print("Best score:", my_best_score)

    with open(output_file, 'w') as f:
        f.write("DNA Sequence Alignment Results\n")
        f.write("=" * 50 +"\n\n")
        f.write("Best Alignment:\n")
        f.write(f"{my_best_align}\n")
        f.write(f"{s1}\n")
        f.write(f"Best score:{my_best_score}\n")
    print(f"\n Results saved to '{output_file}'")
if __name__ == "__main__":
    main()