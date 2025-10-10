def buggyfunc(x):
    y = x
    for i in range(x):
        try:
            y = y-1
            z = x/y
        except:
            print(f"This didn't work;{x = }; {y = }")
    return z

buggyfunc(20)