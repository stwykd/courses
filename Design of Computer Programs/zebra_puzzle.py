import itertools

def zebra_puzzle():
    "Return a tuple (WATER, ZEBRA) indicating their house numbers."
    houses = first, _, middle, _, _ = [1, 2, 3, 4, 5]
    orderings = list(itertools.permutations(houses))
    return next((WATER, ZEBRA)
            for (red, green, ivory, yellow, blue) in orderings
            for (Englishman, Spaniard, Ukranian, Japanese, Norwegian) in orderings
            for (dog, snails, fox, horse, ZEBRA) in orderings
            for (coffee, tea, milk, oj, WATER) in orderings
            for (OldGold, Kools, Chesterfields, LuckyStrike, Parliaments) in orderings
            if Englishman is red
            if Spaniard is dog
            if coffee is green
            if Ukranian is tea
            if imright(green, ivory)
            if OldGold is snails
            if Kools is yellow
            if milk is middle
            if Norwegian is first
            if nextto(Chesterfields, fox)
            if nextto(Kools, horse)
            if LuckyStrike is oj
            if Japanese is Parliaments
            if nextto(Norwegian, blue)
            )

def imright(h1, h2):
    return h2 - h1 == 1

def nextto(h1, h2):
    return abs(h2 - h1) == 1
