
grammar = [
    ("S", ["P"]),   # S -> P
    ("S", ["(", "P", ")"]), # S -> ( P )
    ("S", [])   # S ->
]

tokens = ["(", "(", ")", ")"]

def addtochart(chart, index, state):
    if not (state in chart[index]):
        chart[index] = [state] + chart[index]
        return True
    return False

def closure(grammar, i, x, ab, cd, j):
    return [(rule[0], [], rule[1], i) for rule in grammar if cd <> [] and rule[0] == cd[0]]

def shift (tokens, i, x, ab, cd, j):
    if cd <> [] and tokens[i] == cd[0]:
        return (x, ab + [cd[0]], cd[1:], j)
    return None

def reductions(chart, i, x, ab, cd, j):
    return [(jstate[0], jstate[1]+[x], (jstate[2])[1:], jstate[3])
        for jstate in chart[j] if cd == [] and jstate[2] <> [] and (jstate[2])[0] == x]

def parse(tokens, grammar):
    tokens = tokens + ["end_of_input_marker"]
    chart = {}
    start_rule = grammar[0]
    for i in range(len(tokens)+1):
        chart[i] = []
    start_state = (start_rule[0], [], start_rule[1], 0)
    chart[0] = [start_state]
    for i in range(len(tokens)):
        while True:
            changes = False
            for state in chart[i]:
                # x -> a b . c d from j
                (x, ab, cd, j) = (state[0], state[1], state[2], state[3])

                # CLOSURE
                # if c is non-terminal, for each grammar rule c -> p q r
                # make a next state c -> . p q r from i
                next_states = closure(grammar, i , x, ab, cd, j)
                for next_state in next_states:
                    changes = addtochart(chart, i, next_state) or changes

                # SHIFT
                # If tokens[i] == c
                # make a next state x -> a b c . d, from j
                # in chart[i+1]
                next_state = shift(tokens, i, x, ab, cd, j)
                if next_state <> None:
                    any_changes = addtochart(chart, i+1, next_state) or any_changes

                # REDUCE if cd is [], the state is just x -> a b . from j
                # for each p -. q , x r from l in chart[j]
                # make a new state p -> q x . r from l
                # in chart[i]
                next_states = reductions(chart, i, x, ab, cd, j)
                for next_state in next_states:
                    changes = addtochart(chart, i, next_state) or changes

            if not changes:
                break

    for i in range(len(tokens)):
        print "== chart " + str(i)
        for state in chart[i]:
            x = state[0]
            ab = state[1]
            cd = state[2]
            j = state[3]
            print "    " + x + " ->"
            for sym in ab:
                print " " + sym,
            print " .",
            for sym in cd:
                print " " + sym,
            print "   from " + str(j)

    accepting_state = (start_rule[0], start_rule[1], [], 0)
    return accepting_state in chart[len(tokens)-1]

print parse(tokens, grammar)
