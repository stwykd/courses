def nfsmsim(string, current, edges, accepting):
# fill in your code here
    if string == "":
        return current in accepting
    else:
        letter = string[0]
        if (current, letter) in edges:
            remainder = string[1:]
            newstates = edges[(current, letter)]
            for newstate in newstates:
                if nfsmsim(remainder, newstate, edges, accepting):
                    return True
        return False
