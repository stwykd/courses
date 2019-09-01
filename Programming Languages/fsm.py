def fsmsim(string, current, edges, accepting):
    if string == "":
        return current in accepting
    else:
        letter = string[0]
        if (current, letter) in edges:
            return fsmsim(string[1:], edges[(current,letter)], edges, accepting)
        else:
            return False
