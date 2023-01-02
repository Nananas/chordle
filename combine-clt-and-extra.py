import json

hanzi = {}
parts = {}

def addWords(fname):
    with open(fname + ".json" ) as f:
        words = []
        for line in f.readlines():
            try:
                j = json.loads(line[:-1])
            except Exception as e:
                print("Could not load json", line[:-1])
                print(e)
                exit(1)

            if j[0] not in hanzi:
                words.append(j)
                hanzi[j[0]] = fname
            else:
                print("Duplicate: ", j[0], fname, ", previous:", hanzi[j[0]], )
        words.sort(key=lambda w: w[1])
        parts[fname] = words
    return fname

dicts = [
    ("NPCR CLT year 1",
        [("Book", addWords("clt1")),
        ("Extra", addWords("extra-clt1"))]),
    ("NPCR CLT year 2",
        [("Book", addWords("clt2")), 
        ("Extra", addWords("extra-clt2"))]),
    ("NPCR CLT year 3",
        [("Book", addWords("clt3")),
        ("Extra", addWords("extra-clt3"))]),
    ("NPCR CLT year 4",
        [("Book Semester 1", addWords("clt4-semester1")), 
        ("Extra Semester 1", addWords("extra-clt4-semester1"))])
    ]


print(len(parts))
print(dicts)

with open("static/dictionaries.json", "w") as f:
    out = {"structure": dicts, "parts": parts}
    json.dump(out, f, ensure_ascii=False)
