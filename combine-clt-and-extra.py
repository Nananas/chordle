import json

hanzi = {}
parts = {}

def addWords(fname):
    print(f"-- Processing {fname}")
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
                hanzi[j[0]] = (fname, j[2])
            else:
                print("Duplicate: ", j[0], fname, ", previous:", hanzi[j[0]][0], )
                print("\tN:", j[2])
                print("\tO:", hanzi[j[0]][1])
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
        ("Extra Semester 1", addWords("extra-clt4-semester1")),
        ("Book Semester 2", addWords("clt4-semester2")),
        ("Extra Semester 2", addWords("extra-clt4-semester2"))]),
    ("NPCR CLT year 5",
        [("Book Semester 1", addWords("clt5-semester1")),
        ("Extra Semester 1", addWords("extra-clt5-semester1")),
        ("Book Semester 2", addWords("clt5-semester2")),
        ("Extra Semester 2", addWords("extra-clt5-semester2"))])
    ]


print(len(parts))
print(dicts)

with open("static/dictionaries.json", "w") as f:
    out = {"structure": dicts, "parts": parts}
    json.dump(out, f, ensure_ascii=False)
