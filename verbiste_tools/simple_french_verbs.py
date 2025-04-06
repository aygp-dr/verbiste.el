import json

# Common French verbs with their infinitive forms
french_verbs = [
    "aller",    # to go
    "venir",    # to come
    "faire",    # to do/make
    "dire",     # to say
    "voir",     # to see
    "savoir",   # to know (fact)
    "pouvoir",  # to be able to
    "vouloir",  # to want
    "devoir",   # to have to
    "prendre",  # to take
    "parler",   # to speak
    "manger",   # to eat
    "boire",    # to drink
    "dormir",   # to sleep
    "courir"    # to run
]

# Save to file for later use
with open("data/simple_french_verbs.json", "w") as f:
    json.dump(french_verbs, f, indent=2)
