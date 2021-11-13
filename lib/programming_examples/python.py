parties = {
    1: {'votes': 400000, 'seats': 0},
    2: {'votes': 250000, 'seats': 0},
    3: {'votes': 100000, 'seats': 0},
    4: {'votes': 73000, 'seats': 0},
    5: {'votes': 5000, 'seats': 0}
}


def sortRemainder(val):
    return val['remainder']


def totalVotes(parties):
    total = 0

    for party in parties.values():
        total += party['votes']

    return total


total_votes = totalVotes(parties)
seats = 5
quota = total_votes // seats
awarded = 0

for p in parties.values():
    awarding = p['votes'] // quota
    p['seats'] += awarding
    awarded += awarding
    p['remainder'] = p['votes'] - (awarding * quota)

for p in sorted(list(parties.values()), key=sortRemainder, reverse=True)[:seats-awarded]:
    p['seats'] += 1

print(parties)
