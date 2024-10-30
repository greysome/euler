from collections import Counter

hand_types = ('Royal flush', 'Straight flush', 'Four of a kind', 'Full house', 'Flush', 'Straight', 'Three of a kind', 'Two pairs', 'One pair', 'High card')

def parse_card(s):
    value = None
    match s[0]:
        case 'T': value = 10
        case 'J': value = 11
        case 'Q': value = 12
        case 'K': value = 13
        case 'A': value = 14
        case _: value = int(s[0])
    suit = s[1]
    return (value, suit)

def parse_hand(s):
    return tuple(parse_card(card_raw) for card_raw in s.split(' '))

def search_by_value(pairs, value):
    return tuple(k for k, v in pairs if v == value)

def search_by_key(pairs, key):
    return tuple(v for k, v in pairs if k == key)

def _tuple_minus_one_elem(T, x):
    L = list(T)
    for idx, i in enumerate(L):
        if i == x:
            del L[idx]
            break
    return tuple(L)

def tuple_minus(T, *x):
    for i in x:
        T = _tuple_minus_one_elem(T, i)
    return T

"True if s1 > s2"
def compare_hands_by_max_value(s1, s2):
    while True:
        if len(s1) == len(s2) == 0: return True
        if max(s1) < max(s2): return False
        elif max(s1) > max(s2): return True
        s1 = tuple_minus(s1, max(s1))
        s2 = tuple_minus(s2, max(s2))

def get_rank(hand):
    values = tuple(value for value, suit in hand)
    suits = tuple(suit for value, suit in hand)
    values_counter = Counter(values)
    suits_counter = Counter(suits)
    m = min(values)
    # Royal flush
    if len(set(suits)) == 1 and set(values) == {10,11,12,13,14}:
        return 0, -1, -1, tuple()
    # Straight flush
    elif len(set(suits)) == 1 and set(values) == {m,m+1,m+2,m+3,m+4}:
        return 1, m, -1, tuple()
    # Four of a kind
    elif 4 in values_counter.values():
        quad_value = search_by_value(values_counter.items(), 4)[0]
        return 2, quad_value, -1, tuple_minus(values, quad_value, quad_value, quad_value, quad_value)
    # Full house
    elif set(values_counter) == {2,3}:
        return 3, search_by_value(values_counter.items(), 3)[0], search_by_value(values_counter.items(), 2)[0], tuple()
    # Flush
    elif len(set(suits)) == 1:
        return 4, -1, -1, values
    # Straight
    elif set(values) == {m,m+1,m+2,m+3,m+4}:
        return 5, m, -1, tuple()
    # Three of a kind
    elif 3 in values_counter.values():
        triple_value = search_by_value(values_counter.items(), 3)[0]
        return 6, triple_value, -1, tuple_minus(values, triple_value, triple_value, triple_value)
    # Two pairs
    elif 2 in values_counter.values() and len(search_by_value(values_counter.items(), 2)) == 2:
        pair_values = search_by_value(values_counter.items(), 2)
        M, m = max(pair_values), min(pair_values)
        return 7, M, m, tuple_minus(values, M, M, m, m)
    # One pair
    elif 2 in values_counter.values() and len(search_by_value(values_counter.items(), 2)) == 1:
        pair_value = search_by_value(values_counter.items(), 2)[0]
        return 8, pair_value, -1, tuple_minus(values, pair_value, pair_value)
    # High card
    return 9, -1, -1, values

"True if hand1 > hand2"
def compare_hands(hand1, hand2):
    hand_type_1, special_value1_1, special_value2_1, rest_of_hand_1 = get_rank(parse_hand(hand1))
    hand_type_2, special_value1_2, special_value2_2, rest_of_hand_2 = get_rank(parse_hand(hand2))
    print(f'{hand1} vs {hand2}')
    print(hand_types[hand_type_1], special_value1_1, special_value2_1, rest_of_hand_1)
    print(hand_types[hand_type_2], special_value1_2, special_value2_2, rest_of_hand_2)
    if hand_type_1 > hand_type_2: return False
    elif hand_type_1 < hand_type_2: return True
    else:
        if special_value1_1 < special_value1_2: return False
        elif special_value1_1 > special_value1_2: return True
        else:
            if special_value2_1 < special_value2_2: return False
            elif special_value2_1 > special_value2_2: return True
            else:
                return compare_hands_by_max_value(rest_of_hand_1, rest_of_hand_2)

rows = open('p054_poker.txt', 'r').readlines()
rows = [row.strip('\n') for row in rows]
wins = 0
for row in rows:
    if compare_hands(row[:14], row[15:]):
        wins += 1
        print('win')
    print()
print(wins)