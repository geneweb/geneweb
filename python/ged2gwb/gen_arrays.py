from typing import Any, Dict, List, Tuple, Optional

# Use shared types
from .types import Tab, Gen

def assume_tab(tab: Tab, none):
    if tab.tlen == len(tab.arr):
        new_len = 2 * max(1, len(tab.arr)) + 1
        tab.arr.extend([none] * (new_len - len(tab.arr)))

def add_string(gen: Gen, s: str) -> int:
    if s in gen.g_hstr:
        return gen.g_hstr[s]
    i = gen.g_str.tlen
    assume_tab(gen.g_str, "")
    if i >= len(gen.g_str.arr):
        gen.g_str.arr.append(s)
    else:
        gen.g_str.arr[i] = s
    gen.g_str.tlen += 1
    gen.g_hstr[s] = i
    return i

def extract_addr(addr: str) -> str:
    if addr and addr.startswith('@'):
        try:
            r = addr.index('@', 1)
            return addr[:r+1]
        except ValueError:
            return addr
    return addr

def output_pindex(i: int, s: str):
    # placeholder for track mapping
    pass

def per_index(gen: Gen, lab: str) -> int:
    lab2 = extract_addr(lab)
    if lab2 in gen.g_hper:
        return gen.g_hper[lab2]
    i = gen.g_per.tlen
    assume_tab(gen.g_per, None)
    if i >= len(gen.g_per.arr):
        gen.g_per.arr.append(("Left3", lab2))
    else:
        gen.g_per.arr[i] = ("Left3", lab2)
    gen.g_per.tlen += 1
    gen.g_hper[lab2] = i
    output_pindex(i, lab2)
    return i

def fam_index(gen: Gen, lab: str) -> int:
    lab2 = extract_addr(lab)
    if lab2 in gen.g_hfam:
        return gen.g_hfam[lab2]
    i = gen.g_fam.tlen
    assume_tab(gen.g_fam, None)
    if i >= len(gen.g_fam.arr):
        gen.g_fam.arr.append(("Left3", lab2))
    else:
        gen.g_fam.arr[i] = ("Left3", lab2)
    gen.g_fam.tlen += 1
    gen.g_hfam[lab2] = i
    return i

def unknown_per(i: int, sex: str):
    person = {"first_name": 0, "surname": 0, "occ": i, "sex": sex, "key_index": i}
    ascend = {"parents": None, "consang": -1}
    union = {"family": []}
    return person, ascend, union

def phony_per(gen: Gen, sex: str) -> int:
    i = gen.g_per.tlen
    assume_tab(gen.g_per, None)
    p, a, u = unknown_per(i, sex)
    if i >= len(gen.g_per.arr):
        gen.g_per.arr.append(("Right3", (p, a, u)))
    else:
        gen.g_per.arr[i] = ("Right3", (p, a, u))
    gen.g_per.tlen += 1
    return i

def unknown_fam(gen: Gen, i: int):
    father = phony_per(gen, "Male")
    mother = phony_per(gen, "Female")
    fam = {"fam_index": i}
    cpl = (father, mother)
    des = {"children": []}
    return fam, cpl, des

def phony_fam(gen: Gen) -> int:
    i = gen.g_fam.tlen
    assume_tab(gen.g_fam, None)
    f, c, d = unknown_fam(gen, i)
    if i >= len(gen.g_fam.arr):
        gen.g_fam.arr.append(("Right3", (f, c, d)))
    else:
        gen.g_fam.arr[i] = ("Right3", (f, c, d))
    gen.g_fam.tlen += 1
    return i
