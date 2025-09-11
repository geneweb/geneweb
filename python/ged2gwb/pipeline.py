from .types import Gen
from .gen_arrays import add_string, phony_fam
from .io_parser import open_in_bin_with_bom_check, find_lev0, get_lev0
from .io_parser import find_notes_record, find_sources_record

def fill_g_per(gen: Gen, lst):
    for ipp, ip in lst:
        cell = gen.g_per.arr[ipp]
        if isinstance(cell, tuple) and cell[0] == "Right3":
            p, a, u = cell[1]
            p.setdefault("related", [])
            if ip not in p["related"]:
                p["related"].append(ip)
                gen.g_per.arr[ipp] = ("Right3", (p, a, u))

def pass1(gen: Gen, fname: str):
    ic = open_in_bin_with_bom_check(fname)
    try:
        while True:
            st = find_lev0(ic)
            if st is None:
                break
            bp, r1, r2 = st
            if r2 == "NOTE":
                gen.g_not[r1] = bp
            elif r2 == "SOUR":
                gen.g_src[r1] = bp
    finally:
        ic.close()

def make_gen2(gen: Gen, r):
    if r.rlab == "INDI":
        ip = gen.g_hper.get(r.rval) or None
        if ip is None:
            # create placeholder
            from .gen_arrays import per_index
            ip = per_index(gen, r.rval)
        # minimal Right3 placeholder
        try:
            gen.g_per.arr[ip] = ("Right3", ({"first_name": add_string(gen, "?"),
                                            "surname": add_string(gen, "?"),
                                            "occ": ip, "key_index": ip},
                                           {"parents": None, "consang": -1},
                                           {"family": []}))
        except Exception:
            pass

def pass2(gen: Gen, fname: str):
    ic = open_in_bin_with_bom_check(fname)
    gen.g_ic = ic
    try:
        while True:
            rec = get_lev0(ic)
            if rec is None:
                break
            make_gen2(gen, rec)
    finally:
        try:
            ic.close()
        except Exception:
            pass
    fill_g_per(gen, gen.g_godp)
    fill_g_per(gen, gen.g_prelated)

def make_gen3(gen: Gen, r):
    if r.rlab == "FAM":
        from .gen_arrays import fam_index
        fam_index(gen, r.rval)

def pass3(gen: Gen, fname: str):
    ic = open_in_bin_with_bom_check(fname)
    gen.g_ic = ic
    try:
        while True:
            rec = get_lev0(ic)
            if rec is None:
                break
            make_gen3(gen, rec)
    finally:
        try:
            ic.close()
        except Exception:
            pass
    fill_g_per(gen, gen.g_frelated)

def check_undefined(gen: Gen):
    # small wrapper: replace Left3 placeholders with phony entries
    for i in range(gen.g_per.tlen):
        cell = gen.g_per.arr[i]
        if isinstance(cell, tuple) and cell[0] == "Left3":
            lab = cell[1]
            p, a, u = ("_phony_p", None, None)  # minimal
            gen.g_per.arr[i] = ("Right3", ({"first_name": 0, "surname": 0, "occ": i, "key_index": i}, {"parents": None, "consang": -1}, {"family": []}))
    for i in range(gen.g_fam.tlen):
        cell = gen.g_fam.arr[i]
        if isinstance(cell, tuple) and cell[0] == "Left3":
            gen.g_fam.arr[i] = ("Right3", ({"fam_index": i}, (0, 0), {"children": []}))

def add_parents_to_isolated(gen: Gen):
    # minimal no-op; real logic to be ported later
    return

def make_arrays(in_file_path: str):
    fname = in_file_path if in_file_path.lower().endswith(".ged") else in_file_path + ".ged"
    gen = Gen()
    # initial strings
    add_string(gen, "")
    add_string(gen, "?")
    add_string(gen, "x")
    # passes
    pass1(gen, fname)
    pass2(gen, fname)
    pass3(gen, fname)
    try:
        gen.g_ic.close()
    except Exception:
        pass
    check_undefined(gen)
    add_parents_to_isolated(gen)
    return gen.g_per, gen.g_fam, gen.g_str, gen.g_bnot

def make_subarrays(arrays):
    g_per, g_fam, g_str, g_bnot = arrays
    # convert Tabs -> arrays (minimal)
    persons, ascends, unions = [], [], []
    for i in range(g_per.tlen):
        cell = g_per.arr[i]
        if isinstance(cell, tuple) and cell[0] == "Right3":
            p, a, u = cell[1]
            persons.append(p); ascends.append(a); unions.append(u)
        else:
            raise RuntimeError(f"undefined person {cell}")
    families, couples, descends = [], [], []
    for i in range(g_fam.tlen):
        cell = g_fam.arr[i]
        if isinstance(cell, tuple) and cell[0] == "Right3":
            f, c, d = cell[1]; families.append(f); couples.append(c); descends.append(d)
        else:
            raise RuntimeError(f"undefined family {cell}")
    strings = g_str.arr[:g_str.tlen]
    bnotes = {"nread": (lambda s, _: g_bnot if s == "" else ""), "norigin_file": ""}
    return (persons, ascends, unions), (families, couples, descends), strings, bnotes

def finish_base(arrays):
    # stub: finalization placeholder
    return arrays
