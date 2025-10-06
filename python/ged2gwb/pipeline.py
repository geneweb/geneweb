import os, sys
from typing import Any, Dict, List
import logging
from models import Gen, Tab
from io_parser import open_in_bin_with_bom_check, find_lev0, get_lev0
from gen_arrays import add_string, add_indi, add_fam, fill_g_per, check_undefined, add_parents_to_isolated
from records import find_all_fields, treat_notes

def make_arrays(in_file: str) -> Dict[str, Any]:
    """Parse GEDCOM file and create arrays"""
    logging.info(f"Processing file: {in_file}")
    gen = Gen()
    gen.g_ic = open_in_bin_with_bom_check(in_file)
    assert add_string(gen, "") == 0
    assert add_string(gen, "?") == 1
    assert add_string(gen, "x") == 2
    try:
        while True:
            s = find_lev0(gen.g_ic)
            if s is None:
                break
            bp, r1, r2 = s
            if r2 == "NOTE":
                gen.g_not[r1] = bp
            elif r2 == "SOUR":
                gen.g_src[r1] = bp
    finally:
        gen.g_ic.seek(0)
    try:
        while True:
            rec = get_lev0(gen.g_ic)
            if rec is None:
                break
            if rec.rlab == "INDI":
                add_indi(gen, rec)
            elif rec.rlab == "HEAD":
                rl = find_all_fields("NOTE", rec.rsons)
                if rl:
                    gen.g_bnot = treat_notes(gen, rl)
    finally:
        gen.g_ic.seek(0)
    try:
        while True:
            rec = get_lev0(gen.g_ic)
            if rec is None:
                break
            if rec.rlab == "FAM":
                add_fam(gen, rec)
    finally:
        gen.g_ic.close()
        gen.g_ic = None
    check_undefined(gen)
    fill_g_per(gen, gen.g_godp)
    fill_g_per(gen, gen.g_prelated)
    add_parents_to_isolated(gen)
    return {
        "individuals": gen.g_per,
        "families": gen.g_fam,
        "sources": gen.g_str,
        "notes": gen.g_bnot
    }

def make_subarrays(arrays: Dict[str, Any]) -> Dict[str, Any]:
    """Process arrays into subarrays"""
    logging.info("Creating subarrays")
    g_per, g_fam, g_str, g_bnot = arrays["individuals"], arrays["families"], arrays["sources"], arrays["notes"]
    persons = [None] * g_per.tlen
    ascends = [None] * g_per.tlen
    unions = [None] * g_per.tlen
    for i in range(g_per.tlen):
        ent = g_per.arr[i]
        if isinstance(ent, tuple) and ent[0] == "Right3":
            person, ascend, union = ent[1]
            persons[i] = person
            ascends[i] = ascend
            unions[i] = union
        else:
            raise RuntimeError(f"undefined person at index {i}")
    families = [None] * g_fam.tlen
    couples = [None] * g_fam.tlen
    descends = [None] * g_fam.tlen
    for i in range(g_fam.tlen):
        ent = g_fam.arr[i]
        if isinstance(ent, tuple) and ent[0] == "Right3":
            fam, cpl, des = ent[1]
            families[i] = fam
            couples[i] = cpl
            descends[i] = des
        else:
            raise RuntimeError(f"undefined family at index {i}")
    strings = g_str.arr[:g_str.tlen]
    bnotes = {"nread": (lambda s, _ : g_bnot if s=="" else ""), "norigin_file": "", "efiles": (lambda _ : [])}
    return {
        "persons": persons,
        "ascends": ascends,
        "unions": unions,
        "families": families,
        "couples": couples,
        "descends": descends,
        "strings": strings,
        "bnotes": bnotes
    }

def finish_base(subarrays: Dict[str, Any]) -> None:
    """Finalize the genealogy base"""
    logging.info("Finishing base")
    persons, ascends, unions = subarrays["persons"], subarrays["ascends"], subarrays["unions"]
    families, couples, descends = subarrays["families"], subarrays["couples"], subarrays["descends"]
    for i, des in enumerate(descends):
        ch = des.get("children", []) if isinstance(des, dict) else []
        des["children"] = sorted(ch)
    for i, u in enumerate(unions):
        fams = u.get("family", []) if isinstance(u, dict) else []
        u["family"] = sorted(fams)
    # TODO: implement full check_parents_children and check_parents_sex as OCaml
    print("finish_base: basic finalisation complete", file=sys.stderr)
    pass
