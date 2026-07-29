#!/usr/bin/env python3
# Face-validity audit of the KLR dictionaries. Classifies every discovered bigram
# into clean-domain / domain+generic / pure-generic (contamination) / noise, and
# tags domain terms by theme (energy, trade, region, supply, tech, macro, ...).
# Run: python3 pipeline/python/analyze_dicts.py
import csv, os, re
from collections import Counter, OrderedDict

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
D = os.path.join(ROOT, "out", "dict")
DICTS = OrderedDict([
    ("v1 (41 old seeds)",   os.path.join(D, "run_v1_41seeds", "dictionary_v1.csv")),
    ("v2 R (50 clean)",     os.path.join(D, "dictionary_v2_50seeds.csv")),
    ("py-sautner",          os.path.join(D, "dictionary_v1_py.csv")),
    ("py-rparity",          os.path.join(D, "dictionary_v1_pyr.csv")),
])

# ---- theme keyword sets (token-level, substring match) ----------------------
THEMES = OrderedDict([
 ("trade",      ["tariff","tariffs","trade","import","imports","export","exports","dumping",
                 "customs","duty","duties","wto","quota","protection","protectionist"]),
 ("sanctions",  ["sanction","sanctions","embargo","entity","blacklist"]),
 ("geopolitics",["geopolit","geopolitical","war","conflict","military","defense","defence",
                 "election","political","government","federal","sovereign","tension","tensions",
                 "regime","security","nato","terror"]),
 ("region",     ["china","chinese","russia","russian","ukraine","taiwan","kong","hong",
                 "europe","european","asia","asian","mexico","india","korea","korean","japan",
                 "japanese","germany","brexit","domestic","overseas","american","america",
                 "united","states","middle","east","north","south","saudi","iran","israel",
                 "gulf","pacific","britain","british","france","french","canada","canadian",
                 "country","countries","cross","border","emerging","international","global",
                 "world","regional","euro","eurozone","yuan","renminbi"]),
 ("supply",     ["supply","chain","chains","shoring","reshoring","nearshoring","friendshoring",
                 "logistics","sourcing","supplier","suppliers","disruption","disruptions",
                 "bottleneck","backlog","procurement","constraints","shortage","shortages"]),
 ("critical",   ["semiconductor","semiconductors","chip","chips","lithium","cobalt","nickel",
                 "copper","steel","aluminum","aluminium","ore","mineral","minerals","rare",
                 "earth","metal","metals","commodity","commodities","material","materials",
                 "raw","input","inputs","feedstock","resin","chemical","grain","wheat","corn",
                 "agricultural","fertilizer","gold","silver","platinum","palladium","uranium"]),
 ("energy",     ["oil","gas","crude","energy","lng","opec","pipeline","renewable","renewables",
                 "power","electric","electricity","coal","fuel","petroleum","refining","barrel",
                 "cubic","feet","bcf","solar","wind","nuclear","hydrogen","drilling","upstream",
                 "downstream","reserves"]),
 ("tech",       ["technology","technologies","tech","decoupling","cyber","digital","ai",
                 "artificial","semiconductor","intellectual","patent","data","cloud"]),
 ("macro",      ["inflation","inflationary","recession","gdp","interest","monetary","currency",
                 "exchange","dollar","macro","macroeconomic","economy","economic","uncertainty",
                 "volatility","rates","fed","reserve","fiscal","confidence","sentiment"]),
 ("climate",    ["climate","carbon","emission","emissions","sustainab","decarbon","zero",
                 "esg","environmental","green"]),
 ("covid",      ["covid","pandemic","lockdown","coronavirus","virus"]),
])

GENERIC = set("""price prices pricing cost costs margin margins gross revenue revenues sales
volume volumes earnings ebitda guidance profit profits income selling sell mix basis points
sequential organic capex expenditure inventory inventories backlog gaap eps dividend buyback
quarter quarterly sequentially yoy""".split())

# explicit filler / function-word-led bigrams that are neither domain nor generic
FILLER = set(["et cetera","cetera et","important role","near term","long term","great job",
              "parts world","continue monitor","really talking","impact on","on supply",
              "on global","mission critical","strong demand","growing demand","demand growth",
              "potential impact","key role","moving parts"])
FUNC_LEAD = ("on ","in ","of ","at ","to ","for ","the ","that ","this ","with ","from ")

def themes_of(bg):
    toks = bg.split()
    hits = set()
    for th, kws in THEMES.items():
        for t in toks:
            if any(k == t or (len(k) > 4 and k in t) for k in kws):
                hits.add(th); break
    return hits

def has_generic(bg):
    return any(t in GENERIC for t in bg.split())

def classify(bg):
    th = themes_of(bg)
    g = has_generic(bg)
    if bg in FILLER or bg.startswith(FUNC_LEAD):
        # function-word/filler: but if it carries a real theme keep it as domain-ish
        if th and not g:
            return "domain", th
        return "noise", th
    if th and not g:   return "domain", th
    if th and g:       return "domain+generic", th
    if g and not th:   return "generic", th
    return "noise", th

def load_discovered(path):
    out = []
    with open(path) as f:
        for r in csv.DictReader(f):
            if r.get("origin") == "discovered":
                out.append(r["bigram"].strip())
    return out

print("="*78)
for name, path in DICTS.items():
    if not os.path.exists(path):
        print(f"{name}: MISSING ({path})"); continue
    disc = load_discovered(path)
    K = min(500, len(disc))
    top = disc[:K]
    cls = Counter(); theme_ct = Counter()
    examples = {"generic": [], "noise": [], "domain": [], "domain+generic": []}
    for bg in top:
        c, th = classify(bg)
        cls[c] += 1
        if c in ("domain","domain+generic"):
            for t in th: theme_ct[t] += 1
        if len(examples[c]) < 12: examples[c].append(bg)
    n = len(top)
    domain = cls["domain"] + cls["domain+generic"]
    print(f"\n### {name}   (top {n} discovered)")
    print(f"  CLEAN DOMAIN      : {cls['domain']:3d}  ({100*cls['domain']/n:4.1f}%)")
    print(f"  DOMAIN+generic    : {cls['domain+generic']:3d}  ({100*cls['domain+generic']/n:4.1f}%)   (commodity-pricing gray zone)")
    print(f"  GENERIC (contam.) : {cls['generic']:3d}  ({100*cls['generic']/n:4.1f}%)   <-- the v1 disease")
    print(f"  NOISE / filler    : {cls['noise']:3d}  ({100*cls['noise']/n:4.1f}%)")
    print(f"  => signal (domain) : {100*domain/n:4.1f}%   contamination (gen+noise): {100*(cls['generic']+cls['noise'])/n:4.1f}%")
    # theme mix among domain terms
    tot_th = sum(theme_ct.values()) or 1
    mix = ", ".join(f"{t} {100*theme_ct[t]/tot_th:.0f}%" for t,_ in theme_ct.most_common())
    print(f"  theme mix (domain): {mix}")
    print(f"  energy share of domain: {100*theme_ct['energy']/ (domain or 1):.0f}%")
    print(f"  e.g. GENERIC: {examples['generic'][:8]}")
    print(f"  e.g. NOISE  : {examples['noise'][:8]}")
    # full noise list for the clean runs, to eyeball for proper-noun/company contamination
    if name in ("v2 R (50 clean)", "py-sautner"):
        noise_all = [bg for bg in top if classify(bg)[0] == "noise"]
        print(f"  FULL NOISE list ({len(noise_all)}): {noise_all}")

# top-100 contamination focus (what the eye sees first)
print("\n" + "="*78)
print("TOP-100 contamination rate (generic+noise), the part anyone reads:")
for name, path in DICTS.items():
    if not os.path.exists(path): continue
    disc = load_discovered(path)[:100]
    bad = sum(1 for bg in disc if classify(bg)[0] in ("generic","noise"))
    print(f"  {name:22s}: {bad:3d}/100  ({bad:.0f}% generic+noise)")
