"""
COMP3020 - Steam review collector (controversy & review-bomb games).

Public Steam endpoints, no API key:
  - store.steampowered.com/appreviews/<appid>?json=1       (review text + metadata)
  - store.steampowered.com/appreviewhistogram/<appid>      (monthly up/down counts, whole lifetime)
  - store.steampowered.com/api/appdetails?appids=<appid>   (genres, release date, price)

Privacy: Steam IDs are SHA-256 hashed (salted) and usernames/profile URLs are dropped.

Outputs (in ./data):
  steam_reviews.csv    one row per review
  steam_histogram.csv  one row per game x month (up/down counts)
  steam_games.csv      one row per game (metadata)
"""
import csv, hashlib, json, os, sys, time, urllib.parse, urllib.request

GAMES = {  # appid: short name  (event) -- controversy / review-bomb set
    553850:  "Helldivers 2",          # May 2024 PSN account requirement
    949230:  "Cities Skylines II",    # Oct 2023 performance launch
    1716740: "Starfield",
    2357570: "Overwatch 2",           # Aug 2023 most-negative-reviewed launch
    1623730: "Palworld",
    1272080: "Payday 3",              # Sep 2023 always-online servers
    2344520: "Diablo IV",
    1294810: "Redfall",
    2853730: "Skull and Bones",
    315210:  "Suicide Squad KTJL",
    2215430: "Ghost of Tsushima",     # May 2024 PSN region lock
    990080:  "Hogwarts Legacy",       # boycott
    1086940: "Baldur's Gate 3",       # positive control
    1245620: "Elden Ring",            # Jun 2024 DLC difficulty/perf bomb
    730:     "Counter-Strike 2",      # Sep 2023 CS:GO replaced
    440:     "Team Fortress 2",       # 2024 #SaveTF2 bot crisis
    2246340: "Monster Hunter Wilds",  # Feb 2025 PC performance
    1285190: "Borderlands 4",         # Sep 2025 PC performance
    1517290: "Battlefield 2042",
    3159330: "Assassin's Creed Shadows",
    1771300: "Kingdom Come Deliverance II",  # positive control
    1091500: "Cyberpunk 2077",        # redemption arc
    275850:  "No Man's Sky",          # redemption arc
    1151340: "Fallout 76",            # redemption arc
}

N_HELPFUL = 1500   # filter=all  -> most helpful reviews (spread over lifetime)
N_RECENT = 500     # filter=recent -> newest reviews
# Secret salt for hashing Steam IDs. Kept out of the code so the hashes cannot be
# reversed by anyone who has this file: set it before running, e.g.
#   set STEAM_ID_SALT=<your secret>   (Windows)   /   export STEAM_ID_SALT=...
SALT = os.environ.get("STEAM_ID_SALT")
if not SALT:
    sys.exit("Set the STEAM_ID_SALT environment variable before running.")
OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data")
UA = {"User-Agent": "COMP3020-student-project (non-commercial research)"}


def get_json(url, tries=5):
    for i in range(tries):
        try:
            req = urllib.request.Request(url, headers=UA)
            with urllib.request.urlopen(req, timeout=30) as r:
                return json.load(r)
        except Exception as e:
            wait = 5 * (i + 1)
            print(f"   retry {i+1} in {wait}s ({e})", flush=True)
            time.sleep(wait)
    return None


def fetch_reviews(appid, filt, n):
    out, cursor, seen_cursors = [], "*", set()
    while len(out) < n:
        q = dict(json=1, filter=filt, language="english", review_type="all",
                 purchase_type="all", num_per_page=100, cursor=cursor,
                 filter_offtopic_activity=0)          # 0 = KEEP review-bomb periods
        if filt == "all":
            q["day_range"] = 9223372036854775807    # whole lifetime
        d = get_json(f"https://store.steampowered.com/appreviews/{appid}?" + urllib.parse.urlencode(q))
        if not d or not d.get("success") or not d.get("reviews"):
            break
        out.extend(d["reviews"])
        cursor = d.get("cursor")
        if not cursor or cursor in seen_cursors:
            break
        seen_cursors.add(cursor)
        time.sleep(1.2)
    return out[:n]


def hash_id(sid):
    return hashlib.sha256((SALT + str(sid)).encode()).hexdigest()[:16]


def main():
    os.makedirs(OUT, exist_ok=True)
    rev_cols = ["appid", "game", "source", "recommendationid", "user", "voted_up", "review",
                "timestamp_created", "timestamp_updated", "votes_up", "votes_funny",
                "weighted_vote_score", "comment_count", "steam_purchase", "received_for_free",
                "written_during_early_access", "primarily_steam_deck", "num_games_owned",
                "num_reviews", "playtime_forever_min", "playtime_at_review_min"]
    fr = open(os.path.join(OUT, "steam_reviews.csv"), "w", newline="", encoding="utf-8")
    wr = csv.DictWriter(fr, fieldnames=rev_cols); wr.writeheader()
    fh = open(os.path.join(OUT, "steam_histogram.csv"), "w", newline="", encoding="utf-8")
    wh = csv.writer(fh); wh.writerow(["appid", "game", "month_start_unix", "up", "down"])
    fg = open(os.path.join(OUT, "steam_games.csv"), "w", newline="", encoding="utf-8")
    wg = csv.writer(fg); wg.writerow(["appid", "game", "release_date", "genres", "developer",
                                      "publisher", "is_free", "price_aud", "total_reviews",
                                      "total_positive", "total_negative", "review_score_desc"])

    for k, (appid, name) in enumerate(GAMES.items(), 1):
        print(f"[{k}/{len(GAMES)}] {name} ({appid})", flush=True)

        # metadata
        det = get_json(f"https://store.steampowered.com/api/appdetails?appids={appid}&cc=au&l=english") or {}
        dd = det.get(str(appid), {}).get("data", {}) if det.get(str(appid), {}).get("success") else {}
        summ = (get_json(f"https://store.steampowered.com/appreviews/{appid}?json=1&num_per_page=0"
                         "&language=english&purchase_type=all") or {}).get("query_summary", {})
        wg.writerow([appid, name, dd.get("release_date", {}).get("date", ""),
                     "|".join(g["description"] for g in dd.get("genres", [])),
                     "|".join(dd.get("developers", [])), "|".join(dd.get("publishers", [])),
                     dd.get("is_free", ""), (dd.get("price_overview") or {}).get("final", 0) / 100,
                     summ.get("total_reviews"), summ.get("total_positive"),
                     summ.get("total_negative"), summ.get("review_score_desc")])

        # monthly histogram (entire lifetime, all languages)
        h = get_json(f"https://store.steampowered.com/appreviewhistogram/{appid}?l=english&review_score_preference=0")
        for r in ((h or {}).get("results") or {}).get("rollups", []):
            wh.writerow([appid, name, r["date"], r["recommendations_up"], r["recommendations_down"]])

        # review text
        seen = set()
        for filt, n, src in (("all", N_HELPFUL, "helpful"), ("recent", N_RECENT, "recent")):
            got = fetch_reviews(appid, filt, n)
            new = 0
            for r in got:
                if r["recommendationid"] in seen:
                    continue
                seen.add(r["recommendationid"]); new += 1
                a = r.get("author", {})
                wr.writerow({
                    "appid": appid, "game": name, "source": src,
                    "recommendationid": r["recommendationid"], "user": hash_id(a.get("steamid")),
                    "voted_up": int(r["voted_up"]), "review": r.get("review", "").replace("\r", " "),
                    "timestamp_created": r["timestamp_created"], "timestamp_updated": r["timestamp_updated"],
                    "votes_up": r["votes_up"], "votes_funny": r["votes_funny"],
                    "weighted_vote_score": r["weighted_vote_score"], "comment_count": r["comment_count"],
                    "steam_purchase": int(r["steam_purchase"]), "received_for_free": int(r["received_for_free"]),
                    "written_during_early_access": int(r["written_during_early_access"]),
                    "primarily_steam_deck": int(r.get("primarily_steam_deck", False)),
                    "num_games_owned": a.get("num_games_owned"), "num_reviews": a.get("num_reviews"),
                    "playtime_forever_min": a.get("playtime_forever"),
                    "playtime_at_review_min": a.get("playtime_at_review"),
                })
            print(f"   {src}: {len(got)} fetched, {new} new", flush=True)
        fr.flush(); fh.flush(); fg.flush()

    fr.close(); fh.close(); fg.close()
    print("DONE", flush=True)


if __name__ == "__main__":
    main()
