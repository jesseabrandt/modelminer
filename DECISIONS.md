# Decision log

Design- and model-affecting choices for this project, from **2026-06-09** forward.
Reversible choices are EXECUTE-and-logged by agents; substantive ones are routed
through `dq` → `/decisions` and recorded here on resolution. This file is the
permanent, inspectable record of *why this project is shaped the way it is*.

**Provenance note:** anything in this repo predating this log is **unattributed and
not settled** — it may be Jesse's choice or an agent's, reviewed or not. `north_star.md`
(if present) is Jesse's, as of its date. Agents: do not cite pre-log code or structure
as "already decided" — promote load-bearing pre-log choices through `dq` (substantive)
or `dlog` (reversible) before building on them.
See `~/workspace/docs/decision-log.md` for the convention.

---

## 2026-06-09 — adopt the decision-log convention
- **Choice:** DECISIONS.md per docs/decision-log.md (workspace repo)
- **Why:** design choices from today forward are logged or queued, never silent; pre-log contents are unattributed and not settled until promoted
- **Reversible:** no · **Decided by:** jesse

## 2026-06-10 — MI-aware variable selection lives in GLM, not modelminer
- **Choice:** stability voting across imputations in GLM (extend main-effects approach); no modelminer change
- **Why:** reuses proven approach, keeps modelminer lean; promote to Tier 1 only if another project needs it
- **Reversible:** no · **Decided by:** jesse

## 2026-06-10 — frame 0.2.0 release as PR #8 (dev → main), not a new PR
- **Choice:** Repurposed the existing dev→main PR (#8) into the 0.2.0 release PR — rewrote body to Bottom line / Decisions / Open questions / How to review, set reviewer jesseabrandt, do-not-merge
- **Why:** GitHub forbids two open PRs with the same head→base, and #8's head already IS dev; a duplicate was impossible and unnecessary
- **Reversible:** yes · **Decided by:** agent

## 2026-06-10 — add method = "none" to 0.2.0 NEWS
- **Choice:** Added a NEWS.md entry for the shipped method='none' escape hatch under 0.2.0
- **Why:** the feature was on dev but NEWS listed only from_slot(); release notes must be complete
- **Reversible:** yes · **Decided by:** agent

## 2026-06-10 — add LICENSE.md (full MIT text), build-ignore it
- **Choice:** Added LICENSE.md with standard MIT text matching the LICENSE stub's holders (Jesse Brandt, Zakaria Zerhouni); added ^LICENSE\.md$ to .Rbuildignore
- **Why:** DESCRIPTION declares 'MIT + file LICENSE' but the full text was absent; usethis convention build-ignores LICENSE.md so R CMD check stays NOTE-free (main CRAN-ready). MIT + Zakaria co-copyright confirmation flagged to Jesse as his obligation before CRAN
- **Reversible:** yes · **Decided by:** agent

## 2026-06-10 — defer PR #7 (deprecate-old-fields), do not fold into 0.2.0
- **Choice:** Left PR #7 open with a pointer comment; did not close or fold it into the 0.2.0 release
- **Why:** #7 carries real unmerged work (deprecation + new_mine/validate_mine) not on dev and needs reconciliation with lasso PR #6; closing loses work, folding expands release scope — surfaced as Open Question for Jesse
- **Reversible:** yes · **Decided by:** agent
