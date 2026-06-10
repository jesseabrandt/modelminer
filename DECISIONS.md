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
