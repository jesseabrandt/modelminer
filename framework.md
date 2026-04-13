# Framework — modelminer

**North star:** [north_star.md](north_star.md)

## Invariants

1. **The triple is sacred.** Any model function, any metric function, any comparison function. No change may break this pluggability.
2. **Functional, not object-oriented.** Users compose behavior by passing functions, not by subclassing objects or learning a type system.
3. **NSE at the boundary, not inside.** Public functions accept unquoted names. Internals use whatever representation is practical — the rule is that NSE doesn't leak inward.
4. **Main stays CRAN-ready.** Feature work happens on branches. Main passes `R CMD check` at all times.
5. **Variable selection, not hyperparameter tuning.** The package answers "which terms?"

## Decision Tree

```
├─ Explicit in active spec/plan?              → EXECUTE
├─ Standing invariant above?                  → EXECUTE
├─ Closed decision (spec, todo, memory)?      → EXECUTE
├─ Bug in call graph, blocking task?          → EXECUTE (note in commit)
├─ Bug outside call graph?                    → SURFACE (don't touch)
├─ Design decision with >1 valid answer?      → ASK
├─ Touches shared state (remote, CI, etc.)?   → ASK
├─ Would violate an invariant?                → ASK
├─ New search algorithm or method variant?    → ASK (these are design decisions)
├─ Changing mine()'s public signature?        → ASK (CRAN stability)
└─ Opportunistic cleanup, unrelated?          → QUEUE to todo (don't touch)
```

**Test for "closed decision":** Has the user already said what the answer is, anywhere — spec, todo, memory, conversation? If yes, execute. If no, ask.

## Session Start Ritual

1. Read `north_star.md`
2. Read this framework
3. Read `TODO.md` for current priorities
4. Check `git status` and `git log --oneline -5`
5. **State intent:** current task, files to touch, relevant invariants, session mode

## Completion Criteria

- All spec/plan steps committed
- All invariants verified to hold
- Tests pass (`testthat::test_dir("tests/testthat")`)
- `R CMD check` clean (no new NOTEs/WARNINGs/ERRORs) if touching R/ or man/
- `/audit` on changed files
- Any gaps documented in TODO.md
