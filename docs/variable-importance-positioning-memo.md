# Positioning memo: modelminer as a variable-importance / inference tool

**To:** Jesse
**From:** anvil-bot (research/positioning memo — no code, no API commitments)
**Date:** 2026-06-12
**Status:** Exploratory. Situates your inbox note against the literature and
sketches where it could go. Every literature claim below was checked against the
cited source (you said you hadn't researched it yet); the *positioning* and
*design shapes* are my suggestions, not findings.

---

## 1. Your note, restated

> "A potential broader pitch of modelminer … beyond what it does today, is in
> favor of feature engineering — but more from an inference perspective. … there's
> something in seeing what predictive capability is subtracted by taking out a
> variable. It's not a modeling tweak that slightly improves your models — in fact
> it may make them perform a bit worse, and that's interesting — that's saying
> something about a variable." (inbox, 2026-06-12 00:04)

The reframe is a change of **goal**, not mechanism. Today modelminer *searches*
to find a good model (selection: optimize a metric). Your note points at *measuring*
what each variable buys you (importance/inference: report a per-variable change in
predictive capability, with the sign and size being the answer — not a means to a
better model). The mechanism — fit a model, drop something, refit, compare — is
the same one modelminer already runs.

This is a real, named idea in the statistics literature, and modelminer happens to
sit unusually close to it.

## 2. Where this sits in the literature (verified)

Your "predictive capability subtracted by taking out a variable" is, almost
exactly, **drop-column / leave-one-covariate-out importance** — and the "that's
saying something about a variable" part is the **inference** layer built on top of
it.

- **LOCO — leave-one-covariate-out.** Lei, G'Sell, Rinaldo, Tibshirani & Wasserman,
  *Distribution-Free Predictive Inference for Regression*, **JASA** 113(523), 2018.
  Drop covariate *j*, refit, and measure the **increase in out-of-sample prediction
  error**; sample-splitting makes the increase a quantity you can put a confidence
  interval and a test around. This is your idea with the inference attached, and it
  is model-agnostic. (R package `conformalInference`.) Note the method has known
  critiques — see Tibshirani's own talk "LOCO: the Good, the Bad, and the Ugly" —
  so it's a starting point, not a settled answer.

- **Algorithm-agnostic variable importance with valid inference.** Williamson,
  Gilbert, Carone & Simon, *A general framework for inference on algorithm-agnostic
  variable importance*, arXiv:2004.03683 (and *Biometrics* 2021, "Nonparametric
  variable importance assessment using machine learning techniques"). Defines a
  variable's importance as the **population-level drop in predictiveness** (e.g.
  R², AUC, deviance explained) when you go from "all features" to "all features
  except this one", and gives **asymptotically valid confidence intervals plus a
  test of the zero-importance null**, even when the learner is a black box. This is
  the most rigorous published version of exactly your framing. (R package `vimp`,
  on CRAN.)

- **Drop-column vs. permutation importance.** Two ways to "remove" a variable:
  *drop-column* (retrain without it — `n_features + 1` fits, expensive) vs.
  *permutation* (shuffle that column on a single trained model — cheap). They
  usually agree, but drop-column better reflects a variable's **unique**
  contribution when predictors are correlated, because permutation lets the fixed
  model lean on a correlated substitute and so **under**-states correlated
  features. Your phrasing ("taking out a variable", refit, see what's lost) is the
  drop-column / LOCO version — the more faithful and more expensive one. (Refs:
  scikit-learn permutation-importance docs; Molnar, *Interpretable ML*,
  Permutation Feature Importance chapter.)

- **Importance as a range, not a point.** Fisher, Rudin & Dominici, *All Models are
  Wrong, but Many are Useful*, **JMLR** 20(177), 2019. "Model reliance" is a
  permutation importance; "**model class reliance**" is the **range** of a
  variable's importance across *all* near-optimal models in a class — a variable
  can look unimportant in one good model and essential in another. Relevant because
  modelminer already produces a whole set of near-optimal models as a byproduct
  (its `$trace`).

**Two distinctions to keep straight** (both verified, both matter for credibility):

1. **Conditional, not marginal.** Importance-by-removal measures a variable's
   contribution *given the others*. With collinearity, a genuinely predictive
   variable can score ~0 because a correlated partner covers for it. That's not a
   bug — it's the question "what does this variable add that nothing else does?" —
   but it must be stated, or users will misread a zero.
2. **Out-of-sample, not in-sample.** "Predictive capability" only means something
   measured on held-out data. modelminer's default metric is in-sample AIC (a
   penalized in-sample quantity, not a generalization estimate). Importance work
   should default to a CV / held-out metric. (modelminer already ships these — see
   below.)

## 3. What modelminer already has that serves this

The drop-and-refit machinery is, mechanically, what modelminer *is*:

- **The `model_func` / `metric` / `metric_comparison` abstraction** is precisely a
  drop-column importance engine: "fit any model, score it by any metric, compare
  two scores." That is the LOCO/vimp inner loop, already general over model and
  metric (the north-star "any model, any metric, any comparison").
- **`backward` and `forward_backward` already remove terms and compute the metric
  delta** of doing so — they just *use* those deltas to decide what to drop and
  then discard them. Importance reporting would **surface** the same deltas instead
  of throwing them away. The search already computes the raw signal.
- **`$trace` records every formula evaluated and its metric** — a ready-made set of
  near-optimal models for a model-class-reliance-style "is this variable important
  across *all* the good models, or swappable?" view.
- **`method = "none"`** builds the full engineered candidate pool and fits the full
  model once — the natural **baseline** ("all features") that a drop-column contrast
  measures against.
- **Out-of-sample metrics already exist:** `lm_loocv()`, `make_cv_metric()`,
  `make_cp_metric()`. The "predictive capability" half can be honest OOS today, not
  in-sample AIC.
- **`compare_methods()`** is batch-refit infrastructure (run many configurations,
  collect their scores) — the same shape as "refit once per variable."

**The distinctive angle — importance over an *engineered* feature space.** Standard
importance tools (`vimp`, `rfpimp`, DALEX) operate on raw input columns. modelminer
engineers `I(x^2)`, `x:z`, etc., and *knows the provenance* of each derived term. So
it can ask questions those tools can't phrase cleanly: *does `I(x^2)` add predictive
capability beyond linear `x`?* (term-level) and *what is lost if we remove `x` and
every term derived from it?* (variable-level, grouping derived terms back to their
source). That grouping — importance over a structured feature space, attributed back
to original variables — is the niche your note is pointing at, and it's one
modelminer is naturally built for.

## 4. What it would need (gaps — not a design)

None of this is "free"; here's the honest gap list:

- **An out-of-sample default.** Importance should not be reported off in-sample AIC.
  The helpers exist; an importance workflow would need to *default* to a CV/held-out
  metric and say so.
- **Variable-level grouping.** "Remove a variable" must remove *all* its derived
  terms (`x`, `I(x^2)`, `x:z`, …), not one term. The generator already knows the
  provenance; it would need to be exposed/used for grouping.
- **A defined baseline + contrast.** A fixed "full model" anchor and an explicit
  full-vs-(full−v) comparison, rather than a search path.
- **Uncertainty — the genuinely new part.** A point delta isn't inference. LOCO
  tests the increase in held-out error via sample-splitting; `vimp` builds
  influence-function-based CIs. modelminer currently reports point metrics with no
  uncertainty. Adding a defensible CI / test on the delta is the real work — and
  it's the part that lands in **your** math-credibility gate, not something to ship
  unreviewed.
- **Sample splitting.** Valid inference (both LOCO and vimp) relies on evaluating
  importance on data not used to fit — a workflow contract modelminer doesn't
  currently impose.

## 5. Two-to-three concrete shapes it could take

Ordered cheapest/most-reuse first. All are *memo-level sketches*, no API here.

**Shape A — "Ablation report" (drop-column / LOCO point estimates).**
Given a fitted or full model, for each source variable drop all its derived terms,
refit, and measure the change in an OOS metric (`lm_loocv` / `make_cv_metric`).
Output a tidy table: `variable | Δ predictiveness | direction`. Pure reuse of the
existing harness + backward-removal + CV metrics; *no inference yet.* Honest framing
required: conditional-on-the-rest, collinearity caveat, sign is informative in both
directions (a variable whose removal *helps* OOS is a real finding). This is the
"version 1" and it's mostly assembly of parts that already exist.

**Shape B — "Importance with inference" (LOCO inference / vimp-style).**
Shape A + uncertainty: use CV folds or an explicit sample split to get a
*distribution* of per-fold loss differences, then a CI and a test of the
zero-importance null. This is the rigorous "that's saying something about a
variable." It's the credibility-bearing shape — cite Lei et al. (LOCO test) and
Williamson et al. (`vimp` CIs), and route the statistical choices through your
review. Don't ship the inference as agent-authored fact.

**Shape C — "Importance from the search" (model-class-reliance flavour).**
modelminer's `$trace` already holds many near-optimal models. Summarize a variable's
importance *across that set* — important in every good model vs. swappable — à la
Fisher–Rudin–Dominici model class reliance. Cheap (reuses already-computed fits) and
distinctive to modelminer's byproduct. **Caveat to state loudly:** the trace is a
greedy/stepwise *search path*, not an unbiased sample of the model class, so this is
an *exploratory, descriptive* view — it does **not** inherit MCR's formal guarantees,
which come from optimizing over the whole class. Frame as a visualization, not a test.

## 6. Recommendation & the one thing to research next

- **The pitch is real and well-grounded.** Your instinct names an established
  idea (LOCO / algorithm-agnostic variable importance), and modelminer is closer to
  being a drop-column importance engine than to most things it's compared with —
  the refit-and-score loop *is* the package. The differentiator worth leaning on is
  **importance over an engineered feature space attributed back to source
  variables**, which the existing VI tools don't do.
- **Don't conflate the cheap part with the hard part.** Shape A (point ablation) is
  near-free assembly. Shape B (inference) is the valuable, credibility-bearing,
  math-reviewed part — keep them separate so the easy 80% doesn't smuggle in
  unreviewed statistical claims.
- **One thing to read before committing:** the `vimp` paper + package
  (Williamson et al., arXiv:2004.03683) — it is the closest existing thing to your
  idea *with* valid inference, so the real question is "what does modelminer add
  over `vimp`?" The honest candidate answer is *the engineered-feature-space
  framing and the friendly fit→ablate workflow*, not the inference math (which
  `vimp` already does rigorously). Worth confirming that gap before building.

---

## Sources (verified 2026-06-12)

- Lei, G'Sell, Rinaldo, Tibshirani, Wasserman, *Distribution-Free Predictive
  Inference for Regression*, JASA 113(523):1094–1111, 2018 —
  [arXiv:1604.04173](https://arxiv.org/abs/1604.04173) ·
  [JASA](https://www.tandfonline.com/doi/full/10.1080/01621459.2017.1307116)
- Tibshirani, *LOCO: the Good, the Bad, and the Ugly* (talk, caveats) —
  [stat.cmu.edu](https://www.stat.cmu.edu/~ryantibs/talks/loco-2018.pdf)
- Williamson, Gilbert, Carone, Simon, *A general framework for inference on
  algorithm-agnostic variable importance* —
  [arXiv:2004.03683](https://arxiv.org/abs/2004.03683) ·
  [`vimp` package](https://github.com/bdwilliamson/vimp)
- Fisher, Rudin, Dominici, *All Models are Wrong, but Many are Useful*, JMLR
  20(177):1–81, 2019 — [jmlr.org](https://jmlr.org/papers/v20/18-760.html) ·
  [arXiv:1801.01489](https://arxiv.org/abs/1801.01489)
- Molnar, *Interpretable Machine Learning* (Permutation Feature Importance chapter) —
  [christophm.github.io](https://christophm.github.io/interpretable-ml-book/feature-importance.html)
- scikit-learn, *Permutation feature importance* (drop-column vs. permutation,
  correlated-feature behaviour) —
  [scikit-learn.org](https://scikit-learn.org/stable/modules/permutation_importance.html)
