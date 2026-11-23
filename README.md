# Does a higher Selic reduce credit in Brazil?

A replication, with public data, of Section 4 of Banco Central do Brasil (2026), *Transmissão da
Política Monetária por meio do Crédito: evidência teórica e empírica*, released on 18 Nov 2026.

**Question.** The study estimates, series by series, how lending rates and new loans respond over 18
months to a 1 p.p. higher Selic, using local projections with an external instrument (the cumulative
multiplier of Jordà and Taylor 2025). Press coverage read it as "a higher Selic cuts directed credit
more than free credit". Does the evidence support that reading, and what happens to total credit?

## Two replications

1. **Focus surprise, 2011–2025** (`R/01`–`R/05`). Instrument: Copom decision minus the median Focus
   forecast in the last survey before the meeting, the robustness check the BCB reports (note 41).
2. **BCB specification** (`py/06`, `R/07`–`R/09`). The equation on p. 45 (sums, not averages), the
   controls on p. 46 (lags of the dependent variable and of the policy-rate change; for loan volumes
   also unemployment, log CPI, log IBC-Br, log IIE-Br and a pandemic dummy), lags chosen by BIC, and
   the **DI futures surprise built exactly as on p. 44**: next DI1 contract with 10–40 business days to
   maturity, settlement rates, forward rate from the day after the meeting, surprises below 5 bp set to
   zero. B3 publishes the daily price files only from February 2018, so this instrument covers
   2018–2025.

## Results (h = 18)

| | Focus, 2011–2025 | DI futures, 2018–2025 | BCB |
|---|---|---|---|
| Rate pass-through, directed credit | 0.26 | 0.21 | 0.28 |
| Rate pass-through, free credit | 1.72 | 0.87 | 1.05 |
| New loans, directed, households | −1.2% (n.s.) | **−3.3% [−5.1, −1.5]** | −2.8% |
| New loans, directed, firms | −3.2% (n.s.) | −7.2% [−14.8, 0.4] | −5.3% |
| New loans, free (firms / households) | n.s. | n.s. | n.s. |
| Directed minus free, firms | −3.1 (n.s.) | **−7.8 [−10.8, −4.7]** | not tested |
| Directed minus free, households | −0.8 (n.s.) | **−3.2 [−5.3, −1.2]** | not tested |
| Total new loans | −0.1% [−1.9, 1.7] | not identified (first-stage F = 0.6) | not reported |

- Rate pass-through replicates.
- The volume result depends on the instrument and the period: with the market (DI) surprise since 2018,
  directed credit falls more than free credit; with the analysts' (Focus) surprise since 2011, there is
  no clear difference. The BCB states the result is robust to the Focus measure.
- Total new loans, the number that says how much the Selic takes out of credit, is estimated in the
  study (note 42) but not shown; here it is either zero (Focus) or not identified (DI).
- The study reports no first-stage statistics. Several DI regressions here have a first-stage F below
  10; Anderson-Rubin sets are reported in `output/replica_bc_h18.csv`.

A step-by-step document of the two-stage least squares regressions (in Portuguese) is in
`tex/replicacao_secao4.pdf`.

## Data

| Source | What | How |
|---|---|---|
| BCB SGS | new loans (seasonally adjusted by the BCB), lending rates, Selic, CPI, IBC-Br, unemployment (PNAD Contínua and PME), daily CDI | downloaded by the scripts into `data/` |
| BCB | Focus forecasts by Copom meeting, Copom history | downloaded by `R/01_dados.R` |
| B3 | daily price reports (BVBG.086) on each decision day and the next trading day, 2018–2025 | downloaded by `py/06_surpresa_di.py` into `data_b3/` (about 440 MB) |
| FGV | IIE-Br uncertainty index (series 1428452) | manual: free registration at FGV Dados; save as `data_fgv/fgv_series_largo.csv` with columns `data`, `iie_br`. Without it, `R/07` runs without this control, a variant the BCB says leaves results unchanged (note 41) |

## Run

From this folder: `Rscript R/01_dados.R`, `Rscript R/02_lpiv.R`, `Rscript R/03_placebo.R`,
`Rscript R/04_figura.R`, `python -I py/06_surpresa_di.py`, `Rscript R/07_replica_bc.R`,
`Rscript R/08_figuras_surpresa.R`, `Rscript R/09_figura_post2.R`, `Rscript R/05_documento.R`, then
compile `tex/replicacao_secao4.tex` twice with XeLaTeX.

## Choices the study does not specify

Maximum BIC lag (6 here; with 12 the short DI sample saturates), Newey-West lags (h + 1), the Selic
measure (SGS 4189, monthly average), monthly aggregation of surprises (sum), lags of the dependent
variable in levels (differences as a robustness run), and the unemployment series before March 2012
(PME, spliced in first differences).

## References

Banco Central do Brasil (2026). Jordà, Ò. and A. M. Taylor (2025), "Local Projections", *Journal of
Economic Literature* 63(1): 59–110. Nakamura, E. and J. Steinsson (2018), "Identification in
Macroeconomics", *Journal of Economic Perspectives* 32(3): 59–86. Verbeek, M. (2017), *A Guide to
Modern Econometrics*, 5th ed.
