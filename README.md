# Does a higher Selic reduce credit volumes in Brazil?

A replication, with public data, of the volume results in Banco Central do Brasil (2026),
*Transmissão da Política Monetária por meio do Crédito: evidência teórica e empírica*, Section 4.

**Question.** The study estimates, series by series, how lending rates and new loans respond over 18
months to a 1 p.p. higher Selic, using local projections with an external instrument (Jordà and
Taylor 2025). Press coverage read it as "a higher Selic cuts directed credit more than free credit".
Does the evidence support that reading, and what happens to total credit?

**Data.** Central Bank SGS series (new loans seasonally adjusted by the BCB, codes 24439–24447; average
rates; IPCA; IBC-Br; PNAD Contínua unemployment), Focus survey by Copom meeting, Copom history.
Monthly, 2011–2025.

**Method.** Cumulative multiplier: mean of the outcome over t..t+h minus its t−1 value, on the mean
Selic over t..t+h minus its t−1 value, by 2SLS. Instrument: Copom decision minus the median Focus
forecast in the last survey before the meeting (the BCB's stated robustness check; its main
instrument uses DI futures). Controls follow the BCB (lags of the outcome and of the policy rate;
unemployment, inflation, activity, a pandemic dummy). Newey-West errors. Added: first-stage effective
F, Anderson-Rubin sets, the test of the directed-minus-free difference, total credit, and an
anticipation placebo.

**Results (h = 18, sample to June 2025).**
- Rate pass-through replicates: directed credit 0.28 (BCB: 0.28), free credit 1.46 (BCB: 1.05).
- No volume response is significant at 95%. Directed loans to firms: −2.7% [−10.2, 4.8] (BCB: −5.3%).
- Directed minus free, firms: −4.4 p.p. [−11.4, 2.6].
- Total new loans: +0.1% [−2.2, 2.3]. The BCB estimates but does not report it.
- First-stage F between 26 and 32; Anderson-Rubin sets close to the Wald intervals.

**Run.** From this folder: `Rscript R/01_dados.R`, `R/02_lpiv.R`, `R/03_placebo.R`, `R/04_figura.R`.
`R/01_dados.R` downloads missing series into `data/`.

**References.** Banco Central do Brasil (2026). Jordà, Ò. and A. M. Taylor (2025), "Local
Projections", *Journal of Economic Literature*. Nakamura, E. and J. Steinsson (2018), "Identification
in Macroeconomics", *Journal of Economic Perspectives* 32(3): 59–86.
