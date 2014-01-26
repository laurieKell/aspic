aspic
=====

ASPIC biomass dynamic model extends biodyn

̣̣̣̣̣
New Features
---------------------
The major changes in ASPIC will be:

1. Estimation by MLE (or concentrated likelihood), rather than SSE. Gives the same answers, but better for AIC, etc.
2. Estimation by penalized likelihood (MAP, maximum a posteriori = ML with priors) if desired.
3. Fully tested and supported annual CVs on CPUE.
4. INP file rationalized and consolidated; may contain comment lines starting with # (sample attached, but format may change).
5. Explicit bounds on catchability coefficients.
6. Input guesses for MSY and Fmsy, instead of MSY and K.
7. A few rarely used options (e.g., IRLS) will not be available in the new version.
