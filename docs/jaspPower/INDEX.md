# jaspPower Internal Documentation Index

This folder documents the current `jaspPower` module implementation for future development work.

## Read Order

1. [OVERVIEW.md](./OVERVIEW.md)
2. [ARCHITECTURE.md](./ARCHITECTURE.md)
3. [ANALYSES.md](./ANALYSES.md)
4. [UI_QML_MAP.md](./UI_QML_MAP.md)
5. [R_CODE_MAP.md](./R_CODE_MAP.md)
6. [TESTING.md](./TESTING.md)
7. [GLOSSARY.md](./GLOSSARY.md)
8. [OPEN_QUESTIONS.md](./OPEN_QUESTIONS.md)

## Primary Source Files

- UI metadata: `inst/Description.qml`
- Analysis UI: `inst/qml/Power.qml`
- User help: `inst/help/Power.md`
- R entry point: `R/Power.R`
- Shared helpers: `R/commonPower.R`, `R/commonPowerPlotting.R`
- Analysis implementations: `R/pwr_*.R`
- Tests: `tests/testthat/test-Power.R`, `tests/testthat/_snaps/Power/*`
- CI workflows: `.github/workflows/*.yml`
