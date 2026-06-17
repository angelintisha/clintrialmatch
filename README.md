# ClinTrialMatch 🏥
### AI-Powered Clinical Trial Investigator Site Recommendation Engine

[![Shiny](https://img.shields.io/badge/Built%20with-R%20Shiny-276DC3?logo=r)](https://tishaangelin.shinyapps.io/clintrialmatch/)
[![Live App](https://img.shields.io/badge/Live%20App-shinyapps.io-brightgreen)](https://tishaangelin.shinyapps.io/clintrialmatch/)

> **Live Demo:** https://tishaangelin.shinyapps.io/clintrialmatch/

---

## Overview

ClinTrialMatch is a web-based recommendation engine that helps clinical 
trial sponsors and CROs identify the best investigator sites for their 
trials. Given five trial-specific parameters, the system scores and ranks 
20 major U.S. investigator sites and returns the top three recommendations 
with match scores, performance metrics, and reasoned explanations.

---

## The Problem

Site selection is one of the most consequential decisions in clinical trial 
operations. Poor site selection is a leading cause of enrollment failure, 
which can cost sponsors $600K–$8M per month of delay. Yet no accessible, 
free recommendation engine exists — current tools are either expensive 
enterprise databases (Medidata, Veeva) or manual spreadsheets.

---

## Features

- **Content filtering** — hard filters sites by trial phase and therapeutic 
  area match
- **Similarity scoring** — composite weighted score across 4 performance 
  dimensions (enrollment rate, screen failure rate, PI experience, 
  infrastructure)
- **Dynamic weighting** — enrollment rate weight increases for Accelerated 
  urgency trials, modeling user preference
- **Collaborative filtering proxy** — region bonus reflects sponsor's 
  implicit regional familiarity
- **Input validation** — all fields must be selected before recommendations 
  are generated
- **No API required** — entire engine runs in pure R at runtime

---

## Recommendation Logic

Score = (w_enroll × EnrollRate) + (0.25 × ScreenFailScore)
+ (0.25 × PIExperience) + (w_infra × Infrastructure)
+ RegionBonus
Where:
w_enroll    = 0.35 (Accelerated) or 0.20 (Standard/Flexible)
RegionBonus = 5 if region matches or No Preference selected
---

## Tech Stack

| Component | Technology |
|---|---|
| Language | R 4.5 |
| Web framework | Shiny |
| Deployment | shinyapps.io (free tier) |
| UI styling | Custom CSS with Inter font |
| AI assistance | Claude (Anthropic) — code generation & logic design |

---

## Running Locally

```r
# 1. Install dependencies
install.packages("shiny")

# 2. Clone the repo
git clone https://github.com/angelintisha/clintrialmatch.git

# 3. Run the app
shiny::runApp("app.R")
```

---

## Project Structure

```
clintrialmatch/
└── app.R          # Full Shiny app — UI, server, and scoring engine
```

---

## Site Database

The prototype includes 20 major U.S. investigator sites scored across 
9 attributes:

- Trial phases supported
- Therapeutic area specializations
- Geographic region
- Historical enrollment rate
- Screen failure rate
- Active competing trials
- PI experience score
- Infrastructure score

In a production version, this database would be replaced with live data 
from ClinicalTrials.gov, FDA registries, and sponsor CTMS systems.

---

## Limitations

- Uses synthetic site data (not real CTMS data)
- Static database — does not update in real time
- No patient-level modeling or disease prevalence data
- No regulatory data (GCP inspections, FDA warning letters)
- Six broad therapeutic areas — production would require finer granularity

---

## Author

**Angelin Tisha**

---

## Acknowledgements

Built with assistance from Claude (Anthropic) for code generation, 
debugging, and report drafting. All problem framing, logic design, 
and business decisions were made by the author.
