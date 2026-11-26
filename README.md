# Music & Studying: STA304 Survey Explorer

**Live app:** [PASTE-YOUR-POSIT-CONNECT-CLOUD-URL-HERE](https://saadhussain-affectsofmusiconacademics.share.connect.posit.cloud/)

An interactive R Shiny website presenting our STA304 (Fall 2026, UTM) group
project, *The Impact of Music on Performance During Intellectually Demanding
Tasks*. Visitors can filter the 79 survey responses, change what each plot
shows and how it looks, and read plain-language explanations of the results.

## What's in the app

| Tab | What it shows |
|---|---|
| Start here | Headline numbers for the selected students and a summary of our findings |
| Music vs. silence | RQ3: STA256 performance by music group (box plot, bubble chart or stacked bars), group summaries, and a one-way ANOVA (or a t-test when two groups are selected) |
| Time with music | RQ4: performance against percent of study time with music (or another study habit), with the fitted regression line and slope test |
| Explore answers | Any survey question as a bar chart or histogram, optionally split by music group |
| All responses | Searchable, sortable table of the filtered responses |
| About | The survey, sampling method and caveats |

The sidebar filters (music group, weekly study hours, session length, genres)
and colour controls apply to every tab.

## Files

- `app.R`: the whole app (data cleaning, user interface and server)
- `data/music_survey.csv`: our cleaned survey data (student IDs and timestamps removed)
- `manifest.json`: package list used by Posit Connect Cloud to build the app

## Run it locally

```r
install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "tidyr", "DT", "colourpicker"))
```

Then open `app.R` in RStudio and click **Run App**.

## Team

Saad Hussain, Calvin, Khalid Abdullayev, Lu Hoang-Nguyen, Ashritha Tangella, Divjot Sidhar
