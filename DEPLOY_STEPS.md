# Getting the website live (for our group)

Due: **November 27, 2026**. Allow about an hour, most of it waiting for the
first Connect Cloud build.

## 1. Run it on your own computer (10 min)

1. Unzip this folder somewhere sensible. Keep the structure:
   ```
   app.R
   data/music_survey.csv
   README.md
   ```
2. In RStudio, run once in the console:
   ```r
   install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "tidyr", "DT",
                      "colourpicker", "rsconnect"))
   ```
3. Open `app.R` and click **Run App**. Click through every tab and control.

## 2. Make it ours before submitting (30 min, split it up)

The originality policy caps a copied app at 1/7, and the TA can diff groups'
code. Go through `app.R` together so everyone can explain it, and change
anything you'd word or design differently. In particular:

- **About tab** (search for `EDIT` in `app.R`): check the description of how
  we collected responses matches what we actually did.
- **Plain-language text**: reword anything that doesn't sound like us.
- **Demographic filter**: our survey has no demographic questions, so the
  sidebar filters by study habits instead. Ask the TA (Piazza or email)
  whether that satisfies the "filter by a demographic variable" rubric line.

## 3. Put it on GitHub (10 min)

Posit Connect Cloud's free plan only deploys **public** repositories.

1. Use the GitHub account that has access to our STA304 project. Make the repo
   public (Settings, then Danger Zone, then Change visibility) or create a new
   public repo.
2. Simplest layout: `app.R`, `data/` and `README.md` at the **top level** of
   the repo.
3. Create `manifest.json`. In RStudio, set the working directory to the folder
   with `app.R` (Session, then Set Working Directory, then To Source File
   Location) and run:
   ```r
   rsconnect::writeManifest()
   ```
   This writes `manifest.json`, listing our packages and their versions.
   Re-run it any time you add a new `library()` to `app.R`.
4. Commit and push `app.R`, `data/music_survey.csv`, `README.md` and
   `manifest.json`.

## 4. Deploy on Posit Connect Cloud (10 to 20 min)

1. Go to connect.posit.cloud and sign in with that GitHub account.
2. Click **Publish**, choose **Shiny**.
3. Pick the repository, branch `main`, primary file `app.R`, then **Publish**.
4. Watch the build log. Common errors:
   - *No manifest.json*: step 3.3 was skipped or the file wasn't pushed.
   - *Package not found*: re-run `rsconnect::writeManifest()` and push again.
   - *Cannot open file 'data/music_survey.csv'*: the data folder wasn't
     pushed, or the folder/file name differs (the server is case-sensitive).

## 5. Check and submit

1. Open the public URL in a **private/incognito window**. If it only works
   while you're logged in, the TA can't mark it.
2. Click every control on every tab.
3. Paste the URL at the top of `README.md` (replace the placeholder), commit
   and push.
4. Submit the URL.

## Rubric checklist (7 marks)

| Rubric item | Where it is |
|---|---|
| Deployed, URL works, code committed, own data (1) | Steps 3 to 5 |
| 2+ interactive plots, one changes what is plotted (1) | "Compare the groups on", "Compare STA256 performance with", "Survey question" drop-downs |
| Adjustable scales / axis limits (0.75) | y-axis sliders (Music vs. silence), x- and y-axis sliders and log scale (Time with music), log scale and bins (Explore) |
| Change plot colours (0.75) | Sidebar "Colours": 5 palettes (2 colour-blind safe) plus 2 colour pickers |
| Filter by demographic variable (0.75) | Sidebar filters, **study habits only** (see step 2) |
| Choose which graph/output to display (0.75) | Tabs, plus a "Plot style" choice on Music vs. silence |
| Plain-language interpretations (0.6) | "What does this tell us?" / "Reading this plot" cards, which rewrite themselves with the filters |
| Readable names, not codebook names (0.4) | All axes, legends, tables and text |
| Organised and easy to navigate (0.5) | Sidebar + tabs, "Start here" guide |
| Plots labelled and polished (0.5) | Titles, axis labels, legends, n per group |

## Changes from the EDA, based on the TA's feedback

- No jitter: overlapping answers are shown with bigger dots (or see-through
  dots), so every point sits at the value the student reported.
- No Tukey HSD after a non-significant ANOVA. Pairwise comparisons (with a
  Bonferroni correction) only appear if a filtered ANOVA comes out significant.
- Conclusions say this is an observational study and talk about associations,
  without the "doesn't prove no effect" sentence.
