<p align="center">
  <img src="logos/GaitTrackR_logo.png" width="720"/>
</p>

<p align="center">
  <a href="https://opensource.org/licenses/MIT"><img src="https://img.shields.io/badge/License-MIT-yellow.svg"/></a>
  <a href="https://shiny.posit.co/"><img src="https://img.shields.io/badge/Built%20with-R%20Shiny-blue.svg"/></a>
  <img src="https://img.shields.io/badge/Platform-Mac%20%7C%20Windows%20%7C%20Linux-lightgrey.svg"/>
</p>

---

**GaitTrackR** is an interactive R Shiny app for analyzing mouse gait from paw-print coordinate data (e.g. footprint tracking from walking assays).
It includes a built-in image annotation tool to extract paw coordinates directly from photos, and computes mouse-level gait metrics visualized across genotypes, treatments, or genotype–treatment combinations.

> ⚠️ **No hypothesis testing is performed.** GaitTrackR reports descriptive statistics only (mean, SD, CV). Statistical comparisons are left to the user.

---

## Schematic overview

![Schematic of gait measures](logos/gait_measures_schematic_v2.png)

The schematic illustrates:
- **Step length** — same paw, along the walking direction
- **Front–hind (FB) distance** — as full 2D distance and as x-only distance
- **Perpendicular deviation** — shortest distance from an intermediate paw print to the line connecting two consecutive prints of the opposite-side paw (computed separately for front and hind legs, and for L→R and R→L)

---

## Requirements

- [R](https://cran.r-project.org/) (≥ 4.0)
- The following R packages (installed automatically on first run if missing):

```r
shiny, dplyr, tidyr, ggplot2, readxl, writexl, DT,
ggprism, ggpattern, ggrepel, scales, magick, imager
```

---

## Quick Start

### 💻 From R console (any platform)

```r
shiny::runGitHub("GaitTrackR", "camillaelbaek", subdir = "GaitTrackR_App")
```

### Mac

Double-click `GaitTrackR_App/Mac_Run_Mouse_App.command`.

> First time only: right-click → Open → Open (to bypass Gatekeeper).

### Windows

Double-click `GaitTrackR_App/Windows_Run_Mouse_App.bat`.

> If R is not found, install it from [https://cran.r-project.org](https://cran.r-project.org) and try again.

---

## What the app does

The app has two tabs:

### 🖼 Image → Data
Extract paw-print coordinates directly from walking assay photos (JPG/PNG):

| Feature | Details |
|---|---|
| **Upload image** | JPG or PNG of a single mouse walking track |
| **Auto-detect paws** | Color segmentation (red = front, blue = hind, or configurable) |
| **Set scale** | Click two points on the ruler → enter distance in cm |
| **Edit annotations** | Add, delete, or toggle L/R for any detected point |
| **Export** | Saves an Excel file in the exact format expected by the Analysis tab |

### 📊 Analysis
Compute and visualize gait metrics from paw-print coordinate data:

| Feature | Details |
|---|---|
| **Upload data** | Excel file (.xlsx) with paw-print coordinates |
| **Assign groups** | Genotype and treatment from file columns or manually |
| **Straighten tracks** | Optional: estimate and align the walking axis |
| **Compute gait measures** | Step length, FB distance (2D and x-only), perpendicular deviation |
| **Visualize** | Bar plots (mean ± SD), CV plots, individual mice overlaid |
| **Export** | Mouse-level feature table and plots |

---

## Input data format (Analysis tab)

Your Excel file **must** contain the following columns:

| Column | Description |
|---|---|
| `mouse_id` | Unique identifier for each mouse |
| `dot_id` | Sequential index of paw prints within a paw (must increase along walking direction) |
| `x` | X coordinate of paw print (pixels) |
| `y` | Y coordinate of paw print (pixels) |
| `paw` | Paw identity: `FL`, `FR`, `HL`, `HR` |

Strongly recommended:

| Column | Description |
|---|---|
| `image_id` | Identifier for the walking track / image |
| `pixels_per_cm` | Pixel-to-cm conversion factor (must be numeric) |
| `genotype` | Genotype label (e.g. `wt`, `het`, `ko`) |
| `treatment` | Treatment label (e.g. `vehicle`, `drug`) |

Other columns (e.g. `sex`, `color`) are allowed and ignored unless explicitly used.

> 💡 The Image → Data tab exports files in exactly this format.

---

## Gait measures computed

All measures are computed per track and summarized per mouse.

| Measure | Description |
|---|---|
| **Step length** | Forward distance between consecutive prints of the same paw |
| **FB distance (2D)** | Full 2D spacing between paired front and hind paws |
| **FB distance (x-only)** | Forward-only spacing between paired front and hind paws |
| **Perpendicular deviation** | Shortest distance from an intermediate paw to the line defined by two consecutive opposite-side paws |

---

## Troubleshooting

1. Check that `dot_id` increases correctly within each paw
2. Make sure `pixels_per_cm` is numeric and correct
3. Verify genotype/treatment spelling consistency
4. Try toggling "Straighten tracks" on/off
5. If paw detection finds too many/few blobs, adjust "Min blob size" in the Image → Data tab
6. Restart the app and re-upload the file

---

## Citation

If you use **GaitTrackR** in your research, please cite:

> Elbaek, CR. (2026). *GaitTrackR: An interactive Shiny app for mouse gait analysis from paw-print coordinate data*. GitHub. https://github.com/camillaelbaek/GaitTrackR

---

## License

This project is licensed under the **MIT License** — see the [LICENSE](LICENSE) file for details.

---

## Contact

For questions or bug reports, please open a [GitHub issue](https://github.com/camillaelbaek/GaitTrackR/issues).
