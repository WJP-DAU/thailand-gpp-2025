# WJP 2025 Thailand General Population Poll (GPP) Report

This repository contains the code for producing the World Justice Project's 2025 Thailand GPP Report. The project combines R-based data processing and visualization with a Flask web application for HTML report generation.

## Table of Contents

- [Project Overview](#project-overview)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Project Structure](#project-structure)
- [Usage](#usage)
- [Data Pipeline](#data-pipeline)
- [Visualization Types](#visualization-types)
- [Report Generation](#report-generation)
- [Configuration](#configuration)
- [Authors](#authors)

## Project Overview

The Thailand GPP Report presents findings from the World Justice Project's General Population Poll conducted in Thailand. The report covers topics including:

- Fundamental Rights and Freedoms
- Discrimination Experiences
- Trust and Corruption Perceptions
- Security and Criminal Justice
- Rule of Law Index (ROLI) Comparisons

The project uses a dual-stack architecture:
- **R** for data wrangling and chart generation (ggplot2-based visualizations)
- **Python/Flask** for HTML report rendering with Jinja2 templates

## Prerequisites

### R Environment
- R version 4.4.1 or higher
- RStudio (recommended)
- `renv` package for dependency management

### Python Environment
- Python 3.8 or higher
- pip package manager

### External Dependencies
- Access to WJP OneDrive/SharePoint for fonts (Inter Tight font family)
- Font files located in: `OneDrive - World Justice Project/Data Analytics/6. Country Reports/0. Fonts/`

## Installation

### 1. Clone the Repository

```bash
git clone https://github.com/[organization]/thailand-gpp-2025.git
cd thailand-gpp-2025
```

### 2. Set Up R Environment

```bash
cd data-viz
```

Open R or RStudio and run:

```r
# Install renv if not already installed
install.packages("renv")

# Restore project dependencies from renv.lock
renv::restore()
```

This will install all required R packages including:
- `tidyverse` (data manipulation)
- `ggplot2` (visualization)
- `ggrepel` (label placement)
- `ggtext` (rich text in plots)
- `readxl` (Excel file reading)
- `showtext` (custom font support)
- `optparse` (CLI argument parsing)
- `WJPr` (WJP custom visualization package)

### 3. Set Up Python Environment

```bash
cd report

# Create virtual environment (optional but recommended)
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install dependencies
pip install flask pandas beautifulsoup4 python-docx markdown
```

### 4. Configure User Paths

Edit `data-viz/R/config.R` to add your OneDrive paths:

```r
paths <- list(
  "your_username" = list(
    "path2DA"  = "/path/to/OneDrive - World Justice Project/Data Analytics",
    "path2GPP" = "/path/to/OneDrive - World Justice Project/General Population Poll"
  )
)
```

## Project Structure

```
thailand-gpp-2025/
├── data/                              # Raw data files
│   ├── thailand_gpp_data.csv         # Main GPP survey data
│   ├── thailand_data_bank.csv        # Comparative APAC data
│   ├── ROLI_data.xlsx                # Rule of Law Index data
│   └── tabs/                         # Generated data tabs per figure
│
├── data-viz/                         # R visualization project
│   ├── main.R                        # Entry point with CLI options
│   ├── renv.lock                     # R package dependencies
│   ├── data-viz.Rproj               # RStudio project file
│   ├── R/
│   │   ├── config.R                  # Configuration (paths, fonts)
│   │   ├── outline.R                 # Chart specifications/metadata
│   │   ├── data_loading.R            # Data wrangling routines
│   │   ├── data_visualization.R      # Main visualization orchestrator
│   │   ├── viz_dumbbells.R           # Dumbbell chart functions
│   │   ├── viz_waffle.R              # Waffle chart function
│   │   ├── viz_logit.R               # Logit visualization
│   │   ├── viz_rose.R                # Rose/radar diagram
│   │   ├── viz_roli.R                # ROLI chart module
│   │   ├── viz_dots_roli.R           # ROLI dots visualization
│   │   ├── viz_donuts2.R             # Donut chart variant
│   │   └── viz_horizontal_edge_bars.R # Edge bar visualization
│   └── outputs/                      # Generated SVG charts
│
├── report/                           # Flask web application
│   ├── app.py                        # Flask main application
│   ├── index.html                    # Generated HTML report output
│   ├── templates/
│   │   ├── index.html                # Main Jinja2 template
│   │   ├── functions.py              # Python helper functions
│   │   ├── general.html              # General sections template
│   │   ├── thematic_findings.html    # Thematic findings template
│   │   ├── executive_findings.html   # Executive summary template
│   │   └── methodology.html          # Methodology section template
│   ├── text/                         # Report content (markdown/docx)
│   │   ├── about_this_report.md
│   │   ├── acknowledgements.md
│   │   ├── executive_findings.md
│   │   ├── methodology.md
│   │   └── sample_description.md
│   └── static/
│       ├── assets/                   # CSS, images, branding
│       ├── charts_and_images/        # Chart outputs (Figure_1/, Figure_2/, etc.)
│       └── js/                       # JavaScript files
│
├── report_outline.xlsx               # Central configuration file
├── CLAUDE.md                         # Claude Code guidance
└── README.md                         # This file
```

## Usage

### Generate Visualizations (R)

From the `data-viz/` directory:

```bash
# Load and wrangle data only
Rscript main.R --data

# Generate all visualizations only
Rscript main.R --viz

# Full pipeline (data + visualizations)
Rscript main.R --data --viz

# Silent mode (no verbose output)
Rscript main.R --data --viz --verbose=FALSE
```

Alternatively, in RStudio:
1. Open `data-viz/data-viz.Rproj`
2. Source individual scripts interactively

### Run Flask Web Server (Python)

From the `report/` directory:

```bash
python app.py
```

The server starts at `http://localhost:5001`. Accessing this URL:
1. Renders the full HTML report
2. Prettifies the HTML using BeautifulSoup
3. Saves the output to `report/index.html`

## Data Pipeline

```
┌─────────────────────────────────────────────────────────────────┐
│                         DATA SOURCES                            │
├─────────────────────────────────────────────────────────────────┤
│  thailand_gpp_data.csv    → GPP survey responses                │
│  thailand_data_bank.csv   → Comparative APAC metrics            │
│  ROLI_data.xlsx           → Rule of Law Index scores            │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    R DATA PROCESSING                            │
├─────────────────────────────────────────────────────────────────┤
│  config.R      → Load fonts, set paths                          │
│  outline.R     → Define chart specifications                    │
│  data_loading.R → Filter variables, wrangle data                │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    R VISUALIZATION                              │
├─────────────────────────────────────────────────────────────────┤
│  data_visualization.R orchestrates:                             │
│    → viz_dumbbells.R    (comparison charts)                     │
│    → viz_waffle.R       (proportion displays)                   │
│    → viz_rose.R         (radar/spider charts)                   │
│    → viz_roli.R         (ROLI comparisons)                      │
│    → viz_donuts2.R      (donut charts)                          │
│    → viz_horizontal_edge_bars.R (bar charts)                    │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    SVG OUTPUT                                   │
├─────────────────────────────────────────────────────────────────┤
│  Saved to: report/static/charts_and_images/Figure_X/            │
│  Format: SVG (scalable vector graphics)                         │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    FLASK REPORT GENERATION                      │
├─────────────────────────────────────────────────────────────────┤
│  app.py reads:                                                  │
│    → report_outline.xlsx (structure, page order, metadata)      │
│    → text/*.md (narrative content)                              │
│    → static/charts_and_images/ (generated charts)               │
│                                                                 │
│  Renders via Jinja2 templates → Prettifies with BeautifulSoup   │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    HTML OUTPUT                                  │
├─────────────────────────────────────────────────────────────────┤
│  report/index.html (200+ KB formatted HTML report)              │
└─────────────────────────────────────────────────────────────────┘
```

## Visualization Types

The project generates several types of visualizations:

| Type | Function | Description |
|------|----------|-------------|
| Large Dumbbells | `gen_large_dumbbells()` | Multi-panel comparison charts with connected dots |
| Single Dumbbells | `gen_single_dumbbells()` | Single-row comparison charts |
| Bars | `WJPr::wjp_bars()` | Horizontal bar charts |
| Slopes | `WJPr::wjp_slope()` | Time-series slope charts |
| Radar | `WJPr::wjp_radar()` | Spider/radar charts for multi-variable comparison |
| Lollipops | `WJPr::wjp_lollipops()` | Lollipop charts |
| Waffle | `gen_waffle()` | Waffle charts for proportions |
| Donut | `donut_plot2()` | Donut/pie charts |
| Police Bars | `horizontal_edgebars()` | Edge-aligned horizontal bars |
| ROLI Rose | `gen_roli_rose()` | Rule of Law Index rose diagrams |
| ROLI Dots | `gen_roli_dots.fn()` | ROLI regional comparison dots |

## Report Generation

### Configuration Hub: `report_outline.xlsx`

This Excel file serves as the central configuration with multiple sheets:

| Sheet | Purpose |
|-------|---------|
| `general_info` | Report metadata (title, subtitle, description) |
| `outline` | Page structure, section IDs, page numbers, macros |
| `figure_map` | Chart-to-section mappings, titles, subtitles, legends |
| `methodological_materials` | Links to methodology resources |
| `other_publications` | Related WJP publications |

### Template Macros

The `thematic_findings.html` template supports multiple panel layouts:
- `section` - Section header pages
- `singlepanel` - Single chart per page
- `bipanel` - Two charts per page
- `tripanel` - Three charts per page
- `quadpanel` - Four charts per page
- `pentapanel` - Five charts per page
- `hexpanel` - Six charts per page

### Helper Functions (`templates/functions.py`)

| Function | Purpose |
|----------|---------|
| `df2dict()` | Convert DataFrame to nested dictionary |
| `get_section_data()` | Extract section metadata from outline |
| `get_page_data()` | Build complete page data structure |
| `load_markdown_file()` | Load and wrap markdown content |
| `process_word()` | Extract formatted text from Word documents |
| `get_dynamic_data()` | Assemble all dynamic report data |
| `get_thematic_parameters()` | Get parameters for thematic pages |

## Configuration

### Chart Specifications (`data-viz/R/outline.R`)

Each figure is defined as a list with:

```r
"Figure_X" = list(
  figure_id = "Figure_X",
  panel = "A",
  chart_title = "Chart Title",
  chart_subtitle = "Subtitle text...",
  var_id = c("variable1", "variable2"),  # Variables to plot
  reportValues = c("1", "2"),             # Response values to include
  type = "Large Dumbbells",               # Visualization type
  legend_text = c("Label1", "Label2"),
  legend_color = c("#2a2a94", "#a90099"),
  sample = "National",                    # Sample filter
  years = c("2018", "2025")              # Years to include
)
```

### Color Palette

Primary colors used throughout:
- `#2a2a94` - WJP Blue (primary)
- `#a90099` - WJP Magenta (secondary)
- `#575796` - WJP Purple (accent)
- `#524F4C` - Dark gray (text)
- `#c4c4c4` - Light gray (gridlines)

## Authors

- **Carlos Toruno** - ctoruno@worldjusticeproject.org
- **Santiago Pardo** - spardo@worldjusticeproject.org

World Justice Project - Data Analytics Team

---

For more information about the World Justice Project and the Rule of Law Index, visit [worldjusticeproject.org](https://worldjusticeproject.org).
