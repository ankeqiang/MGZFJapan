**Japan-Trained Chinese Officials in China’s Wartime Central Administration (1944)**
Overview

This repository presents the data, figures, and scripts used in the research paper:
"Japan-Trained Chinese Officials in China’s Wartime Central Administration (1944)"
by Christian Henriot (Aix-Marseille University).
The study analyzes a unique group of 402 Chinese officials educated in Japan who held positions in the Nationalist central administration in March 1944, using a combination of prosopographic data, press analysis, and digital methods such as network and sequence analysis.
Project Structure

The repository is organized into six main folders:

Figures/      → All figures used in the final paper (PNG, SVG, or PDF format)
Maps/         → Geographical visualizations and spatial data files
Markdowns/    → Annotated research notes and intermediate text outputs in Markdown format
Networks/     → Graph files and network analysis outputs (Cytoscape)
Scripts/      → Code used for data analysis (R)
Tables/       → Cleaned datasets, summary statistics, and career sequence tables (CSV, Excel)

Description of Contents
📊 Figures

Includes publication-ready visualizations:

    Distribution of officials by birthplace and institution

    Network graphs (e.g., co-occurrence in Shenbao articles)

    Career sequence visualizations and PCA cluster plots

🗺️ Maps

Contains static and interactive maps of:

    Regional origins of Japan-trained officials

    Institutional and administrative mobility across China

📄 Markdowns

Houses interim working notes, data summaries, and topic modeling outputs that trace thematic evolution in the historiography and press mentions.
🕸️ Networks

Includes:

    CSV and Excel files for Cytoscape visualization

    Computed metrics (centrality, clustering, etc.)

    Temporal networks (Cytoscape) across 1920–1949 based on Shenbao co-occurrences

💻 Scripts

All scripts used for data preprocessing, analysis, and visualization:

    R scripts for sequence analysis and clustering

    Python notebooks for data cleaning and topic modeling

    Network construction routines from name/entity co-occurrence data

📂 Tables

Final datasets used in the study, including:

    Structured data extracted from the Zhiyuanlu (職員錄) directory

    Education and career data of the 402 Japan-trained officials

    Career trajectory sequences and typology clusters

    Metadata of Shenbao mentions (1920–1949)

Data Sources

    Primary Dataset: Minguo zhengfu ge yuanhui keyuan yishang zhiyuanlu (1944), a personnel directory covering officials from the rank of keyuan to ministerial positions in the wartime Nationalist government.

    Press Data: Mentions of the 402 officials extracted from the digital archives of Shenbao (Shanghai, 1920–1949), used to assess their visibility and institutional networks in public discourse.

    Historiographic Corpus: Metadata and abstracts of 446 Chinese-language studies on Chinese students in Japan, analyzed through topic modeling.

Methodology

    Prosopographic Analysis: Systematic profiling of educational and administrative careers.

    Sequence Analysis: Career trajectories modeled through Optimal Matching and PCA.

    Network Analysis: Institutional and individual networks derived from Shenbao co-mentions.

    Topic Modeling: Applied to Chinese historiography to identify dominant research themes.

Citation

If you use the data or analysis provided in this repository, please cite:

Henriot, Christian. "Japan-Trained Chinese Officials in China’s Wartime Central Administration (1944)," Twentieth-Century China, Vol. 51, no. xx, 2026 (forthcoming)
