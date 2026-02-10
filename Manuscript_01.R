
library(readxl)
FINALIZED_DATA <- read_excel("~/WORKINGS/DATABASES/FINALIZED DATA.xlsx")
View(FINALIZED_DATA)

setwd("C:/Users/sulsowe/Documents/WORKINGS/PLOTS/Manuscript_01")

# TABLE:

# Cohort Age Breakdown Table

library(dplyr)

# 1. Standardize age units to months and fix missing sex data
cleaned_cohort <- FINALIZED_DATA %>%
  mutate(
    Age_In_Months = ifelse(Age_Type == "Days", round(Age / 30.44, 1), Age),
    Sex = ifelse(Sex == "" | is.na(Sex), "Unknown", Sex),
    Season = tools::toTitleCase(tolower(Season))
  )

# 2. Get quick count breakdowns
cleaned_cohort %>% count(Sex)
cleaned_cohort %>% count(Season)

# 3. Get median and range metrics for age
summary(cleaned_cohort$Age_In_Months)
median(cleaned_cohort$Age_In_Months, na.rm = TRUE)
#Figure 2a

library(tidyverse)
library(patchwork)
library(scales)
library(ggplot2)

# Proportions per year (For stacked bars)
finalzed_data_prop <- FINALIZED_DATA %>%
  filter(!is.na(`Site of Specimen Collection`) & `Site of Specimen Collection` != "")%>%
  count(Year, `Site of Specimen Collection`) %>%
  group_by(Year) %>%
  mutate(
    prop = n / sum(n)
  ) %>%
  ungroup()

# Total isolates per year (For inset)
finalzed_data_total <- FINALIZED_DATA %>%
  filter(!is.na(Year) & Year != "")%>% 
  count(Year, name = "total_isolates")   

# Main Plot 
p_main <- ggplot(finalzed_data_prop, aes(x = Year, y = prop, fill = `Site of Specimen Collection`)) +
  geom_col(width = 0.85, colour = "white", linewidth = 0.2) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_fill_brewer(palette = "Set1") +   # change if needed
  labs(
    x = "Year of isolation",
    y = "Proportion of isolates",
    fill = "Specimen type"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(linewidth = 0.6)
  )

# Inset plot: Total isolates per year
p_inset <- ggplot(finalzed_data_total, aes(x = Year, y = total_isolates)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  labs(
    x = NULL,
    y = "Isolates / year"
  ) +
  theme_classic(base_size = 8) +
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 9),
    plot.background = element_rect(colour = NA, fill = "transparent", linewidth = 0.4)
  )

# Combine inset inside main plot
final_plot <- p_main +
  inset_element(
    p_inset,
    left = 0.62,
    bottom = 0.60,
    right = 0.98,
    top = 0.95
  )

final_plot

p_main
p_inset

# Figure 2a Alternative

library(dplyr)
library(ggplot2)


# 1. Create counts


df_counts <- FINALIZED_DATA %>%
  filter(!is.na(`Site of Specimen Collection`) & `Site of Specimen Collection` != "")%>%
  count(Year, `Site of Specimen Collection`) %>%
  group_by(Year) %>%
  mutate(
    prop = n / sum(n)
  ) %>%
  ungroup()

# Total per year
year_totals <- FINALIZED_DATA %>%
  filter(!is.na(Year), Year != "") %>%
  count(Year, name = "total_year")

# Merge + proportions
df_plot <- df_counts %>%
  left_join(year_totals, by = "Year")


# 2. Scaling factor

scale_factor <- max(year_totals$total_year)

# 3. Plot


ggplot(df_plot, aes(x = Year)) +
  
  geom_col(aes(y = prop, fill = `Site of Specimen Collection`,),
           width = 0.8) +
  
  geom_line(data = year_totals,
            aes(y = total_year / scale_factor, group = 1),
            linewidth = 1, colour = "black") +
  
  geom_point(data = year_totals,
             aes(y = total_year / scale_factor),
             size = 2, colour = "black") +
  
  scale_y_continuous(
    name = "Proportion of isolates",
    labels = scales::percent_format(accuracy = 1),
    expand = c(0,0),
    sec.axis = sec_axis(~ . * scale_factor,
                        name = "Total isolates per year")
  ) +
  
  scale_fill_brewer(palette = "Set1") +
  
  labs(
    x = "Year",
    fill = "Specimen",
  ) +
  
  theme_minimal(base_size = 12)

ggsave("Fig 2a--.png", width = 7, height = 5, dpi = 300)

# FIGURE 2b

library(tidyverse)
library(patchwork)
library(scales)
library(ggplot2)
library(RColorBrewer)

# Proportions per year (For stacked bars)
finalzed_data_prop <- FINALIZED_DATA %>%
  filter(!is.na(`Clonal Complex`) & `Clonal Complex` != "")%>%
  filter(!is.na(Year) & Year != "")%>%
  count(Year, `Clonal Complex`) %>%
  group_by(Year) %>%
  mutate(
    prop = n / sum(n)
  ) %>%
  ungroup()

# ggplot (Stacked Bar)

colourCount = length(unique(finalzed_data_prop$`Clonal Complex`))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))             # a way to produce larger palettes by interpolating existing ones with constructor function colorRampPalette

Fig_2b <- ggplot(finalzed_data_prop, aes(x = Year, y = prop, fill = `Clonal Complex`))+
  geom_col(width = 0.85, colour = "white", linewidth = 0.2) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = getPalette(colourCount)) +   # change if needed
  labs(
    x = "Year of isolation",
    y = "Proportion of isolates",
    fill = "Clonal Complex",
  ) +
  theme_classic(base_size = 10) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(linewidth = 0.6)
  )

ggsave("Fig 2b.png", width = 8, height = 6, dpi = 300)

# ggplot(Stacked area)

ggplot(finalzed_data_prop,
       aes(x = Year, y = prop, fill = Clonal_Complex)) +
  geom_area(position = "stack", alpha = 0.9) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = getPalette(colourCount))+
  labs(
    x = "Year of Isolation",
    y = "Percentage of Isolates",
    fill = "Clonal Complex",
    title = expression("Distribution of " * italic("s. aureus") * " clonal complexes over time"),
    subtitle = expression("clinical " * italic("staphylococcus aureus") * " , The Gambia (2005-2023)")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )



# Figure 4

library(tidyverse)
library(patchwork)
library(scales)
library(ggplot2)
library(RColorBrewer)


finalzed_data_prop <- FINALIZED_DATA %>%
  filter(!is.na(`Clonal Complex`) & `Clonal Complex` != "") %>%
  filter(!is.na(`Site of Specimen Collection`) & `Site of Specimen Collection` != "") %>%
  count(`Site of Specimen Collection`, `Clonal Complex`) %>%
  group_by(`Site of Specimen Collection`) %>%
  mutate(
    "Proportion of Clonal Complex" = n / sum(n),
    total_n = sum(n) # Calculates total isolates per body site
  ) %>%
  ungroup()

# Plot
colourCount = length(unique(finalzed_data_prop$`Clonal Complex`))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))   

Fig_4 <- ggplot(finalzed_data_prop,
       aes(x = `Site of Specimen Collection`, y = `Proportion of Clonal Complex`, fill = `Clonal Complex`)) +
  geom_col(width = 0.8, colour = "white", linewidth = 0.2) +
  
  # FIX: Use geom_text with unique positions at y = 1
  geom_text(
    aes(y = 1, label = paste0("n = ", total_n)), 
    vjust = -0.5, 
    size = 3.5,
    check_overlap = TRUE # Prevents duplicate text layers since total_n is repeated per group
  ) +
  
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.1))) + 
  scale_fill_manual(values = getPalette(colourCount)) +
  theme_classic(base_size = 10) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.key.height = unit(1, "lines"))


ggsave("Fig 4.png", width = 12, height = 6, dpi = 300)


# Combined Figure
library(patchwork) # A composer of plots

final_plot <- (Fig_2b / Fig_4) 
ggsave("Clonal_Complexes1.png", width = 10, height = 5, dpi = 300)


# Combined fig 2b and 4

library(tidyverse)
library(patchwork)
library(scales)
library(ggplot2)
library(RColorBrewer)


# STEP 1: DEFINE A MASTER UNIFIED PALETTE 

# This extracts every single unique Clonal Complex across the ENTIRE dataset
# to guarantee colors never shuffle or scramble between different plots.
all_unique_ccs <- FINALIZED_DATA %>%
  filter(!is.na(`Clonal Complex`) & `Clonal Complex` != "") %>%
  pull(`Clonal Complex`) %>%
  unique() %>%
  sort() # Sorting ensures alphabetical stability

colourCount <- length(all_unique_ccs)
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

# Create a NAMED vector. This binds each specific CC code tightly to one specific hex color.
master_cc_colors <- getPalette(colourCount)
names(master_cc_colors) <- all_unique_ccs



# STEP 2: PLOT 1 (FIG 2b - STACKED BAR PER YEAR)

finalzed_data_prop_year <- FINALIZED_DATA %>%
  filter(!is.na(`Clonal Complex`) & `Clonal Complex` != "") %>%
  filter(!is.na(Year) & Year != "") %>%
  count(Year, `Clonal Complex`) %>%
  group_by(Year) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

Fig_2b <- ggplot(finalzed_data_prop_year, aes(x = Year, y = prop, fill = `Clonal Complex`)) +
  geom_col(width = 0.85, colour = "white", linewidth = 0.2) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  # USE THE NAMED VECTOR: Guarantees color consistency
  scale_fill_manual(values = master_cc_colors) +   
  labs(
    x = "Year of isolation",
    y = "Proportion of isolates",
    fill = "Clonal Complex"
  )



# STEP 3: PLOT 2 (FIG 4 - STACKED BAR PER BODY SITE)

finalzed_data_prop_site <- FINALIZED_DATA %>%
  filter(!is.na(`Clonal Complex`) & `Clonal Complex` != "") %>%
  filter(!is.na(`Site of Specimen Collection`) & `Site of Specimen Collection` != "") %>%
  count(`Site of Specimen Collection`, `Clonal Complex`) %>%
  group_by(`Site of Specimen Collection`) %>%
  mutate(
    `Proportion of Clonal Complex` = n / sum(n),
    total_n = sum(n) 
  ) %>%
  ungroup()

Fig_4 <- ggplot(finalzed_data_prop_site,
                aes(x = `Site of Specimen Collection`, y = `Proportion of Clonal Complex`, fill = `Clonal Complex`)) +
  geom_col(width = 0.8, colour = "white", linewidth = 0.2) +
  
  # Total isolate indicators printed clean above the bars
  geom_text(
    aes(y = 1, label = paste0("n = ", total_n)), 
    vjust = -0.5, 
    size = 3.5,
    check_overlap = TRUE 
  ) +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.1))) + 
  # USE THE SAME NAMED VECTOR HERE TOO
  scale_fill_manual(values = master_cc_colors) +
  labs(
    x = "Site of Specimen Collection",
    y = "Proportion of Clonal Complex",
    fill = "Clonal Complex"
  ) 


# STEP 4: UNIFICATION MASTER BLOCK

# Ensure your master vector contains all possible categories alphabetically
all_unique_ccs <- names(master_cc_colors)

# 1. Stack the figures cleanly without any conflicting internal themes or scales
final_combined_plot <- (Fig_2b / Fig_4) + 
  plot_layout(guides = "collect")

# 2. Inject identical scale attributes to guarantee unified scale properties
final_combined_plot <- final_combined_plot & 
  scale_fill_manual(
    values = master_cc_colors, 
    limits = all_unique_ccs,         # Forces identical categories
    drop = FALSE,                    # Keeps SINGLETON visible across panels
    name = "Clonal Complex"          # Identical legend header strings
  )

# 3. FORCE IDENTICAL OUTLINE LAYOUTS: This overrides the border mismatch 
#    and forces patchwork to collapse the legends clean.
final_combined_plot <- final_combined_plot &
  theme_classic(base_size = 10) & # Resets both panels to an identical theme framework
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    # Explicitly clear out any top/right border noise from individual steps
    axis.line = element_line(linewidth = 0.6),
    panel.border = element_blank()
  )

# 4. Save the finalized master canvas
ggsave(
  filename = "Clonal_Complexes.png", 
  plot = final_combined_plot, 
  width = 12,   
  height = 8, 
  dpi = 300,
  bg = "white"
)


# Figure 5: AMR resistance gene profile

top_CCs <- FINALIZED_DATA%>%
  count(`Clonal Complex`, sort = TRUE) %>%
  slice_head(n = 11) %>%
  pull(`Clonal Complex`)

df_filtered <- FINALIZED_DATA %>%
  filter(`Clonal Complex` %in% top_CCs)
 # Convert to long format required for ggplot2)

df_long <- df_filtered %>%
  pivot_longer(
    cols =  c("blaZ","dfrG","tet(K)","msr(A)","tet(M)","tet(L)","grlA","mecA",
               "aac(6')-aph(2'')","fusB","qacD","erm(C)","cat(pC233)","aph(3')-III",
               "ant(6)-Ia","ant(9)-Ia","erm(A)","gyrA","qacB","qacJ","rpoB","aadD",
               "bleO","mph(C)","vga(A)","dfrB","fusC","mecA1","sal(A)",
               "fosD","Isa(A)","vga(A)V","fosB","cat(pC221)","fusA"), 
    names_to = "Gene",
    values_to = "Presence"
  )

df_long <- df_long %>%
  filter(!is.na(`Clonal Complex`), `Clonal Complex` != "-")%>%
  mutate(
    Presence = ifelse(Presence == "Yes", 1, 0)
  )

df_heat <- df_long %>%
  group_by(`Clonal Complex`, Gene) %>%
  summarise(
    Presence = mean(Presence, na.rm = TRUE),  # proportion of isolates carrying gene
    .groups = "drop"
  )


# Define gene classes


gene_classes <- tibble(
  Gene = c(
    "blaZ","mecA","mecA1",
    "dfrG","dfrB",
    "tet(K)","tet(M)","tet(L)",
    "erm(C)","erm(A)","msr(A)","mph(C)","Isa(A)","vga(A)","vga(A)V",
    "aac(6')-aph(2'')","aph(3')-III","ant(6)-Ia","ant(9)-Ia","aadD",
    "cat(pC233)","cat(pC221)",
    "fusB","fusC","fusA",
    "fosB","fosD",
    "qacB","qacD","qacJ",
    "bleO",
    "sal(A)",
    "grlA","gyrA",
    "rpoB"
  ),
  
  Class = c(
    # Beta-lactam
    "Beta-lactam","Beta-lactam","Beta-lactam",
    
    # Trimethoprim
    "Trimethoprim","Trimethoprim",
    
    # Tetracycline
    "Tetracycline","Tetracycline","Tetracycline",
    
    # MLS (Macrolide-Lincosamide-Streptogramin)
    "MLS","MLS","MLS","MLS","MLS","MLS","MLS",
    
    # Aminoglycoside
    "Aminoglycoside","Aminoglycoside","Aminoglycoside","Aminoglycoside","Aminoglycoside",
    
    # Chloramphenicol
    "Chloramphenicol","Chloramphenicol",
    
    # Fusidic acid
    "Fusidic acid","Fusidic acid","Fusidic acid",
    
    # Fosfomycin
    "Fosfomycin","Fosfomycin",
    
    # QAC (Biocide) 
    "QAC","QAC","QAC",
    
    # Bleomycin
    "Bleomycin",
    
    # Pleuromutilin / Lincosamide / Streptogramin A Resistance
    "Lincosamide",
    
    # Fluoroquinolone
    "Fluoroquinolone","Fluoroquinolone",
    
    # Rifampicin
    "Rifampicin"
  ),
)


df_heat <- df_heat %>%
  left_join(gene_classes, by = "Gene")

# Order genes by class

df_heat <- df_heat %>%
  arrange(Class, Gene) %>%
  mutate(Gene = factor(Gene, levels = unique(Gene)))

# # Highlight key genes
highlight_genes <- c("blaZ", "dfrG", "mecA")

df_heat <- df_heat %>%
  mutate(
    highlight = ifelse(Gene %in% highlight_genes, "yes", "no")
  )

# Plot Heatmap 

library(ggplot2)
library(grid)

df_heat$Class_short <- recode(df_heat$Class,
                              "Aminoglycoside" = "Amino",
                              "Beta-lactam" = "B-lac",
                              "Trimethoprim" = "Prim",
                              "Tetracycline" = "Tetra",
                              "Macrolide-Lincosamide-Streptogramin" = "MLS",
                              "Chloramphenicol" = "Chlo",
                              "Fusidic acid" = "Fus",
                              "Fosfomycin" = "Fos",
                              "Bleomycin" = "Ble",
                              "Fluoroquinolone" = "Fluoro",
                              "Rifampicin" = "Fa",
                              "Lincosamide" = "Lin",
                              "Quaternary Ammonium Compound" = "QAC"
                              )
long_caption <- "AMR gene categories: Amino = Aminoglycoside; B-lac = Beta-lactam; Prim = Trimethoprim; Tetra = Tetracycline; MLS = Macrolide-Lincosamide-Streptogramin; Chlo = Chloramphenicol; Fus = Fusidic acid; Fos = Fosfomycin; 
Ble = Bleomycin; Fluoro = Fluoroquinolone; Fa = Rifampicin;Lin = Lincosamide; and QAC = Quaternary ammonium compounds."

PanelA <- ggplot(df_heat, aes(x = Gene, y = `Clonal Complex`, fill = Presence)) +
  
  # MAIN heatmap layer 
  geom_tile(color = "white", linewidth = 0.2) +

  # highlight selected genes
  geom_tile(
    data = subset(df_heat, highlight == "yes"),
    colour = "black",
    linewidth = 0.8,
    fill = NA
  ) +
  
  facet_grid(~ Class_short, scales = "free_x", space = "free_x") +
 
  scale_fill_gradient(
    low = "grey90",
    high = "red3",
    limits = c(0, 1),
    na.value = "white"
  ) +
  
  labs( 
    x = "AMR genes",
    y = "Clonal Complexes",
    fill = "Prevalence",
    caption = str_wrap(long_caption, width = 146) # wrap at 150 characters
  ) +
  
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8, face = "italic"),
    panel.spacing.x = unit(0.4, "lines"),
    panel.grid = element_blank(),
    plot.caption = element_text(hjust = 0.5, size = 8)
    
  )

ggsave("Fig 5.png", width = 12, height = 5, dpi = 300)


# FIGURE 6: Temporal trends in Antimicrobial resistance

library(tidyverse)

df_long <- FINALIZED_DATA %>%
  pivot_longer(
    cols = c("blaZ","dfrG","mecA",
             "tet(K)","tet(M)","tet(L)",
             "erm(A)","erm(C)"),
    names_to = "Gene",
    values_to = "Presence"
  ) %>%
  mutate(
    Presence = ifelse(Presence == "Yes", 1, 0),
    
    Gene_group = case_when(
      Gene == "blaZ" ~ "blaZ",
      Gene == "dfrG" ~ "dfrG",
      Gene == "mecA" ~ "mecA",
      str_detect(Gene, "^tet") ~ "Tetracycline (tet)",
      str_detect(Gene, "^erm") ~ "MLS (erm)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Gene_group))

# Yearly prevalence 

df_trend <- df_long %>%
  group_by(Year, Gene_group, Sample_ID) %>%
  summarise(Presence = max(Presence), .groups = "drop") %>%
  group_by(Year, Gene_group) %>%
  summarise(
    prevalence = mean(Presence),
    .groups = "drop"
  )

# Plot (Multiline)

library(ggplot2)
library(scales)

PanelB <- ggplot(df_trend,
       aes(x = Year, y = prevalence, colour = Gene_group)) +
  
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  
  scale_colour_manual(
    values = c(
      "blaZ" = "#1b9e77",
      "dfrG" = "#d95f02",
      "mecA" = "#7570b3",
      "Tetracycline (tet)" = "#e7298a",
      "MLS (erm)" = "#66a61e"
    )
  ) +
  
  labs(
    x = "Year",
    y = "Prevalence (%)",
    colour = "Resistance determinant"
  ) +
  
  theme_classic(base_size = 12)

ggsave("Fig 6.png", width = 8, height = 5, dpi = 300)


# FIGURE 7A: Virulence gene distribution over time

gene_class_map <- data.frame(
  Gene = c(
    # Adhesion (5)
    "clfA,clfB","fnbA,fnbB","cna","ebpS","sfa,sfb","lipoteichoic acid","peptidoglycan",
    
    # Biofilm (2)
    "icaA,icaB,icaC,icaD","bap",
    
    # Iron acquisition (1)
    "isdA,isdB,isdC",
    
    # Immune evasion (6)
    "sak","chp","cap operon","spa","nuc","coa,vwb",
    
    # Toxins (8)
    "hla/hly","hlb","hld","hlg,lukS-PV,lukF-PV","tst","sea-e, seg-sei","eta,etb","psmα, psmβ",
    
    # Quorum sensing(3)
    "agrA,agrB,agrC,agrD","sarA",
    "saeR,saeS",
    
    # Exoenzymes (6)
    "aur","coa,vwb","lip","nuc","sak","hysA"
    
  ),
  
  Class = c(
    rep("Adhesion", 7),
    rep("Biofilm", 2),
    rep("Iron acquisition", 1),
    rep("Immune evasion", 6),
    rep("Toxins", 8),
    rep("Quorum sensing", 3),
    rep("Exoenzymes", 6)
  ),
  
  stringsAsFactors = FALSE
)




virulence_genes <- c(
  "clfA,clfB","fnbA,fnbB","spa","cna","ebpS","coa,vwb","sak","hysA",
  "lip","nuc","hla/hly","hlb","hld","hlg,lukS-PV,lukF-PV","tst",
  "sea-e, seg-sei","eta,etb","cap operon","chp","isdA,isdB,isdC",
  "sfa,sfb","psmα, psmβ","agrA,agrB,agrC,agrD","sarA","saeR,saeS",
  "icaA,icaB,icaC,icaD","bap","aur","lipoteichoic acid","peptidoglycan"
)

 # Lets go
library(tidyverse)

# Long format virulence genes
df_long <- FINALIZED_DATA%>%
  pivot_longer(cols = all_of(virulence_genes),
               names_to = "Gene",
               values_to = "Presence")

# Convert Yes/No → numeric
df_long <- df_long %>%
  mutate(Presence = ifelse(Presence == "Yes", 1, 0))

# merge gene class
df_long <- df_long %>%
  left_join(gene_class_map, by = "Gene")

# Collapse to class-level per isolate
df_class <- df_long %>%
  group_by(Sample_ID, Year, Class) %>%
  summarise(
    Class_Presence = mean(Presence, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate yearly prevalence
df_time <- df_class %>%
  filter(!is.na(Class), Class != "") %>%
  filter(!is.na(Year), Year != "") %>%
  group_by(Year, Class) %>%
  summarise(
    Prevalence = mean(Class_Presence, na.rm = TRUE),
    .groups = "drop"
  )



# Plot 
library(ggplot2)
library(scales)
library(RColorBrewer)

Fig_7a <- ggplot(df_time, aes(x = Year, y = Prevalence, colour = Class)) +
  geom_line(linewidth = 1.2, alpha = 0.6) + # alpha < 1 makes lines visible when they overlap
  geom_point(size = 2) +
  
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  
  labs(
    x = "Year of Isolation",
    y = "Prevalence (%)",
    color = "Virulence Category"
  ) +
  
  theme_classic(base_size = 12) +
  theme(
    legend.position = "right",
    axis.line = element_line(linewidth = 0.6)
  )

ggsave("Fig 7a.png", width = 7, height = 5, dpi = 300)

# FIGURE 7B: Virulence gene distribution across lineages


# Lets go 

library(dplyr)
library(tidyr)
library(ggplot2)


top_CCs <- FINALIZED_DATA%>%
  count(`Clonal Complex`, sort = TRUE) %>%
  slice_head(n = 11) %>%
  pull(`Clonal Complex`)

df_filtered <- FINALIZED_DATA %>%
  filter(`Clonal Complex` %in% top_CCs)

# 2. Long format virulence genes

df_long <- df_filtered %>%
  pivot_longer(cols = all_of(virulence_genes),
               names_to = "Gene",
               values_to = "Presence")

# Convert Yes/No → numeric
df_long <- df_long %>%
  filter(!is.na(`Clonal Complex`), `Clonal Complex` != "-")%>%
  mutate(Presence = ifelse(Presence == "Yes", 1, 0))


# 3. Calculate prevalence

df_heat <- df_long %>%
  group_by(`Clonal Complex`, Gene) %>%
  summarise(Prevalence = mean(Presence, na.rm = TRUE), .groups = "drop")


# 4. Add gene class mapping

df_heat <- df_heat %>%
  left_join(gene_class_map, by = "Gene")

# Ensure order of categories
df_heat$Class <- factor(df_heat$Class,
                        levels = c("Adhesion", "Biofilm", "Iron acquisition",
                                   "Immune evasion", "Toxins", "Quorum sensing", "Exoenzymes"))


# 5. Plot

# Create short codes 
df_heat$Class_short <- recode(df_heat$Class,
                              "Adhesion" = "Adh",
                              "Biofilm" = "Bio",
                              "Iron acquisition" = "Iron",
                              "Immune evasion" = "Immune",
                              "Toxins" = "Tox",
                              "Exoenzymes" = "Exo",
                              "Quorum sensing" = "Quorum"
)

long_caption_vir <- "Virulence gene categories: Adh = Adhesion; Bio = Biofilm; Iron = Iron acquisition; Immune = Immune evasion; Tox = Toxins; Exo = Exoenzymes; Quorum = Quorum sensing."
Fig_7b <- ggplot(df_heat, aes(x = Gene, y = `Clonal Complex`, fill = Prevalence)) +
  
  geom_tile(color = "white", linewidth = 0.2) +
  
  facet_grid(~ Class_short, scales = "free_x", space = "free_x") +
  
  scale_fill_gradient(low = "grey90", high = "red3",
                      name = "Prevalence") +
  
  labs(
    x = "Virulence Genes",
    y = "Clonal Complex (CC)",
    
    caption = str_wrap(long_caption_vir, width = 197) # wrap at 60 characters
  ) +
  
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8, face = "italic"),
    panel.spacing.x = unit(1, "lines"),
    plot.caption = element_text(size = 8, hjust = 0.5)
  )

ggsave("Fig 7b.png", width = 12, height = 5, dpi = 300)

# Combined Figure
library(patchwork) # A composer of plots

final_plot <- (PanelB / PanelA) 
ggsave("AMR.png", width = 12, height = 6, dpi = 300)


final_plot <- ( Fig_7a / Fig_7b ) 
ggsave("Virulence.png", width = 12, height = 6, dpi = 300)
# Figure: Distribution of samples per sites

library(tidyverse)
library(scales)

data_invasive<- FINALIZED_DATA %>%
  count(Specimen_Type) %>%
  mutate(
    prop = n / sum(n) *100
  )

ggplot(data_site, aes(x = Site, y = prop)) +
  geom_col(width = 0.6, fill = "steelblue") +
  geom_text(aes(label = percent(prop, accuracy = 0.1)),
            vjust = -0.4,
            size = 4) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Collection Site",
    y = "Percentage of isolates"
  ) +
  theme_classic(base_size = 12)


# Distribution of detected resistance genes by class

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Classification vector
amr_class_map <- c(
  "blaZ" = "Beta-lactam",
  "mecA" = "Beta-lactam",
  "mecA1" = "Beta-lactam",
  
  "tet(K)" = "Tetracycline",
  "tet(M)" = "Tetracycline",
  "tet(L)" = "Tetracycline",
  
  "erm(A)" = "MLSB",
  "erm(C)" = "MLSB",
  "msr(A)" = "MLSB",
  "mph(C)" = "MLSB",
  "vga(A)" = "MLSB",
  "vga(A)V" = "MLSB",
  "Isa(A)" = "MLSB",
  
  "aac(6')-aph(2'')" = "Aminoglycoside",
  "aph(3')-III" = "Aminoglycoside",
  "ant(6)-Ia" = "Aminoglycoside",
  "ant(9)-Ia" = "Aminoglycoside",
  "aadD" = "Aminoglycoside",
  
  "dfrG" = "Trimethoprim",
  "dfrB" = "Trimethoprim",
  
  "gyrA" = "Fluoroquinolone",
  "grlA" = "Fluoroquinolone",
  
  "fosB" = "Fosfomycin",
  "fosD" = "Fosfomycin",
  
  "cat(pC233)" = "Chloramphenicol",
  "cat(pC221)" = "Chloramphenicol",
  
  "fusB" = "Fusidic acid",
  "fusC" = "Fusidic acid",
  "fusA" = "Fusidic acid",
  
  "rpoB" = "Rifampicin",
  
  "qacB" = "Biocide (QAC)",
  "qacD" = "Biocide (QAC)",
  "qacJ" = "Biocide (QAC)",
  
  "bleO" = "Bleomycin",
  "sal(A)" = "Pleuromutilin"
)

# Convert to long format and compute proportions
amr_long <- FINALIZED_DATA%>%
  pivot_longer(
    cols = names(amr_class_map),
    names_to = "Gene",
    values_to = "Presence"
  ) %>%
  mutate(
    Presence = tolower(Presence),
    Class = amr_class_map[Gene]
  ) %>%
  filter(Presence == "yes")

amr_class_proportions <- amr_long %>%
  count(Class) %>%
  mutate(Proportion = n / sum(n)) %>%
  arrange(desc(Proportion))


# Plot
ggplot(amr_class_proportions, aes(x = reorder(Class, Proportion), y = Proportion)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Distribution of AMR Determinants by Antimicrobial Class",
    x = "Antimicrobial Class",
    y = "Proportion of Total Detected Genes"
  ) +
  theme_minimal(base_size = 14)

# Isolate level antimicrobial class prevalence
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Convert to long format
amr_long <- FINALIZED_DATA %>%
  pivot_longer(
    cols = names(amr_class_map),
    names_to = "Gene",
    values_to = "Presence"
  ) %>%
  mutate(
    Presence = tolower(Presence),
    Class = amr_class_map[Gene]
  )

# Convert to isolate level class
isolate_class_presence <- amr_long %>%
  group_by(Sample_ID, Class) %>%   
  summarise(
    Class_Present = any(Presence == "yes"),
    .groups = "drop"
  )

# Calculate prevalence per class
class_prevalence <- isolate_class_presence %>%
  group_by(Class) %>%
  summarise(
    Prevalence = mean(Class_Present),
    .groups = "drop"
  ) %>%
  arrange(desc(Prevalence))

# Plot
ggplot(class_prevalence, aes(x = reorder(Class, Prevalence), y = Prevalence)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Isolate-Level Antimicrobial Class Prevalence",
    x = "Antimicrobial Class",
    y = "Prevalence (% of isolates)"
  ) +
  theme_minimal(base_size = 14)


# Calculating virulence class proportions

# Attach class mapping
df_long <- df_long %>%
  left_join(gene_class_map, by = "Gene")

# Collapse to isolate level
df_class <- df_long %>%
  group_by(Sample_ID, Class) %>%
  summarise(
    Class_Presence = max(Presence, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Cal overall prevalence
vir_class_prev <- df_class %>%
  group_by(Class) %>%
  summarise(
    Prevalence = mean(Class_Presence, na.rm = TRUE),
    .groups = "drop"
  )


# INVASIVE ISOLATES
library(dplyr)
INVASIVE_ISOLATES <- FINALIZED_DATA %>%
  filter(Specimen_Type == "INVASIVE")
  

