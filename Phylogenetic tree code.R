# Load required packages
library(tidyverse)
library(ggtree)
library(treeio)
library(tidytree)
library(ggtreeExtra)
library(ggfun)
library(ggnewscale)
library(patchwork)
library(readxl)

# Load data
tree <- read.newick("Newick Export.nwk")
WRKY_info <- read_excel("WRKY_information.xlsx")

# Create group list
group_list <- split(WRKY_info$ID, WRKY_info$Group)

# Group the tree
grouped_tree <- groupOTU(tree, group_list)

# Define color palette
group_colors <- c(
  "Group I" = "#FF9999", 
  "Group II-a" = "#377EB8", 
  "Group II-b" = "#4DAF4A", 
  "Group II-c" = "#984EA3", 
  "Group II-d" = "#FF7F00", 
  "Group II-e" = "#99CCFF",
  "Group III" = "#A65628"
)

# Create the base tree
p <- ggtree(grouped_tree, layout = "circular", open.angle = 15) +
  geom_tree(aes(color = group), show.legend = FALSE) +  # Turn off legend for geom_tree
  scale_color_manual(values = group_colors, na.value = "grey50") +
  geom_tiplab(align = TRUE, linesize = 0.5, offset = 0.9) +
  theme_tree2()+
  theme(
    plot.margin = unit(c(0, 0, -1, 0), "cm")  # Top, right, bottom, left margins
  )



# Add bootstrap values
p <- p + geom_nodepoint(
  aes(fill = cut(as.numeric(label), c(0, 50, 75, 100))),
  shape = 21,
  size = 2,
  na.rm = TRUE
) +
  scale_fill_manual(
    values = c("(75,100]" = "black", "(50,75]" = "grey", "(0,50]" = "white", "NA" = "transparent"),
    guide = 'legend',
    name = 'Bootstrap Percentage',
    breaks = c('(75,100]', '(50,75]', '(0,50]'),
    labels = c("BP > 75", "50 < BP ≤ 75", "BP ≤ 50"),
    na.value = "transparent"
  )

# Add group information
p <- p + new_scale_fill() +
  geom_fruit(
    data = WRKY_info,
    geom = geom_tile,
    mapping = aes(y = ID, x = "Group", fill = Group),
    offset = 0.2,
    width = 0.3
  ) +
  scale_fill_manual(values = group_colors)
# Adding a line below subcellullar localization
p <- p + new_scale_fill() +
  geom_fruit(
    data = WRKY_info,
    geom = geom_tile,
    mapping = aes(y = ID, x = "Group"),
    offset = 0.009,
    width = 0.04
  ) +
  scale_fill_manual(values = group_colors)
# Add subcellular localization
p <- p + new_scale_color() +
  geom_fruit(
    data = WRKY_info,
    geom = geom_point,
    mapping = aes(y = ID, x = "Localization", color = subcellular),
    offset = 0.05,
    size = 2
  ) +
  scale_color_brewer(palette = "Set2", name = "Subcellular Localization")
# Adding a line below bars
p <- p + new_scale_fill() +
  geom_fruit(
    data = WRKY_info,
    geom = geom_tile,
    mapping = aes(y = ID, x = "Group"),
    offset = 0.06,
    width = 0.04
  ) +
  scale_fill_manual(values = group_colors)
# Add gene length
p <- p + new_scale_fill() +
  geom_fruit(
    data = WRKY_info,
    geom = geom_bar,
    mapping = aes(y = ID, x = Length, fill = Group),
    orientation = "y",
    stat = "identity",
    offset = 0.001,
    width = 0.5
  ) +
  scale_fill_manual(values = group_colors, guide = "none")  # Hide the legend for gene length


p <- p + theme(
  # Place legend inside the plot at specific coordinates (x, y)
  legend.position = c(1.0, 0.5),  # Adjust the coordinates as needed
  # Customize legend title and text
  legend.title = element_text(size = 12, face = "bold"),  # Title size and style
  legend.text = element_text(size = 12),  # Text size
  # Adjust legend background
  legend.background = element_rect(fill = "white", color = "black"),  # Background color and border
  # Adjust spacing between legends
  legend.spacing.y = unit(0.1, "cm"),  # Vertical spacing
  legend.spacing.x = unit(0.1, "cm"),  # Horizontal spacing
  # Remove radial axis labels and ticks
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  panel.grid = element_blank()
)
# Display the plot
print(p)

# Save the plot
ggsave("/Users/macbook/Documents/at tree/WRKY_tree_plot.pdf", p, width = 25, height = 18, limitsize = FALSE)

