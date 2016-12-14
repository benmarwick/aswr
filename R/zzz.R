

# Match by intervals ------------------------------------------------------------

# Find which interval that each element of the vector belongs in

library(tidyverse)
elements <- c(0.1, 0.2, 0.5, 0.9, 1.1, 1.9, 2.1)

intervals <-
  frame_data(  ~phase, ~start, ~end,
               "a",     0,      0.5,
               "b",     1,      1.9,
               "c",     2,      2.5
  )


library(data.table) #v1.10.0
setDT(intervals)[data.table(elements), on = .(start <= elements, end >= elements)]

library(fuzzyjoin)
library(tidyverse)

fuzzy_left_join(data.frame(elements),
                intervals,
                by = c("elements" = "start",
                       "elements" = "end"),
                match_fun = list(`>=`, `<=`)) %>%
  distinct()

# Match by ranges ------------------------------------------------

# We assign the spits into layers

# Here's a list of the spits in each layer, for example
# layer 4 includes spits 4 to 7
spits_in_layers <- list(1, 2, 3, 4:7, 8:11,
                        12:14, 15:17, 18:22,
                        23:26, 27:31, 32:38,
                        39:50, 51:59)
layers <- 1:13
# get the start and end spits for each layer
spits_in_layers_start <- map_dbl(spits_in_layers, min)
spits_in_layers_end <- map_dbl(spits_in_layers, max)

# Compute which layer each spit belongs in using the
# IRanges package

# source("https://bioconductor.org/biocLite.R")
# biocLite("GenomicRanges")
library(IRanges)
spits_Ranges <-
  IRanges(na.omit(mmc1_extract_spit$spit),
          width = 1,
          names = na.omit(mmc1_extract_spit$spit))
spit_ranges_for_levels <-
  IRanges(start = spits_in_layers_start,
          end = spits_in_layers_end,
          names = layers)
olaps <- findOverlaps(spits_Ranges, spit_ranges_for_levels)
levels_from_spits <- subjectHits(olaps)

# random samples ----------------------------------------------------------

x <- 1:10
sample(x, 3)
replicate(10, rnorm(10))

library(dplyr)
data_frame(x = 1:10, y = x + rnorm(10))


# Find multiple strings in any order --------------------------------------

# for example, we might want to find all the artefacts classified as
# 'red green', 'redgreen', 'green red', how can we select those?

Reduce(intersect, lapply(matches, grep, my_vector))

matches <- c("fe", "ve")

#                1    2    3      4        5       6       7       8      9
my_vector <- c("fv", "v", "f", "f_v_e", "fe_ve", "feve", "vefe", "fve" , "a")

# want to get 5, 6, 7

Reduce(intersect, lapply(matches, grep, my_vector))


# last observation carried forward (LOCF) ---------------------------------

# basic use
library(zoo)
x <- c(1, NA, NA, NA, 2, NA, NA, NA, 3, NA, NA, NA)
y <- na.locf(x)

# more complex: by group
library(dplyr)
dfr <- frame_data(
         ~group, ~value,
         'a', 2,
         'a', NA,
         'a', NA,
         'b', 3,
         'b', NA,
         'b', NA)

dfr %>%
  group_by(group) %>%
  mutate(value = na.locf(value,
                         na.rm = TRUE))


# plot stacked bars where width = depth of layer --------------------------

# Plot frequencies of items in groups by depth

dfr <- data.frame(
  V1 = c(0.1, 0.2, 0.3, 0.01, 0.5),
  V2 = c(0.2, 0.3, 0.2, 0.01, 0.4),
  V3 = c(0.3, 0.6, 0.5, 0.01, 0.3),
  V4 = c(0.5, 0.1, 0.7, 0.01, 0.1),
  XX = 1:5,
  end_depth = c(1.5, 2.2, 3.9, 4.2, 5.4)
)

# mid points should be   1.5/2, (2.2-1.5)/2 + 1.5
# 0.75, 1.85, 3.05,
# widths should be  2.2 - 1.5
# 1.5, 0.7, 1.7



dfr %>%
  mutate(x_centre = c(end_depth[1]/2, zoo::rollmean(end_depth, 2)),
         x_width = c(end_depth[1], diff(end_depth))) %>%
  gather(variable,
         value,
         -XX,
         -x_centre,
         - x_width) %>%
  filter(variable != "end_depth") %>%
  ggplot(aes(x_centre,
             value,
             fill = variable)) +
  geom_bar(position = "fill",
           stat = "identity",
           aes(width = x_width))
