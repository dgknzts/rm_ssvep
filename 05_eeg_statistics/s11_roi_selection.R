# Libraries
library(dplyr)
source("_config.R")

# Define frequencies
f1 <- c(4.8, 6, 7.5)
ims <- c(10.8, 12.3, 13.5, 18.3)

# Read data
data <- read.csv(file.path(data_dir, "all_SNR_data_regular.csv")) %>%
  rename(Channels = Channel)

# Process fundamental frequencies
fundamental_roi_channels <- data %>%
  filter(Frequency %in% f1) %>%
  mutate(Side = ifelse(Side == 1, "Right", "Left")) %>%
  group_by(Side, Channels) %>%
  summarise(avg_SNR = mean(SNR, na.rm = TRUE)) %>%
  group_by(Side) %>%
  slice_max(order_by = avg_SNR, n = 6)

# Process IMS frequencies
ims_roi_channels <- data %>%
  filter(Frequency %in% ims) %>%
  mutate(Side = ifelse(Side == 1, "Right", "Left")) %>%
  group_by(Side, Channels) %>%
  summarise(avg_SNR = mean(SNR, na.rm = TRUE)) %>%
  group_by(Side) %>%
  slice_max(order_by = avg_SNR, n = 6)

# Extract channel lists
fundamental_channels_right <- fundamental_roi_channels %>% 
  filter(Side == "Right") %>% 
  pull(Channels)

fundamental_channels_left <- fundamental_roi_channels %>% 
  filter(Side == "Left") %>% 
  pull(Channels)

ims_channels_right <- ims_roi_channels %>% 
  filter(Side == "Right") %>% 
  pull(Channels)

ims_channels_left <- ims_roi_channels %>% 
  filter(Side == "Left") %>% 
  pull(Channels)

# Save channel lists
save(
  fundamental_channels_right,
  fundamental_channels_left,
  ims_channels_right,
  ims_channels_left,
  file = file.path(data_dir, "roi_channels.RData")
)

# Print results
print("Fundamental Frequencies Top Channels:")
print(fundamental_roi_channels)

print("\nIMS Frequencies Top Channels:")
print(ims_roi_channels)

# Create CSV for easy access
write.csv(
  rbind(
    data.frame(Group = "Fundamental Right", Channels = fundamental_channels_right),
    data.frame(Group = "Fundamental Left", Channels = fundamental_channels_left),
    data.frame(Group = "IMS Right", Channels = ims_channels_right),
    data.frame(Group = "IMS Left", Channels = ims_channels_left)
  ),
  file.path(data_dir, "roi_channels.csv"),
  row.names = FALSE
)
