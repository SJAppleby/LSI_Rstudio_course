#Challenges!

#1-4
Ca %>% 
  ggplot(aes(x = frame, y = intensity, color = genotype, 
             group = genotype)) +
  geom_smooth(level = 0.99, size = 0.5, span = 0.1, method = "loess") +
  geom_line(aes(group = sample), size =  0.5, alpha = 0.1) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(face = "bold"))+
  scale_colour_manual(values = Okabe_Ito)+
  facet_wrap(vars(cell, genotype))

#5
Ca %>%
  ggplot(aes(x = genotype, y = intensity, color = genotype, 
             group = genotype))+
  geom_boxplot()
#I don't understand the point of this data in a boxplot

#6
#tidying I have to do first to get it to work
Syn_tb_sa <- Syn_data %>%
  rename_with(~ gsub("aSyn_D", "aSyn-D", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("_13", "-13", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("_0.6", "-0.6", .x, fixed = TRUE)) %>%
  pivot_longer(matches("aSyn"), 
               names_to = c("condition", "sample"), names_sep = "_",
               values_to = "fluorescence") %>%
  group_by(condition)

Syn_tb_sa <- Syn_tb_sa%>%
  mutate(sample = case_when(
    sample == 1 ~ "B",
    sample == 2 ~ "C",
    TRUE ~ "A"
  ))

Syn_tb_sa %>%
  ggplot(aes(x = Time, y = fluorescence, color = condition)) +
  geom_smooth() +
  #geom_line(aes(group = sample), size =  0.5, alpha = 0.1)+
  #facet_wrap(vars(condition))+
  geom_point(aes(colour = sample), size =  0.5, alpha = 0.1)+
  theme_minimal()

Syn_tb_sa %>%
  ggplot(aes(x = Time, y = fluorescence, color = condition,
             group = condition)) +
  geom_smooth() +
  geom_line(aes(group = sample), size =  0.5, alpha = 0.1)+
  #facet_wrap(vars(condition))+
  #geom_point(aes(group = sample), size =  0.5, alpha = 0.1)+
  theme_classic()

##doesn't work because sample number not unique for each condition 
##- can't just have 1,2,3 for all or it tries to join up all 1's

