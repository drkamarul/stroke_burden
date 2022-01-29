levels(factor(data1$redcap_event_name))
summary(data3)
glimpse(data3)


data2id <- data1 %>%
  select(record_id, 
        pt_name,
        cg_name,
        redcap_event_name,
        zbi_total_score)

data1id  

left_join(data2id, data1id, by = 'record_id')


res_by_fu <- 
  data3 %>%
  select(redcap_event_name, 
         pt_gender, pt_age, 
         cg_gender, 
         starts_with('zbi'), starts_with('cafu')) %>%
  group_by(redcap_event_name) %>%
  nest() %>%
  mutate(descri = map(.x = data, 
                      ~ tbl_summary(data = .x, by = 'cg_gender', 
                                    statistic = list(all_continuous() ~ "{mean} ({sd})"), 
                                    digits = all_continuous() ~ 2,)
                      ))


data3 %>%
  select(redcap_event_name, 
         pt_gender, pt_age, 
         cg_gender, 
         starts_with('zbi'), starts_with('cafu')) %>%
  group_by(redcap_event_name) %>%
  nest() %>%
  mutate(descri = map(.x = data, 
                      ~ tbl_summary(data = .x, by = 'cg_gender', 
                                    statistic = list(all_continuous() ~ "{mean} ({sd})"), 
                                    digits = all_continuous() ~ 2,)
                      )) %>%
  ungroup(descri)


