# ============================================================================
# P2P网络（人物-人物网络）
# ============================================================================

# 提取PERSON实体
zsh_PerStrt <- zsh_sbconc_ner_6 %>% 
  filter(Type == "PERSON") %>%
  unique()  # 1041037条

write_csv(zsh_PerStrt, "zsh_PerStrt.csv")

# 提取总商会附近15字符的PERSON实体
person_data <- zsh_PerStrt[zsh_PerStrt$Type == "PERSON", ]

results <- list()
for (i in 1:nrow(zsh_data)) {
  current_entry <- zsh_data[i, ]
  same_doc_persons <- person_data[person_data$DocId == current_entry$DocId, ]
  close_persons <- same_doc_persons[
    abs(same_doc_persons$Start - current_entry$End) <= 15 |
    abs(same_doc_persons$End - current_entry$Start) <= 15, 
  ]
  if (nrow(close_persons) > 0) {
    results[[length(results) + 1]] <- close_persons
  }
}

final_results_person <- do.call(rbind, results) %>% 
  unique() %>% 
  select(-7)  # 11639条

# 删除敬语
honorifics_to_remove <- c("云敬", "云頃", "文云", "諸公", "公鑒", 
                          "奉鈞", "鑒元", "台鑒", "均鑒")

final_results_person_2 <- final_results_person %>%
  filter(!Text %in% honorifics_to_remove) %>%
  mutate(Text = str_replace(Text, "君$", "")) %>%
  filter(str_length(Text) > 1)  # 11241条

# 人名标准化
final_results_person_clean <- final_results_person_2 %>%
  mutate(Text_clean = Text %>%
           str_replace_all("\\s+", " ") %>%
           str_trim() %>%
           str_replace_all("\u3000", " "))

dict <- tibble::tibble(
  variant = c(
    "虞和德", "虞和德啓", "虞洽", "虞洽節", "虞洽老", "虞洽卿", "虞洽卿", "虞卿", "虞治卿",
    "馮少山", "馮培熹", "馮培熺",
    "方椒伯", "方椒伯二", "方椒", "方積蕃", "方伯", "方栩伯",
    "朱葆三", "朱葆", "朱葆珊", "朱佩珍", "朱公葆三", "朱偑珍",
    "林康侯", "林康候", "林康",
    "沈聯芳", "沈聨芳",
    "趙晋卿", "趙晉卿", "趙錫恩",
    "葉惠鈞", "葉惠釣",
    "孫傳芳", "孫停芳", "孫聨帥", "孫聯帥", "孫馨帥", "孫馨師",
    "王曉籟", "王孝賚",
    "宋漢章", "宋漢", "宋漢傘",
    "周金箴", "周金旗",
    "聞蘭亭", "聞闌亭",
    "聶雲台", "聶雲臺", "聶君雲", "聶其杰",
    "穆藕初", "穆抒齋", "穆藉初", "穆杼齋", "穆藕",
    "傅筱庵", "傅筱菴", "傅宗耀", "傅筱董",
    "盧永祥", "盧護軍"
  ),
  standard = c(
    rep("虞治卿", 9), rep("馮少山", 3), rep("方椒伯", 6),
    rep("朱葆三", 6), rep("林康侯", 3), rep("沈聯芳", 2),
    rep("趙晋卿", 3), rep("葉惠鈞", 2), rep("孫傳芳", 6),
    rep("王曉籟", 2), rep("宋漢章", 3), rep("周金箴", 2),
    rep("聞蘭亭", 2), rep("聶雲台", 4), rep("穆藕初", 5),
    rep("傅筱庵", 4), rep("盧永祥", 2)
  )
)

dict <- dict %>%
  mutate(variant_clean = variant %>%
           str_replace_all("\\s+", " ") %>%
           str_trim() %>%
           str_replace_all("\u3000", " "))

final_results_person_standardized <- final_results_person_clean %>%
  left_join(dict, by = c("Text_clean" = "variant_clean")) %>%
  mutate(Text_Standard = dplyr::coalesce(standard, Text)) %>%
  select(-8, -9, -10) %>%
  filter(str_length(Text) > 1)

write.csv(final_results_person_standardized, "final_results_person_standardized_2.csv")

# 生成P2P边和节点
edges_person <- final_results_person_standardized %>%
  group_by(DocId) %>%
  distinct(Text_Standard) %>%
  filter(n() >= 2) %>%
  summarise(combinations = list(combn(Text_Standard, 2, simplify = FALSE)),
            .groups = 'drop') %>%
  unnest(combinations) %>%
  mutate(source = map_chr(combinations, 1),
         target = map_chr(combinations, 2)) %>%
  select(source, target, DocId)

edges_weighted_person <- edges_person %>%
  group_by(source, target) %>%
  summarise(weight = n(), doc_ids = list(DocId), .groups = 'drop')

nodes_person <- final_results_person_standardized %>%
  distinct(Text_Standard) %>%
  rename(id = Text_Standard) %>%
  mutate(degree = map_int(id, ~sum(final_results_person_standardized$Text_Standard == .x)),
         docs = map(id, ~unique(final_results_person_standardized$DocId[final_results_person_standardized$Text_Standard == .x])))

# 权重阈值分析
weight_threshold <- 4
edges_filtered_person <- edges_weighted_person %>% filter(weight > weight_threshold)

nodes_in_edges_person <- unique(c(edges_filtered_person$source, 
                                   edges_filtered_person$target))
nodes_filtered_person <- nodes_person %>% filter(id %in% nodes_in_edges_person)

# 创建P2P网络
g_person <- tbl_graph(nodes = nodes_filtered_person,
                      edges = edges_filtered_person,
                      directed = FALSE) %>%
  activate(nodes) %>%
  mutate(degree_network = centrality_degree(),
         betweenness = centrality_betweenness(),
         closeness = centrality_closeness())

# 可视化P2P网络
set.seed(123)

p_person_basic <- ggraph(g_person, layout = 'fr') +
  geom_edge_link(aes(width = weight), alpha = 0.3, color = 'gray60') +
  geom_node_point(aes(size = degree_network), color = 'steelblue', alpha = 0.8) +
  geom_node_text(aes(label = id), repel = TRUE, size = 3, max.overlaps = 20) +
  scale_edge_width(range = c(0.5, 3)) +
  scale_size_continuous(range = c(2, 12)) +
  theme_graph() +
  labs(title = '人物共现网络 (P2P Network)',
       subtitle = paste('Weight >', weight_threshold),
       size = 'Network Degree')

print(p_person_basic)
ggsave("P2P_basic.png", p_person_basic, width = 12, height = 8, dpi = 500)

# 介数中心性分析
p_person_betweenness <- ggraph(g_person, layout = 'fr') +
  geom_edge_link(aes(width = weight), alpha = 0.2, color = 'gray70') +
  geom_node_point(aes(size = degree_network, color = betweenness), alpha = 0.8) +
  geom_node_text(aes(label = id), repel = TRUE, size = 3, max.overlaps = 20) +
  scale_edge_width(range = c(0.5, 3)) +
  scale_size_continuous(range = c(2, 12)) +
  scale_color_gradient(low = 'lightblue', high = 'red', name = 'Betweenness\n(桥梁作用)') +
  theme_graph() +
  labs(title = '人物网络 - Betweenness分析',
       subtitle = '红色 = 关键桥梁人物',
       size = 'Degree')

print(p_person_betweenness)
ggsave("P2P_betweenness.png", p_person_betweenness, width = 12, height = 8, dpi = 300)

# 社区检测
communities_person <- cluster_louvain(as.igraph(g_person))

g_person_community <- g_person %>%
  activate(nodes) %>%
  mutate(community = as.factor(membership(communities_person)))

p_person_community <- ggraph(g_person_community, layout = 'fr') +
  geom_edge_link(aes(width = weight), alpha = 0.2, color = 'gray70') +
  geom_node_point(aes(size = degree_network, color = community), alpha = 0.8) +
  geom_node_text(aes(label = id), repel = TRUE, size = 2.5, max.overlaps = 15) +
  scale_edge_width(range = c(0.5, 3)) +
  scale_size_continuous(range = c(2, 12)) +
  theme_graph() +
  labs(title = '人物网络社区结构',
       subtitle = paste('检测到', length(unique(membership(communities_person))), '个社区'),
       color = 'Community', size = 'Degree')

print(p_person_community)
ggsave("P2P_community.png", p_person_community, width = 12, height = 8, dpi = 500)

# 导出P2P结果
write.csv(edges_filtered_person, 'edges_person_filtered.csv', row.names = FALSE)
write.csv(g_person %>% activate(nodes) %>% as_tibble(), 
          'nodes_person_filtered.csv', row.names = FALSE)
write.csv(g_person_community %>% activate(nodes) %>% as_tibble() %>%
            select(id, degree, degree_network, betweenness, community) %>%
            arrange(community, desc(degree_network)),
          'person_community_info.csv', row.names = FALSE)
