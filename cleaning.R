d$share_diff <- d$indegree_share - d$outdegree_share
d$osotua_shared <- round(d$osotua_shared)
osotua_net <- induced_subgraph(gs, vids=1:nrow(d))
osotua_net <- induced_subgraph(osotua_net, vids=V(osotua_net)[components(osotua_net)$membership==1])
d$age_set <- factor(d$age_set, levels=c(
  'Nyangulo',
  'Irkorianga',
  'Irkidotu',
  'Irkishomo',
  'Irseuri',
  'Nyangusi',
  'Irtorito'
))
